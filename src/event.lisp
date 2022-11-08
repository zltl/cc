(in-package :cc-event)


;; id generator for base:id
;; class BASE have an id field, function next-id generate an autoincrement
;; integer id for it.
(defvar *event-base-next-id* 0)
(defvar *event-base-id-lock* (bt:make-lock))
(defun next-id ()
  "Advance *event-base-next-id* and return old value."
  (bt:with-lock-held (*event-base-id-lock*)
    (let* ((r *event-base-next-id*)
	   (*event-base-next-id* (+ r 1)))
      r)))


;; wraper for C libevent's event, containing C event, base instance and
;; callback functions.
(defstruct event
  ;; result of event_new()  
  c
  ;; the base object  
  base 

  ;; default callback function  
  ;; FORM: (cb event &optional arg1 arg2 arg3 ...)
  cb

  ;; argument list of callback function  
  cb-arg-list

  ;; the event types that trigger
  triggered-event-types)

(defun event-fd (ev)
  "Get the socket or signal assigned to an event, or -1 if the
event has no socket."
  (cc-libevent:event-get-fd (event-c ev)))



;; map from C event pointer to struct event
(defvar *event-table* (make-hash-table :test #'eql))
(defvar *event-table-lock* (bt:make-lock))
(defun event-table-get (pointer)
  "Get event struct by pointer form hash table *event-table*"
  (bt:with-lock-held (*event-table-lock*)
    (gethash (cc-util:make-pointer-eql-able pointer)
	     *event-table*)))
(defun event-table-set (pointer event)
  "Set pointer-> event-struct into hash table *event-table*"
  (bt:with-lock-held (*event-table-lock*)
    (setf (gethash (cc-util:make-pointer-eql-able pointer)
		   *event-table*)
	  event)))
(defun event-table-del (pointer)
  "Clear out event for the givent pointer"
  (bt:with-lock-held (*event-table-lock*)
    (remhash (cc-util:make-pointer-eql-able pointer)
	     *event-table*)))


;; default defer queue size 
(defconstant *default-defer-task-queue-size* 40000)

;; the core dispather class
(defclass base ()
  ((c :accessor base-c :initarg c :initform nil
      :documentation "Holds the C object that event_base_new() created.")

   (ev :accessor base-ev :initarg ev :initform nil
       :documentation "Holds the event for running differ queues tasks")

   (id :accessor base-id :initarg :id :initform (next-id))

   (defer-task-queue
       :accessor base-defer-task-queue :initarg defer-task-queue
       :initform (lparallel.queue:make-queue
		  :fixed-capacity *default-defer-task-queue-size*))

   (lock :accessor base-lock :initarg :lock :initform (bt:make-lock)))

  (:documentation
   "A class that holds event base and all the state it manages."))



;; Callback function that event base differ task will excecute
(defcallback base-event-callback :void
    ((fd :int) (event-types :short) (arg :pointer))
  ;; arg always be pointer to event it self
  (let* ((ev (event-table-get arg))
	 (cb-args (event-cb-arg-list ev)))
    (if ev
	;; call cb if ev exists
	;; (apply (alexandria:curry (event-cb ev) ev) cb-args)
	;; seems APPLY is enough
	(apply (event-cb ev) ev cb-args)	

	;; log error if event-struct not exists
	;; MUST be a bug here
	(log:error "event not found ~a" arg))))

(defmethod event-new ((eb base) fd event-types cb &optional &rest arg-list)
  "Create a event struct and register callback functions."
  (let ((event (make-event :base eb :cb cb :cb-arg-list arg-list))
	(event-c (cc-libevent:event-new (base-c eb)
					fd
					event-types
					(callback base-event-callback)
					(cc-libevent:event-self-cbarg))))
    (if (null-pointer-p event-c) (error oom :msg "event-new"))
    (setf (event-c event) event-c)
    (event-table-set event-c event)
    event))

(defmethod event-free (event)
  "Deallocate event struct and free c event. If the event is pending or
active, this function makes it non-pending and non-active first."
  (cc-libevent:event-free (event-c event))
  (event-table-del (event-c event)))



;; struct holding defer task fn and arg
(defstruct defer-task
  ;; function
  ;; form (cb-name cb-arg1 cb-arg2 cb-arg2)
  cb
  ;; argument list
  cb-args)

(defmethod defer-task-runner ((eb base) event)
  "Pop all task and run until queue empty"
  (loop for task = (lparallel.queue:try-pop-queue (base-defer-task-queue eb))
	while task
	do             
	   ;; apply function
	   (apply (defer-task-cb task)
		  (defer-task-cb-args task))))

(defun defer-task-callback (event)
  "Callback of defer task event."
  (defer-task-runner (event-base event) event))

(defmethod defer-submmit ((eb base) cb &optional &rest cb-args)
  "Submit defer task."
  (lparallel.queue:push-queue
   (make-defer-task :cb cb :cb-args cb-args)
   (base-defer-task-queue eb))
  (cc-libevent:event-active (event-c (base-ev eb)) 0 0))


;; timer

(defstruct timer
  ;; the callback
  ;; FORM as (cb cb-arg)
  cb
  ;; arguments of callback
  cb-args)

(defun timer-base-callback (event timer)
  "callback function of timer, arg always be the timer object"
  ;; callback
  (apply (timer-cb timer) (timer-cb-args timer))
  ;; free event
  (event-free event))

(defun real-timer-submit (eb ttl cb &optional &rest cb-args)
  "Create a timer event.
TTL: '(second, microsecond) the time to wait for the event
CB: callback function when timer trigger. Form as (cb event event-types cb-arg)
CB-ARG-LIST: arguments of cb
"
  (let* ((timer (make-timer :cb cb :cb-args cb-args))
	 (event (event-new eb -1 0 #'timer-base-callback timer))
	 (res 0))
    (cc-timeval:with-c-timeval-value
	tv (car ttl) (car (cdr ttl))
      (cc-libevent:event-add (event-c event) tv))))

(defmethod timer-submit ((eb base) ttl cb &optional &rest cb-args)
  "Create a timer event.
TTL: '(second, microsecond) the time to wait for the event
CB: callback function when timer trigger. Form as (cb event event-types cb-arg)
CB-ARG-LIST: arguments of cb
"
  ;; for multiple thread app, we add timer task in a defer task
  (apply #'defer-submmit eb #'real-timer-submit eb ttl cb cb-args))

(defmethod %init ((eb base))
  "Alloc c and ev"
  ;; just return if already init
  (if (base-c eb)
      ()
      (let* ((eb-c (cc-libevent:event-base-new))
	     ;; new defer task notificer event
	     (eb-ev nil))
	;; error when return null
	(and (null-pointer-p eb-c)
	     (error cc-error:oom :msg "event-base-new"))
	(setf (base-c eb) eb-c)
	(setf eb-ev (event-new eb -1 cc-libevent:*EV-PERSIST*
			       #'defer-task-callback))
	(setf (base-ev eb) eb-ev))))

(defmethod %deinit ((eb base))
  "Dealloc c and ev"
  (if (base-c eb)
      (progn (cc-libevent:event-free (event-c (base-ev eb)))
	     (cc-libevent:event-base-free (base-c eb))
	     (setf (base-ev eb) nil)
	     (setf (base-c eb) nil))))

(defmethod %start ((eb base))
  (unwind-protect
       (progn
	 (bt:with-lock-held ((base-lock eb))
	   (%init eb))
	 ;; start loop
	 (cc-libevent:event-base-loop
	  (base-c eb)
	  cc-libevent:*EVLOOP-NO-EXIT-ON-EMPTY*))
    
    (progn
      (log:debug "event base ~a exited" (base-id eb))
      (bt:with-lock-held ((base-lock eb))
	(%deinit eb)))))

(defmethod start ((eb base))
  "Init and start the event loop"
  (cc-libevent:evthread-use-pthreads)
  (%start eb))

(defmethod started-p ((eb base))
  "Return t if loop started, else nil."
  (bt:with-lock-held ((base-lock eb))
    (and (base-c eb) (base-ev eb))))

(defmethod stop ((eb base))
  "Stop the event loop."
  (cc-libevent:event-base-loopexit (base-c eb) (null-pointer)))
