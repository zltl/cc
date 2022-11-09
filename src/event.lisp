(in-package :cc-event)


;; use pthread globally
(cc-libevent:evthread-use-pthreads)


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

(defun event-types (ev)
  "Get the event that triggered, only canbe call from event
callback functions."
  (event-triggered-event-types ev))



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

   (dns-c :accessor base-dns-c :initarg dns-c :initform nil
	  :documentation "Holds the C object that evdns_base-new()
	  created.")
   
   (ev :accessor base-ev :initarg ev :initform nil
       :documentation "Holds the event for running differ queues tasks")

   (id :accessor base-id :initarg :id :initform (next-id))

   (defer-task-queue
       :accessor base-defer-task-queue :initarg defer-task-queue
       :initform (lparallel.queue:make-queue
		  :fixed-capacity *default-defer-task-queue-size*))

   (lock :accessor base-lock :initarg :lock :initform (bt:make-lock))

   (started :accessor base-started :initarg :started :initform nil
	    :documentation "Flag to specify that if loop started and
	    not stop yet."))

  (:documentation
   "A class that holds event base and all the state it manages."))



;; Callback function call be libevent when event trigger.
;; Find the lispy event from hash-table and call the real
;; function in lisp.
(defcallback base-event-callback :void
    ((fd :int) (event-types :short) (arg :pointer))
  ;; arg always be pointer to event it self
  (let* ((ev (event-table-get arg))
	 (cb-args (event-cb-arg-list ev)))
    (if ev
        
	;; (apply (alexandria:curry (event-cb ev) ev) cb-args)
	;; seems APPLY is enough
	(progn
	  (setf (event-triggered-event-types ev) event-types)
	  (apply (event-cb ev) ev cb-args))

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

(defun event-free (event)
  "Deallocate event struct and free c event. If the event is pending or
active, this function makes it non-pending and non-active first."
  (defer-submit (event-base event)
      (lambda()
	(and (event-c event)
	    (cc-libevent:event-free (event-c event))
	    (event-table-del (event-c event))
	    (setf (event-c event) nil)))))

(defun event-add (ev timeout)
  "Add event to the set of pending events
EB: base
EV: the event struct
TIMEOUT: '(second, microsecond) the time to wait for the event"
  (defer-submit (event-base ev)
      (lambda ()
	(and (event-c ev)
	     (let ((eb (event-base ev)))
	       (cc-timeval:with-c-timeval-values tv timeout
		 (cc-libevent:event-add (event-c ev) tv)))))))

(defun event-del (event)
  "Remove an event from the set of monitored events."
  (defer-submit (event-base ev)
      (lambda ()
	(and (event-c event)
	     (cc-libevent:event-del (event-c event))))))

  


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

(defmethod defer-submit ((eb base) cb &optional &rest cb-args)
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

(defmethod timer-submit ((eb base) timeout cb &optional &rest cb-args)
  "Create, pending and return timer event. The return value can be use
  EVENT-FREE to cancel

TIMEOUT: '(second, microsecond) the time to wait for the event
CB: callback function when timer trigger. Form as (cb event event-types cb-arg)
CB-ARG-LIST: arguments of cb
"
  ;; for multiple thread app, we add timer task in a defer task
  (let* ((timer (make-timer :cb cb :cb-args cb-args))
	 (event (event-new eb -1 0 #'timer-base-callback timer)))
    (event-add event timeout)
    event))

(defmethod base-init/nolock ((eb base))
  "Alloc c and ev"
  ;; just return if already init
  
  (or (base-c eb)
      
      (let* ((eb-c (cc-libevent:event-base-new))
	     ;; new dns-base
	     (dns-c nil)
	     ;; new defer task notificer event
	     (eb-ev nil))
        
	;; error when return null
	(and (null-pointer-p eb-c)
	     (error cc-error:oom :msg "event-base-new"))

 	(setf dns-c
 	      (cc-libevent:evdns-base-new
 	       eb-c
 	       cc-libevent:*EVDNS-BASE-INITIALIZE-NAMESERVERS*))
	
	(and (null-pointer-p dns-c)
	     (progn
	       (cc-libevent:event-base-free eb-c)
	       (error cc-error:oom :msg "evdns-base-new")))

	(setf (base-c eb) eb-c)
	(setf (base-dns-c eb) dns-c)	
	(setf eb-ev (event-new eb -1 cc-libevent:*EV-PERSIST*
			       #'defer-task-callback))
	(setf (base-ev eb) eb-ev)))
  eb)

(defmethod base-init ((eb base))
  "Initialize base"
  (bt:with-lock-held ((base-lock eb))
    (base-init/nolock eb)))

(defmethod base-deinit/nolock ((eb base))
  "Dealloc c and ev"

  (and (base-ev eb)
       (cc-libevent:event-free (event-c (base-ev eb))))
  (and (base-dns-c eb)
       (cc-libevent:evdns-base-free (base-dns-c eb) 0))
  (and (base-c eb)
       (cc-libevent:event-base-free (base-c eb)))
  (setf (base-ev eb) nil)
  (setf (base-c eb) nil)
  (setf (base-dns-c eb) nil))

(defmethod base-deinit ((eb base))
  "Uninitialize base"
  (bt:with-lock-held ((base-lock eb))
    (base-deinit/nolock eb)))

(defmethod base-loop-started-p ((eb base))
  "Return t if loop started"
  (bt:with-lock-held ((base-lock eb))
    (base-started eb)))

(defmethod base-loop-start ((eb base))
  "Wait for events to become active, and run their callbacks."
  ;; start loop
  (or (base-loop-started-p eb)
      (progn
	(bt:with-lock-held ((base-lock eb))
	  (setf (base-started eb) t))
	(cc-libevent:event-base-loop
	 (base-c eb)
	 cc-libevent:*EVLOOP-NO-EXIT-ON-EMPTY*)
	(bt:with-lock-held ((base-lock eb))
	  (setf (base-started eb) nil)))))

(defmethod base-loop-stop ((eb base))
  "Stop the event loop."
  (and (base-loop-started-p eb)
       (cc-libevent:event-base-loopexit (base-c eb) (null-pointer))))

