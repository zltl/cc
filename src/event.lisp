(in-package :cc-event)

;; id generator for base:id
(defvar *event-base-next-id* 0)
(defvar *event-base-id-lock* (bt:make-lock))
(defun next-id ()
  "Advance *event-base-next-id* and return old value."
  (bt:with-lock-held (*event-base-id-lock*)
    (let* ((r *event-base-next-id*)
	   (*event-base-next-id* (+ r 1)))
      r)))


;; event-pointer -> callback-item
(defstruct event
  ;; result of event_new()  
  c
  ;; the base object  
  base 

  ;; default callback function  
  ;; FORM: (cb-name event-struct event-types cb-arg)
  cb 

  ;; argument of callback function  
  cb-arg)


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
  (bt:with-lock-held (*event-table*)
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
(defcallback base-ev-callback :pointer
    ((fd :int) (event-types :short) (arg :pointer))
  ;; arg always be pointer to event it self
  (let ((ev (event-table-get arg)))
    (if ev
	;; call callback if ev exists
	(funcall (event-cb ev)
		 ev
		 event-types
		 (event-cb-arg ev))
	;; log error if event-struct not exists
	;; MUST be a bug here
	(log:error "event not found ~a" arg))))


;; struct holding defer task fn and arg
(defstruct defer-task
  ;; function
  ;; form (cb-name eb cb-arg)
  cb
  ;; argument
  cb-arg)

(defmethod defer-task-runner ((eb base) event)
  "Pop all task and run until queue empty"
  (loop for task = (lparallel.queue:try-pop-queue (base-defer-task-queue eb))
	while task
	do             
	   ;; apply function
	   (funcall (defer-task-cb task) eb (defer-task-cb-arg task))))

(defun defer-task-cb (event event-types arg)
  "Callback to run defer task of base."
  (defer-task (base event) event))


(defmethod event-new ((eb base) fd event-types cb arg)
  "Create a event struct and register callback functions."
  (let ((event (make-event :base eb :cb cb :cb-arg arg))
	(event-c (cc-libevent:event-new (base-c c)
					fd
					event-types
					(callback base-even-callback)
					(cc-libevent:event-self-cbarg))))
    (if (null-pointer-p event-c) (error oom :msg "event-new"))
    (setf (event-c event) event-c)
    (event-table-set event-c event)
    event-c))

(defmethod event-free (event)
  "Deallocate event struct and free c event. If the event is pending or
active, this function makes it non-pending and non-active first."
  (cc-libevent:event-free (event-c event))
  (event-table-del (event-c event)))

(defmethod %init ((eb base))
  "Alloc c and ev"
  (let* ((eb-c (cc-libevent:event-base-new))
	 ;; new defer task notificer event
	 (eb-ev (event-new eb -1 cc-libevent:*EV-PERSIST* #'defer-task-cb arg)))

    ;; error when return null
    (and (null-pointer-p eb-c)
	 (error cc-error:oom :msg "event-base-new"))))

(defmethod %deinit ((eb base))
  "Dealloc c and ev"
  (cc-libevent:event-free (base-ev eb))
  (cc-libevent:event-base-free (base-c eb))
  (setf (base-ev eb) nil)
  (setf (base-c eb) nil))

(defmethod %start ((eb base))
  (unwind-protect
       (progn
	 (%init eb) ;; alloc base-c and base-ev
         
	 ;; start loop
	 (cc-libevent:event-base-loop
	  (base-c eb)
	  cc-libevent:*EVLOOP-NO-EXIT-ON-EMPTY*))
    
    (progn
      (log:debug "event base ~a exited" (base-id eb))
      (%deinit eb)))
  )

(defmethod start ((eb base))
  "Init and start the event loop"
  (bt:with-lock-held ((base-lock eb))
    (if (base-c eb)
	(error cc-error:already-start :msg "event base already start")
	(%start eb))))


