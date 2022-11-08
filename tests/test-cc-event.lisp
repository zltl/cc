(in-package :cc-tests)

;; Define your project tests here...

(ql:quickload "cc")

(defun wait-base-start (eb)
  (loop while (not (cc-event:started-p eb))
	do
	   (sleep 0.1)))

(defun base-create-init ()
  "Test cc-event:base create init functions"
  (let ((eb (make-instance 'cc-event:base))
	(st-thread nil))

    (setf st-thread
	  (bt:make-thread
	   (lambda ()
             (wait-base-start eb)
	     (log:debug "stop thread")       
	     (cc-event:stop eb)
	     (log:debug "stop thread exit"))))

    (log:debug "start main")
    (cc-event:start eb)
    (log:debug "stop main")

    (bt:join-thread st-thread)
    (log:debug "Joined"))
  1)

(defun defer-task-test (n)
  "test cc-event defer task"
  (let ((eb (make-instance 'cc-event:base))
	(thread nil)
	(lock (bt:make-lock))
	(run-flag 0))

    (setf thread
	  (bt:make-thread
	   (lambda ()
	     (wait-base-start eb)
	     (log:debug "loop started")

	     (loop repeat n
		   do
		      (cc-event:defer-submmit
			  eb
			  (lambda (arg1 arg2 arg3)
			    (log:debug "defer-task callback invoked")
			    (bt:with-lock-held (lock)
			      (setf run-flag (+ run-flag 1))))
			1 2 3))
             
	     (loop while (bt:with-lock-held (lock) (> n run-flag))
		   do (sleep 0.1))
	     (log:debug "stoping loop")
	     (cc-event:stop eb))))

    (log:debug "starting loop")
    (cc-event:start eb)
    (log:debug "loop stoped")

    (bt:join-thread thread)
    (log:debug "Joined")
    run-flag))

(defun timer-test (cnt)
  "test cc-event defer task"
  (let ((eb (make-instance 'cc-event:base))
	(thread nil)
	(lock (bt:make-lock))
	(timer-run-flag 0))

    (setf thread
	  (bt:make-thread
	   (lambda ()
	     (wait-base-start eb)
	     (log:debug "loop started")

 	     (loop for i from 0 upto (- cnt 1)
		   do
		      (sleep 1)
		      (log:info "submit ~a at ~a" i (local-time:now))
		      (cc-event:timer-submit
		       eb
		       '(3 0)
		       (lambda (i j k)
			 (let ((f (bt:with-lock-held (lock)
				    (log:info "callback ~a at ~a ~a ~a" i (local-time:now) j k)
				    (setf timer-run-flag (+ timer-run-flag 1)))))))
		       i 0 0))
             
	     (loop while (> cnt (bt:with-lock-held (lock) timer-run-flag))
		   do
		      (sleep 0.1))
	     (cc-event:stop eb))))

    (log:debug "starting loop")
    (cc-event:start eb)
    (log:debug "loop stoped")

    (bt:join-thread thread)
    (log:debug "Joined")
    timer-run-flag))

(test event
      (is (= 1 (base-create-init)))
      (is (= 100 (defer-task-test 100)))
      (is (= 3 (timer-test 3))))

