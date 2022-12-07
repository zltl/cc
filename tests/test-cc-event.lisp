(in-package :cc-tests)

;; Define your project tests here...

(ql:quickload "cc")

(def-suite eventest
  :description "Test event loop")
(in-suite eventest)

(defun wait-base-start (eb)
  (loop while (not (cc-event:base-loop-started-p eb))
	do
	   (sleep 0.1)))

(defun base-create-init ()
  "Test cc-event:base create init functions"
  (let ((eb (make-instance 'cc-event:base))
	(st-thread nil))

    (cc-event:base-init eb)
    
    (setf st-thread
	  (bt:make-thread
	   (lambda ()
             (wait-base-start eb)
	     (log:debug "stop thread")       
	     (cc-event:base-loop-stop eb)
	     (log:debug "stop thread exit"))))

    (log:debug "start main")
    (cc-event:base-loop-start eb)
    (log:debug "stop main")
    (cc-event:base-deinit eb)

    (bt:join-thread st-thread)
    (log:debug "Joined"))
  1)

(defun defer-task-test (n)
  "test cc-event defer task"
  (let ((eb (make-instance 'cc-event:base))
	(thread nil)
	(lock (bt:make-lock))
	(run-flag 0))

    (cc-event:base-init eb)
    
    (setf thread
	  (bt:make-thread
	   (lambda ()
	     (wait-base-start eb)
	     (log:debug "loop started")

	     (loop repeat n
		   do
		      (cc-event:defer-submit
			  eb
			  (lambda (arg1 arg2 arg3)
			    (bt:with-lock-held (lock)
			      (setf run-flag (+ run-flag 1))))
			1 2 3))
             
	     (loop while (bt:with-lock-held (lock) (> n run-flag))
		   do (sleep 0.1))
	     (log:debug "stoping loop")
	     (cc-event:base-loop-stop eb))))

    (log:debug "starting loop")
    (cc-event:base-loop-start eb)
    (cc-event:base-deinit eb)
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

    (cc-event:base-init eb)
    (log:info "submit eb-c: ~a" (cc-event:base-c eb))
    (log:debug "trace loop started: ~a"
	       (cc-event:base-loop-started-p eb))
    
    (setf thread
	  (bt:make-thread
	   (lambda ()
	     (log:debug "in lambda eb-c: ~a"
			(cc-event:base-c eb))
	     (wait-base-start eb)
	     (log:debug "loop started: ~a"
			(cc-event:base-loop-started-p eb))

 	     (loop for i from 0 upto (- cnt 1)
		   do
		      (sleep 0.1)
		      (log:info "in loop eb-c=~a" (cc-event:base-c eb))
		      (log:info "submit ~a at ~a ---"
				i (local-time:now))
		      (cc-event:timer-submit
		       eb
		       '(1 0)
		       (lambda (i j k)
			 (let ((f (bt:with-lock-held (lock)
				    (log:info "callback ~a at ~a ~a ~a" i (local-time:now) j k)
				    (setf timer-run-flag (+ timer-run-flag 1)))))))
		       i 0 0))
             
	     (loop while (> cnt (bt:with-lock-held (lock) timer-run-flag))
		   do
		      (sleep 0.1))
	     (cc-event:base-loop-stop eb))))

    (log:debug "starting loop")
    (cc-event:base-loop-start eb)
    (cc-event:base-deinit eb)
    (log:debug "loop stoped")

    (bt:join-thread thread)
    (log:debug "Joined")
    timer-run-flag))


(defun resolve-test (host)
  (let ((eb (make-instance 'cc-event:base))
	(thread nil)
	(lock (bt:make-lock))
	(cb-flag nil))

    (cc-event:base-init eb)
    
    (setf thread
	  (bt:make-thread
	   (lambda ()
	     (log:debug "in lambda eb-c: ~a"
			(cc-event:base-c eb))
	     (wait-base-start eb)
	     (log:debug "loop started: ~a"
			(cc-event:base-loop-started-p eb))

	     (cc-net:dns-lookup
	      eb host
	      (lambda (iplist  a b c d)
		(bt:with-lock-held (lock) (setf cb-flag t))
		(dolist (ip iplist)
		  (log:info "get ip: ~a" (cc-net:ip-to-string ip))))
	      1 2 3 4)
	     (loop while (not (bt:with-lock-held (lock) cb-flag))
		   do
		      (sleep 0.1))	     
	     (cc-event:base-loop-stop eb))))

    (log:debug "starting loop")
    (cc-event:base-loop-start eb)
    (cc-event:base-deinit eb)
    (log:debug "loop stoped")

    (bt:join-thread thread)
    (log:debug "Joined"))
  1)

;; (resolve-test "ip6-allnodes")

(defun tcp-connect-test (n)
  "test cc-event tcp task"

  (cc-event:with-base-loop (eb)
    (cc-net:bufev-with-tcp-connect
     eb :host "quant67.com" :port 80
     :read-cb 
       (lambda (e)
	 (log:info "read-cb")
	 (let* ((x (cc-net:bufev-read e 10000)))
	   (log:info "read size ~a" (length x))
	   ;; (log:info "reading ~&~a " (cc-net:buffer-vec-to-string x))
	   (cc-net:bufev-free e)
	   (cc-event:base-loop-stop eb)))
     :write-cb
     (lambda (e)
       (log:info "write-cb")
       (cc-net:bufev-enable e cc-net:*EV-READ*))
     :event-cb 
     (lambda (e what)
       (log:info "event-cb ~X" what)
       (if (equal what cc-net:*BEV-EVENT-CONNECTED*)
	   (progn
	     (log:info "connected")
	     (cc-net:bufev-write-string
	      e
	      "GET / HTTP/1.1
Host: quant67.com

")
	     (cc-net:bufev-enable e
				  cc-net:*EV-READ*)))))    
    )
  1)


(defun with-loop-test ()
  (cc-event:with-base-loop (eb)
    (log:warn "base loop started")
    (cc-event:base-loop-stop eb))
  1)

(defun tls-connect-test ()
  "test cc-event tcp task"

  (cc-event:with-base-loop (eb)
    (cc-net:bufev-with-tls-connect
     eb :host "quant67.com" :port 443
     :read-cb 
       (lambda (e)
	 (log:info "read-cb")
	 (let* ((x (cc-net:bufev-read e 10000)))
	   (log:info "read size ~a" (length x))	   
	   ;; (log:info "reading ~&~a " (cc-net:buffer-vec-to-string x))
	   (cc-net:bufev-free e)
	   (cc-event:base-loop-stop eb)))
     :write-cb
     (lambda (e)
       (log:info "write-cb")
       (cc-net:bufev-enable e cc-net:*EV-READ*))
     :event-cb 
     (lambda (e what)
       (log:info "event-cb ~X" what)
       (if (equal what cc-net:*BEV-EVENT-CONNECTED*)
	   (progn
	     (log:info "connected")
	     (cc-net:bufev-write-string
	      e
	      "GET / HTTP/1.1
Host: quant67.com

")
	     (cc-net:bufev-enable e
				  cc-net:*EV-READ*)))))
    )
  1)

(test event
      (is (= 1 (base-create-init)))
      (is (= 100 (defer-task-test 100)))
      (is (= 3 (timer-test 3)))
      (is (= 1 (resolve-test "quant67.com")))
      (is (= 1 (with-loop-test)))
      (is (= 1 (tcp-connect-test 1)))
      (is (= 1 (tls-connect-test))))

