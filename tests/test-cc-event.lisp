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
    (cc-net:bufev-tcp-connect-with-cb
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
       (cc-net:bufev-enable e cc-net:+EV-READ+))
     :event-cb 
     (lambda (e what)
       (log:info "event-cb ~X" what)
       (if (equal what cc-net:+BEV-EVENT-CONNECTED+)
	   (progn
	     (log:info "connected")
	     (cc-net:bufev-write-string
	      e
	      "GET / HTTP/1.1
Host: quant67.com

")
	     (cc-net:bufev-enable e
				  cc-net:+EV-READ+)))))    
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
    (cc-net:bufev-tls-connect-with-cb
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
       (cc-net:bufev-enable e cc-net:+EV-READ+))
     :event-cb 
     (lambda (e what)
       (log:info "event-cb ~X" what)
       (if (equal what cc-net:+BEV-EVENT-CONNECTED+)
	   (progn
	     (log:info "connected")
	     (cc-net:bufev-write-string
	      e
	      "GET / HTTP/1.1
Host: quant67.com

")
	     (cc-net:bufev-enable e
				  cc-net:+EV-READ+)))))
    )
  1)


(defun tcp-listen-test (n)
  "test cc-event tcp task"

  (defconstant PORT 8888)
  
  (cc-event:with-base-loop (eb)
    ;; listen
    (let ((sockstr (concatenate 'string
				"[::]:"
				(write-to-string PORT))))
      (cc-net:listener-new
       eb sockstr
       :cb
       (lambda (lev fd sock)
	 (log:info "listener -cb ---")
	 (let ((econ nil))
	   (setf econ (cc-net:bufev-socket-new
		       eb fd cc-net:+BEV-OPT-CLOSE-ON-FREE+))
	   (cc-net:bufev-setcb
	    econ
	    :read-cb
	    (lambda (e)
	      (log:info "s/read-cb")
	      (let* ((x (cc-net:bufev-read e 10000)))
		(log:info "s/echoing: ~a" (cc-net:buffer-vec-to-string x))
		(cc-net:bufev-write e x)))
	    :write-cb
	    (lambda (e)
	      (log:info "s/write-cb"))
	    :event-cb
	    (lambda (e what)
	      (log:info "s/event-cb what=~a" what)
	      (if (/= 0 (logand what cc-net:+BEV-EVENT-EOF+))
		  (progn
		    (log:info "s/close...")
		    (cc-net:bufev-free e)
		    ;; free listener
		    (log:info "s/free lev")
		    (cc-net:listener-free lev)
		    (log:info "s/stop loop")
		    (cc-event:base-loop-stop eb)))))

	   (cc-net:bufev-enable econ (logior cc-net:+EV-READ+
					     cc-net:+EV-WRITE+)))))

      ;; client connect
      (cc-event:defer-submit eb
	  (lambda ()
	    (cc-net:bufev-tcp-connect-with-cb
	     eb :host "127.0.0.1" :port PORT
		:read-cb
		(lambda (e)
		  (log:info "c/reac-cb")
		  (let ((x (cc-net:bufev-read e 10000)))
		    (log:info "c/read ~a  ~&c/closing..." (cc-net:buffer-vec-to-string x)))
		  (cc-net:bufev-free e))

		:write-cb
		(lambda (e)
		  (log:info "c/write-cb"))

		:event-cb
		(lambda (e what)
		  (log:info "c/event-cb ~X" what)
		  (cc-net:bufev-write-string e "hello")		  
		  (cc-net:bufev-enable e
				       (logior cc-net:+EV-WRITE+
					       cc-net:+EV-READ+))))))))
  1)


(defun http-client-test ()
  "test cc-event http task"

  (cc-event:with-base-loop (eb)
    (let ((hcon (cc-http:http-conn-new eb "http://quant67.com:80"))
	  (r nil))
      (setf r (cc-http:request-new
	       :cb
	       (lambda (req)
		 (log:info "request done: ~a"
			   (http:request-get-response-code req))
		 (http:request-free r)
		 (cc-event:base-loop-stop eb))))

      (cc-http:request-do hcon r http:+get+)))
  1)

(defun http-server-test ()
  (cc-event:with-base-loop (eb)
    (let ((s (http:server-new eb)))
      (http:server-bind s
			(cc-net:sockaddr-from-string "0.0.0.0:8899"))
      (http:server-set-default-content-type s "text/html")
      (http:server-set-cb s
			  :cb
			  (lambda (req)
			    (log:info "requesting...")
			    (let ((buf (net:buffer-new)))
			      (net:buffer-add-string buf "OK HTTP SERVER")
			      (http:request-reply req
						  http:+ok+
						  buf)
			      (net:buffer-free buf)
			      (log:info "response..."))))

      (event:defer-submit
	  eb
	  (lambda ()
	    (let ((hcon (cc-http:http-conn-new eb "http://localhost:8899"))
		  (r nil))

	      (log:info "before new")
	      (setf r (cc-http:request-new
		       :cb
		       (lambda (req)
			 (log:info "request done: ~a"
				   (http:request-get-response-code req))
			 
			 (http:request-free r)
			 (http:server-free s)
			 (cc-event:base-loop-stop eb))))
	      (log:info "after new")
	      (cc-http:request-do hcon r http:+get+))))))
  1)


(defun http-mux-test ()
  (cc-event:with-base-loop (eb)
    (let ((s (http:server-new eb))
	  (mux (http:mux-new)))
      ;; mux
      (http:mux-get mux "/"
		    (lambda (req)
		      (log:info "/ => lllllllll")
		      (http:request-reply-error req http:+ok+)))

      ;; server
      (http:server-bind s
			(cc-net:sockaddr-from-string "0.0.0.0:8899"))
      (http:server-set-default-content-type s "text/html")

      (http:server-set-cb s
			  :cb
			  (http:mux-serve mux))
      ))
  1)

;; (http-mux-test)

(test event
      (is (= 1 (base-create-init)))
      (is (= 100 (defer-task-test 100)))
      (is (= 3 (timer-test 3)))
      (is (= 1 (resolve-test "quant67.com")))
      (is (= 1 (with-loop-test)))
      (is (= 1 (tcp-connect-test 1)))
      (is (= 1 (tls-connect-test)))
      (is (= 1 (tcp-listen-test 1)))
      (is (= 1 (http-client-test)))
      (is (= 1 (http-server-test))))

