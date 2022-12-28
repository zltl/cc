(in-package :cc-net)

(defstruct bufev
  ;; result of bufferevent_new()
  c

  ;; the base object
  base

  ;; (read-cb bev cb-args...)
  read-cb
  ;; (write-cb bev cb-args...)
  write-cb
  ;; (event-cb bev what cb-args...)
  event-cb

  ;; arguments of readcb, writecb, errorcb
  cb-args

  ;; ssl
  ssl
  ;; ssl-ctx
  ssl-ctx  
  )

(defconstant +EV-TIMEOUT+ #x01)
(defconstant +EV-READ+ #x02)
(defconstant +EV-WRITE+ #x04)
(defconstant +EV-SIGNAL+ #x08)
(defconstant +EV-PERSIST+ #x10)
(defconstant +EV-ET+ #x20)
(defconstant +EV-FINALIZE+ #x40)
(defconstant +EV-CLOSED+ #x80)

(defconstant +BEV-OPT-CLOSE-ON-FREE+ #x01)
(defconstant +BEV-OPT-THREADSAFE+ #x02)
(defconstant +BEV-OPT-DEFER-CALLBACKS+ #x04)
(defconstant +BEV-OPT-UNLOCK-CALLBACKS+ #x08)

(defconstant +BEV-EVENT-READING+ #x01)
(defconstant +BEV-EVENT-WRITING+ #x02)
(defconstant +BEV-EVENT-EOF+ #x10)
(defconstant +BEV-EVENT-ERROR+ #x20)
(defconstant +BEV-EVENT-TIMEOUT+ #x40)
(defconstant +BEV-EVENT-CONNECTED+ #x80)

(defun bufev-socket-new (bev fd options)
  "Create a new socket bev over an existing socket.
BEV: the base instance
FD: the file descriptor from which data is read and written to, or -1.
OPTIONS: zero or more +BEV-OPT-* flags
return an instance of struct bufev  
"
  (let* ((e (make-bufev :base bev))
	 (c (cc-libevent:bufferevent-socket-new (base-c bev) fd options)))
    (if (null-pointer-p c)
	(error (cc-error:oom "bufferevent-socket-new")))
    (setf (bufev-c e) c)
    (event-table-set c e)
    e))

(defun bufev-tls-socket-new (bev fd options)
  ;; create SSL*
  (let* ((method (cc-openssl:tlsv1.2-method))
	 (ctx (cc-openssl:ssl-ctx-new method))
	 (ssl nil))
    (if (null-pointer-p ctx)
	(progn (cc-openssl:perror)
	       (cc-error:oom "ssl-ctx-new")))
    (setf ssl (cc-openssl:ssl-new ctx))
    (if (null-pointer-p ssl)
	(progn
	  (cc-openssl:perror)
	  (cc-error:oom "ssl-new")))

    ;; create bufferevent
    (let* ((e (make-bufev :base bev))
	   (c (cc-libevent:bufferevent-openssl-socket-new
	       (base-c bev)
	       fd
               ssl
               1 ;; BUFFEREVENT_SSL_CONNECTING
	       options)))
      (if (null-pointer-p c)
	  (error (cc-error:oom "bufferevent-openssl-socket-new")))
      (setf (bufev-c e) c)
      (event-table-set c e)
      e)))

(defun bufev-new (bev options)
  (bufev-socket-new bev -1 options))
(defun bufev-tls-new (bev options)
  (bufev-tls-socket-new bev -1 options))

(defun bufev-free (e)
  "Deallocate the bufev instance E."
  (cc-libevent:bufferevent-free (bufev-c e))
  (if (bufev-ssl e) (cc-openssl:ssl-free (bufev-ssl e)))
  (if (bufev-ssl-ctx e) (cc-openssl:ssl-ctx-free (bufev-ssl-ctx e))))

(defcallback b-event-read-callback :void
    ((e-ptr :pointer) (ctx :pointer))
  (let* ((e (event-table-get e-ptr))
	 (cb-args (bufev-cb-args e))
	 (read-cb (bufev-read-cb e)))
    (if read-cb 
	(apply read-cb e cb-args))))

(defcallback b-event-write-callback :void
    ((e-ptr :pointer) (ctx :pointer))
  (let* ((e (event-table-get e-ptr))
	 (cb-args (bufev-cb-args e))
	 (write-cb (bufev-write-cb e)))
    (if write-cb 
	(apply write-cb e cb-args))))

(defcallback b-event-event-callback :void
    ((e-ptr :pointer) (what :short) (ctx :pointer))
  (let* ((e (event-table-get e-ptr))
	 (cb-args (bufev-cb-args e))
	 (event-cb (bufev-event-cb e)))
    (if event-cb 
	(apply event-cb e what cb-args))))

(defun bufev-setcb (e &optional &key read-cb write-cb event-cb cb-args)
  "Changes th ecallbacks for bufev"
  (setf (bufev-read-cb e) read-cb)
  (setf (bufev-write-cb e) write-cb)
  (setf (bufev-event-cb e) event-cb)
  (setf (bufev-cb-args e) cb-args)

  (cc-libevent:bufferevent-setcb
   (bufev-c e)
   (if read-cb (callback b-event-read-callback) (null-pointer))
   (if write-cb (callback b-event-write-callback) (null-pointer))
   (if event-cb (callback b-event-event-callback) (null-pointer))
   (null-pointer))
  e)

(defun bufev-tcp-connect (e hostname port)
  "Resolve the hostname/ip in remote and connect to it.
E: bufev instance
HOSTNAME: address of remote server
PORT: port to connect to on the resolved address.
Recognized HOSTNAME formats are:

    quant67.com
    1.2.3.4
    ::1
    [::1]
"

  (if (= 0 (cffi:with-foreign-string (hname hostname)
	     (cc-libevent:bufferevent-socket-connect-hostname
	      (bufev-c e)
	      (base-dns-c (bufev-base e))
	      cc-net:+AF-INET+
	      hname
	      port)))
      e
      (error cc-error:cc-error :msg "bufferevent-socket-connect-hostname")))

(setf (fdefinition 'bufev-tls-connect) #'bufev-tcp-connect)

(defun bufev-tcp-connect-with-cb (eb
				  &key host port
				    read-cb write-cb event-cb
				    cb-args)
  "Connect to host:port with callbacks.
read-cb: (read-cb e ...cb-args)
write-cb: (write-cb e ...cb-args)
event-cb: (event-cb e what ..cb-args)

You need to (bufev-free e) when finished"
  (let ((e (bufev-socket-new eb -1 (logior +BEV-OPT-CLOSE-ON-FREE+
					   +BEV-OPT-THREADSAFE+))))
    (bufev-setcb e :read-cb read-cb :write-cb write-cb :event-cb event-cb
		   :cb-args cb-args)
    ;; (bufev-enable e (logior *EV-READ* *EV-WRITE*))
    (bufev-tcp-connect e host port)))

(defun bufev-tls-connect-with-cb (eb
				  &key
				    host
				    port
				    read-cb
				    write-cb
				    event-cb
				    cb-args)
  "Connect to host:port with callbacks.
read-cb: (read-cb e ...cb-args)
write-cb: (write-cb e ...cb-args)
event-cb: (event-cb e what ..cb-args)

You need to (bufev-free e) when finished"
  (let ((e (bufev-tls-socket-new eb -1 (logior +BEV-OPT-CLOSE-ON-FREE+))))
    (bufev-setcb e :read-cb read-cb :write-cb write-cb :event-cb event-cb
		   :cb-args cb-args)
    (bufev-tls-connect e host port)))

(defun bufev-set-timeout (e timeout-read timeout-write)
  "Set the read and write timeout for a bufferevent
E: bufev instance
TIMEOUT-READ: '(second microsecond)
TIMEOUT-READ: '(second microsecond)
"
  (cc-timeval:with-c-timeval-values (tv-read timeout-read)
      (cc-timeval:with-c-timeval-values (tv-write timeout-write)
	  (cc-libevent:bufferevent-set-timeouts (bufev-c e)
						tv-read
						tv-write))))

(defun bufev-get-input (e)
  "Return the input buffer."
  (cc-libevent:bufferevent-get-input (bufev-c e)))
(defun bufev-get-output (e)
  "Return the output buffer."
  (cc-libevent:bufferevent-get-output (bufev-c e)))

(defun bufev-enable (e ev-type)
  "Enable a buferevent

E: bufev
EV-TYPE: *EV-READ* | *EV-WRITE*

RETURN 0 if success, -1 error"
  (cc-libevent:bufferevent-enable (bufev-c e) ev-type))

(defun bufev-write-string (e data-str)
  "Write data string to bufev buffer."

  (cffi:with-foreign-string ((data datalen) data-str)
    (cc-libevent:bufferevent-write (bufev-c e)
				   data
				   datalen)))

(defun bufev-write (e data-vec)
  "Write data string to bufev buffer."

  (cffi:with-foreign-object (data :uint8 (length data-vec))
    (let ((i 0))
      (map nil
	   #'(lambda (x)
	       (setf (mem-ref data :uint8 i) x)
	       (setf i (+ 1 i)))
	   data-vec))
    (cc-libevent:bufferevent-write (bufev-c e)
				   data
				   (length data-vec))))


(defun bufev-read (e len)
  "Read len bytes of data from e"
  (with-foreign-object (out :uint8 len)
    (let ((r (cc-libevent:bufferevent-read (bufev-c e) out len)))
      (if (<= r 0)
	  nil
	  (let ((vec (make-array r)))
	    (loop for i from 0 below r
		  do (setf (elt vec i) (cffi:mem-aref out :uint8 i)))
	    vec)))))


(defun buffer-new ()
  "Allocate storage for a new C evbuffer."
  (cc-libevent:evbuffer-new))
(defun buffer-free (ptr)
  "Deallocate storage for a C evbuffer"
  (cc-libevent:evbuffer-free ptr))

(defun buffer-add-string (ptr str)
  "Append str to end of an C evbuffer"
  (cffi:with-foreign-string ((data datalen) str)
    (cc-libevent:evbuffer-add ptr data (- datalen 1))))

(defun buffer-add (ptr sequance)
  "Append byte sequance to end of an C evbuffer."
  (cffi:with-foreign-pointer (data (length sequence) size)
    (let ((i 0))
      (map nil
	   #'(lambda (x)
	       (setf (mem-ref data :uint8 i) x)
	       (setf i (+ 1 i)))
	   sequence))
    (cc-libevent:evbuffer-add ptr data size)))

(defun buffer-drain (ptr len)
  "Remote a specified number of bytes data form the beginning of an evbuffer"
  (cc-libevent:evbuffer-drain ptr len))

(defun buffer-length (ptr)
  "Return the total number of bytes stored in the C evbuffer"
  (cc-libevent:evbuffer-get-length ptr))



(defstruct listener
  c

  base

  ;; (cb listener fd sockaddr ..cb-args)
  cb

  cb-args)

(defun listener-free (e)
  "Free listener instance"
  (cc-libevent:evconnlistener-free (listener-c e))  
  (event-table-del (listener-c e)))

(defcallback listener-event-callback :void
    ((eptr :pointer) (fd :int) (sock-c :pointer) (socklen :int) (arg :pointer))

  (let* ((e (event-table-get eptr))
	 (cb-args (listener-cb-args e))
	 (cb (listener-cb e)))
    (apply cb e fd (sockaddr-from-c sock-c) cb-args)))

(defun listener-new-bind (be sock &key cb cb-args)
  "new listener"
  (let ((e (make-listener))
	(c (null-pointer)))
    (with-sockaddr-c (sock sock-c sock-c-len)
      (setf c (cc-libevent:evconnlistener-new-bind
	       (base-c be)
	       (callback listener-event-callback)
	       (null-pointer)
	       (logior cc-libevent:+LEV-OPT-CLOSE-ON-FREE+
		       ;; cc-libevent:+LEV-OPT-THREADSAFE+
		       cc-libevent:+LEV-OPT-REUSEABLE+
		       cc-libevent:+LEV-OPT-REUSEABLE-PORT+)
               -1
	       sock-c
	       sock-c-len)))
    (setf (listener-c e) c)
    (setf (listener-base e) be)
    (setf (listener-cb e) cb)
    (setf (listener-cb-args e) cb-args)
    (event-table-set c e)
    e))

(defun listener-new (be hostport &key cb cb-args)
  "New listener"
  (let ((e (make-listener))
	(c (null-pointer)))
    (with-sockaddr-c-from-string (hostport sock-c sock-c-len)
      (setf c (cc-libevent:evconnlistener-new-bind
	       (base-c be)
	       (callback listener-event-callback)
	       (null-pointer)
	       (logior cc-libevent:+LEV-OPT-CLOSE-ON-FREE+
		       ;; cc-libevent:*LEV-OPT-THREADSAFE*
		       cc-libevent:+LEV-OPT-REUSEABLE+
		       cc-libevent:+LEV-OPT-REUSEABLE-PORT+)
               -1
	       sock-c
	       sock-c-len)))
    (setf (listener-c e) c)
    (setf (listener-base e) be)
    (setf (listener-cb e) cb)
    (setf (listener-cb-args e) cb-args)
    (event-table-set c e)
    e))



(defcstruct evbuffer-ptr
  (pos :ssize)
  (-internal-chain :pointer)
  (-internal-size :size))



(defun buffer-nth (ptr n)
  "Get the nth element of evbuffer. It may SLOW!"
  (with-foreign-objects ((pos (:struct evbuffer-ptr))
			 (chr :uint8 1))
    (cc-libevent:evbuffer-ptr-set ptr pos n 0)
    (cc-libevent:evbuffer-copyout-from ptr pos chr 1)
    (cffi:mem-aref chr :uint8 0)))

(defun buffer-remove (ptr len)
  "Read data from evbuffer and drain the bytes read.
return the vector of removed bytes"
  (let ((vec (make-array len :initial-element 0)))
    (with-foreign-object (out :uint8 len)
      (cc-libevent:evbuffer-remove ptr out len)
      (loop for i from 0 below len
	    do (setf (elt vec i) (cffi:mem-aref out :uint8 i))))
    vec))

(defun buffer-copyout (ptr len)
  "Read data from an evbuffer, and leave the buffer unchanged."
  (let ((vec (make-array len :initial-element 0)))
    (with-foreign-object (out :uint8 len)
      (cc-libevent:evbuffer-copyout ptr out len)
      (loop for i from 0 below len
	    do (setf (elt vec i) (cffi:mem-aref out :uint8 i))))
    vec))

(defun buffer-copyout-string (ptr len)
  "Read data string from an evbuffer, and leave the buffer unchanged."
  (with-foreign-object (str-c :char (1+ len))
    (setf (mem-aref str-c :char len) 0)
    (cc-libevent:evbuffer-copyout ptr str-c len)
    (foreign-string-to-lisp str-c)))

(defun buffer-to-vec (ptr)
  "Copy all data from buffer out to vector."
  (buffer-copyout ptr (buffer-length ptr)))

(defun buffer-to-string (ptr)
  "Copy all data from buffer out to string"
  (buffer-copyout-string ptr (buffer-length ptr)))

(defun buffer-vec-to-string (vec)
  (flexi-streams:octets-to-string vec :external-format :utf-8))
(defun buffer-string-to-vec (str)
  (flexi-streams:string-to-octets str :external-format :utf-8))
