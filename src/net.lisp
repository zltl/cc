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
  )

(defconstant *BEV-OPT-CLOSE-ON-FREE* #x01)
(defconstant *BEV-OPT-THREADSAFE* #x02)
(defconstant *BEV-OPT-DEFER-CALLBACKS* #x04)
(defconstant *BEV-OPT-UNLOCK-CALLBACKS* #x08)

(defun bufev-socket-new (bev fd options)
  "Create a new socket bev over an existing socket.
BEV: the base instance
FD: the file descriptor from which data is read and written to, or -1.
OPTIONS: zero or more *BEV-OPT-* flags
return an instance of struct bufev  
"
  (let* ((e (make-bufev :base bev))
	 (c (cc-libevent:bufferevent-socket-new (base-c bev) fd options)))
    (if (null-pointer-p c)
	(error (cc-error:oom "bufferevent-socket-new")))
    (setf (bufev-c e) c)
    (event-table-set c e)
    e))

(defun bufev-new (bev options)
  (bufev-socket-new bev -1 options))

(defun bufev-free (e)
  "Deallocate the bufev instance E."
  (cc-libevent:bufferevent-free (bufev-c e)))

;; TODO: defcfuncs

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
	      cc-net:*AF-INET*
	      hname
	      port)))
      e
     (error cc-error:cc-error :msg "bufferevent-socket-connect-hostname")))

