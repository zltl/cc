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

(defun bufev-free (e)
  "Deallocate the bufev instance E."
  (cc-libevent:bufferevent-free (bufev-c e)))

;; TODO: defcfuncs 

(defun bufev-setcb (e &optional &key read-cb write-cb event-cb cb-args)
  "Changes th ecallbacks for bufev"
  (setf (bufev-read-cb e) readcb)
  (setf (bufev-write-cb e) writecb)
  (setf (bufev-event-cb e) eventcb)
  (setf (bufev-cb-args e) cbargs)
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

