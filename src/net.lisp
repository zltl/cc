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

(defconstant *EV-TIMEOUT* #x01)
(defconstant *EV-READ* #x02)
(defconstant *EV-WRITE* #x04)
(defconstant *EV-SIGNAL* #x08)
(defconstant *EV-PERSIST* #x10)
(defconstant *EV-ET* #x20)
(defconstant *EV-FINALIZE* #x40)
(defconstant *EV-CLOSED* #x80)

(defconstant *BEV-OPT-CLOSE-ON-FREE* #x01)
(defconstant *BEV-OPT-THREADSAFE* #x02)
(defconstant *BEV-OPT-DEFER-CALLBACKS* #x04)
(defconstant *BEV-OPT-UNLOCK-CALLBACKS* #x08)

(defconstant *BEV-EVENT-READING* #x01)
(defconstant *BEV-EVENT-WRITING* #x02)
(defconstant *BEV-EVENT-EOF* #x10)
(defconstant *BEV-EVENT-ERROR* #x20)
(defconstant *BEV-EVENT-TIMEOUT* #x40)
(defconstant *BEV-EVENT-CONNECTED* #x80)

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

  (cffi:with-foreign-object (data (length data-vec) size)
    (let ((i 0))
      (map nil
	   #'(lambda (x)
	       (setf (mem-ref data :uint8 i) x)
	       (setf i (+ 1 i)))
	   sequence))
    (cc-libevent:bufferevent-write (bufev-c e)
				   data
				   size)))

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
  (cc-libevent:evbuffer-free (ptr)))

(defun buffer-add-string (ptr str)
  "Append str to end of an C evbuffer"
  (cffi:with-foreign-string ((data datalen) str)
    (cc-libevent:evbuffer-add ptr data datalen)))

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

(defun buffer-vec-to-string (vec)
  (flexi-streams:octets-to-string vec :external-format :utf-8))
(defun buffer-string-to-vec (str)
  (flexi-streams:string-to-octets str :external-format :utf-8))
