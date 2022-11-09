
(in-package :cc-timeval)

;;; Define the TIMEVAL structure used by 'gettimeofday'.  This assumes
;;; that 'time_t' is a 'long' --- it would be nice if CFFI could
;;; provide a proper :TIME-T type to help make this portable.
(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

;;; Define the Lisp function %GETTIMEOFDAY to call the C function
;;; 'gettimeofday', passing a pointer to the TIMEVAL structure to fill
;;; in.  The TZP parameter is deprecated and should be NULL --- we can
;;; enforce this by using our NULL-POINTER type defined above.
(defcfun ("gettimeofday" %gettimeofday) :int
  (tp :pointer)
  (tzp :pointer))

(defmacro with-c-timeval-value (tv sec micro &body body)
  "Create timeval object tv, then set sec, micro to tv, call body."
  `(with-foreign-object (,tv '(:struct timeval))
     (if (not 'micro) (setf micro 0))
     (setf (foreign-slot-value ,tv '(:struct timeval) 'tv-sec) ,sec)
     (setf (foreign-slot-value ,tv '(:struct timeval) 'tv-usec) ,micro)
     ,@body))

(defmacro with-c-timeval-values (tv timeout &body body)
  `(if ,timeout
       (cc-timeval:with-c-timeval-value
	   ,tv (car ,timeout) (car (cdr ,timeout))
	 ,@body)
       (let ((,tv (null-pointer)))
	 ,@body)))

(defun get-values (tv)
  "Return second and microsecond for tv."
  (with-foreign-slots ((tv-sec tv-usec) tv (:struct timeval))
    (values tv-sec tv-usec)))

;;; Define a Lispy interface to 'gettimeofday' that returns the
;;; seconds and microseconds as multiple values.
(defun gettimeofday ()
  (with-foreign-object (tv '(:struct timeval))
    (let ((r (%gettimeofday tv (null-pointer))))
      (if (< r 0) (error (cc-errno:str)))
      (get-values tv))))

(defun to-timestamp (sec micro)
  "Convert sec micro pair to local-time:timestamp."
  (local-time:unix-to-timestamp sec :nsec (* 1000 micro)))

(defun from-timestamp (ts)
  "Convert local-time:timestamp to sec and micro"
  (values (local-time:timestamp-to-unix ts)
	  (/ (local-time:nsec-of ts) 1000)))
