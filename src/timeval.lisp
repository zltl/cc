
(in-package :cc-timeval)

;;; Define the TIMEVAL structure used by 'gettimeofday'.  This assumes
;;; that 'time_t' is a 'long' --- it would be nice if CFFI could
;;; provide a proper :TIME-T type to help make this portable.
(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

;;; A NULL-POINTER is a foreign :POINTER that must always be NULL.
;;; Both a NULL pointer and NIL are legal values---any others will
;;; result in a runtime error.
(define-foreign-type null-pointer-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser null-pointer))

;;; This type translator is used to ensure that a NULL-POINTER has a
;;; null value.  It also converts NIL to a null pointer.
(defmethod translate-to-foreign (value (type null-pointer-type))
  (cond
    ((null value) (null-pointer))
    ((null-pointer-p value) value)
    (t (error "~A is not a null pointer." value))))

;;; The SYSCALL-RESULT type is an integer type used for the return
;;; value of C functions that return -1 and set errno on errors.
;;; Someday when CFFI has a portable interface for dealing with
;;; 'errno', this error reporting can be more useful.
(define-foreign-type syscall-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser syscall-result))

;;; Type translator to check a SYSCALL-RESULT and signal a Lisp error
;;; if the value is negative.
(defmethod translate-from-foreign (value (type syscall-result-type))
  (if (minusp value)
      (error "System call failed with return value ~D." value)
      value))

;;; Define the Lisp function %GETTIMEOFDAY to call the C function
;;; 'gettimeofday', passing a pointer to the TIMEVAL structure to fill
;;; in.  The TZP parameter is deprecated and should be NULL --- we can
;;; enforce this by using our NULL-POINTER type defined above.
(defcfun ("gettimeofday" %gettimeofday) syscall-result
  (tp :pointer)
  (tzp null-pointer))

;;; Define a Lispy interface to 'gettimeofday' that returns the
;;; seconds and microseconds as multiple values.
(defun gettimeofday ()
  (with-foreign-object (tv 'timeval)
    (%gettimeofday tv nil)
    (with-foreign-slots ((tv-sec tv-usec) tv timeval)
      (values tv-sec tv-usec))))

(defmacro with-c-timeval-value (tv second microsecond &body body)
  `(with-foreign-object (,tv 'timeval)
     (with-foreign-slots (tv-sec tv-usec ,tv (:struct timeval))
       (setf tv-sec ,second
	     tv-usec ,microsecond)
       ,@body)))

