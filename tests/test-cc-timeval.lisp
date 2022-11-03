(in-package :cc-tests)

(def-suite testtimeval
  :description "timeval test")

(in-suite testtimeval)

(defun gettimeofday-sample ()
  (progn
    (let ((tv (cc-timeval:gettimeofday)))
      (log:info "gettimeofday -> ~a" tv))
    1))

(defun get-sec ()
  (multiple-value-bind (sec micro)
      (cc-timeval:gettimeofday)
    (log:info "sec=~a usec=~a" sec micro)
    sec))

(defun conv-timeval ()
  (multiple-value-bind (sec micro)
      (cc-timeval:gettimeofday)
    (cc-timeval:with-c-timeval-value tv sec micro
      (multiple-value-bind (tv-sec tv-usec) (cc-timeval:get-values tv)
	(log:info "tv-sec=~a tv-usec=~a" tv-sec tv-usec)
 	(and (equal sec tv-sec)
 	     (equal micro tv-usec))))))

(test get
      (is (= 1 (gettimeofday-sample)))
      (is (> (get-sec) 0)))

(test conv
  (is (conv-timeval)))
