(in-package :cc-tests)

(def-suite testtimeval
  :description "timeval test")

(in-suite testtimeval)

(defun gettimeofday-sample ()
  (progn
    (let ((tv (cc-timeval:gettimeofday)))
      (log:info "gettimeofday -> ~a" tv))
    1))

(test get
      (is (= 1 (gettimeofday-sample))))


