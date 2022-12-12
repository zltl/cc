(in-package :cc-error)

(define-condition cc-error (error)
  ((msg :initarg :msg
	:initform ""
	:accessor msg))
  (:documentation "Custom error of cc"))

(define-condition invalid-duration-string (cc-error)
  ()
  (:documentation "Error when parse duration that not a valid duration string"))

(define-condition oom (cc-error)
  ()
  (:documentation "Error when alloc functions return nil"))

(define-condition already-start (cc-error)
  ()
  (:documentation "Error when duplicate call starting functions"))

(define-condition bad-argument (cc-error)
  ()
  (:documentation "Error processing arguments"))
