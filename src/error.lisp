(in-package :cc-error)

(define-condition cc-error (error)
  ((msg :initarg :msg
	:initform ""
	:accessor msg))
  (:documentation "Custom error of cc"))

(define-condition invalid-duration-string (cc-error)
  ()
  (:documentation "Error when parse duration that not a valid duration string"))
