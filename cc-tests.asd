(in-package :asdf-user)
(defsystem "cc-tests"
  :description "Test suite for the cc system"
  :author "liaotonglang@gmail.com <liaotonglang@gmail.com@mail.com>"
  :version "0.0.1"
  :depends-on (:cc
               :fiveam
	       :log4cl
	       :cffi
	       :uiop)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cc")
				     (:file "test-cc-conf")
				     (:file "test-cc-timeval")
				     (:file "test-cc-event")
				     (:file "test-cc-ip"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
