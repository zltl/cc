(in-package :asdf-user)

(defsystem "cc"
  :author "liaotonglang <liaotonglang@gmail.com@mail.com>"
  :version "0.0.1"
  :license "GPL-3.0"
  :description ""
  :homepage ""
  :bug-tracker ""
  :source-control (:git "git@github.com:zltl/cc.git")

  ;; Dependencies.
  :depends-on (:cffi
	       :cl-autowrap/libffi
	       :local-time
	       :log4cl
	       :uiop
	       :cl-yaml
	       :cl-ppcre
	       :alexandria
	       :parse-float
	       :bt-semaphore
	       :lparallel
	       :for
	       :flexi-streams)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "cc")
				     (:file "libevent")
				     (:file "timeval")
				     (:file "log")
				     (:file "conf")
				     (:file "util")
				     (:file "error")
				     (:file "event")
				     (:file "dns")
				     (:file "ip")
				     (:file "net")
				     (:file "openssl")
				     )))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cc"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cc:main")
