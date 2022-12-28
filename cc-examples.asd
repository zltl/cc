(in-package :asdf-user)
(defsystem "cc-examples"
  :description "Examples for the cc system"
  :author "liaotonglang@gmail.com <liaotonglang@gmail.com@mail.com>"
  :version "0.0.1"
  :depends-on (:cc
	       :log4cl
	       :cffi
	       :uiop
	       :spinneret
	       :cl-json)
  :license "BSD"
  :serial t
  :components ((:module "examples"
                        :serial t
                        :components ((:file "packages")
                                     (:file "http-simple-server")))))
