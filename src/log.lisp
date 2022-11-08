(in-package :cc-log)

;; TODO change to info

(log:config :D
	    :pretty
	    :thread
	    :package
	    :sane :this-console)

(log:debug "logger loaded")

