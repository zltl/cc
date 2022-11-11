(in-package :cc-dns)

(defstruct dns-task
  ;; (cb ip-list cb-args...)
  cb
  cb-args)

(defcstruct linux-addrinfo
  (ai_flags :int)
  (ai_family :int)
  (ai_socktype :int)
  (ai_protocol :int)
  (ai_addrlen :size)
  (ai_addr :pointer)
  (ai_canonname :string)
  (ai_next :pointer))

;; type evdns_getaddrinfo_cb.
(defcallback base-dns-callback :void
    ((result :int) (res :pointer) (arg :pointer))
  
  (let ((inf res)
	(ipset ())
	(task (cc-event:event-table-get arg)))
    (cc-event:event-table-del arg)    
    (foreign-free arg)
    
    (loop while (not (null-pointer-p inf))
	  do
	     (with-foreign-slots
		 ((ai_flags
		   ai_family
		   ai_socktype
		   ai_protocol
		   ai_addrlen
		   ai_addr
		   ai_canonname
		   ai_next) inf (:struct linux-addrinfo))
	       (let ((ip (cc-ip:ip-from-c-sockaddr ai_addr)))
		 (setf ipset (adjoin ip ipset :test #'cc-ip:ip-equal)))
	       (setf inf ai_next)))
    (apply (dns-task-cb task) ipset (dns-task-cb-args task)))

  (and res
       (cc-libevent:evutil-freeaddrinfo res)))

(defmethod dns-lookup ((eb cc-event:base) hostname cb &rest cb-args)
  "Resolve hostname to ip V4/V6
HOSTNAME: domain name
CB: callback function form as (cb result res-list cb-args...)
CB-ARGS: extra arguments of cb"
  (let ((task (make-dns-task :cb cb :cb-args cb-args))
	(aptr (foreign-alloc :char)))
    (cc-event:event-table-set aptr task)
    (with-foreign-string (name hostname)
      (cc-libevent:evdns-getaddrinfo
       (cc-event:base-dns-c eb)
       name
       (null-pointer)
       (null-pointer)
       (callback base-dns-callback)
       aptr))))
