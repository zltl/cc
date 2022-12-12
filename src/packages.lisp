(defpackage :cc
  (:use :cl :common-lisp :cffi)
  (:export :main
	   :case-expr))

(defpackage :cc-openssl
  (:use :cl :common-lisp :cffi)
  (:export
   :tlsv1.2-method
   :ssl-ctx-new
   :ssl-new
   :ssl-ctx-free
   :ssl-free
   :perror))

(defpackage :cc-libevent
  (:use :cl :common-lisp :cffi)
  (:export

   :strlen-ptr
   
   :+LEV-OPT-LEAVE-SOCKET-BLOCKING+ 
   :+LEV-OPT-CLOSE-ON-FREE+ 
   :+LEV-OPT-CLOSE-ON-EXEC+ 
   :+LEV-OPT-REUSEABLE+ 
   :+LEV-OPT-THREADSAFE+ 
   :+LEV-OPT-DISABLED+ 
   :+LEV-OPT-DEFERRED-ACCEPT+
   :+LEV-OPT-REUSEABLE-PORT+
   :+LEV-OPT-BIND-IPV6ONL+

   :evconnlistener-new-bind
   :evconnlistener-new
   :evconnlistener-free
   :evconnlistener-enable
   :evconnlistener-disable
   :evconnlistener-set-cb
   :evconnlistener-set-error-cb
   
   :event-enable-debug-mode
   :event-debug-unassign
   :event-base-new
   :event-reinit
   :event-base-dispatch
   :event-base-get-method
   :event-get-supported-methods
   :event-gettime-monotonic
   :+EVENT-BASE-COUNT-ACTIVE+
   :+EVENT-BASE-COUNT-VIRTUAL+
   :+EVENT-BASE-COUNT-ADDED+
   :event-base-get-num-events
   :event-base-get-max-events
   :event-config-new
   :event-config-free
   :event-config-avoid-method
   :+EV-FEATURE-ET+
   :+EV-FEATURE-O1+
   :+EV-FEATURE-FDS+
   :+EV-FEATURE-CLOSE+
   :+EVENT-BASE-FLAG-NOLOCK+
   :+EVENT-BASE-FLAG-IGNORE-ENV+
   :+EVENT-BASE-FLAG-STARTUP-IOCP+
   :+EVENT-BASE-FLAG-NO-CACHE-TIME+
   :+EVENT-BASE-FLAG-EPOLL-USE-CHANGELIST+
   :+EVENT-BASE-FLAG-PRECISE-TIMER+
   :event-base-get-features
   :event-config-require-features
   :event-config-set-flag
   :event-config-set-num-cpus-hint
   :event-config-set-max-dispatch-interval
   :event-base-new-with-config
   :event-base-free
   :event-base-free-nofinalize
   :+EVENT-LOG-DEBUG+
   :+EVENT-LOG_MSG+
   :+EVENT-LOG-WARN+
   :+EVENT-LOG-ERR+
   :event-set-log-callback
   :event-set-fatal-callback
   :+EVENT-DBG-ALL+
   :+EVENT-DBG-NONE+
   :event-enable-debug-logging
   :event-base-set
   :+EVLOOP-ONCE+
   :+EVLOOP-NONBLOCK+
   :+EVLOOP-NO-EXIT-ON-EMPTY+
   :event-base-loop
   :event-base-loopexit
   :event-base-loopbreak
   :event-base-loopcontinue
   :event-base-got-exit
   :event-base-got-break
   :+EV-TIMEOUT+
   :+EV-READ+
   :+EV-WRITE+
   :+EV-SIGNAL+
   :+EV-PERSIST+
   :+EV-ET+
   :+EV-FINALIZE+
   :+EV-CLOSED+
   :event-self-cbarg
   :event-new
   :event-assign
   :event-free
   :event-finalize
   :event-free-finalize
   :event-base-once
   :event-add
   :event-remove-timer
   :event-del
   :event-del-noblock
   :event-del-block
   :event-active
   :event-pending
   :event-base-get-running-event
   :event-initialized
   :event-get-fd
   :event-get-base
   :event-get-events
   :event-get-callback
   :event-get-callback-arg
   :event-get-priority
   :event-get-assignment
   :event-get-struct-event_size
   :event-get-version
   :event-get-version-number
   :event-base-priority-init
   :event-base-get-npriorities
   :event-priority-set
   :event-base-init-common-timeout
   :event-set-mem-functions
   :event-base-dump-events
   :event-base-active-by-fd
   :event-base-active-by-signal
   :event-base-foreach-event
   :event-base-gettimeofday-cached
   :event-base-update-cache-time
   :libevent-global-shutdown
   ;; threads
   :+EVTHREAD-WRITE+
   :+EVTHREAD-READ+
   :+EVTHREAD-TRY+
   :+EVTHREAD-LOCKTYPE-RECURSIVE+
   :+EVTHREAD-LOCKTYPE-READWRIT+
   :evthread-set-lock-callbacks
   :evthread-set-condition-callbacks
   :evthread-set-id-callback
   :evthread-use-pthreads
   :evthread-enable-lock-debugging
   :evthread-make-base-notifiable
   ;; buffer
   :evbuffer-iovec
   :evbuffer-free
   :evbuffer-new
   :evbuffer-enable-locking
   :evbuffer-lock
   :evbuffer-unlock
   :+EVBUFFER-FLAG-DRAINS-TO-FD+
   :evbuffer-set-flags
   :evbuffer-clear-flags
   :evbuffer-get-length
   :evbuffer-get-contiguous-space
   :evbuffer-expand
   :evbuffer-reserve-space
   :evbuffer-commit-space
   :evbuffer-add
   :evbuffer-remove
   :evbuffer-copyout
   :evbuffer-copyout-from
   :evbuffer-remove-buffer
   :evbuffer-eol-style
   :evbuffer-readln
   :evbuffer-add-buffer
   :evbuffer-add-buffer-reference
   :evbuffer-add-reference
   :evbuffer-add-file
   :evbuffer-file-segment-new
   :evbuffer-file-segment-free
   :evbuffer-file-segment-add-cleanup-cb
   :evbuffer-add-file-segment
   :evbuffer-add-printf
   :evbuffer-add-vprintf
   :evbuffer-drain
   :evbuffer-write
   :evbuffer-write-atmost
   :evbuffer-ptr
   :evbuffer-read
   :evbuffer-search
   :evbuffer-search-range
   :evbuffer-ptr-how
   :evbuffer-ptr-set
   :evbuffer-search-eol
   :evbuffer-peek
   :evbuffer-cb-info
   :evbuffer-add-cb
   :evbuffer-remove-cb-entry
   :evbuffer-remove-cb
   :evbuffer-cb-set-flags
   :evbuffer-cb-clear-flags
   :evbuffer-pullup
   :evbuffer-prepend
   :evbuffer-prepend-buffer
   :evbuffer-freeze
   :evbuffer-unfreeze
   :evbuffer-defer-callbacks
   :evbuffer-add-iovec
   :+BEV-EVENT-READING+
   :+BEV-EVENT-WRITING+
   :+BEV-EVENT-EOF+
   :+BEV-EVENT-ERROR+
   :+BEV-EVENT-TIMEOUT+
   :+BEV-EVENT-CONNECTED+
   :+BEV-OPT-CLOSE-ON-FREE+
   :+BEV-OPT-THREADSAFE+
   :+BEV-OPT-DEFER-CALLBACKS+
   :+BEV-OPT-UNLOCK-CALLBACKS+
   :bufferevent-socket-new
   :bufferevent-socket-connect
   :bufferevent-socket-connect-hostname
   :bufferevent-socket-get-dns-error
   :bufferevent-base-set
   :bufferevent-get-base
   :bufferevent-priority-set
   :bufferevent-get-priority
   :bufferevent-free
   :bufferevent-setcb
   :bufferevent-getcb
   :bufferevent-setfd
   :bufferevent-getfd
   :bufferevent-get-underlying
   :bufferevent-write
   :bufferevent-write-buffer
   :bufferevent-read
   :bufferevent-read-buffer
   :bufferevent-get-input
   :bufferevent-get-output
   :bufferevent-enable
   :bufferevent-disable
   :bufferevent-get-enabled
   :bufferevent-set-timeouts
   :bufferevent-setwatermark
   :bufferevent-getwatermark
   :bufferevent-lock
   :bufferevent-unlock
   :bufferevent-incref
   :bufferevent-decref
   :+BEV-NORMAL+
   :+BEV-FLUSH+
   :+BEV-FINISHED+
   :bufferevent-flush
   :+BEV-TRIG-IGNORE-WATERMARKS+
   :+BEV-TRIG-DEFER-CALLBACKS+
   :bufferevent-trigger
   :bufferevent-trigger-event
   :+BEV-OK+
   :+BEV-NEED-MORE+
   :+BEV-ERROR+
   :bufferevent-filter-new
   :bufferevent-pair-new
   :bufferevent-pair-get-partner
   :ev-token-bucket-cfg-new
   :ev-token-bucket-cfg-free
   :bufferevent-set-rate-limit
   :bufferevent-rate-limit-group
   :bufferevent-rate-limit-group-set-cfg
   :bufferevent-rate-limit-group-set-min-share
   :bufferevent-rate-limit-group-free
   :bufferevent-add-to-rate-limit-group
   :bufferevent-remove-from-rate-limit-group
   :bufferevent-set-max-single-read
   :bufferevent-set-max-single-write
   :bufferevent-get-max-single-read
   :bufferevent-get-max-single-write
   :bufferevent-get-read-limit
   :bufferevent-get-write-limit
   :bufferevent-get-max-to-read
   :bufferevent-get-max-to-write
   :bufferevent-get-token-bucket-cfg
   :bufferevent-rate-limit-group-get-read-limit
   :bufferevent-rate-limit-group-get-write-limit
   :bufferevent-decrement-read-limit
   :bufferevent-decrement-write-limit
   :bufferevent-rate-limit-group-decrement-read
   :bufferevent-rate-limit-group-decrement-write
   :bufferevent-rate-limit-group-get-totals
   :bufferevent-rate-limit-group-reset-totals
   :evutil-inet-ntop
   :evutil-inet-pton
   :evutil-freeaddrinfo
   :evutil-socketpair
   :evutil-make-socket-nonblocking
   :evutil-make-listen-socket-reuseable
   :evutil-make-listen-socket-reuseable-port
   :evutil-make-listen-socket-ipv6only
   :evutil-make-socket-closeonexec
   :evutil-closesocket
   :evutil-make-tcp-listen-socket-deferred
   :evutil-parse-sockaddr-port
   :+EVDNS-BASE-DISABLE-WHEN-INACTIVE+
   :+EVDNS-BASE-INITIALIZE-NAMESERVERS+
   :+EVDNS-BASE-NAMESERVERS-NO-DEFAULT+
   :evdns-base-new
   :evdns-base-free
   :evdns-base-clear-host-addresses
   :evdns-err-to-string
   :evdns-base-nameserver-add
   :evdns-base-count-nameservers
   :evdns-base-clear-nameservers-and-suspend
   :evdns-base-resume
   :evdns-base-nameserver-ip-add
   :evdns-base-nameserver-sockaddr-add
   :evdns-base-resolve-ipv4
   :evdns-base-resolve-ipv6
   :evdns-base-resolve-reverse
   :evdns-base-resolve-reverse-ipv6
   :evdns-cancel-request
   :evdns-base-set-option
   :evdns-base-resolv-conf-parse
   :evdns-base-load-hosts
   :evdns-base-search-clear
   :evdns-base-search-add
   :evdns-base-search-ndots-set
   :evdns-set-log-fn
   :evdns-set-transaction-id-fn
   :evdns-set-random-bytes-fn
   :evdns-add-server-port-with-base
   :evdns-close-server-port
   :evdns-server-request-set-flags
   :evdns-server-request-add-reply
   :evdns-server-request-add-a-reply
   :evdns-server-request-add-aaaa-reply
   :evdns-server-request-add-ptr-reply
   :evdns-server-request-add-cname-reply
   :evdns-server-request-respond
   :evdns-server-request-drop
   :evdns-server-request-get-requesting-addr
   :evdns-getaddrinfo
   :evdns-getaddrinfo-cancel
   :evdns-base-get-nameserver-addr
   :evhttp-new
   :evhttp-bind-socket
   :evhttp-bind-socket-with-handle
   :evhttp-accept-socket
   :evhttp-accept-socket-with-handle
   :evhttp-bind-listener
   :evhttp-bound-socket-get-listener
   :evhttp-foreach-bound-socket
   :evhttp-del-accept-socket
   :evhttp-bound-socket-get-fd
   :evhttp-free
   :evhttp-set-max-headers-size
   :evhttp-set-max-body-size
   :evhttp-set-default-content-type
   :evhttp-set-allowed-methods
   :evhttp-set-cb
   :evhttp-del-cb
   :evhttp-set-gencb
   :evhttp-set-bevcb
   :evhttp-add-virtual-host
   :evhttp-remove-virtual-host
   :evhttp-add-server-alias
   :evhttp-remove-server-alias
   :evhttp-set-timeout
   :evhttp-set-timeout-tv
   :+EVHTTP-SERVER-LINGERING-CLOSE+
   :evhttp-set-flags
   :evhttp-send-error
   :evhttp-send-reply
   :evhttp-send-reply-start
   :evhttp-send-reply-chunk
   :evhttp-send-reply-chunk-with-cb
   :evhttp-send-reply-end
   :+EVHTTP-REQ-GET+
   :+EVHTTP-REQ-POST+
   :+EVHTTP-REQ-HEAD+
   :+EVHTTP-REQ-PUT+
   :+EVHTTP-REQ-DELETE+
   :+EVHTTP-REQ-OPTIONS+
   :+EVHTTP-REQ-TRACE+
   :+EVHTTP-REQ-CONNECT+
   :+EVHTTP-REQ-PATCH+
   :evhttp-connection-base-bufferevent-new
   :evhttp-connection-get-bufferevent
   :evhttp-connection-get-server
   :evhttp-request-new
   :evhttp-request-set-chunked-cb
   :evhttp-request-set-header-cb
   :evhttp-request-set-error-cb
   :evhttp-request-set-on-complete-cb
   :evhttp-request-free
   :evhttp-connection-base-new
   :evhttp-connection-set-family
   :+EVHTTP-CON-REUSE-CONNECTED-ADDR+
   :+EVHTTP-CON-READ-ON-WRITE-ERROR+
   :+EVHTTP-CON-LINGERING-CLOSE+
   :+EVHTTP-CON-PUBLIC-FLAGS-END
   :evhttp-connection-set-flags
   :evhttp-request-own
   :evhttp-request-is-owned
   :evhttp-request-get-connection
   :evhttp-connection-get-base
   :evhttp-connection-set-max-headers-size
   :evhttp-connection-set-max-body-size
   :evhttp-connection-free
   :evhttp-connection-free-on-completion
   :evhttp-connection-set-local-address
   :evhttp-connection-set-local-port
   :evhttp-connection-set-timeout
   :evhttp-connection-set-timeout-tv
   :evhttp-connection-set-initial-retry-tv
   :evhttp-connection-set-retries
   :evhttp-connection-set-closecb
   :evhttp-connection-get-peer
   :evhttp-connection-get-addr
   :evhttp-make-request
   :evhttp-cancel-request
   :evhttp-request-get-uri
   :evhttp-request-get-evhttp-uri
   :evhttp-request-get-command
   :evhttp-request-get-response-code
   :evhttp-request-get-response-code-line
   :evhttp-request-get-input-headers
   :evhttp-request-get-output-headers
   :evhttp-request-get-input-buffer
   :evhttp-request-get-output-buffer
   :evhttp-request-get-host
   :evhttp-find-header
   :evhttp-remove-header
   :evhttp-add-header
   :evhttp-clear-headers
   :evhttp-encode-uri
   :evhttp-uriencode
   :evhttp-decode-uri
   :evhttp-uridecode
   :evhttp-parse-query
   :evhttp-parse-query-str
   :evhttp-htmlescape

   :evhttp-connection-set-connect-timeout-tv
   :evhttp-connection-set-read-timeout-tv
   :evhttp-connection-set-write-timeout-tv
   :evhttp-connection-set-initial-retry-tv
   
   :evhttp-uri-new
   :evhttp-uri-set-flags
   :evhttp-uri-get-scheme
   :evhttp-uri-get-userinfo
   :evhttp-uri-get-host
   :evhttp-uri-get-port
   :evhttp-uri-get-path
   :evhttp-uri-get-query
   :evhttp-uri-get-fragment
   :evhttp-uri-set-scheme
   :evhttp-uri-set-userinfo
   :evhttp-uri-set-host
   :evhttp-uri-set-port
   :evhttp-uri-set-path
   :evhttp-uri-set-query
   :evhttp-uri-set-fragment
   :evhttp-uri-parse-with-flags
   :+EVHTTP-URI-MONCONFORMANT
   :evhttp-uri-parse
   :evhttp-uri-free
   :evhttp-uri-join
   :+BUFFEREVENT-SSL-OPEN+
   :+BUFFEREVENT-SSL-CONNECTING+
   :+BUFFEREVENT-SSL-ACCEPTING+
   :bufferevent-openssl-filter-new
   :bufferevent-openssl-socket-new
   :bufferevent-openssl-get-allow-dirty-shutdown
   :bufferevent-openssl-set-allow-dirty-shutdown
   :bufferevent_openssl_get_ssl
   :bufferevent-ssl-renegotiate
   :bufferevent-get-openssl-error))

(defpackage :cc-timeval
  (:use :cl :common-lisp :cffi)
  (:export
   :gettimeofday
   :timeval
   :get-values
   :with-c-timeval-value
   :with-c-timeval-values   
   :to-timestamp
   :from-timestamp))

(defpackage :cc-log
  (:use :cl :local-time :log4cl))

(defpackage :cc-conf
  (:nicknames :conf)
  (:use :cl)
  (:export
   :*default-conf*
   :conf
   :name
   :try-files
   :from-env
   :env-prefix
   :get-value
   :load-yaml
   :+load-yaml
   :+get-value
   :parse-duration
   :parse-float))

(defpackage :cc-errno
  (:nicknames :errno)
  (:use :cl :cffi)
  (:export :str :code))

(defpackage :cc-util
  (:nicknames :util)
  (:use :cl :cffi)
  (:export :make-pointer-eql-able))

(defpackage :cc-error
  (:nicknames :error)
  (:use :cl)
  (:export
   :cc-error
   :invalid-duration-string
   :oom
   :already-start
   :bad-argument))

(defpackage :cc-event
  (:nicknames :event)
  (:use :cl :cffi)
  (:export
   :event-table-get
   :event-table-set
   :event-table-del
   
   :event
   :event-c
   :event-base
   :event-cb
   :event-cb-arg-list
   :event-fd
   :event-types
   :event-new
   :event-free
   :event-add
   :event-del
   
   :base
   :base-c
   :base-ev
   :base-id
   :base-defer-task-queue
   :base-lock
   :base-dns-c

   :base-init
   :base-deinit
   :base-loop-start
   :base-loop-stop
   :base-loop-started-p
   :with-base-loop
   
   :defer-task
   :defer-task-cb
   :defer-task-cb-arg



   :defer-submit

   :timer-submit
   ))

(defpackage :cc-net
  (:nicknames :net)
  (:use :cl :cffi :cc-event)
  (:export
   :+AF-INET+
   :+AF-INET6+
   :+V4+
   :+V6+
   :ip
   :ip-len
   :ip-family
   :ip-addr
   :ip-to-string
   :ip-from-string
   :ip-from-c-sockaddr
   :ip-from-c-addr
   :ipv4-address-p
   :ipv6-address-p
   :ip-address-p
   :ip-equal
   :with-sockaddr-c
   :+sock-addr-len+

   :sockaddr
   :sockaddr-ip
   :sockaddr-port
   :sockaddr-from-c
   :sockaddr-to-string

   :dns-lookup

   :+EV-TIMEOUT+
   :+EV-READ+
   :+EV-WRITE+
   :+EV-SIGNAL+
   :+EV-PERSIST+
   :+EV-ET+
   :+EV-FINALIZE+
   :+EV-CLOSED+

   :+BEV-EVENT-READING+
   :+BEV-EVENT-WRITING+
   :+BEV-EVENT-EOF+
   :+BEV-EVENT-ERROR+
   :+BEV-EVENT-TIMEOUT+
   :+BEV-EVENT-CONNECTED+
   :+BEV-OPT-CLOSE-ON-FREE+
   :+BEV-OPT-THREADSAFE+
   :+BEV-OPT-DEFER-CALLBACKS+
   :+BEV-OPT-UNLOCK-CALLBACKS+

   :bufev
   :bufev-c
   :bufev-socket-new
   :bufev-tls-socket-new
   :bufev-new
   :bufev-tls-new
   :bufev-free
   :bufev-tcp-connect
   :bufev-tcp-connect-with-cb
   :bufev-tls-connect
   :bufev-tls-connect-with-cb
   
   :bufev-setcb
   :bufev-enable
   :bufev-write
   :bufev-write-string
   :bufev-read
   :bufev-set-timeout
   :bufev-get-input
   :bufev-get-output

   
   :buffer-new
   :buffer-free
   :buffer-add-string
   :buffer-add
   :buffer-drain
   :buffer-length
   :buffer-nth
   :buffer-remove
   :buffer-copyout
   :buffer-vec-to-string
   :buffer-string-to-vec

   :listener
   :listener-c
   :listener-base
   :listener-cb
   :listener-cb-args
   :listener-free
   :listener-new
   :listener-new-bind
   ))

(defpackage :cc-net/http
  (:nicknames :cc-http :http :net/http)
  (:use :cl :cffi :cc-event)
  (:export

   :+continue+ 
   :+switch-protocols+ 
   :+processing+ 
   :+earlyhints+ 
   :+ok+ 
   :+created+ 
   :+accepted+ 
   :+nonauthoritative+ 
   :+nocontent+ 
   :+moveperm+ 
   :+movetemp+
   :+notmodified+ 
   :+badrequest+ 
   :+unauthorized+ 
   :+paymentrequired+ 
   :+forbidden+ 
   :+notfound+ 
   :+badmethod+ 
   :+entitytoolarge+ 
   :+expectationfailed+ 
   :+internal+ 
   :+notimplemented+ 
   :+badgateway+ 
   :+servunavail+ 

;;; requests

   :+get+ 
   :+post+ 
   :+head+ 
   :+put+ 
   :+delete+ 
   :+options+ 
   :+trace+ 
   :+connect+ 
   :+patch+ 
   :+propfind+ 
   :+proppatch+ 
   :+mkcol+ 
   :+lock+ 
   :+unlock+ 
   :+copy+ 
   :+move+ 

   :+request+ 
   :+response+ 

   :+timeout+ 
   :+eof+ 
   :+invalid-header+ 
   :+buffer-error+ 
   :+request-cancel+ 
   :+data-too-long+ 


   
   :http-conn
   :http-conn-c
   :http-conn-base
   :http-conn-url
   :http-conn-cheme
   :http-conn-host
   :http-conn-port
   :http-conn-uri 

   :http-conn-new
   :http-conn-free
   :http-conn-set-max-header-size
   :http-conn-set-max-body-size
   :http-conn-set-connect-timeout
   :http-conn-set-read-timeout
   :http-conn-set-write-timeout
   :http-conn-set-initial-retry-timeout
   :http-conn-set-retries
   :http-conn-set-closecb

   :make-keyvals
   :keyvals-get
   :keyvals-gets
   :keyvals-set
   :keyvals-sets
   :keyvals-add
   :evkeyvalq-to-hash-table
   :evkeyvalq-from-hash-table

   :request
   :request-c
   :request-cb
   :request-cb-args
   :request-new
   :request-free
   :request-do
   :request-get-response-code
   :request-set-chunk-cb
   :request-set-header-cb
   :request-set-error-cb
   :request-set-complete-cb

   :request-get-input-headers
   :request-get-ouput-headers
   :request-set-input-headers
   :request-set-ouput-headers
   
   ))

