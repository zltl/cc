
;; (ql:quickload "cffi")

;; bind libevent

(in-package :cc-libevent)

(define-foreign-library libevent
    (:unix (:or "libevent.so"))
    (t (:default "libevent")))
(define-foreign-library libevent_core
    (:unix (:or "libevent_core.so"))
    (t (:default "libevent_core")))
(define-foreign-library libevent_extra
    (:unix (:or "libevent_extra.so"))
    (t (:default "libevent_extra")))
(define-foreign-library libevent_openssl
    (:unix (:or "libevent_openssl.so"))
    (t (:default "libevent_openssl")))
(define-foreign-library libevent_pthreads
    (:unix (:or "libevent_pthreads.so"))
    (t (:default "libevent_pthreads")))

(use-foreign-library libevent)
(use-foreign-library libevent_core)
(use-foreign-library libevent_extra)
(use-foreign-library libevent_openssl)
(use-foreign-library libevent_pthreads)


;; begin /event2/event.h
(defcfun (event-enable-debug-mode "event_enable_debug_mode") :void)
(defcfun (event-debug-unassign "event_debug_unassign") :void
  (event :pointer))
(defcfun (event-base-new "event_base_new") :pointer)
(defcfun (event-reinit "event_reinit") :int (base :pointer))
(defcfun (event-base-dispatch "event_base_dispatch") :int (base :pointer))
(defcfun (event-base-get-method "event_base_get_method") :string
  (base :pointer))
(defcfun (event-get-supported-methods "event_get_supported_methods")
  :pointer)

(defcstruct timeval
    (tv_sec :ulong)
    (tv_usec :ulong))

(defcfun (event-gettime-monotonic "event_gettime_monotonic") :int
  (base :pointer)
  (tp :pointer))

(defconstant *EVENT-BASE-COUNT-ACTIVE* 1)
(defconstant *EVENT-BASE-COUNT-VIRTUAL* 2)
(defconstant *EVENT-BASE-COUNT-ADDED* 4)

(defcfun (event-base-get-num-events "event_base_get_num_events") :int
  (eb :pointer)
  (flags :uint))

(defcfun (event-base-get-max-events "event_base_get_max_events") :int
  (eb :pointer)
  (flags :uint)
  (clear :int))

(defcfun (event-config-new "event_config_new") :pointer)
(defcfun (event-config-free "event_config_free") :void
  (cfg :pointer))

(defcfun (event-config-avoid-method "event_config_avoid_method") :int
  (cfg :pointer)
  (method :pointer))

(defconstant *EV-FEATURE-ET* #x01)
(defconstant *EV-FEATURE-O1* #x02)
(defconstant *EV-FEATURE-FDS* #x04)
(defconstant *EV-FEATURE-CLOSE* #x08)

(defconstant *EVENT-BASE-FLAG-NOLOCK* #x01)
(defconstant *EVENT-BASE-FLAG-IGNORE-ENV* #x02)
(defconstant *EVENT-BASE-FLAG-STARTUP-IOCP* #x04)
(defconstant *EVENT-BASE-FLAG-NO-CACHE-TIME* #x08)
(defconstant *EVENT-BASE-FLAG-EPOLL-USE-CHANGELIST* #x10)
(defconstant *EVENT-BASE-FLAG-PRECISE-TIMER* #x20)

(defcfun (event-base-get-features "event_base_get_features") :int
  (base :pointer))

(defcfun (event-config-require-features "event_config_require_features")
  :int
  (cfg :pointer)
  (feature :int))

(defcfun (event-config-set-flag "event_config_set_flag") :int
  (cfg :pointer)
  (flag :int))

(defcfun (event-config-set-num-cpus-hint "event_config_set_num_cpus_hint")
  :int
  (cfg :pointer)
  (cpus :int))

(defcfun (event-config-set-max-dispatch-interval
	  "event_config_set_max_dispatch_interval")
  :int
  (cfg :pointer)
  (max_interval :pointer)
  (max_callbacks :int)
  (min_priority :int))

(defcfun (event-base-new-with-config "event_base_new_with_config")
  :pointer
  (cfg :pointer))

(defcfun (event-base-free "event_base_free") :void (eb :pointer))

(defcfun (event-base-free-nofinalize "event_base_free_nofinalize") :void
  (eb :pointer))

(defconstant *EVENT-LOG-DEBUG* 0)
(defconstant *EVENT-LOG_MSG* 1)
(defconstant *EVENT-LOG-WARN* 2)
(defconstant *EVENT-LOG-ERR* 3)

(defcfun (event-set-log-callback "event_set_log_callback") :void
  (cb :pointer))

(defcfun (event-set-fatal-callback "event_set_fatal_callback") :void
  (cb :pointer))

(defconstant *EVENT-DBG-ALL* #xffffffff)
(defconstant *EVENT-DBG-NONE* 0)

(defcfun (event-enable-debug-logging "event_enable_debug_logging") :void
  (which :uint32))

(defcfun (event-base-set "event_base_set") :int
  (eb :pointer)
  (ev :pointer))

(defconstant *EVLOOP-ONCE* #x01)
(defconstant *EVLOOP-NONBLOCK* #x02)
(defconstant *EVLOOP-NO-EXIT-ON-EMPTY* #x04)

(defcfun (event-base-loop "event_base_loop") :int
  (eb :pointer)
  (flags :int))

(defcfun (event-base-loopexit "event_base_loopexit") :int
  (eb :pointer)
  (tv :pointer))

(defcfun (event-base-loopbreak "event_base_loopbreak") :int
  (eb :pointer))

(defcfun (event-base-loopcontinue "event_base_loopcontinue") :int
  (eb :pointer))

(defcfun (event-base-got-exit "event_base_got_exit") :int
  (eb :pointer))

(defcfun (event-base-got-break "event_base_got_break") :int
  (eb :pointer))

(defconstant *EV-TIMEOUT* #x01)
(defconstant *EV-READ* #x02)
(defconstant *EV-WRITE* #x04)
(defconstant *EV-SIGNAL* #x08)
(defconstant *EV-PERSIST* #x10)
(defconstant *EV-ET* #x20)
(defconstant *EV-FINALIZE* #x40)
(defconstant *EV-CLOSED* #x80)

(defcfun (event-self-cbarg "event_self_cbarg") :pointer)

(defcfun (event-new "event_new") :pointer
  (base :pointer)
  (fd :int)
  (events :short)
  (callback :pointer)
  (callback_arg :pointer))

(defcfun (event-assign "event_assign") :int
  (ev :pointer)
  (base :pointer)
  (fd :int)
  (events :short)
  (callback :pointer)
  (callback_arg :pointer))

(defcfun (event-free "event_free") :void
  (ev :pointer))

(defcfun (event-finalize "event_finalize") :int
  (flags :uint)
  (ev :pointer)
  (callback :pointer))
(defcfun (event-free-finalize "event_free_finalize") :int
  (flags :uint)
  (ev :pointer)
  (callback :pointer))

(defcfun (event-base-once "event_base_once") :int
  (base :pointer)
  (fd :int)
  (events :short)
  (callback :pointer)
  (arg :pointer)
  (timeout :pointer))

(defcfun (event-add "event_add") :int
  (ev :pointer)
  (timeout :pointer))

(defcfun (event-remove-timer "event_remove_timer") :int
  (ev :pointer))

(defcfun (event-del "event_del") :int
  (ev :pointer))


(defcfun (event-del-noblock "event_del_noblock") :int
  (ev :pointer))

(defcfun (event-del-block "event_del_block") :int
  (ev :pointer))

(defcfun (event-active "event_active") :void
  (ev :pointer)
  (res :int)
  (ncalls :short))

(defcfun (event-pending "event_pending") :int
  (ev :pointer)
  (events :short)
  (tv :pointer))

(defcfun (event-base-get-running-event "event_base_get_running_event")
  :pointer
  (base :pointer))

(defcfun (event-initialized "event_initialized") :int
  (ev :pointer))

(defcfun (event-get-fd "event_get_fd") :int
  (ev :pointer))

(defcfun (event-get-base "event_get_base") :pointer
  (ev :pointer))

(defcfun (event-get-events "event_get_events") :short
  (ev :pointer))

(defcfun (event-get-callback "event_get_callback") :pointer
  (ev :pointer))

(defcfun (event-get-callback-arg "event_get_callback_arg") :pointer
  (ev :pointer))

(defcfun (event-get-priority "event_get_priority") :int
  (ev :pointer))

(defcfun (event-get-assignment "event_get_assignment") :void
  (event :pointer)
  (base_out :pointer)
  (fd_out :pointer)
  (events_out :pointer)
  (callback_out :pointer)
  (arg_out :pointer))

(defcfun (event-get-struct-event_size "event_get_struct_event_size")
  :size)

(defcfun (event-get-version "event_get_version") :string)

(defcfun (event-get-version-number "event_get_version_number") :uint32)

(defcfun (event-base-priority-init "event_base_priority_init") :int
  (eb :pointer)
  (npriorities :int))

(defcfun (event-base-get-npriorities "event_base_get_npriorities") :int
  (eb :pointer))

(defcfun (event-priority-set "event_priority_set") :int
  (ev :pointer)
  (priority :int))

(defcfun (event-base-init-common-timeout "event_base_init_common_timeout")
  :pointer
  (base :pointer)
  (duration :pointer))

(defcfun (event-set-mem-functions "event_set_mem_functions") :void
  (malloc_fn :pointer)
  (realloc_fn :pointer)
  (free_fn :pointer))

(defcfun (event-base-dump-events "event_base_dump_events") :void
  (eb :pointer)
  (filepointer :pointer))

(defcfun (event-base-active-by-fd "event_base_active_by_fd") :void
  (base :pointer)
  (fd :int)
  (events :short))

(defcfun (event-base-active-by-signal "event_base_active_by_signal") :void
  (base :pointer)
  (sig :int))

(defcfun (event-base-foreach-event "event_base_foreach_event") :int
  (base :pointer)
  (fn :pointer)
  (arg :pointer))

(defcfun (event-base-gettimeofday-cached "event_base_gettimeofday_cached")
  :int
  (base :pointer)
  (tv :pointer))

(defcfun (event-base-update-cache-time "event_base_update_cache_time") :int
  (base :pointer))

(defcfun (libevent-global-shutdown "libevent_global_shutdown") :void)

;; end event2/event.h


;; begin event2/thread.h

(defconstant *EVTHREAD-WRITE* #x04)
(defconstant *EVTHREAD-READ* #x08)
(defconstant *EVTHREAD-TRY* #x10)

(defconstant *EVTHREAD-LOCKTYPE-RECURSIVE* 1)
(defconstant *EVTHREAD-LOCKTYPE-READWRIT* 2)

(defcfun (evthread-set-lock-callbacks "evthread_set_lock_callbacks") :int
  (cbs :pointer))

(defcfun (evthread-set-condition-callbacks "evthread_set_condition_callbacks")
  :int
  (cbs :pointer))

;; TODO: defcstruct callback structs

(defcfun (evthread-set-id-callback "evthread_set_id_callback") :void
  (id_fn :pointer))

(defcfun (evthread-use-pthreads "evthread_use_pthreads") :int)
(defcfun (evthread-enable-lock-debugging "evthread_enable_lock_debugging")
  :void)

(defcfun (evthread-make-base-notifiable "evthread_make_base_notifiable")
  :int
  (base :pointer))

;; end event2/thread.h


;; begin event2/buffer.h

(defcstruct evbuffer-iovec
  (iov_base :pointer)
  (iov_len :size))
(defcfun (evbuffer-free "evbuffer_free") :void
  (buf :pointer))
(defcfun (evbuffer-new "evbuffer_new") :pointer)
(defcfun (evbuffer-enable-locking "evbuffer_enable_locking") :int
  (buf :pointer)
  (lock :pointer))
(defcfun (evbuffer-lock "evbuffer_lock") :void
  (buf :pointer))
(defcfun (evbuffer-unlock "evbuffer_unlock") :void
  (buf :pointer))

(defconstant *EVBUFFER-FLAG-DRAINS-TO-FD* 1)

(defcfun (evbuffer-set-flags "evbuffer_set_flags") :int
  (buf :pointer)
  (flags :uint64))

(defcfun (evbuffer-clear-flags "evbuffer_clear_flags") :int
  (buf :pointer)
  (flags :uint64))

(defcfun (evbuffer-get-length "evbuffer_get_length") :size
  (buf :pointer))

(defcfun (evbuffer-get-contiguous-space "evbuffer_get_contiguous_space")
  :size
  (buf :pointer))
(defcfun (evbuffer-expand "evbuffer_expand") :int
  (buf :pointer)
  (datlen :size))
(defcfun (evbuffer-reserve-space "evbuffer_reserve_space") :int
  (buf :pointer)
  (size :size)
  (vec :pointer))

(defcfun (evbuffer-commit-space "evbuffer_commit_space") :int
  (buf :pointer)
  (vec :pointer)
  (n_vecs :int))

(defcfun (evbuffer-add "evbuffer_add") :int
  (buf :pointer)
  (data :pointer)
  (datlen :size))

(defcfun (evbuffer-remove "evbuffer_remove") :int
  (buf :pointer)
  (data :pointer)
  (dattlen :size))

(defcfun (evbuffer-copyout "evbuffer_copyout") :ssize
  (buf :pointer)
  (data_out :pointer)
  (datlen :size))

(defcfun (evbuffer-copyout-from "evbuffer_copyout_from") :ssize
  (buf :pointer)
  (pos :pointer)
  (data_out :pointer)
  (datlen :size))

(defcfun (evbuffer-remove-buffer "evbuffer_remove_buffer") :int
  (src :pointer)
  (dst :pointer)
  (datlen :size))

(defcenum evbuffer-eol-style
  :EVBUFFER-EOL-ANY
  :EVBUFFER-EOL-CRLF
  :EVBUFFER-EOL-CRLF_STRICT
  :EVBUFFER-EOL-LF
  :EVBUFFER-EOL-NUL)

(defcfun (evbuffer-readln "evbuffer_readln") :pointer
  (buffer :pointer)
  (n_read_out :pointer)
  (eol_style evbuffer-eol-style))

(defcfun (evbuffer-add-buffer "evbuffer_add_buffer") :int
  (outbuf :pointer)
  (evbuffer :pointer))

(defcfun (evbuffer-add-buffer-reference "evbuffer_add_buffer_reference")
  :int
  (outbuf :pointer)
  (inbuf :pointer))

(defcfun (evbuffer-add-reference "evbuffer_add_reference") :int
  (outbuf :pointer)
  (data :pointer)
  (datlen :pointer)
  (cleanupfn :pointer)
  (cleanupfn_arg :pointer))

(defcfun (evbuffer-add-file "evbuffer_add_file") :int
  (outbuf :pointer)
  (fd :int)
  (offset :offset)
  (length :offset))

(defconstant *EVBUF-FS-CLOSE-ON-FREE* #x01)
(defconstant *EVBUF-FS-DISABLE-MMAP* #x02)
(defconstant *EVBUF-FS-DISABLE-SENDFILE* #x04)
(defconstant *EVBUF-FS-DISABLE-LOCKING* #x08)


(defcfun (evbuffer-file-segment-new "evbuffer_file_segment_new") :pointer
  (fd :int)
  (offset :offset)
  (length :offset)
  (flags :uint))


(defcfun (evbuffer-file-segment-free "evbuffer_file_segment_free") :void
  (set :pointer))

(defcfun (evbuffer-file-segment-add-cleanup-cb
	  "evbuffer_file_segment_add_cleanup_cb")
  :void
  (seg :pointer)
  (cb :pointer)
  (arg :pointer))

(defcfun (evbuffer-add-file-segment "evbuffer_add_file_segment") :int
  (buf :pointer)
  (seg :pointer)
  (offset :offset)
  (length :offset))

(defcfun (evbuffer-add-printf "evbuffer_add_printf") :int
  (buf :pointer)
  (fmt :pointer)
  &rest)

(defcfun (evbuffer-add-vprintf "evbuffer_add_vprintf") :int
  (buf :pointer)
  (fmt :pointer)
  (ap :pointer)) ;; TODO


(defcfun (evbuffer-drain "evbuffer_drain") :int (buf :pointer) (len :size))

(defcfun (evbuffer-write "evbuffer-write") :int (buf :pointer) (fd :int))

(defcfun (evbuffer-write-atmost "evbuffer_write_atmost") :int
  (buffer :pointer)
  (fd :int)
  (howmuch :ssize))

(defcunion evbuffer-ptr-internal
  (chain :pointer)
  (pos_in_chain :size))
(defcstruct evbuffer-ptr
  (pos :ssize)
  (internal_ evbuffer-ptr-internal))

(defcfun (evbuffer-read "evbuffer_read") :int
  (buf :pointer)
  (fd :int)
  (howmuch :int))

(defcfun (evbuffer-search "evbuffer_search") evbuffer-ptr
  (buffer :pointer)
  (what :pointer)
  (len :size)
  (start :pointer))

(defcfun (evbuffer-search-range "evbuffer_search_range") evbuffer-ptr
  (buffer :pointer)
  (what :pointer)
  (len :size)
  (start :pointer)
  (end :pointer))

(defcenum evbuffer-ptr-how
  :EVBUFFER-PTR-SET
  :EVBUFFER-PTR-ADD)


(defcfun (evbuffer-ptr-set "evbuffer_ptr_set") :int
  (buffer :pointer)
  (ptr :pointer)
  (position :size)
  (how evbuffer-ptr-how))

(defcfun (evbuffer-search-eol "evbuffer_search_eol") evbuffer-ptr
  (buffer :pointer)
  (start :pointer)
  (eol_len_out :pointer)
  (eol_style evbuffer-eol-style))

(defcfun (evbuffer-peek "evbuffer_peek") :int
  (buffer :pointer)
  (len :ssize)
  (start_at :pointer)
  (vec_out :pointer)
  (n_vec :int))

(defcstruct evbuffer-cb-info
  (orig_size :size)
  (n_added :size)
  (n_deleted :size))

(defcfun (evbuffer-add-cb "evbuffer_add_cb") :pointer
  (buffer :pointer)
  (cb :pointer)
  (cbarg :pointer))

(defcfun (evbuffer-remove-cb-entry "evbuffer_remove_cb_entry") :int
  (buffer :pointer)
  (ent :pointer))

(defcfun (evbuffer-remove-cb "evbuffer_remove_cb") :int
  (buffer :pointer)
  (cb :pointer)
  (cbarg :pointer))

(defconstant *EVBUFFER-CB-ENABLED* 1)

(defcfun (evbuffer-cb-set-flags "evbuffer_cb_set_flags") :int
  (buffer :pointer)
  (cb :pointer)
  (flags :uint32))
(defcfun (evbuffer-cb-clear-flags "evbuffer_cb_clear_flags") :int
  (buffer :pointer)
  (cb :pointer)
  (flags :uint32))

(defcfun (evbuffer-pullup "evbuffer_pullup") :pointer
  (buf :pointer)
  (size :ssize))
(defcfun (evbuffer-prepend "evbuffer_prepend") :int
  (buf :pointer)
  (data :pointer)
  (size :size))

(defcfun (evbuffer-prepend-buffer "evbuffer_prepend_buffer") :int
  (dst :pointer)
  (src :pointer))

(defcfun (evbuffer-freeze "evbuffer_freeze") :int
  (buf :pointer)
  (at_front :int))

(defcfun (evbuffer-unfreeze "evbuffer_unfreeze") :int
  (buf :pointer)
  (at_front :int))

(defcfun (evbuffer-defer-callbacks "evbuffer_defer_callbacks") :int
  (buffer :pointer)
  (base :pointer))

(defcfun (evbuffer-add-iovec "evbuffer_add_iovec") :size
  (buffer :pointer)
  (iov :pointer)
  (n_vec :int))

;; end event2/buffer.h


;; begin event2/bufferevent.h


(defconstant *BEV-EVENT-READING* #x01)
(defconstant *BEV-EVENT-WRITING* #x02)
(defconstant *BEV-EVENT-EOF* #x10)
(defconstant *BEV-EVENT-ERROR* #x20)
(defconstant *BEV-EVENT-TIMEOUT* #x40)
(defconstant *BEV-EVENT-CONNECTED* #x80)

(defconstant *BEV-OPT-CLOSE-ON-FREE* #x01)
(defconstant *BEV-OPT-THREADSAFE* #x02)
(defconstant *BEV-OPT-DEFER-CALLBACKS* #x04)
(defconstant *BEV-OPT-UNLOCK-CALLBACKS* #x08)


(defcfun (bufferevent-socket-new "bufferevent_socket_new") :pointer
  (base :pointer)
  (fd :int)
  (options :int))

(defcfun (bufferevent-socket-connect "bufferevent_socket_connect") :int
  (bufev :pointer)
  (addr :pointer)
  (socklen :int))

(defcfun (bufferevent-socket-connect-hostname
	  "bufferevent_socket_connect_hostname")
  :int
  (bufev :pointer)
  (evdns_base :pointer)
  (family :int)
  (hostname :pointer)
  (port :int))

(defcfun (bufferevent-socket-get-dns-error
	  "bufferevent_socket_get_dns_error")
  :int
  (bev :pointer))

(defcfun (bufferevent-base-set "bufferevent_base_set") :int
  (base :pointer)
  (bufev :pointer))

(defcfun (bufferevent-get-base "bufferevent_get_base") :pointer
  (bev :pointer))

(defcfun (bufferevent-priority-set "bufferevent_priority_set") :int
  (bufev :pointer)
  (pri :int))

(defcfun (bufferevent-get-priority "bufferevent_get_priority") :int
  (bufev :pointer))

(defcfun (bufferevent-free "bufferevent_free") :void
  (e :pointer))

(defcfun (bufferevent-setcb "bufferevent_setcb") :void
  (bufev :pointer)
  (readcb :pointer)
  (writecb :pointer)
  (eventcb :pointer)
  (cbarg :pointer))

(defcfun (bufferevent-getcb "bufferevent_getcb") :void
  (bufev :pointer)
  (readcb_ptr :pointer)
  (writecb_ptr :pointer)
  (eventcb_port :pointer)
  (cbarg_ptr :pointer))


(defcfun (bufferevent-setfd "bufferevent_setfd") :int
  (bufev :pointer)
  (fd :int))

(defcfun (bufferevent-getfd "bufferevent_getfd") :int
  (bufev :pointer))

(defcfun (bufferevent-get-underlying "bufferevent_get_underlying") :pointer
  (bufev :pointer))

(defcfun (bufferevent-write "bufferevent_write") :int
  (bufev :pointer)
  (data :pointer)
  (size :size))

(defcfun (bufferevent-write-buffer "bufferevent_write_buffer") :int
  (bufev :pointer)
  (buf :pointer))

(defcfun (bufferevent-read "bufferevent_read") :size
  (bufev :pointer)
  (data :pointer)
  (size :size))

(defcfun (bufferevent-read-buffer "bufferevent_read_buffer") :int
  (bufev :pointer)
  (buf :pointer))

(defcfun (bufferevent-get-input "bufferevent_get_input") :pointer
  (bufev :pointer))

(defcfun (bufferevent-get-output "bufferevent_get_output") :pointer
  (bufev :pointer))
(defcfun (bufferevent-enable "bufferevent_enable") :int
  (bufev :pointer)
  (event :short))

(defcfun (bufferevent-disable "bufferevent_disable") :int
  (bufev :pointer)
  (event :short))

(defcfun (bufferevent-get-enabled "bufferevent_get_enabled") :short
  (bufferev :pointer))

(defcfun (bufferevent-set-timeouts "bufferevent_set_timeouts") :int
  (bufev :pointer)
  (timeout_read :pointer)
  (timeout_write :pointer))

(defcfun (bufferevent-setwatermark "bufferevent_setwatermark") :void
  (bufev :pointer)
  (events :short)
  (lowmark :size)
  (highmark :size))

(defcfun (bufferevent-getwatermark "bufferevent_getwatermark") :int
  (bufev :pointer)
  (events :short)
  (lowmark :pointer)
  (highmark :pointer))

(defcfun (bufferevent-lock "bufferevent_lock") :void
  (bufev :pointer))

(defcfun (bufferevent-unlock "bufferevent_unlock") :void
  (bufev :pointer))

(defcfun (bufferevent-incref "bufferevent_incref") :void
  (bufev :pointer))

(defcfun (bufferevent-decref "bufferevent_decref") :int
  (bufev :pointer))

(defconstant *BEV-NORMAL* 0)
(defconstant *BEV-FLUSH* 1)
(defconstant *BEV-FINISHED* 2)

(defcfun (bufferevent-flush "bufferevent_flush") :int
  (bufev :pointer)
  (mode :int))

(defconstant *BEV-TRIG-IGNORE-WATERMARKS* (ash 1 16))
(defconstant *BEV-TRIG-DEFER-CALLBACKS* *BEV-OPT-DEFER-CALLBACKS*)

(defcfun (bufferevent-trigger "bufferevent_trigger") :void
  (bufev :pointer)
  (iotype :short)
  (options :int))

(defcfun (bufferevent-trigger-event "bufferevent_trigger_event") :void
  (bufev :pointer)
  (what :short)
  (options :int))

(defconstant *BEV-OK* 0)
(defconstant *BEV-NEED-MORE* 1)
(defconstant *BEV-ERROR* 2)

(defcfun (bufferevent-filter-new "bufferevent_filter_new") :pointer
  (underlying :pointer)
  (input_filter :pointer)
  (output_filter :pointer)
  (options :int)
  (free_context :pointer)
  (ctx :pointer))

(defcfun (bufferevent-pair-new "bufferevent_pair_new") :int
  (base :pointer)
  (options :int)
  (pair :pointer))

(defcfun (bufferevent-pair-get-partner "bufferevent_pair_get_partner")
  :pointer
  (bev :pointer))

(defcfun (ev-token-bucket-cfg-new "ev_token_bucket_cfg_new") :pointer
  (read_rate :size)
  (read_burst :size)
  (write_rate :size)
  (write_burst :size)
  (tick_len :pointer))

(defcfun (ev-token-bucket-cfg-free "ev_token_bucket_cfg_free") :void
  (cfg :pointer))

(defcfun (bufferevent-set-rate-limit "bufferevent_set_rate_limit") :int
  (bev :pointer)
  (cfg :pointer))

(defcfun (bufferevent-rate-limit-group "bufferevent_rate_limit_group")
  :pointer
  (base :pointer)
  (cfg :pointer))

(defcfun (bufferevent-rate-limit-group-set-cfg
	  "bufferevent_rate_limit_group_set_cf")
  :int
  (group :pointer)
  (cfg :pointer))

(defcfun (bufferevent-rate-limit-group-set-min-share
	  "bufferevent_rate_limit_group_set_min_share") :int
  (group :pointer)
  (len :size))


(defcfun (bufferevent-rate-limit-group-free
	  "bufferevent_rate_limit_group_free")
  :void
  (group :pointer))

(defcfun (bufferevent-add-to-rate-limit-group
	  "bufferevent_add_to_rate_limit_group") :int
  (bev :pointer)
  (g :pointer))

(defcfun (bufferevent-remove-from-rate-limit-group
	  "bufferevent_remove_from_rate_limit_group")
  :int
  (bev :pointer)
  (g :pointer))

(defcfun (bufferevent-set-max-single-read
	  "bufferevent_set_max_single_read")
  :int
  (bev :pointer)
  (size :size))

(defcfun (bufferevent-set-max-single-write
	  "bufferevent_set_max_single_write")
  :int
  (bev :pointer)
  (size :size))

(defcfun (bufferevent-get-max-single-read "bufferevent_get_max_single_read")
  :ssize
  (bev :pointer))

(defcfun (bufferevent-get-max-single-write "bufferevent_get_max_single_write")
  :ssize
  (bev :pointer))

(defcfun (bufferevent-get-read-limit "bufferevent_get_read_limit") :ssize
  (bev :pointer))

(defcfun (bufferevent-get-write-limit "bufferevent_get_write_limit") :ssize
  (bev :pointer))

(defcfun (bufferevent-get-max-to-read "bufferevent_get_max_to_read") :ssize
  (bev :pointer))

(defcfun (bufferevent-get-max-to-write "bufferevent_get_max_to_write") :ssize
  (bev :pointer))

(defcfun (bufferevent-get-token-bucket-cfg "bufferevent_get_token_bucket_cfg")
  :pointer
  (bev :pointer))

(defcfun (bufferevent-rate-limit-group-get-read-limit
	  "bufferevent_rate_limit_group_get_read_limit")
  :ssize
  (g :pointer))

(defcfun (bufferevent-rate-limit-group-get-write-limit
	  "bufferevent_rate_limit_group_get_write_limit")
  :ssize
  (g :pointer))

(defcfun (bufferevent-decrement-read-limit
	  "bufferevent_decrement_read_limit")
  :int
  (bev :pointer)
  (decr :ssize))

(defcfun (bufferevent-decrement-write-limit
	  "bufferevent_decrement_write_limit")
  :int
  (bev :pointer)
  (decr :ssize))

(defcfun (bufferevent-rate-limit-group-decrement-read
	  "bufferevent_rate_limit_group_decrement_read")
  :int
  (g :pointer)
  (size :ssize))

(defcfun (bufferevent-rate-limit-group-decrement-write
	  "bufferevent_rate_limit_group_decrement_write")
  :int
  (g :pointer)
  (size :ssize))

(defcfun (bufferevent-rate-limit-group-get-totals
	  "bufferevent_rate_limit_group_get_totals")
  :void
  (grp :pointer)
  (total_read_out :pointer)
  (total_write_out :pointer))

(defcfun (bufferevent-rate-limit-group-reset-totals
	  "bufferevent_rate_limit_group_reset_totals")
  :void
  (grp :pointer))

;; end event2/bufferevent.h


;; start event2/util.h


(defcfun (evutil-inet-ntop "evutil_inet_ntop") :string
  (af :int)
  (src :pointer)
  (dst :string)
  (size :size))

(defcfun (evutil-inet-pton "evutil_inet_pton") :int
  (af :int)
  (src :string)
  (dst :pointer))

(defcfun (evutil-freeaddrinfo "evutil_freeaddrinfo") :void
  (ai :pointer))

(defcfun (evutil-socketpair "evutil_socketpair") :int
  (d :int)
  (type :int)
  (protocol :int)
  (sv :pointer))

(defcfun (evutil-make-socket-nonblocking "evutil_make_socket_nonblocking")
  :int
  (sock :int))

(defcfun (evutil-make-listen-socket-reuseable
	  "evutil_make_listen_socket_reuseable")
  :int
  (sock :int))

(defcfun (evutil-make-listen-socket-reuseable-port
	  "evutil_make_listen_socket_reuseable_port")
  :int
  (sock :int))

(defcfun (evutil-make-listen-socket-ipv6only
	  "evutil_make_listen_socket_ipv6only")
  :int
  (sock :int))

(defcfun (evutil-make-socket-closeonexec "evutil_make_socket_closeonexec")
  :int
  (sock :int))

(defcfun (evutil-closesocket "evutil_closesocket") :int (sock :int))

(defcfun (evutil-make-tcp-listen-socket-deferred
	  "evutil_make_tcp_listen_socket_deferred")
  :int
  (sock :int))

(defcfun (evutil-parse-sockaddr-port "evutil_parse_sockaddr_port") :int
  (str :string)
  (out :pointer)
  (outlen :pointer))

;; end event2/util.h

;; start event2/dns.h

(defconstant *EVDNS-BASE-DISABLE-WHEN-INACTIVE* #x8000)
(defconstant *EVDNS-BASE-INITIALIZE-NAMESERVERS* 1)
(defconstant *EVDNS-BASE-NAMESERVERS-NO-DEFAULT* #x10000)

(defcfun (evdns-base-new "evdns_base_new") :pointer
  (event_base :pointer)
  (initialize_nameservers :int))

(defcfun (evdns-base-free "evdns_base_free") :void
  (base :pointer)
  (fail_requests :int))

(defcfun (evdns-base-clear-host-addresses "evdns_base_clear_host_addresses")
  :void
  (base :pointer))


(defcfun (evdns-err-to-string "evdns_err_to_string") :string
  (err :int))


(defcfun (evdns-base-nameserver-add "evdns_base_nameserver_add") :int
  (base :pointer)
  (address :ulong))

(defcfun (evdns-base-count-nameservers "evdns_base_count_nameservers") :int
  (base :pointer))

(defcfun (evdns-base-clear-nameservers-and-suspend
	  "evdns_base_clear_nameservers_and_suspend")
  :int
  (base :pointer))

(defcfun (evdns-base-resume "evdns_base_resume") :int (base :pointer))

(defcfun (evdns-base-nameserver-ip-add "evdns_base_nameserver_ip_add")
  :int
  (base :pointer)
  (ip_as_string :string))

(defcfun (evdns-base-nameserver-sockaddr-add
	  "evdns_base_nameserver_sockaddr_add")
  :int
  (base :pointer)
  (sa :pointer)
  (len :size)
  (flags :uint))

(defcfun (evdns-base-resolve-ipv4 "evdns_base_resolve_ipv4") :pointer
  (base :pointer)
  (name :string)
  (flags :int)
  (callback :pointer)
  (ptr :pointer))

(defcfun (evdns-base-resolve-ipv6 "evdns_base_resolve_ipv6") :pointer
  (base :pointer)
  (name :string)
  (flags :int)
  (callback :pointer)
  (ptr :pointer))

(defcfun (evdns-base-resolve-reverse "evdns_base_resolve_reverse")
  :pointer
  (base :pointer)
  (in :pointer)
  (flags :int)
  (callback :pointer)
  (ptr :pointer))

(defcfun (evdns-base-resolve-reverse-ipv6 "evdns_base_resolve_reverse_ipv6")
  :pointer
  (base :pointer)
  (in :pointer)
  (flags :int)
  (callback :pointer)
  (ptr :pointer))

(defcfun (evdns-cancel-request "evdns_cancel_request") :void
  (base :pointer)
  (req :pointer))

(defcfun (evdns-base-set-option "evdns_base_set_option") :int
  (base :pointer)
  (option :string)
  (val :string))

(defcfun (evdns-base-resolv-conf-parse "evdns_base_resolv_conf_parse") :int
  (base :pointer)
  (flags :int)
  (filename :string))

(defcfun (evdns-base-load-hosts "evdns_base_load_hosts") :int
  (base :pointer)
  (hosts_fname :string))

(defcfun (evdns-base-search-clear "evdns_base_search_clear") :void
  (base :pointer))

(defcfun (evdns-base-search-add "evdns_base_search_add") :void
  (base :pointer)
  (domain :string))

(defcfun (evdns-base-search-ndots-set "evdns_base_search_ndots_set") :void
  (base :pointer)
  (ndots :int))

(defcfun (evdns-set-log-fn "evdns_set_log_fn") :void
  (fn :pointer))

(defcfun (evdns-set-transaction-id-fn "evdns_set_transaction_id_fn") :void
  (fn :pointer))

(defcfun (evdns-set-random-bytes-fn "evdns_set_random_bytes_fn") :void
  (fn :pointer))

(defcfun (evdns-add-server-port-with-base "evdns_add_server_port_with_base")
  :pointer
  (base :pointer)
  (socket :int)
  (flags :int)
  (callback :pointer)
  (user_data :pointer))

(defcfun (evdns-close-server-port "evdns_close_server_port") :void
  (port :pointer))

(defcfun (evdns-server-request-set-flags "evdns_server_request_set_flags")
  :void
  (req :pointer)
  (flags :int))
(defcfun (evdns-server-request-add-reply "evdns_server_request_add_reply")
  :int
  (req :pointer)
  (section :int)
  (name :string)
  (type :int)
  (dns_class :int)
  (ttl :int)
  (datalen :int)
  (is_name :int)
  (data :pointer))

(defcfun (evdns-server-request-add-a-reply
	  "evdns_server_request_add_a_reply")
  :int
  (req :pointer)
  (name :string)
  (n :int)
  (addrs :pointer)
  (ttl :int))

(defcfun (evdns-server-request-add-aaaa-reply
	  "evdns_server_request_add_aaaa_reply")
  :int
  (req :pointer)
  (name :string)
  (n :int)
  (addrs :pointer)
  (ttl :int))
(defcfun (evdns-server-request-add-ptr-reply
	  "evdns_server_request_add_ptr_reply")
  :int
  (req :pointer)
  (in :pointer)
  (addrs :pointer)
  (hostname :string)
  (ttl :int))
(defcfun (evdns-server-request-add-cname-reply
	  "evdns_server_request_add_cname_reply")
  :int
  (req :pointer)
  (name :string)
  (cname :string)
  (ttl :int))

(defcfun (evdns-server-request-respond "evdns_server_request_respond") :int
  (req :pointer)
  (err :int))

(defcfun (evdns-server-request-drop "evdns_server_request_drop") :int
  (req :pointer))

(defcfun (evdns-server-request-get-requesting-addr
	  "evdns_server_request_get_requesting_addr")
  :int
  (req :pointer)
  (sa :pointer)
  (addr_len :int))

(defcfun (evdns-getaddrinfo "evdns_getaddrinfo") :pointer
  (dns_base :pointer)
  (nodename :string)
  (servvname :string)
  (hints_in :pointer)
  (cb :pointer)
  (arg :pointer))

(defcfun (evdns-getaddrinfo-cancel "evdns_getaddrinfo_cancel") :void
  (req :pointer))


(defcfun (evdns-base-get-nameserver-addr "evdns_base_get_nameserver_addr")
  :int
  (base :pointer)
  (idx :int)
  (sa :pointer)
  (len :size))

;; end event2/dns.h 

;; start event2/http.h

(defcfun (evhttp-new "evhttp_new") :pointer
  (base :pointer))

(defcfun (evhttp-bind-socket "evhttp_bind_socket") :int
  (http :pointer)
  (address :string)
  (port :uint16))

(defcfun (evhttp-bind-socket-with-handle "evhttp_bind_socket_with_handle")
  :pointer
  (http :pointer)
  (address :string)
  (port :uint16))

(defcfun (evhttp-accept-socket "evhttp_accept_socket") :int
  (http :pointer)
  (fd :int))

(defcfun (evhttp-accept-socket-with-handle
	  "evhttp_accept_socket_with_handle")
  :pointer
  (http :pointer)
  (fd :int))

(defcfun (evhttp-bind-listener "evhttp_bind_listener") :pointer
  (http :pointer)
  (listener :pointer))

(defcfun (evhttp-bound-socket-get-listener "evhttp_bound_socket_get_listener")
  :pointer
  (bound :pointer))

(defcfun (evhttp-foreach-bound-socket "evhttp_foreach_bound_socket") :void
  (function :pointer)
  (argument :pointer))

(defcfun (evhttp-del-accept-socket "evhttp_del_accept_socket") :void
  (http :pointer)
  (bound_socket :pointer))

(defcfun (evhttp-bound-socket-get-fd "evhttp_bound_socket_get_fd") :int
  (bound_socket :pointer))

(defcfun (evhttp-free "evhttp_free") :void
  (http :pointer))

(defcfun (evhttp-set-max-headers-size "evhttp_set_max_headers_size") :void
  (http :pointer)
  (max_headers_size :ssize))

(defcfun (evhttp-set-max-body-size "evhttp_set_max_body_size") :void
  (http :pointer)
  (max_body_size :ssize))

(defcfun (evhttp-set-default-content-type "evhttp_set_default_content_type")
  :void
  (http :pointer)
  (content_type :string))

(defcfun (evhttp-set-allowed-methods "evhttp_set_allowed_methods") :void
  (http :pointer)
  (methods :uint16))

(defcfun (evhttp-set-cb "evhttp_set_cb") :int
  (http :pointer)
  (path :string)
  (cb :pointer)
  (cb_arg :pointer))

(defcfun (evhttp-del-cb "evhttp_del_cb") :int
  (http :pointer)
  (path :pointer))

(defcfun (evhttp-set-gencb "evhttp_set_gencb") :void
  (http :pointer)
  (cb :pointer)
  (arg :pointer))

(defcfun (evhttp-set-bevcb "evhttp_set_bevcb") :void
  (http :pointer)
  (cb :pointer)
  (arg :pointer))

(defcfun (evhttp-add-virtual-host "evhttp_add_virtual_host") :int
  (http :pointer)
  (pattern :string)
  (vhost :pointer))

(defcfun (evhttp-remove-virtual-host "evhttp_remove_virtual_host") :int
  (http :pointer)
  (vhost :pointer))

(defcfun (evhttp-add-server-alias "evhttp_add_server_alias") :int
  (http :pointer)
  (alias :string))

(defcfun (evhttp-remove-server-alias "evhttp_remove_server_alias") :int
  (http :pointer)
  (alias :string))

(defcfun (evhttp-set-timeout "evhttp_set_timeout") :void
  (http :pointer)
  (timeout_in_secs :int))

(defcfun (evhttp-set-timeout-tv "evhttp_set_timeout_tv") :void
  (http :pointer)
  (tv :pointer))

(defconstant *EVHTTP-SERVER-LINGERING-CLOSE* #x0001)

(defcfun (evhttp-set-flags "evhttp_set_flags") :int
  (http :pointer)
  (flags :int))

(defcfun (evhttp-send-error "evhttp_send_error") :void
  (req :pointer)
  (err :int)
  (reason :string))

(defcfun (evhttp-send-reply "evhttp_send_reply") :void
  (req :pointer)
  (code :int)
  (reason :string)
  (databuf :pointer))

(defcfun (evhttp-send-reply-start "evhttp_send_reply_start") :void
  (req :pointer)
  (code :int)
  (reason :string))

(defcfun (evhttp-send-reply-chunk "evhttp_send_reply_chunk") :void
  (req :pointer)
  (databuf :pointer))

(defcfun (evhttp-send-reply-chunk-with-cb "evhttp_send_reply_chunk_with_cb")
  :void
  (cb :pointer)
  (arg :pointer))

(defcfun (evhttp-send-reply-end "evhttp_send_reply_end") :void
  (req :pointer))

(defconstant *EVHTTP-REQ-GET* (ash 1 0))
(defconstant *EVHTTP-REQ-POST* (ash 1 1))
(defconstant *EVHTTP-REQ-HEAD* (ash 1 2))
(defconstant *EVHTTP-REQ-PUT* (ash 1 3))
(defconstant *EVHTTP-REQ-DELETE* (ash 1 4))
(defconstant *EVHTTP-REQ-OPTIONS* (ash 1 5))
(defconstant *EVHTTP-REQ-TRACE* (ash 1 6))
(defconstant *EVHTTP-REQ-CONNECT* (ash 1 7))
(defconstant *EVHTTP-REQ-PATCH* (ash 1 9))

(defcenum evhttp-request-kind
  :EVHTTP-REQUEST
  :EVHTTP-RESPONSE)

(defcfun (evhttp-connection-base-bufferevent-new
	  "evhttp_connection_base_bufferevent_new")
  :pointer
  (base :pointer)
  (dnsbase :pointer)
  (bev :pointer)
  (address :string)
  (port :uint16))

(defcfun (evhttp-connection-get-bufferevent
	  "evhttp_connection_get_bufferevent")
  :pointer
  (evcon :pointer))

(defcfun (evhttp-connection-get-server "evhttp_connection_get_server")
  :pointer
  (evcon :pointer))

(defcfun (evhttp-request-new "evhttp_request_new")
  :pointer
  (cb :pointer)
  (arg :pointer))

(defcfun (evhttp-request-set-chunked-cb "evhttp_request_set_chunked_cb")
  :void
  (req :pointer)
  (cb :pointer))

(defcfun (evhttp-request-set-header-cb "evhttp_request_set_header_cb") :void
  (req :pointer)
  (cb :pointer))

(defcenum evhttp-request-error
  :EVREQ-HTTP-TIMEOUT
  :EVREQ-HTTP-EOF
  :EVREQ-HTTP-INVALID-HEADER
  :EVREQ-HTTP-BUFFER-ERROR
  :EVREQ-HTTP-REQUEST-CANCEL
  :EVREQ-HTTP-DATA-TOO-LONG)

(defcfun (evhttp-request-set-error-cb "evhttp_request_set_error_cb") :void
  (req :pointer)
  (cb :pointer))

(defcfun (evhttp-request-set-on-complete_cb
	  "evhttp_request_set_on_complete_cb")
  :void
  (req :pointer)
  (cb :pointer))

(defcfun (evhttp-request-free "evhttp_request_free") :void
  (req :pointer))

(defcfun (evhttp-connection-base-new "evhttp_connection_base_new") :pointer
  (base :pointer)
  (dnsbase :pointer)
  (address :string)
  (port :uint16))

(defcfun (evhttp-connection-set-family "evhttp_connection_set_family") :void
  (evcon :pointer)
  (family :int))

(defconstant *EVHTTP-CON-REUSE-CONNECTED-ADDR* #x0008)
(defconstant *EVHTTP-CON-READ-ON-WRITE-ERROR* #x0010)
(defconstant *EVHTTP-CON-LINGERING-CLOSE* #x0020)
(defconstant *EVHTTP-CON-PUBLIC-FLAGS-END #x100000)

(defcfun (evhttp-connection-set-flags "evhttp_connection_set_flags") :int
  (evcon :pointer)
  (flags :int))

(defcfun (evhttp-request-own "evhttp_request_own") :void (req :pointer))
(defcfun (evhttp-request-is-own "evhttp_request_is_own") :void (req :pointer))

(defcfun (evhttp-request-get-connection "evhttp_request_get_connection")
  :pointer
  (req :pointer))

(defcfun (evhttp-connection-get-base "evhttp_connection_get_base") :pointer
  (req :pointer))

(defcfun (evhttp-connection-set-max-headers-size
	  "evhttp_connection_set_max_headers_size")
  :void
  (evcon :pointer)
  (new_max_headers_size :ssize))

(defcfun (evhttp-connection-set-max-body-size
	  "evhttp_connection_set_max_body_size")
  :void
  (evcon :pointer)
  (new_max_body_size :ssize))

(defcfun (evhttp-connection-free "evhttp_connection_free") :void
  (evcon :pointer))

(defcfun (evhttp-connection-free-on-completion
	  "evhttp_connection_free_on_completion")
  :void
  (evcon :pointer))

(defcfun (evhttp-connection-set-local-address
	  "evhttp_connection_set_local_address")
  :void
  (evcon :pointer)
  (address :string))

(defcfun (evhttp-connection-set-local-port
	  "evhttp_connection_set_local_port")
  :void
  (evcon :pointer)
  (port :uint16))

(defcfun (evhttp-connection-set-timeout
	  "evhttp_connection_set_timeout")
  :void
  (evcon :pointer)
  (timeout_in_secs :int))

(defcfun (evhttp-connection-set-timeout-tv
	  "evhttp_connection_set_timeout_tv")
  :void
  (evcon :pointer)
  (tv :pointer))

(defcfun (evhttp-connection-set-initial-retry-tv
	  "evhttp_connection_set_initial_retry_tv")
  :void
  (evcon :pointer)
  (tv :pointer))

(defcfun (evhttp-connection-set-retries "evhttp_connection_set_retries")
  :void
  (evcon :pointer)
  (retry_max :int))

(defcfun (evhttp-connection-set-closecb "evhttp_connection_set_closecb")
  :void
  (evcon :pointer)
  (cb :pointer)
  (arg :pointer))

(defcfun (evhttp-connection-get-peer "evhttp_connection_get_peer")
  :void
  (evcon :pointer)
  (address :pointer)
  (port :pointer))

(defcfun (evhttp-connection-get-addr "evhttp_connection_get_addr") :pointer
  (evcon :pointer))

(defcfun (evhttp-make-request "evhttp_make_request") :int
  (evcon :pointer)
  (req :pointer)
  (type :int)
  (uri :string))

(defcfun (evhttp-cancel-request "evhttp_cancel_request") :void
  (req :pointer))

(defcfun (evhttp-request-get-uri "evhttp_request_get_uri") :string
  (req :pointer))

(defcfun (evhttp-request-get-evhttp-uri "evhttp_request_get_evhttp_uri")
  :pointer
  (req :pointer))

(defcfun (evhttp-request-get-command "evhttp_request_get_command") :int
  (req :pointer))

(defcfun (evhttp-request-get-response-code
	  "evhttp_request_get_response_code")
  :int
  (req :pointer))

(defcfun (evhttp-request-get-response-code-line
	  "evhttp_request_get_response_code_line")
  :pointer
  (req :pointer))

(defcfun (evhttp-request-get-input-headers
	  "*evhttp_request_get_input_headers")
  :pointer
  (req :pointer))

(defcfun (evhttp-request-get-output-headers
	  "*evhttp_request_get_output_headers")
  :pointer
  (req :pointer))
(defcfun (evhttp-request-get-input-buffer
	  "*evhttp_request_get_input_buffer")
  :pointer
  (req :pointer))

(defcfun (evhttp-request-get-output-buffer
	  "*evhttp_request_get_output_buffer")
  :pointer
  (req :pointer))

(defcfun (evhttp-request-get-host "evhttp_request_get_host") :string
  (req :pointer))

(defcfun (evhttp-find-header "evhttp_find_header") :string
  (headers :pointer)
  (key :string))

(defcfun (evhttp-remove-header "evhttp_remove_header") :int
  (headers :pointer)
  (key :pointer))

(defcfun (evhttp-add-header "evhttp_add_header") :int
  (key :string)
  (value :string))

(defcfun (evhttp-clear-headers "evhttp_clear_headers") :void
  (headers :pointer))

(defcfun (evhttp-encode-uri "evhttp_encode_uri") :string (str :string))

(defcfun (evhttp-uriencode "evhttp_uriencode") :string
  (str :string)
  (size :ssize)
  (space_to_plus :int))


(defcfun (evhttp-decode-uri "evhttp_decode_uri") :string (uri :string))

(defcfun (evhttp-uridecode "evhttp_uridecode") :string
  (uri :string)
  (decode_plus :int)
  (size_out :size))

(defcfun (evhttp-parse-query "evhttp_parse_query") :int
  (uri :string)
  (headers :pointer))

(defcfun (evhttp-parse-query-str "evhttp_parse_query_str") :int
  (uri :pointer)
  (headers :pointer))

(defcfun (evhttp-htmlescape "evhttp_htmlescape") :string
  (html :string))

(defcfun (evhttp-uri-new "evhttp_uri_new") :pointer)

(defcfun (evhttp-uri-set-flags "evhttp_uri_set_flags") :void
  (uri :pointer)
  (flags :uint))

(defcfun (evhttp-uri-get-scheme "evhttp_uri_get_scheme") :string
  (uri :pointer))

(defcfun (evhttp-uri-get-userinfo "evhttp_uri_get_userinfo") :string
  (uri :pointer))

(defcfun (evhttp-uri-get-host "evhttp_uri_get_host") :string (uri :string))
(defcfun (evhttp-uri-get-port "evhttp_uri_get_port") :int (uri :string))
(defcfun (evhttp-uri-get-path "evhttp_uri_get_path") :string (uri :string))
(defcfun (evhttp-uri-get-query "evhttp_uri_get_query") :string (uri :string))
(defcfun (evhttp-uri-get-fragment "evhttp_uri_get_fragment") :string (uri :string))

(defcfun (evhttp-uri-set-scheme "evhttp_uri_set_scheme") :int
  (uri :string) (scheme :string))
(defcfun (evhttp-uri-set-userinfo "evhttp_uri_set_userinfo") :int
  (uri :string) (userinfo :string))
(defcfun (evhttp-uri-set-host "evhttp_uri_set_host") :int
  (uri :string) (host :string))
(defcfun (evhttp-uri-set-port "evhttp_uri_set_port") :int
  (uri :string) (port :int))
(defcfun (evhttp-uri-set-path "evhttp_uri_set_path") :int
  (uri :string) (path :string))
(defcfun (evhttp-uri-set-query "evhttp_uri_set_query") :int
  (uri :string) (query :string))
(defcfun (evhttp-uri-set-fragment "evhttp_uri_set_fragment") :int
  (uri :string) (fragment :string))


(defcfun (evhttp-uri-parse-with-flags "evhttp_uri_parse_with_flags") :pointer
  (source_uri :string)
  (flags :uint))

(defconstant *EVHTTP-URI-MONCONFORMANT #x01)

(defcfun (evhttp-uri-parse "evhttp_uri_parse") :pointer (source_uri :string))

(defcfun (evhttp-uri-free "evhttp_uri_free") :void (uri :pointer))

(defcfun (evhttp-uri-join "evhttp_uri_join") :string
  (uri :pointer)
  (buf :pointer)
  (limit :size))

;; end event2/http.h

;; start event2/bufferevent_ssl.h

(defconstant *BUFFEREVENT-SSL-OPEN* 0)
(defconstant *BUFFEREVENT-SSL-CONNECTING* 1)
(defconstant *BUFFEREVENT-SSL-ACCEPTING* 2)

(defcfun (bufferevent-openssl-filter-new "bufferevent_openssl_filter_new")
  :pointer
  (base :pointer)
  (underlying :pointer)
  (ssl :pointer)
  (state :int)
  (options :int))

(defcfun (bufferevent-openssl-socket-new "bufferevent_openssl_socket_new")
  :pointer
  (base :pointer)
  (fd :int)
  (ssl :pointer)
  (state :int)
  (options :int))

(defcfun (bufferevent-openssl-get-allow-dirty-shutdown
	  "bufferevent_openssl_get_allow_dirty_shutdown")
  :int
  (bev :pointer))

(defcfun (bufferevent-openssl-set-allow-dirty-shutdown
	  "bufferevent_openssl_set_allow_dirty_shutdown")
  :void
  (bev :pointer)
  (allow_dirty_shutdown :int))

(defcfun (bufferevent_openssl_get_ssl "bufferevent_openssl_get_ssl")
  :pointer
  (bufev :pointer))

(defcfun (bufferevent-ssl-renegotiate "bufferevent_ssl_renegotiate") :int
  (befev :pointer))

(defcfun (bufferevent-get-openssl-error "bufferevent_get_openssl_error") :long
  (bev :pointer))

