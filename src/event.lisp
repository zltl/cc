
(ql:quickload "cffi")

;; bind libevent

(in-package :cc-event)

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

(defvar *EVENT-BASE-COUNT-ACTIVE* 1)
(defvar *EVENT-BASE-COUNT-VIRTUAL* 2)
(defvar *EVENT-BASE-COUNT-ADDED* 4)

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

(defvar *EV-FEATURE-ET* #x01)
(defvar *EV-FEATURE-O1* #x02)
(defvar *EV-FEATURE-FDS* #x04)
(defvar *EV-FEATURE-CLOSE* #x08)

(defvar *EVENT-BASE-FLAG-NOLOCK* #x01)
(defvar *EVENT-BASE-FLAG-IGNORE-ENV* #x02)
(defvar *EVENT-BASE-FLAG-STARTUP-IOCP* #x04)
(defvar *EVENT-BASE-FLAG-NO-CACHE-TIME* #x08)
(defvar *EVENT-BASE-FLAG-EPOLL-USE-CHANGELIST* #x10)
(defvar *EVENT-BASE-FLAG-PRECISE-TIMER* #x20)

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

(defvar *EVENT-LOG-DEBUG* 0)
(defvar *EVENT-LOG_MSG* 1)
(defvar *EVENT-LOG-WARN* 2)
(defvar *EVENT-LOG-ERR* 3)

(defcfun (event-set-log-callback "event_set_log_callback") :void
  (cb :pointer))

(defcfun (event-set-fatal-callback "event_set_fatal_callback") :void
  (cb :pointer))

(defvar *EVENT-DBG-ALL* #xffffffff)
(defvar *EVENT-DBG-NONE* 0)

(defcfun (event-enable-debug-logging "event_enable_debug_logging") :void
  (which :uint32))

(defcfun (event-base-set "event_base_set") :int
  (eb :pointer)
  (ev :pointer))

(defvar *EVLOOP-ONCE* #x01)
(defvar *EVLOOP-NONBLOCK* #x02)
(defvar *EVLOOP-NO-EXIT-ON-EMPTY* #x04)

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

(defvar *EV-TIMEOUT* #x01)
(defvar *EV-READ* #x02)
(defvar *EV-WRITE* #x04)
(defvar *EV-SIGNAL* #x08)
(defvar *EV-PERSIST* #x10)
(defvar *EV-ET* #x20)
(defvar *EV-FINALIZE* #x40)
(defvar *EV-CLOSED* #x80)

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

(defun ev ()
  (defparameter *ev-version* (event-get-version)))

