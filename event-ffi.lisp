;; Description: cl-event
;; Author: Jonas Ådahl <tox@dtek.chalmers.se>
;; License: LGPL
;; (c) Copyright 2006

(in-package :event)

;;(define-foreign-library libevent
;;                            (:unix (:or "libevent0-1a.so.1" "libevent.so"))
;;                        (t (:default "libevent")))

;;(use-foreign-library libevent)

(load-foreign-library '(:default "libevent"))

(defconstant *ev-timeout* #x01)
(defconstant *ev-read* #x02)
(defconstant *ev-write* #x04)
(defconstant *ev-signal* #x08)
(defconstant *ev-persist* #x10)

(defvar *event-initialized* nil)

(defcstruct timeval
            (tv-secs :long)
            (tv-usecs :long))

(defcstruct tailq-entry
            (tqe-next :pointer)
            (tqe-prev :pointer))

(defcstruct rb-entry
            (rbe-left :pointer)
            (rbe-right :pointer)
            (rbe-parent :pointer)
            (rbe-color :int))

(defcstruct event
            (ev-next tailq-entry)
            (ev-active-next tailq-entry)
            (ev-signal-next tailq-entry)
            (ev-timeout-node rb-entry)
            (ev-base :pointer)
            (ev-fd :int)
            (ev-events :short)
            (ev-ncalls :short)
            (ev-pncalls :pointer)
            (ev-timoeut timeval)
            (ev-pri :int)
            (ev-callback :pointer)
            (ev-arg :pointer)
            (ev-res :int)
            (ev-flags :int))

;; void event_init()
(defun event-init ()
  (when (not *event-initialized*)
    (foreign-funcall "event_init" :void)
    (setf *event-initialized* t)))

;; void event_set(struct event *ev, int fd, short event, void (*fn)(int, short, void *), void *arg);
(defcfun ("event_set" c-raw-event-set) 
         :void
         (ev :pointer)
         (fd :int)
         (flags :short)
         (callback :pointer)
         (arg :pointer))

(defun c-event-set (ev stream flags callback)
  (c-raw-event-set (event-event e)
                   (get-file-handle stream)
                   (generate-flag flags)
                   callback
                   (null-pointer)))

;; int event_add(struct event *ev, struct timeval *tv);
;; int event_del(struct event *ev);
;; int evtimer_add(struct event *ev, struct timeval *tv);
;; int evtimer_del(struct event *ev);

;; int event_dispatch()
(defcfun ("event_dispatch" c-raw-event-dispatch) :int)

(defun event-dispatch ()
  (let ((r (c-raw-event-dispatch)))
    (cond
      ((eq r 1) t)
      ((eq r 0) nil)
      (t (error (format nil "event-dispatch: ~a" (foreign-funcall "strerror" :string)))))))

