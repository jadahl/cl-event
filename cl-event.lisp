;; Description: cl-event
;; Author: Jonas Ådahl <tox@dtek.chalmers.se>
;; License: LGPL
;; (c) Copyright 2006

(in-package :event)

(defstruct flags
  (ev-read t)
  (ev-write nil)
  (ev-signal nil)
  (ev-persist nil))

(defstruct timeout
  (secs 1)
  (usecs 0))

(defun generate-flag (f)
  (let ((flag 0))
    (dolist (flag-slot '(ev-read ev-write ev-signal ev-persist))
      (let ((flag-slot-accessor
              (read-from-string (format nil "event::flags-~a" flag-slot)))
            (flag-number (read-from-string (format nil "event::*~a*" flag-slot))))
        (when (funcall flag-slot-accessor f)
          (setf flag (logior flag (eval flag-number))))))
    flag))

(defclass event ()
  ((event :accessor event-event
          :initform (foreign-alloc 'event))
   (timeouts :accessor event-timeouts
             :initform nil)))

(defgeneric free-event (event))
(defgeneric free-event-structs (event))
(defgeneric event-set (event stream flags callback arg))
(defgeneric evtimer-set (event callback arg))
(defgeneric event-add (event &optional timeout))
(defgeneric evtimer-add (event &optional timeout))
(defgeneric event-del (event))
(defgeneric evtimer-del (event))

;; this should be run manually when cffi doesn't provide finalize
(defmethod free-event ((e event))
  (when (event-event e)
    (free-event-structs e)))

(defmethod free-event-structs ((e event))
  (foreign-free (event-event e))
  (setf (event-event e) nil)
  (dolist (tv (event-timeouts e))
    (foreign-free tv)))

(defmethod initialize-instance :before ((e event) &key)
  (when (not *event-initialized*)
    (event-init)))

(defmethod initialize-instance :after ((e event) &key)
  (handler-case
    (finalize e (lambda () (free-event-structs e)))
    (undefined-function (condition)
                        (format t "WARNING: You are using a too old version of cffi, allocated data structures will not be free:ed~%"))))

(defmethod event-set ((e event) stream flags callback arg)
  "Associate a callback with an event."
  (let ((callback-block (lambda () (funcall callback arg))))
    (defcallback cb :void ((fd :int) (event :short) (arg :pointer))
                 (funcall callback-block))

    (c-raw-event-set (event-event e)
                     (get-file-handle stream)
                     (generate-flag flags)
                     (callback cb)
                     (null-pointer))))

(defmethod evtimer-set ((e event) callback arg)
  "Associate a callback with a timer."
  (make-callback cb callback)
  (foreign-funcall "evtimer_set"
                   :pointer (event-event e)
                   :pointer (callback cb)
                   :pointer (null-pointer)))

(defmethod event-add ((e event) &optional timeout)
  (let* ((tv (if timeout
               (foreign-alloc 'timeval)
               (null-pointer))))
    (when (not (null-pointer-p tv))
      (setf (foreign-slot-value tv 'timeval 'tv-secs) (timeout-secs timeout)
            (foreign-slot-value tv 'timeval 'tv-usecs) (timeout-usecs timeout))
      ;; add timeval structs to event class so they will be finalized
      ;; Should they actually be free:ed in the callback?
      (pushnew tv (event-timeouts e)))
    (foreign-funcall "event_add" :pointer (event-event e) :pointer tv)))

(defmethod evtimer-add ((e event) &optional timeout)
  ;; evtimer_add is just a ``short'' cut to event_add
  (event-add e timeout))

(defmethod event-del ((e event))
  (if (not (eq (foreign-funcall "event_del" :pointer (event-event e) :int) 0))
    (error "event-del-failed")))

(defmethod evtimer-del ((e event))
  (event-del e))

(defmacro add-stream-callback-block (stream body &optional f)
  `(add-stream-callback ,stream (lambda () ,body) ,f))

(defun add-event-callback (stream flags callback arg &optional (timeout nil))
  (let ((e (make-instance 'event)))
    (event-set e stream flags callback arg)
    (event-add e timeout)))

(defmacro make-callback (name body)
  `(let ((my-name ,name))
     (defcallback my-name :void ((fd :int) (event :short) (arg :pointer))
                  ,body)))
