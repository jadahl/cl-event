(in-package :cl-user)

(defpackage :event
  (:use :cl :cl-user :cffi)
  (:export
    ;; add more stuff here later
    :event
    :event-init
    :event-add
    :evtimer-add
    :event-set
    :evtimer-set
    :event-del
    :evtimer-del
    :add-event-callback-block
    :add-event-callback
    :event-dispatch
    :flags :make-flags
    :timeout))
