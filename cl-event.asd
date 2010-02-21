(defpackage cl-event-system
  (:use :common-lisp :asdf))

(in-package cl-event-system)

(defsystem cl-event
           :version "0.0.1"
           :licence "BSD"
           :depends-on (:cffi)
           :components
           ((:file "packages")
            (:file "utility" :depends-on ("packages"))
            (:file "event-ffi" :depends-on ("packages"))
            (:file "cl-event" :depends-on ("packages" "event-ffi" "utility"))))
