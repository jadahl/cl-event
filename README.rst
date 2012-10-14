########
cl-event
########

Usage
*****

.. code:: lisp

    (in-package :event)
    (defun my-callback (s)
      ...)

    (defvar s ( ... open stream ... ))


Usage Approaches
================

Functions
+++++++++

.. code:: lisp

    (event-init) ;; initialize libevent
    (defvar e (make-instance 'event)) ;; create an instance of an event
    (event-set e s (make-flags :ev-persist t) 'my-callback s) ;; associate event with a callback and a stream
    (event-add e) ;; add event to libevent event-base

    (event-dispatch) ;; main loop of libevent, calls my-callback whenever there is anything to read on s


Objects
+++++++

or this way:
consider we have a CLOS object containing a stream and other stuff called c
and a method called READ-CONNECTION that takes a connection object as
argument.

.. code:: lisp

    (add-event-callback (connection-stream c) (make-flags :ev-persist t) 'read-connection c)
    (event-dispatch)
