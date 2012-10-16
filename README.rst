########
cl-event
########


Usage
*****


Beginning Your Code
+++++++++++++++++++

To use this ``cl-event``, start out with creating a callback and a stream:

.. code:: lisp

    (in-package :event)
    (defun my-callback (s)
      ...)

    (defvar s ( ... open stream ... ))

    ; initialize libevent
    (event-init)

and then either explicitly calling functions from the API or using a
convenience function instead.


The Middle Bits
+++++++++++++++


Option 1: Explicit Calls
========================

Here are the functions that you'll need to call in order to create an event and
have it scheduled:

.. code:: lisp

    ; create an instance of an event
    (defvar e (make-instance 'event))
    ; associate event with a callback and a stream
    (event-set e s (make-flags :ev-persist t) 'my-callback s)
    ; add event to libevent event-base
    (event-add e)


Option 2: Using an Object
=========================

If you would rather use the convenience function ``add-event-callback``, you
won't need to manually instantiate an event as was illustrated in the "Explicit
Calls" section.

Consider that we have defiend a CLOS object (elsewhere) containing a stream and
other stuff called ``CONNECTION`` and a method called ``READ-CONNECTION`` that
takes a ``CONNECTION`` object as argument. You can can implicitly create an
event, set the callback on it, and then schedule the event with the the
``add-event-callback`` convenience function:

.. code:: lisp

    (add-event-callback (connection-stream connection)
      (make-flags :ev-persist t) 'read-connection connection)


Finishing Up
++++++++++++

.. code:: lisp

    ; main loop of libevent, calls my-callback (or read-connection) as defined
    above whenever there is anything to read on the given stream
    (event-dispatch)
