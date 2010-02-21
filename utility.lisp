(in-package :event)

(defun get-file-handle (stream)
  #+clisp (socket:stream-handles stream)
  #+sbcl (sb-sys:fd-stream-fd stream))
