;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

;;; Start-up: read the configuration, open the store, listen.

(defparameter *server* nil)

(defun start (&key port address server)
  "Read the configuration, open the conskivi store and start the
  Clack/Hunchentoot listener."
  (load-config!)
  (let ((port (or port (configured-port)))
        (address (or address (configured-address))))
    (open-store)
    (setf *server*
          (clack:clackup #'app
                          :server (or server :wookie)
                         :address address
                         :port port
                         :debug nil))
    (format t "~&[mtm] serving on http://~a:~a/~%" address port)
    (finish-output)
    *server*))

(defun stop ()
  (when *server*
    (ignore-errors (clack:stop *server*))
    (setf *server* nil))
  (close-store)
  t)

(defun shutdown ()
  "Best effort cleanup, used as an exit hook."
  (ignore-errors (stop))
  nil)

(defun main ()
  ;; Re-open shared objects dropped at image-dump time (focus approach).
  #+sbcl
  (let ((pkg (find-package "LIBUV")))
    (format t "~&[mtm] libuv package: ~a~%" pkg)
    (format t "~&[mtm] reloading libuv...~%")
    (handler-case (cffi:load-foreign-library 'libuv::libuv)
      (error (e) (format t "~&[mtm] load-foreign-library error: ~a~%" e)))
    (format t "~&[mtm] libuv loaded-p: ~a~%" (cffi:foreign-library-loaded-p 'libuv::libuv)))
  (load-config!)
  (let ((port (configured-port))
        (address (configured-address)))
    (format t "~&~a~%" (describe-config))
    (start :port port :address address)
    (format t "~&[mtm] api:    http://~a:~a/api/leaderboard?mode=sum~%~
               [mtm] health: http://~a:~a/api/health~%~
               [mtm] stop with Ctrl-C~%"
            address port address port)
    (finish-output)
    #+sbcl (pushnew #'shutdown sb-ext:*exit-hooks*)
    (handler-case
        (loop
          (sleep 60)
          (format t "~&[mtm] heartbeat~%")
          (finish-output))
      #+sbcl (sb-sys:interactive-interrupt () nil)
      (serious-condition (e)
        (format *error-output* "~&[mtm] serious condition: ~a~%" e)
        (finish-output)
        nil))
    (shutdown)))
