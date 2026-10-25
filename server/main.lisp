;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

(defparameter *port* 8123
  "Default TCP port. Override with MTM_PORT - read at start-up, never at
  build time, so a dumped binary honours the environment.")

(defparameter *address* "127.0.0.1"
  "Default bind address. Override with MTM_ADDRESS (0.0.0.0 for the LAN).")

(defparameter *server* nil)

(defun env-port ()
  (let ((raw (uiop:getenvp "MTM_PORT")))
    (when raw (parse-int raw *port*))))

(defun env-address ()
  (or (uiop:getenvp "MTM_ADDRESS") *address*))

(defun start (&key port address server)
  "Open the conskivi store and start the Clack/Wookie listener."
  (let ((port (or port (env-port) *port*))
        (address (or address (env-address))))
    (open-store)
    (setf *server*
          (clack:clackup #'app
                         :server (or server :wookie)
                         :address address
                         :port port
                         :debug nil
                         :silent t))
    (format t "~&[mtm] Math Training Machine serving on http://~a:~a/~%"
            address port)
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
  (let ((port (or (env-port) *port*))
        (address (env-address)))
    (start)
    (format t "~&[mtm] api:    http://~a:~a/api/leaderboard?mode=sum~%~
               [mtm] health: http://~a:~a/api/health~%~
               [mtm] stop with Ctrl-C~%"
            address port address port)
    (finish-output)
    #+sbcl (pushnew #'shutdown sb-ext:*exit-hooks*)
    (handler-case
        (loop (sleep 3600))
      #+sbcl (sb-sys:interactive-interrupt () nil)
      (serious-condition () nil))
    (shutdown)))
