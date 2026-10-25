;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

;;; Server configuration: listen address, listen port, where the conskivi
;;; database lives and where the static assets come from.
;;;
;;; The file is a Lisp plist, exactly like focus.conf:
;;;
;;;   (:bind-address "0.0.0.0"
;;;    :bind-port 8123
;;;    :data-dir "data/"
;;;    :public-dir "public/")
;;;
;;; Precedence for every setting: environment variable -> config file ->
;;; compiled-in default.  That way mtm.conf describes a deployment while
;;; MTM_* stays handy for tests and one-off runs.

(defparameter *port* 8123
  "Default TCP port (MTM_PORT / :bind-port).")

(defparameter *address* "127.0.0.1"
  "Default bind address (MTM_ADDRESS / :bind-address). 0.0.0.0 for the LAN.")

;;; --- discovery ---------------------------------------------------------

(defun find-config ()
  "Configuration file to read, or NIL when there is none.
  $MTM_CONFIG names an explicit file and is an error when missing."
  (or (let ((explicit (uiop:getenvp "MTM_CONFIG")))
        (when explicit
          (or (probe-file explicit)
              (error "MTM_CONFIG points to ~a, which does not exist" explicit))))
      (probe-file "mtm.conf")
      (probe-file (merge-pathnames ".mtm.conf" (user-homedir-pathname)))
      (probe-file (merge-pathnames ".config/mtm/mtm.conf" (user-homedir-pathname)))
      (probe-file "/etc/mtm.conf")))

(defun validate-config (config)
  (unless (and (listp config) (evenp (length config)))
    (error "configuration must be a plist like (:bind-port 8123)"))
  (flet ((str (key)
           (let ((v (getf config key)))
             (unless (or (null v) (stringp v))
               (error "~a must be a string, got ~s" key v))
             v))
         (port (key)
           (let ((v (getf config key)))
             (unless (or (null v) (and (integerp v) (<= 1 v 65535)))
               (error "~a must be an integer between 1 and 65535, got ~s" key v))
             v)))
    (str :bind-address)
    (port :bind-port)
    (str :data-dir)
    (str :public-dir)
    (str :stats-path))
  t)

(defun read-config (file)
  "Read FILE as a plist. Reader eval is off: the file may only contain data."
  (handler-case
      (let ((*read-eval* nil))
        (with-open-file (stream file)
          (let ((config (read stream nil nil)))
            (validate-config config)
            config)))
    (error (e)
      (error "bad configuration in ~a: ~a" file e))))

(defun load-config! ()
  "Locate and read the configuration file. Finding no file is fine - the
  environment and the defaults take over from there."
  (setf *config* nil
        *config-file* nil)
  (let ((file (find-config)))
    (when file
      (setf *config* (read-config file)
            *config-file* (namestring file))))
  *config-file*)

;;; --- effective settings ------------------------------------------------

(defun configured-port ()
  "Environment, then config file, then the default."
  (let ((raw (setting "MTM_PORT" :bind-port *port*)))
    (if (integerp raw)
        raw
        (let ((port (parse-int raw)))
          (unless (and port (<= 1 port 65535))
            (error "listen port must be 1-65535, got ~s" raw))
          port))))

(defun configured-address ()
  (setting "MTM_ADDRESS" :bind-address *address*))

(defun configured-stats-path ()
  "Secret stats page path, or NIL when disabled (MTM_STATS_PATH / :stats-path)."
  (let ((raw (setting "MTM_STATS_PATH" :stats-path "/stats")))
    (when (and (stringp raw) (plusp (length raw)))
      (if (char= (char raw 0) #\/)
          raw
          (concatenate 'string "/" raw)))))

(defun describe-config ()
  "Banner block: which file was read and what it all comes out to."
  (format nil "[mtm] config:  ~a~%~
               [mtm] listen:  ~a:~a~%~
               [mtm] data:    ~a~%~
               [mtm] public:  ~a"
          (or *config-file* "none - defaults + environment")
          (configured-address)
          (configured-port)
          (data-dir)
          (public-dir)))
