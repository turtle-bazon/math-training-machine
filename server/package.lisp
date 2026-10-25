;;;; -*- mode: lisp -*-

(defpackage #:mtm-server
  (:use #:cl)
  (:export #:*port*
           #:start
           #:stop
           #:main
           #:shutdown))

(in-package #:mtm-server)

;;; Paths ---------------------------------------------------------------

(defun project-root ()
  "Directory the server was started from (run everything from there)."
  (uiop:getcwd))

(defparameter *config* nil
  "Plist read from the configuration file, or NIL when there is none.
  Filled in by load-config! before the server starts.")

(defparameter *config-file* nil
  "Path of the configuration file in use, or NIL.")

(defun setting (env-name key default)
  "Effective value of a setting: environment variable, then config file,
  then DEFAULT."
  (or (uiop:getenvp env-name)
      (and *config* (getf *config* key))
      default))

(defun resolve-dir (raw)
  "Turn a configured path into an absolute directory. Relative values in
  mtm.conf (\"data/\") mean \"next to where the server was started\" -
  absolute paths are what the store and the static handler expect."
  (uiop:ensure-absolute-pathname
   (uiop:ensure-directory-pathname raw)
   (project-root)))

(defun data-dir ()
  "Where the conskivi database lives (MTM_DATA / :data-dir)."
  (resolve-dir (setting "MTM_DATA" :data-dir "data/")))

(defun public-dir ()
  "Static assets on disk (MTM_PUBLIC / :public-dir). Absent or partial is
  fine: whatever is missing comes from the copy embedded in the image."
  (resolve-dir (setting "MTM_PUBLIC" :public-dir "public/")))

(defun parse-int (value &optional default)
  (handler-case (parse-integer value :junk-allowed t)
    (error () default)))

;;; JSON helpers ---------------------------------------------------------

(defun jobj (&rest pairs)
  "Build a JSON object from a plist, string keys."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash (string-downcase (string k)) h) v))
    h))

(defun json-response (status object &key extra-headers)
  (list status
        (append '(:content-type "application/json; charset=utf-8"
                  :access-control-allow-origin "*"
                  :cache-control "no-store")
                extra-headers)
        (list (with-output-to-string (s) (yason:encode object s)))))

(defun json-error (status message)
  (json-response status (jobj :error message)))

(defun cors-headers ()
  '(:access-control-allow-origin "*"
    :access-control-allow-methods "GET, POST, OPTIONS"
    :access-control-allow-headers "content-type"))
