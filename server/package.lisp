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

(defun data-dir ()
  (uiop:ensure-directory-pathname
   (or (uiop:getenvp "MTM_DATA")
       (merge-pathnames "data/" (project-root)))))

(defun public-dir ()
  (uiop:ensure-directory-pathname
   (or (uiop:getenvp "MTM_PUBLIC")
       (merge-pathnames "public/" (project-root)))))

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
