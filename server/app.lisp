;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

;;; --- helpers -----------------------------------------------------------

(defun qparam (env name &optional default)
  (let ((qs (getf env :query-string)))
    (if (and qs (plusp (length qs)))
        (or (cdr (assoc name (quri:uri-query-params (quri:make-uri :query qs))
                        :test #'string=))
            default)
        default)))

(defun request-body-string (env)
  (let ((stream (getf env :raw-body)))
    (when stream
      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer t)))
        (loop for byte = (read-byte stream nil nil)
              while byte do (vector-push-extend byte bytes))
        (babel:octets-to-string bytes :encoding :utf-8 :errorp nil)))))

(defun body-as-json (env)
  (let ((raw (request-body-string env)))
    (when (and raw (plusp (length raw)))
      (handler-case (yason:parse raw)
        (error () nil)))))

(defun jstr (table key)
  (let ((v (gethash key table)))
    (if (stringp v) v nil)))

(defun jint (table key &optional default)
  (let ((v (gethash key table)))
    (if (integerp v) v default)))

;;; --- API ---------------------------------------------------------------

(defun handle-leaderboard-get (env)
  (let* ((mode  (qparam env "mode" "sum"))
         (limit (min 100 (max 1 (parse-int (qparam env "limit" "20") 20)))))
    (if (mode-p mode)
        (json-response 200
                       (jobj :mode mode
                             :entries (mapcar (lambda (e) (apply #'jobj e))
                                              (or (top mode limit) '()))))
        (json-error 400 "unknown mode"))))

(defun handle-leaderboard-post (env)
  (let ((body (body-as-json env)))
    (unless (hash-table-p body)
      (return-from handle-leaderboard-post (json-error 400 "invalid json")))
    (let* ((mode  (jstr body "mode"))
           (name  (clean-name (jstr body "name")))
           (score (jint body "score" -1))
           (correct (jint body "correct" -1))
           (total   (jint body "total" -1))
           (streak  (jint body "best-streak" 0))
           (avg-ms  (jint body "avg-ms" 0)))
      (cond
        ((not (mode-p mode))
         (json-error 400 "unknown mode"))
        ((or (zerop (length name)) (> (length name) 128))
         (json-error 400 "name must be 1-16 characters"))
        ((or (< score 0) (> score 10000000))
         (json-error 400 "score out of range"))
        ((or (< correct 0) (< total 1) (> correct total) (> total 1000))
         (json-error 400 "question counts out of range"))
        (t
         (let ((result (add-score (list :name name
                                        :mode mode
                                        :score score
                                        :correct correct
                                        :total total
                                        :best-streak (max 0 streak)
                                        :avg-ms (max 0 avg-ms)))))
           (json-response 201 (apply #'jobj result))))))))

;;; --- static files ------------------------------------------------------

(defparameter *content-types*
  '(("html"  . "text/html; charset=utf-8")
    ("htm"   . "text/html; charset=utf-8")
    ("js"    . "application/javascript; charset=utf-8")
    ("mjs"   . "application/javascript; charset=utf-8")
    ("css"   . "text/css; charset=utf-8")
    ("json"  . "application/json; charset=utf-8")
    ("edn"   . "application/edn; charset=utf-8")
    ("map"   . "application/json; charset=utf-8")
    ("svg"   . "image/svg+xml")
    ("png"   . "image/png")
    ("jpg"   . "image/jpeg")
    ("jpeg"  . "image/jpeg")
    ("gif"   . "image/gif")
    ("ico"   . "image/x-icon")
    ("txt"   . "text/plain; charset=utf-8")
    ("woff2" . "font/woff2")))

(defun content-type (path)
  (or (cdr (assoc (or (pathname-type path) "") *content-types* :test #'string=))
      "application/octet-stream"))

(defun bytes-response (bytes rel)
  (list 200
        (list :content-type (content-type rel)
              :content-length (length bytes)
              :cache-control "no-cache")
        (list bytes)))

(defun forbidden ()
  (json-error 403 "forbidden"))

(defun handle-static (path-info)
  "Serve the game shell: public/ on disk when it exists (dev rebuilds),
  otherwise the copy baked into the binary image."
  (let* ((public (uiop:ensure-directory-pathname (public-dir)))
         (rel (if (string= path-info "/")
                  "index.html"
                  (string-left-trim "/" path-info))))
    (cond
      ((or (zerop (length rel))
           (search ".." rel)
           (char= (char rel 0) #\/)
           (find #\Null rel))
       (forbidden))
      (t
       (let* ((target (merge-pathnames rel public))
              (disk (when (uiop:subpathp target public)
                      (uiop:file-exists-p target)))
              (bytes (or (when disk (read-file-bytes disk))
                         (static-asset-bytes rel))))
         (cond
           (bytes (bytes-response bytes rel))
           ((and (uiop:subpathp target public) (null (pathname-type target)))
            ;; unknown extensionless path -> single page app shell
            (let ((idx (or (let ((p (merge-pathnames "index.html" public)))
                             (when (uiop:file-exists-p p) (read-file-bytes p)))
                           (static-asset-bytes "index.html"))))
              (if idx
                  (bytes-response idx "index.html")
                  (json-error 404 "not found"))))
           ((not (uiop:subpathp target public)) (forbidden))
           (t (json-error 404 "not found"))))))))

;;; --- app ---------------------------------------------------------------

(defun app (env)
  (let ((path   (getf env :path-info))
        (method (getf env :request-method)))
    (handler-case
        (cond
          ((string= method "OPTIONS")
           (list 204 (append '(:content-type "text/plain") (cors-headers)) '("")))
          ((string= path "/api/health")
           (json-response 200 (jobj :ok t :service "math-training-machine")))
          ((string= path "/api/leaderboard")
           (if (string= method "POST")
               (handle-leaderboard-post env)
               (handle-leaderboard-get env)))
          ((and (> (length path) 4) (string= (subseq path 0 4) "/api/"))
           (json-error 404 "unknown api endpoint"))
          ((string= method "GET") (handle-static path))
          (t (json-error 405 "method not allowed")))
      (error (e)
        (format *error-output* "~&[mtm] error handling ~a: ~a~%" path e)
        (json-error 500 (princ-to-string e))))))
