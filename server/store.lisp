;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

;;; Leaderboard storage on top of the conskivi key/value database.
;;;
;;; Layout:
;;;   :lb-sum / :lb-diff / :lb-mixed   sorted set  id -> score
;;;   :rec-<id>                         scalar      record plist

(defparameter *max-entries* 100)

(defparameter *modes* '("sum" "diff" "mixed"))

(defvar *db* nil)
(defvar *db-lock* (bt:make-lock "conskivi-leaderboard"))

(defun mode-p (mode)
  (member mode *modes* :test #'string=))

(defun lb-key (mode)
  (intern (format nil "LB-~a" (string-upcase mode)) :keyword))

(defun rec-key (id)
  (intern (format nil "REC-~a" id) :keyword))

(defun make-id ()
  (format nil "~(~36r~)~(~36r~)"
          (mod (get-universal-time) 89161004)
          (random 46656)))

(defun open-store ()
  (let ((path (data-dir)))
    (ensure-directories-exist path)
    (setf *db*
          (make-instance 'conskivi-fileonly:conskivi-fileonly-database
                         :name "mtm-leaderboard"
                         :db-path path))
    (conskivi-fileonly:conskivi-start *db*)
    (format t "~&[mtm] conskivi store open at ~a~%" path)
    *db*))

(defun close-store ()
  (when *db*
    (bt:with-lock-held (*db-lock*)
      (ignore-errors (conskivi-fileonly:conskivi-save *db*))
      (ignore-errors (conskivi-fileonly:conskivi-stop *db*)))
    (setf *db* nil)))

(defun clean-name (raw)
  "Trim, drop control characters, cap at 16 characters."
  (let* ((chars (remove-if (lambda (c)
                             (or (< (char-code c) 32)
                                 (= (char-code c) 127)))
                           (or raw "")))
         (trimmed (string-trim '(#\Space #\Tab #\Newline) chars)))
    (subseq trimmed 0 (min 16 (length trimmed)))))

(defun record-fields (record)
  (list :id          (getf record :id)
        :name        (getf record :name)
        :mode        (getf record :mode)
        :score       (getf record :score)
        :correct     (getf record :correct)
        :total       (getf record :total)
        :best-streak (getf record :best-streak)
        :avg-ms      (getf record :avg-ms)
        :accuracy    (if (plusp (getf record :total))
                         (round (* 100 (/ (getf record :correct)
                                          (getf record :total))))
                         0)
        :ts          (getf record :ts)))

(defun rank-of* (id mode)
  "1-based position of ID inside the mode's sorted set, or NIL.
Caller must hold *DB-LOCK*."
  (let* ((key (lb-key mode))
         (n   (conskivi-core:conskivi-zcard *db* key)))
    (when (plusp n)
      (let ((members (conskivi-core:conskivi-zrevrange *db* key 0 (1- n))))
        (1+ (or (position id members :test #'string=) 0))))))

(defun rank-of (id mode)
  (bt:with-lock-held (*db-lock*)
    (rank-of* id mode)))

(defun top (mode limit)
  "Highest scoring records of MODE as a list of plists with :rank."
  (bt:with-lock-held (*db-lock*)
    (let* ((key (lb-key mode))
           (n   (conskivi-core:conskivi-zcard *db* key)))
      (when (plusp n)
        (let* ((stop (1- (min n limit)))
               (flat (conskivi-core:conskivi-zrevrange *db* key 0 stop t)))
          (loop for (member score) on flat by #'cddr
                for rank from 1
                for rec = (conskivi-core:conskivi-get *db* (rec-key member))
                when rec
                  collect (append (list :rank rank
                                        :score (round score))
                                  (record-fields rec))))))))

(defun trim! (mode)
  "Keep only the best *MAX-ENTRIES* scores of MODE."
  (let* ((key (lb-key mode))
         (n   (conskivi-core:conskivi-zcard *db* key)))
    (when (> n *max-entries*)
      (dolist (id (conskivi-core:conskivi-zrange *db* key 0 (1- (- n *max-entries*))))
        (conskivi-core:conskivi-zrem *db* key id)
        (conskivi-core:conskivi-del *db* (rec-key id))))))

(defun add-score (record)
  "Persist a finished round. RECORD is a plist, returns plist with :rank."
  (bt:with-lock-held (*db-lock*)
    (let* ((id   (make-id))
           (mode (getf record :mode))
           (key  (lb-key mode))
           (full (list :id          id
                       :name        (getf record :name)
                       :mode        mode
                       :score       (getf record :score)
                       :correct     (getf record :correct)
                       :total       (getf record :total)
                       :best-streak (or (getf record :best-streak) 0)
                       :avg-ms      (or (getf record :avg-ms) 0)
                       :ts          (get-universal-time))))
      (conskivi-core:conskivi-put *db* (rec-key id) full)
      (conskivi-core:conskivi-zadd *db* key (float (getf full :score)) id)
      (trim! mode)
      (conskivi-fileonly:conskivi-save *db*)
      (append (list :id id)
              (record-fields full)
              (list :rank (rank-of* id mode))))))
