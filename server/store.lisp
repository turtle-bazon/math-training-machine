;;;; -*- mode: lisp -*-

(in-package #:mtm-server)

;;; Leaderboard storage on top of the conskivi key/value database.
;;;
;;; Layout:
;;;   :lb-sum / :lb-diff / :lb-mixed   sorted set  id -> score
;;;   :rec-<id>                         scalar      record plist

(defparameter *max-entries* 100)

(defparameter *operation-tokens*
  '("sum" "add" "plus"
    "diff" "difference" "subtract" "minus" "sub"
    "mult" "multiply" "product" "prod" "times"
    "mixed" "all"))

(defparameter *class-tokens*
  '("pos" "positive" "signed" "any"
    "frac" "fraction" "fractions" "rational" "rationals"))

(defvar *db* nil)
(defvar *db-lock* (bt:make-lock "conskivi-leaderboard"))

(defun split-mode-tokens (mode)
  (let ((out '())
        (start 0)
        (len (length mode)))
    (labels ((sep-p (c) (or (char= c #\-) (char= c #\_) (char= c #\Space))))
      (loop for i from 0 below len
            when (sep-p (char mode i))
              do (when (> i start)
                   (push (string-downcase (subseq mode start i)) out))
                 (setf start (1+ i)))
      (when (> len start)
        (push (string-downcase (subseq mode start len)) out))
      out)))

(defun operation-token-p (tok)
  (member tok *operation-tokens* :test #'string=))

(defun class-token-p (tok)
  (member tok *class-tokens* :test #'string=))

(defun mode-p (mode)
  "Accept legacy operation modes, the two number classes, and any
  combination of an operation token with a class token (in either order)."
  (and (stringp mode) (plusp (length mode))
       (let ((toks (split-mode-tokens mode)))
         (cond
           ((= (length toks) 1)
            (or (operation-token-p (first toks))
                (class-token-p (first toks))))
           ((= (length toks) 2)
            (and (or (operation-token-p (first toks)) (class-token-p (first toks)))
                 (or (operation-token-p (second toks)) (class-token-p (second toks)))))
           ((= (length toks) 3)
            (and (operation-token-p (first toks))
                 (class-token-p (second toks))
                 (class-token-p (third toks))))
           (t nil)))))

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
    (subseq trimmed 0 (min 128 (length trimmed)))))

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


(defun same-name-ids (mode name)
  "IDs of records in MODE whose :name equals NAME."
  (let* ((key (lb-key mode))
         (n   (conskivi-core:conskivi-zcard *db* key))
         (out '()))
    (when (plusp n)
      (let ((flat (conskivi-core:conskivi-zrevrange *db* key 0 (1- n) t)))
        (loop for (member score) on flat by #'cddr
              for rec = (conskivi-core:conskivi-get *db* (rec-key member))
              when (and rec (string= (getf rec :name) name))
                do (push member out))))
    out))

(defun best-same-name (mode name)
  "The (id . record) of the highest-scoring record in MODE for NAME, or NIL."
  (let ((ids (same-name-ids mode name)))
    (when ids
      (let ((best-id (first ids))
            (best-score -1)
            (best-rec nil))
        (dolist (id ids)
          (let ((rec (conskivi-core:conskivi-get *db* (rec-key id))))
            (when (and rec (> (getf rec :score) best-score))
              (setf best-score (getf rec :score)
                    best-id id
                    best-rec rec))))
        (cons best-id best-rec)))))


(defun inc-stat (key)
  (bt:with-lock-held (*db-lock*)
    (let ((cur (or (conskivi-core:conskivi-get *db* key) 0)))
      (conskivi-core:conskivi-put *db* key (1+ cur))
      (conskivi-fileonly:conskivi-save *db*)
      (1+ cur))))

(defun today-string ()
  (multiple-value-bind (sec min hour date month year) (get-decoded-time)
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun daily-key (prefix date)
  (intern (format nil "~a-~a" prefix (substitute #\- #\- date)) :keyword))

(defun inc-visits (device-id source browser referrer)
  (bt:with-lock-held (*db-lock*)
    (let ((cur (or (conskivi-core:conskivi-get *db* :stat-visits) 0))
          (day (today-string)))
      (conskivi-core:conskivi-put *db* :stat-visits (1+ cur))
      (when (and (stringp device-id) (plusp (length device-id)))
        (conskivi-core:conskivi-zadd *db* :visitors 1.0 device-id)
        (conskivi-core:conskivi-zadd *db* (daily-key "DAILY-VISITORS" day) 1.0 device-id))
      (conskivi-core:conskivi-put *db* (daily-key "DAILY-VISITS" day)
        (1+ (or (conskivi-core:conskivi-get *db* (daily-key "DAILY-VISITS" day)) 0)))
      (when (and (stringp source) (plusp (length source)))
        (let ((k (intern (format nil "SOURCE-~a" (string-upcase source)) :keyword)))
          (conskivi-core:conskivi-put *db* k (1+ (or (conskivi-core:conskivi-get *db* k) 0)))))
      (when (and (stringp browser) (plusp (length browser)))
        (let ((k (intern (format nil "BROWSER-~a" (string-upcase (subseq browser 0 (min 40 (length browser))))) :keyword)))
          (conskivi-core:conskivi-put *db* k (1+ (or (conskivi-core:conskivi-get *db* k) 0)))))
      (when (and (stringp referrer) (plusp (length referrer)))
        (let ((k (intern (format nil "REFERRER-~a" (string-upcase (subseq referrer 0 (min 60 (length referrer))))) :keyword)))
          (conskivi-core:conskivi-put *db* k (1+ (or (conskivi-core:conskivi-get *db* k) 0)))))
      (conskivi-fileonly:conskivi-save *db*)
      (1+ cur))))

(defun game-mode-prefix (mode)
  (format nil "DAILY-GAME-~a" mode))

(defun inc-game (mode)
  (bt:with-lock-held (*db-lock*)
    (let ((day (today-string)))
      ;; total daily games
      (conskivi-core:conskivi-put *db* (daily-key "DAILY-GAMES" day)
        (1+ (or (conskivi-core:conskivi-get *db* (daily-key "DAILY-GAMES" day)) 0)))
      ;; per-mode daily games
      (when (and (stringp mode) (plusp (length mode)))
        (let ((k (daily-key (game-mode-prefix mode) day)))
          (conskivi-core:conskivi-put *db* k
            (1+ (or (conskivi-core:conskivi-get *db* k) 0)))))
      ;; total games
      (conskivi-core:conskivi-put *db* :stat-games
        (1+ (or (conskivi-core:conskivi-get *db* :stat-games) 0)))
      ;; per-mode totals
      (when (and (stringp mode) (plusp (length mode)))
        (let* ((key :game-types)
               (cur (conskivi-core:conskivi-zscore *db* key mode)))
          (conskivi-core:conskivi-zadd *db* key (1+ (or cur 0)) mode)))
      (conskivi-fileonly:conskivi-save *db*))))

(defun collect-daily (prefix days)
  (let ((out '()))
    (dotimes (i days)
      (let* ((universal (get-universal-time))
             (target (- universal (* i 86400)))
             (decoded (multiple-value-list (decode-universal-time target)))
             (date (format nil "~4,'0d-~2,'0d-~2,'0d"
                           (sixth decoded) (fifth decoded) (fourth decoded)))
             (key (daily-key prefix date))
             (val (conskivi-core:conskivi-get *db* key)))
        (push (list date (or val 0)) out)))
    out))

(defun collect-prefixed (prefix)
  (let ((out '()))
    (dolist (k (conskivi-core:conskivi-keys *db* (concatenate 'string "^" prefix)))
      (let ((v (conskivi-core:conskivi-get *db* k)))
        (when (numberp v)
          (push (list (subseq (symbol-name k) (length prefix)) v) out))))
    out))


(defun collect-daily-unique (days)
  (let ((out '()))
    (dotimes (i days)
      (let* ((universal (get-universal-time))
             (target (- universal (* i 86400)))
             (decoded (multiple-value-list (decode-universal-time target)))
             (date (format nil "~4,'0d-~2,'0d-~2,'0d"
                           (sixth decoded) (fifth decoded) (fourth decoded)))
             (key (daily-key "DAILY-VISITORS" date))
             (val (conskivi-core:conskivi-zcard *db* key)))
        (push (list date (or val 0)) out)))
    out))



(defparameter *all-game-types*
  (let ((ops '("sum" "diff" "mult" "mixed"))
        (cls '("signed" "positive" "frac-signed" "frac")))
    (loop for op in ops
          append (loop for cl in cls
                       collect (format nil "~a-~a" op cl)))))

(defun collect-game-types ()
  (let ((out '()))
    (dolist (m *all-game-types*)
      (let ((score (conskivi-core:conskivi-zscore *db* :game-types m)))
        (push (list m (if score (round score) 0)) out)))
    (nreverse out)))


(defun collect-game-type-daily (mode days)
  (let ((out '())
        (prefix (game-mode-prefix mode)))
    (dotimes (i days)
      (let* ((universal (get-universal-time))
             (target (- universal (* i 86400)))
             (decoded (multiple-value-list (decode-universal-time target)))
             (date (format nil "~4,'0d-~2,'0d-~2,'0d"
                           (sixth decoded) (fifth decoded) (fourth decoded)))
             (key (daily-key prefix date))
             (val (conskivi-core:conskivi-get *db* key)))
        (push (list date (or val 0)) out)))
    out))

(defun collect-game-type-series ()
  (let ((out '()))
    (dolist (m *all-game-types*)
      (push (jobj :mode m
                  :daily (collect-game-type-daily m 30)) out))
    (nreverse out)))

(defun get-stats ()
  (bt:with-lock-held (*db-lock*)
    (list :visits (or (conskivi-core:conskivi-get *db* :stat-visits) 0)
          :unique (conskivi-core:conskivi-zcard *db* :visitors)
          :games (or (conskivi-core:conskivi-get *db* :stat-games) 0)
          :scores (or (conskivi-core:conskivi-get *db* :stat-scores) 0)
          :game-types (collect-game-types)
          :game-type-daily (collect-game-type-series)
          :daily-visits (collect-daily "DAILY-VISITS" 30)
          :daily-games (collect-daily "DAILY-GAMES" 30)
          :daily-unique (collect-daily-unique 30)
          :sources (collect-prefixed "SOURCE-")
          :browsers (collect-prefixed "BROWSER-")
          :referrers (collect-prefixed "REFERRER-"))))


(defun add-score (record)
  "Persist a finished round. RECORD is a plist, returns plist with :rank.
  If an entry with the same name already exists in the mode with a score
  greater than or equal to the new one, the new (lower) score is ignored
  and the existing entry's rank is returned. Otherwise any older entries
  for that name are replaced by the new (higher) score."
  (bt:with-lock-held (*db-lock*)
    (let* ((name (getf record :name))
           (mode (getf record :mode))
           (score (getf record :score))
           (existing (best-same-name mode name))
           (existing-id (car existing))
           (existing-rec (cdr existing)))
      (if (and existing-id existing-rec (>= (getf existing-rec :score) score))
          ;; ignore the lower (or equal) score
          (append (list :id existing-id)
                  (record-fields existing-rec)
                  (list :rank (rank-of* existing-id mode)
                        :ignored t))
          (let* ((id   (make-id))
                 (key  (lb-key mode))
                 (full (list :id          id
                             :name        name
                             :mode        mode
                             :score       score
                             :correct     (getf record :correct)
                             :total       (getf record :total)
                             :best-streak (or (getf record :best-streak) 0)
                             :avg-ms      (or (getf record :avg-ms) 0)
                             :ts          (get-universal-time))))
            ;; drop any older, lower entries for the same name
            (dolist (old-id (same-name-ids mode name))
              (conskivi-core:conskivi-zrem *db* key old-id)
              (conskivi-core:conskivi-del *db* (rec-key old-id)))
            (conskivi-core:conskivi-put *db* (rec-key id) full)
            (conskivi-core:conskivi-zadd *db* key (float (getf full :score)) id)
            (trim! mode)
            (conskivi-core:conskivi-put *db* :stat-scores
              (1+ (or (conskivi-core:conskivi-get *db* :stat-scores) 0)))
            (conskivi-fileonly:conskivi-save *db*)
            (append (list :id id)
                    (record-fields full)
                    (list :rank (rank-of* id mode))))))))
