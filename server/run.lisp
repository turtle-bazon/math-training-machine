;;;; -*- mode: lisp -*-
;;;;
;;;; Bootstrap for running the server from source. Run it from the project
;;;; root:
;;;;
;;;;   sbcl --non-interactive --load server/run.lisp
;;;;
;;;; For a self-contained binary use `make build` instead.
;;;;
;;;; Environment: MTM_PORT (8123), MTM_ADDRESS (127.0.0.1),
;;;;              MTM_DATA (./data/), MTM_PUBLIC (./public/)

(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(defparameter *root* (uiop:getcwd))

(unless (probe-file (merge-pathnames "mtm-server.asd" *root*))
  (format *error-output*
          "~&[mtm] run this from the project root (mtm-server.asd not found in ~a)~%"
          *root*)
  (uiop:quit 1))

;; The embedded asset table is a build artifact (`make generate-static`);
;; write an empty one so source runs work straight after a fresh checkout.
(let ((assets (merge-pathnames "server/static-assets.lisp" *root*)))
  (unless (probe-file assets)
    (with-open-file (s assets :direction :output :if-exists nil
                              :if-does-not-exist :create)
      (write-string
       ";;; -*- mode: lisp -*-
;;; Stub written by server/run.lisp - run `make generate-static` to embed
;;; the real frontend assets into the binary.
(in-package #:mtm-server)
(defparameter *static-assets* '())
" s))))

(asdf:load-asd (merge-pathnames "mtm-server.asd" *root*))

(ql:quickload '("clack" "clack-handler-wookie" "yason" "conskivi-fileonly"
                "bordeaux-threads" "quri" "babel" "cl-base64"))

(asdf:load-system "mtm-server")

(mtm-server:main)
