;;;; -*- mode: lisp -*-

(defsystem "mtm-server"
  :description "Math Training Machine - leaderboard server (Clack/Wookie + conskivi)"
  :author "turtle"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on ("clack"
               "clack-handler-wookie"
               "yason"
               "conskivi-fileonly"
               "bordeaux-threads"
               "quri"
               "babel"
               "cl-base64"
               "uiop")
  :serial t
  :components ((:module "server"
                :components ((:file "package")
                             (:file "config")
                             (:file "static")
                             (:file "store")
                             (:file "app")
                             (:file "main")
                             ;; generated last by tools/build-static.lisp
                             (:file "static-assets"))))
  :build-operation "program-op"
  :build-pathname "build/math-training-machine"
  :entry-point "mtm-server:main")
