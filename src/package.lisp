;;;; package.lisp

(defpackage #:orions-furnace
  (:use #:cl #:alexandria)
  (:export #:run))

(in-package #:orions-furnace)

(defmacro standard-optimisation-settings ()
  (progn))

;; Feel free to play around with these if it starts sucking in one direction --GM
(defmacro standard-optimisation-settings ()
  `(declaim (optimize (speed 0) (compilation-speed 0) (space 3) (debug 2) (safety 3))))
