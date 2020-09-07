;;;; orions-furnace.lisp

(in-package #:orions-furnace)

(defparameter *base-screen-width* 640)
(defparameter *base-screen-height* 360)
(defparameter *screen-scale* 2)
(defparameter *cell-w* 24)
(defparameter *cell-h* 24)

(defparameter *cam-w-cells* 15)
(defparameter *cam-h-cells* 15)
(defparameter *cam-x* 0)
(defparameter *cam-y* 0)
(define-symbol-macro *cam-w* (* *cell-w* *cam-w-cells*))
(define-symbol-macro *cam-h* (* *cell-h* *cam-h-cells*))

(define-symbol-macro *sidebar-x* *cam-w*)
(define-symbol-macro *sidebar-y* 0)
(define-symbol-macro *sidebar-w* (- *base-screen-width* *sidebar-x*))
(define-symbol-macro *sidebar-h* *base-screen-height*)

(defparameter *window* nil)
(defparameter *renderer* nil)
(defparameter *backbuf* nil)
(defparameter *current-render-target* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

