;;;; macros.lisp

(in-package #:orions-furnace)

(standard-optimisation-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL2 WITH- helpers

(defmacro with-image-lib ((&key (formats '(:png))) &body body)
  "Temporarily loads the SDL2_Image library."
  `(unwind-protect
     (progn
       (sdl2-image:init ',formats)
       ,@body)
     (sdl2-image:quit)))

(defmacro with-ttf-lib (() &body body)
  "Temporarily loads the SDL2_TTF library."
  `(unwind-protect
     (progn
       (sdl2-ttf:init)
       ,@body)
     (sdl2-ttf:quit)))

(defmacro with-render-target ((renderer texture) &body body)
  "Uses a texture as a render target for the duration of the block."
  (with-gensyms (g-renderer g-texture)
    `(let* ((,g-renderer ,renderer)
            (,g-texture ,texture))
       (unwind-protect
         (let* ((*current-render-target* ,g-texture))
           (sdl2:set-render-target ,g-renderer ,g-texture)
           ,@body)
         (sdl2:set-render-target ,g-renderer *current-render-target*)))))

(defmacro with-texture ((texture renderer pixel-format access width height) &body body)
  "Create a texture, and free it after we leave this block."
  `(let* ((,texture (sdl2:create-texture ,renderer ,pixel-format ,access ,width ,height)))
     (unwind-protect
       (progn ,@body)
       (progn (sdl2:destroy-texture ,texture)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL2 KEYSYM-CASE helper

(defmacro keysym-case (keysym &body body)
  "Case statement for SDL2 keysyms. Use the :SCANCODE-* symbols without the SCANCODE- bit."
  (with-gensyms (g-scancode)
    `(let* ((,g-scancode (sdl2:scancode-value ,keysym)))
       (cond
         ,@(mapcar (lambda (clause)
                     (%keysym-case-clause g-scancode clause))
                   body)
         ))))

(defun %keysym-case-clause (g-scancode clause)
  (let* ((key (intern (format nil "SCANCODE-~a" (symbol-name (car clause)))
                      :keyword))
         (body (cdr clause)))
    ;;(format t "clause ~s / ~s~%" key body)
    `((sdl2:scancode= ,g-scancode ,key)
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL2 pool-based replacement for WITH-RECTS
(defparameter *rects-pool* (make-array 10 :fill-pointer 0 :initial-element nil))

(defmacro with-overall-rects-pool (() &body body)
  `(unwind-protect
     (progn ,@body)
     (progn
       (do () ((= (fill-pointer *rects-pool*) 0))
         (sdl2:free-rect (vector-pop *rects-pool*))))))

(defmacro with-pooled-rects ((&rest rects) &body body)
  `(let (,@(mapcar #'%parse-rect-def rects))
     (unwind-protect
       (progn ,@body)
       (progn
         ,@(mapcar #'%cleanup-rect-def rects)))))
(defun %parse-rect-def (rect-def)
  (etypecase rect-def
    (symbol (%parse-rect-def `(,rect-def 0 0 0 0)))
    (cons
      (destructuring-bind (name x y w h) rect-def
        `(,name (%pop-from-rects-pool ,x ,y ,w ,h))))))
(defun %cleanup-rect-def (rect-def)
  (etypecase rect-def
    (symbol `(%push-into-rects-pool ,rect-def))
    (cons
      (destructuring-bind (name x y w h) rect-def
        (declare (ignore x y w h))
        (%cleanup-rect-def name)))))

(defun %push-into-rects-pool (rect)
  (vector-push-extend rect *rects-pool*))
(defun %pop-from-rects-pool (x y w h)
  (if (>= (fill-pointer *rects-pool*) 1)
    (let* ((rect (vector-pop *rects-pool*)))
      (setf (sdl2:rect-x rect) x)
      (setf (sdl2:rect-y rect) y)
      (setf (sdl2:rect-width rect) w)
      (setf (sdl2:rect-height rect) h)
      rect)
    (sdl2:make-rect x y w h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
