;;;; macros.lisp

(in-package #:orions-furnace)

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
