;;;; assets.lisp

(in-package #:orions-furnace)

(standard-optimisation-settings)

(defparameter *img-floor* ())

(defparameter *asset-image-tuples*
  '((*gfx-unknown* #p"dat/gfx/unknown.png")
    (*gfx-tiles-floor001* #p"dat/gfx/tiles/floor001.png")
    (*gfx-tiles-floortile001* #p"dat/gfx/tiles/floortile001.png")
    (*gfx-tiles-wall001* #p"dat/gfx/tiles/wall001.png")
    (*gfx-player-base* #p"dat/gfx/player/base.png")
    ))

(defparameter *asset-font-tuples*
  '((*font-base* #p"dat/fonts/DejaVuSans.ttf" 11)
    ))

(defmacro with-game-assets (() &body body)
  "Load all game assets, and free them when we leave this block."
  `(unwind-protect
     (progn
       ,@(mapcar #'%with-game-assets-font-tuple-init *asset-font-tuples*)
       ,@(mapcar #'%with-game-assets-image-tuple-init *asset-image-tuples*)
       ,@body)
     (progn
       ,@(mapcar #'%with-game-assets-image-tuple-deinit *asset-image-tuples*)
       ,@(mapcar #'%with-game-assets-font-tuple-deinit *asset-font-tuples*)
       )))

(defun %with-game-assets-image-tuple-init (tuple)
  (destructuring-bind (key fname) tuple
    (with-gensyms (g-surface g-texture)
      `(let* ((,g-surface (sdl2-image:load-image ,fname))
              (,g-texture (sdl2:create-texture-from-surface *renderer* ,g-surface)))
         (sdl2:free-surface ,g-surface)
         (setf ,key ,g-texture)))))

(defun %with-game-assets-image-tuple-deinit (tuple)
  (destructuring-bind (key fname) tuple
    (declare (ignore fname))
    ;; TODO: Catch any conditions which may fire --GM
    `(when ,key
       (sdl2:destroy-texture ,key)
       (setf ,key nil)
       )))

(defun %with-game-assets-font-tuple-init (tuple)
  (destructuring-bind (key fname size) tuple
    (with-gensyms (g-font)
      `(let* ((,g-font (sdl2-ttf:open-font ,fname ,size)))
         (setf ,key ,g-font)))))

(defun %with-game-assets-font-tuple-deinit (tuple)
  (destructuring-bind (key fname size) tuple
    (declare (ignore fname size))
    ;; TODO: Catch any conditions which may fire --GM
    `(when ,key
       (sdl2-ttf:close-font ,key)
       (setf ,key nil)
       )))
