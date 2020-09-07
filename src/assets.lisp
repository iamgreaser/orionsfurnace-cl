;;;; assets.lisp

(in-package #:orions-furnace)

(defparameter *img-floor* ())

(defparameter *asset-pairs*
  '((*gfx-tiles-floor001* #p"dat/gfx/tiles/floor001.png")
    (*gfx-tiles-wall001* #p"dat/gfx/tiles/wall001.png")
    (*gfx-player-base* #p"dat/gfx/player/base.png")
    ))

(defmacro with-game-assets (() &body body)
  `(unwind-protect
     (progn
       ,@(mapcar #'%with-game-assets-pair-init *asset-pairs*)
       ,@body)
     (progn
       ,@(mapcar #'%with-game-assets-pair-deinit *asset-pairs*))))

(defun %with-game-assets-pair-init (pair)
  (destructuring-bind (key fname) pair
    (with-gensyms (g-surface g-texture)
      `(let* ((,g-surface (sdl2-image:load-image ,fname))
              (,g-texture (sdl2:create-texture-from-surface *renderer* ,g-surface)))
         (sdl2:free-surface ,g-surface)
         (setf ,key ,g-texture)))))

(defun %with-game-assets-pair-deinit (pair)
  (destructuring-bind (key fname) pair
    (declare (ignore fname))
    ;; TODO: Catch any conditions which may fire --GM
    `(when ,key
       (sdl2:destroy-texture ,key)
       (setf ,key nil)
       )))
