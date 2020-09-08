;;;; draw.lisp

(in-package #:orions-furnace)

(defun draw-frame ()
  (with-render-target (*renderer* *backbuf*)
    ;; Clear screen
    (sdl2:set-render-draw-color *renderer* 85 85 255 0)
    (sdl2:render-clear *renderer*)

    ;; Draw things
    (draw-playfield)
    (draw-sidebar)
    (draw-gui)
    )

  ;; Blit and present
  (sdl2:render-copy *renderer* *backbuf*)
  (sdl2:render-present *renderer*)
  )

(defun draw-playfield ()
  (sdl2:with-rects ((clip *cam-x* *cam-y* *cam-w* *cam-h*))
    (sdl2:set-render-draw-color *renderer* 40 0 20 0)
    (sdl2:render-fill-rect *renderer* clip)
    (sdl2:set-render-draw-color *renderer* 255 255 255 255)

    (let* ((cam-pixel-x (+ (entity-vis-px *player-entity*) (floor *cell-w* 2)))
           (cam-pixel-y (+ (entity-vis-py *player-entity*) (floor *cell-h* 2)))
           (*cam-offs-x* (- cam-pixel-x (floor *cam-w* 2)))
           (*cam-offs-y* (- cam-pixel-y (floor *cam-h* 2)))
           )

      ;; Draw tiles with entities
      (do ((cy 0 (1+ cy)))
          ((>= (- (* *cell-h* cy) *cam-offs-y*) *cam-h*))
        (do ((cx 0 (1+ cx)))
            ((>= (- (* *cell-w* cx) *cam-offs-x*) *cam-w*))
          (draw-tile-lower cx cy)))

      (do ((cy 0 (1+ cy)))
          ((>= (- (* *cell-h* cy) *cam-offs-y*) *cam-h*))
        (do ((cx 0 (1+ cx)))
            ((>= (- (* *cell-w* cx) *cam-offs-x*) *cam-w*))
          (draw-tile-upper cx cy)))
      )))

(defun draw-tile-lower (cx cy)
  (let* ((px (- (* *cell-w* cx) *cam-offs-x*))
         (py (- (* *cell-h* cy) *cam-offs-y*)))
    (draw-tile-at-lower *board* cx cy px py)))

(defun draw-tile-upper (cx cy)
  (let* ((px (- (* *cell-w* cx) *cam-offs-x*))
         (py (- (* *cell-h* cy) *cam-offs-y*)))
    (draw-tile-at-upper *board* cx cy px py)))

(defun draw-sidebar ()
  (sdl2:with-rects ((clip *sidebar-x* *sidebar-y* *sidebar-w* *sidebar-h*))
    (sdl2:set-render-draw-color *renderer* 0 0 0 0)
    (sdl2:render-fill-rect *renderer* clip)
    (draw-text (+ *sidebar-x* 10)
               (+ *sidebar-y* 10)
               (format nil "Pos (~d, ~d)"
                       (entity-cx *player-entity*)
                       (entity-cy *player-entity*))
               170 170 255)
    ))

(defun draw-gui ()
  )

;; I added a font cache as part of trying to fix a memory leak,
;; but it turns out the memory leak was in sdl2:with-rects.
;; If it's crap, feel free to get rid of it.
;; But if it helps, feel free to keep it. --GM
(defvar *font-texture-cache* nil)
(defparameter *font-texture-cache-max* 50)
(defparameter *font-texture-cache-reduce-to* 10)
(defun draw-text (px py text &optional (r 255) (g 255) (b 255) (a 255))
  (let* ((key (list text r g b a))
         (assoc-result (assoc key *font-texture-cache* :test #'equal))
         (texture (cdr assoc-result)))
    (unless texture
      ;;(format t "Cache len ~d~%" (length *font-texture-cache*))
      (setf texture (make-new-text-texture text r g b a))
      (push (cons key texture) *font-texture-cache*)
      (when (>= (length *font-texture-cache*) *font-texture-cache-max*)
        ;;(format t "Reducing~%")
        (setf *font-texture-cache*
              (subseq *font-texture-cache*
                      0 *font-texture-cache-reduce-to*))))
    (sdl2:with-rects ((d-rect px py
                              (sdl2:texture-width texture)
                              (sdl2:texture-height texture)))
      (sdl2:render-copy *renderer* texture :dest-rect d-rect))))

(defun make-new-text-texture (text r g b a)
  (let* ((surface (sdl2-ttf:render-utf8-blended
                    *font-base*
                    text
                    r g b a)))
    (let* ((texture (sdl2:create-texture-from-surface *renderer* surface)))
      ;; cl-sdl2-ttf autocollects its surfaces,
      ;; and I'm considering patching it to not do that --GM
      ;;(sdl2:free-surface surface)
      texture)))
