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

    (let* ((cam-cell-x 2)
           (cam-cell-y 1)
           (cam-pixel-x (+ (* cam-cell-x *cell-w*) (floor *cell-w* 2)))
           (cam-pixel-y (+ (* cam-cell-y *cell-h*) (floor *cell-h* 2)))
           (cam-offs-x (- cam-pixel-x (floor *cam-w* 2)))
           (cam-offs-y (- cam-pixel-y (floor *cam-h* 2)))
           )

      ;; Draw tiles
      (do ((cy 0 (1+ cy)))
          ((>= (- (* *cell-h* cy) cam-offs-y) *cam-h*))
        (do ((cx 0 (1+ cx)))
            ((>= (- (* *cell-w* cx) cam-offs-x) *cam-w*))
          (let* ((px (- (* *cell-w* cx) cam-offs-x))
                 (py (- (* *cell-h* cy) cam-offs-y)))
            (draw-tile px py cx cy))))

      ;; Draw entities
      (let* ((cx 2)
             (cy 1)
             (px (- (* *cell-w* cx) cam-offs-x))
             (py (- (* *cell-h* cy) cam-offs-y)))
        (draw-entity px py cx cy))
      )))

(defun draw-entity (px py cx cy)
  ;; TODO: Accept a tile as an argument --GM
  (let* ((texture *gfx-player-base*))
    (sdl2:with-rects ((d-rect px py *cell-w* *cell-h*)
                      (s-rect (* *cell-w* 1) 0 *cell-w* *cell-h*))
      (sdl2:render-copy *renderer* texture
                        :source-rect s-rect
                        :dest-rect d-rect))))

(defun draw-tile (px py cx cy)
  ;; TODO: Accept a tile as an argument --GM
  ;; TODO: Not hardcode the board dimensions --GM
  (when (and (<= 0 cx (1- 64))
             (<= 0 cy (1- 64)))
    (let* ((texture *gfx-tiles-floor001*))
      (sdl2:with-rects ((d-rect px py *cell-w* *cell-h*)
                        (s-rect 0 0 *cell-w* *cell-h*))
        (sdl2:render-copy *renderer* texture
                          :source-rect s-rect
                          :dest-rect d-rect)))))

(defun draw-sidebar ()
  (sdl2:with-rects ((clip *sidebar-x* *sidebar-y* *sidebar-w* *sidebar-h*))
    (sdl2:set-render-draw-color *renderer* 0 0 0 0)
    (sdl2:render-fill-rect *renderer* clip)
    ))

(defun draw-gui ()
  )
