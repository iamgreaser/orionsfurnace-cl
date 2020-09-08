(in-package #:orions-furnace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entity generics

(defgeneric entity-vis-px (entity)
  (:documentation "Visual pixel X coordinate."))
(defgeneric entity-vis-py (entity)
  (:documentation "Visual pixel Y coordinate."))
(defgeneric entity-graphic (entity)
  (:documentation "The graphic of this entity, in the form (texture subimg-x subimg-y). Used for normal one-tile-sized graphics."))
(defgeneric draw-entity-at (entity px py)
  (:documentation "Draw the given entity at the given position."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entity default methods

(defmethod entity-vis-px ((entity entity))
  (+ (* *cell-w* (entity-cx entity))
     (if (>= (entity-lerp-den entity) 1)
       (round (* (entity-lerp-num entity) (entity-lerp-pdx entity))
              (entity-lerp-den entity))
       0)))
(defmethod entity-vis-py ((entity entity))
  (+ (* *cell-h* (entity-cy entity))
     (if (>= (entity-lerp-den entity) 1)
       (round (* (entity-lerp-num entity) (entity-lerp-pdy entity))
              (entity-lerp-den entity))
       0)))

(defmethod tick ((entity entity))
  (with-slots (cx cy dir
               walk-dx walk-dy
               lerp-pdx lerp-pdy
               lerp-num lerp-den
               cooldown-ticks)
              *player-entity*

    ;; Update lerp
    (when (> lerp-num 0)
      (decf lerp-num))

    ;; Calculate entity sprite
    (cond
      ((< walk-dy 0) (setf dir :north))
      ((> walk-dy 0) (setf dir :south))
      ((< walk-dx 0) (setf dir :west))
      ((> walk-dx 0) (setf dir :east)))

    ;; Tick movement cooldown
    (when (> cooldown-ticks 0)
      (decf cooldown-ticks))

    ;; Can we move again?
    (when (<= cooldown-ticks 0)
      ;; Yes - are we wanting to move?
      (unless (and (= 0 walk-dx)
                   (= 0 walk-dy))
        ;; Yes - can we move there?
        (let* ((new-x (+ cx walk-dx))
               (new-y (+ cy walk-dy)))
          (when (can-enter-cell new-x new-y)
            ;; Yes - move!
            (let* ((old-vis-px (entity-vis-px entity))
                   (old-vis-py (entity-vis-py entity)))
              (setf cx new-x)
              (setf cy new-y)
              (setf lerp-pdx (- old-vis-px (* cx *cell-w*)))
              (setf lerp-pdy (- old-vis-py (* cy *cell-h*)))
              (setf cooldown-ticks (entity-walk-cooldown entity))
              (setf lerp-den (floor (* cooldown-ticks 3/2)))
              (setf lerp-num lerp-den)
              )))))

    ;; Delete walk step
    (setf walk-dx 0)
    (setf walk-dy 0)
    ))

(defmethod entity-graphic ((entity entity))
  (let* ((dir-idx (ecase (entity-dir entity)
                    (:north 0)
                    (:south 1)
                    (:west  2)
                    (:east  3))))
    (values *gfx-player-base* dir-idx 0)))

(defmethod draw-entity-at ((entity entity) px py)
  (multiple-value-bind (texture subx suby) (entity-graphic entity)
    (sdl2:with-rects ((d-rect px py *cell-w* *cell-h*)
                      (s-rect (* *cell-w* subx)
                              (* *cell-h* suby)
                              *cell-w* *cell-h*))
      (sdl2:render-copy *renderer* texture
                        :source-rect s-rect
                        :dest-rect d-rect))))
