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
  (* *cell-w* (entity-cx entity)))
(defmethod entity-vis-py ((entity entity))
  (* *cell-h* (entity-cy entity)))

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
