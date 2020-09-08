(in-package #:orions-furnace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entity mixins

;; TODO: Make an enum system for dir --GM
(define-serial-class dir4-entity (entity)
  ((dir :type symbol
        :accessor entity-dir
        :initform :south
        :initarg :dir))
  (:documentation "Mixin for ENTITY: 4 directions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concrete entities

(define-serial-class player-entity (dir4-entity entity)
  ()
  (:documentation "A player entity."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entity generics

(defgeneric entity-vis-px (entity)
  (:documentation "Visual pixel X coordinate."))
(defgeneric entity-vis-py (entity)
  (:documentation "Visual pixel Y coordinate."))
(defgeneric entity-texture (entity)
  (:documentation "The texture to use for this entity."))
(defgeneric entity-graphic (entity)
  (:documentation "The graphic of this entity, in the form (texture subimg-x subimg-y). Used for normal one-tile-sized graphics."))
(defgeneric update-entity-direction (entity dx dy)
  (:documentation "Update the direction that this entity is facing."))
(defgeneric draw-entity-at (entity px py)
  (:documentation "Draw the given entity at the given position."))
(defgeneric attempt-move-entity-by (entity dx dy &key cooldown)
  (:documentation "Attempt to move an entity by a vector, with lerping. Returns T if successful, NIL if it failed."))
(defgeneric force-move-entity-by (entity dx dy &key cooldown)
  (:documentation "Force moving an entity by a vector, with lerping."))
(defgeneric force-move-entity-to (entity cx cy &key cooldown)
  (:documentation "Force moving an entity to a position, without lerping."))

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

(defmethod update-entity-direction ((entity entity) dx dy)
  ;; Nondirectional entity needs no direction update.
  )

(defgeneric entity-texture (entity)
  (:documentation "Texture for this entity."))

(defmethod tick ((entity entity))
  (with-slots (board
               cx cy dir
               walk-dx walk-dy
               lerp-pdx lerp-pdy
               lerp-num lerp-den
               cooldown-ticks)
              *player-entity*

    ;; If we aren't on the correct cell on the board, put us there.
    (when board
      (when (cell-on-board board cx cy)
        (unless (member entity (board-entities-at board cx cy)
                        :test #'eq)
          (push entity (board-entities-at board cx cy)))))

    ;; Update lerp
    (when (> lerp-num 0)
      (decf lerp-num))

    ;; Calculate entity sprite
    ;; Tick movement cooldown
    (when (> cooldown-ticks 0)
      (decf cooldown-ticks))

    ;; Are we wanting to go in some direction?
    (unless (and (= 0 walk-dx)
                 (= 0 walk-dy))
      ;; Yes.
      ;; Update direction
      (update-entity-direction entity walk-dx walk-dy)

      ;; Is our cooldown finished?
      (when (<= cooldown-ticks 0)
        ;; Yes - try moving.
        (attempt-move-entity-by
          entity walk-dx walk-dy
          :cooldown (entity-walk-cooldown entity))))

    ;; Delete walk step
    (setf walk-dx 0)
    (setf walk-dy 0)
    ))

(defmethod attempt-move-entity-by ((entity entity) dx dy &key (cooldown 0))
  (let* ((new-x (+ (entity-cx entity) dx))
         (new-y (+ (entity-cy entity) dy)))
    (if (can-enter-cell (entity-board entity) entity new-x new-y)
      (force-move-entity-by entity dx dy
                            :cooldown cooldown))))

(defmethod force-move-entity-by ((entity entity) dx dy &key (cooldown 0))
  (with-slots (cx cy
               lerp-pdx lerp-pdy lerp-num lerp-den
               cooldown-ticks)
              entity
    (let* ((new-x (+ cx dx))
           (new-y (+ cy dy))
           (old-vis-px (entity-vis-px entity))
           (old-vis-py (entity-vis-py entity)))
      (setf cooldown-ticks cooldown)
      (force-move-entity-to entity new-x new-y :cooldown cooldown)
      (setf lerp-pdx (- old-vis-px (* cx *cell-w*)))
      (setf lerp-pdy (- old-vis-py (* cy *cell-h*)))
      (setf lerp-den (floor (* cooldown-ticks 3/2)))
      (setf lerp-num lerp-den)
      )))

(defmethod force-move-entity-to ((entity entity) new-x new-y &key (cooldown 0))
  (with-slots (cx cy
               board
               lerp-pdx lerp-pdy lerp-num lerp-den
               cooldown-ticks)
              entity
    ;; Remove ourself from the cell
    (setf (board-entities-at board cx cy)
          (delete entity (board-entities-at board cx cy)
                  :test #'eq))
    (setf cooldown-ticks cooldown)
    (setf lerp-pdx 0)
    (setf lerp-pdy 0)
    (setf lerp-num 0)
    (setf lerp-den 1)
    (setf cx new-x)
    (setf cy new-y)
    (pushnew entity (board-entities-at board cx cy)
             :test #'eq)
    ))

(defmethod entity-graphic ((entity entity))
  (values (entity-texture entity) 0 0))

;; TODO: Make a decent default texture and use it --GM
(defmethod entity-texture ((entity entity)) *gfx-tiles-wall001*)

(defun draw-entity (entity)
  (let* ((cx (entity-cx entity))
         (cy (entity-cy entity))
         (px (- (entity-vis-px entity) *cam-offs-x*))
         (py (- (entity-vis-py entity) *cam-offs-y*)))
    (draw-entity-at entity px py)))

(defmethod draw-entity-at ((entity entity) px py)
  (multiple-value-bind (texture subx suby) (entity-graphic entity)
    (sdl2:with-rects ((d-rect px py *cell-w* *cell-h*)
                      (s-rect (* *cell-w* subx)
                              (* *cell-h* suby)
                              *cell-w* *cell-h*))
      (sdl2:render-copy *renderer* texture
                        :source-rect s-rect
                        :dest-rect d-rect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directional entity methods

(defmethod entity-graphic ((entity dir4-entity))
  (let* ((dir-idx (ecase (entity-dir entity)
                    (:north 0)
                    (:south 1)
                    (:west  2)
                    (:east  3))))
    (values (entity-texture entity) dir-idx 0)))
(defmethod update-entity-direction ((entity dir4-entity) dx dy)
  ;; 4-way directional entities.
  (with-slots (dir) entity
    (cond
      ((< dy 0) (setf dir :north))
      ((> dy 0) (setf dir :south))
      ((< dx 0) (setf dir :west))
      ((> dx 0) (setf dir :east)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Player entity methods

(defmethod entity-texture ((entity player-entity)) *gfx-player-base*)
