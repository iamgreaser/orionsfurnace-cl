(in-package #:orions-furnace)

(standard-optimisation-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Make an enum system for dir --GM
(define-serial-class dir4-entity (entity)
  ((dir :type symbol
        :accessor entity-dir
        :initform :south
        :initarg :dir))
  (:documentation "Mixin for ENTITY: 4 directions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod entity-graphic ((entity dir4-entity))
  (let* ((dir-idx (ecase (entity-dir entity)
                    (:north 0)
                    (:south 1)
                    (:west  2)
                    (:east  3))))
    (values (entity-texture entity) dir-idx 0)))

(defmethod update-entity-direction ((entity dir4-entity) dx dy)
  (with-slots (dir) entity
    (cond
      ((< dy 0) (setf dir :north))
      ((> dy 0) (setf dir :south))
      ((< dx 0) (setf dir :west))
      ((> dx 0) (setf dir :east)))))
