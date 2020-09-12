(in-package #:orions-furnace)

(standard-optimisation-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-serial-class floor-tile-entity (item-entity)
  ()
  (:documentation "A floor tile item."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod entity-texture ((entity item-entity)) *gfx-tiles-floortile001*)
