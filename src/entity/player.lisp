(in-package #:orions-furnace)

(standard-optimisation-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-serial-class player-entity (dir4-entity entity)
  ()
  (:documentation "A player entity."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod entity-texture ((entity player-entity)) *gfx-player-base*)
(defmethod is-in-player-space ((entity player-entity)) t)

