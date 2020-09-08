(in-package #:orions-furnace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialisation

(defmacro define-serial-class (class-name (&rest subclasses)
                                          (&body field-defs)
                                          &body extra-params)
  `(progn
     (defclass ,class-name (,@subclasses)
       (,@field-defs)
       ,@extra-params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Widespread generics

(defgeneric tick (thing)
  (:documentation "Ticks... well, anything really."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *entity-id-counter* 0)
(define-serial-class entity ()
  ((id :type (unsigned-byte 32)
       :accessor entity-id
       :initarg :id
       :initform (prog1 *entity-id-counter* (incf *entity-id-counter*)))
   (board :type (or null board)
          :accessor entity-board
          :initform nil
          :initarg :board)
   (cx :type (signed-byte 16)
       :accessor entity-cx
       :initform -1
       :initarg :x)
   (cy :type (signed-byte 16)
       :accessor entity-cy
       :initform -1
       :initarg :y)
   (cooldown-ticks :type (unsigned-byte 16)
                   :accessor entity-cooldown-ticks
                   :initform 0)
   (walk-cooldown :type (signed-byte 16)
                  :accessor entity-walk-cooldown
                  :initform 8)

   ;; Input handling
   (walk-dx :type (signed-byte 8)
            :accessor entity-walk-dx
            :initform 0)
   (walk-dy :type (signed-byte 8)
            :accessor entity-walk-dy
            :initform 0)

   ;; For lerping
   (lerp-pdx :type (signed-byte 32)
             :accessor entity-lerp-pdx
             :initform 0)
   (lerp-pdy :type (signed-byte 32)
             :accessor entity-lerp-pdy
             :initform 0)
   (lerp-num :type (unsigned-byte 16)
             :accessor entity-lerp-num
             :initform 0)
   (lerp-den :type (unsigned-byte 16)
             :accessor entity-lerp-den
             :initform 0)
   )
  (:documentation "An entity with a position which can be bound to a board. Subclass me!"))

(define-serial-class player ()
  ((id :type (unsigned-byte 16)
       :initarg :id
       :initform (error "Must provide :ID"))
   (entity :type (or null entity) :initform nil :initarg :entity)
   )
  (:documentation "A player which can be bound to an entity."))

(define-serial-class board ()
  ((id :type (unsigned-byte 32)
       :accessor board-id
       :initarg :id
       :initform (prog1 *entity-id-counter* (incf *entity-id-counter*)))
   ;; TODO: Have some sort of tile type enum here --GM
   (tile-grid :type (simple-array keyword (* *))
              :initarg :tile-grid
              :initform (error "Must provide :ENTITIES-GRID"))
   (entities-grid :type (simple-array list (* *))
                  :initarg :entities-grid
                  :initform (error "Must provide :ENTITIES-GRID"))
   )
  (:documentation "A board with a lot of things in it."))

