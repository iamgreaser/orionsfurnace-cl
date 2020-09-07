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
   ;; TODO: Make an enum system for dir --GM
   (dir :type symbol
        :accessor entity-dir
        :initform :south
        :initarg :dir)
   (cooldown-ticks :type (unsigned-byte 16)
                   :accessor entity-cooldown-ticks
                   :initform 0)
   (walk-cooldown :type (signed-byte 16)
                  :accessor entity-walk-cooldown
                  :initform 8)
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
       :initarg :id
       :initform (error "Must provide :ID"))
   (grid :type (simple-array (or null entity) (* *))
         :initarg :grid
         :initform (error "Must provide :GRID"))
   )
  (:documentation "A board with a lot of things in it."))

