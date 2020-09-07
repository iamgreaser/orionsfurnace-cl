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

(define-serial-class entity ()
  ((id :type (unsigned-byte 32)
       :initarg :id
       :initform (error "Must provide :ID"))
   (world :type (or null world) :initform nil)
   (x :type (signed-byte 16) :initform -1)
   (y :type (signed-byte 16) :initform -1)
   )
  (:documentation "An entity with a position which can be bound to a world. Subclass me!"))

(define-serial-class player ()
  ((id :type (unsigned-byte 16)
       :initarg :id
       :initform (error "Must provide :ID"))
   (entity :type (or null entity) :initform nil)
   )
  (:documentation "A player which can be bound to an entity."))

(define-serial-class world ()
  ((id :type (unsigned-byte 32)
       :initarg :id
       :initform (error "Must provide :ID"))
   (grid :type (simple-array (or null entity) (* *))
         :initarg :grid
         :initform (error "Must provide :GRID"))
   )
  (:documentation "A world with a lot of things in it."))

