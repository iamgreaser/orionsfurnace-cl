(in-package #:orions-furnace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generics

(defgeneric cell-on-board (board cx cy)
  (:documentation "Given a board, does the given cell exist?"))
(defgeneric can-enter-cell (board entity cx cy)
  (:documentation "Can, on the given board, the given entity enter the cell at the given position?"))
(defgeneric draw-tile-at-lower (board cx cy px py)
  (:documentation "Draws a tile onto somewhere on the screen - lower z-level."))
(defgeneric draw-tile-at-upper (board cx cy px py)
  (:documentation "Draws a tile onto somewhere on the screen - upper z-level."))
(defgeneric board-entities-at (board cx cy)
  (:documentation "The list of entities at a given cell on a board."))
(defgeneric (setf board-entities-at) (entities board cx cy)
  (:documentation "Setter for the list of entities at a given cell on a board."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod board-entities-at ((board board) cx cy)
  (with-slots (entities-grid) board
    (aref entities-grid cx cy)))
(defmethod (setf board-entities-at) (entities (board board) cx cy)
  (with-slots (entities-grid) board
    (setf (aref entities-grid cx cy) entities)))

(defun generate-board (&key (w 64) (h 64))
  "Generates and returns a board."
  ;; TODO: Have some sort of tile type enum here --GM
  (let* ((tile-grid (make-array `(,w ,h)
                                :initial-element :floor))
         (entities-grid (make-array `(,w ,h)
                                    :initial-element nil))
         (board (make-instance 'board
                               :tile-grid tile-grid
                               :entities-grid entities-grid)))
    (prog1 board
      ;; TODO: Have some fun with this --GM
      )))

(defmethod cell-on-board ((board (eql nil)) cx cy)
  ;; Nothing has no cells.
  nil)

(defmethod cell-on-board ((board board) cx cy)
  (with-slots (tile-grid) board
    (destructuring-bind (bw bh) (array-dimensions tile-grid)
      (and (<= 0 cx bw)
           (<= 0 cy bh)))))

(defmethod can-enter-cell ((board (eql nil)) (entity entity) cx cy)
  ;; Normal entities cannot move around in nothingness.
  nil)

(defmethod can-enter-cell ((board board) (entity entity) cx cy)
  (block result
    (macrolet ((nope! () `(return-from result nil)))
      (with-slots (tile-grid
                   entities-grid)
                  board
        (destructuring-bind (bw bh) (array-dimensions tile-grid)
          (unless (<= 0 cx bw) (nope!))
          (unless (<= 0 cy bh) (nope!))

          ;; Check against the tile grid
          (case (aref tile-grid cx cy)
            ;; These are good
            (:space)
            (:floor)
            ;; These are not
            (t (nope!))
            )

          ;; Check against the entities list
          (dolist (other (board-entities-at board cx cy))
            ;; If it exists, nope out
            ;; TODO: Add some better logic to this --GM
            (nope!)
            )

          ;; OK, we're good!
          t)))))

(defmethod draw-tile-at-lower ((board board) cx cy px py)
  (with-slots (tile-grid
               entities-grid
               )
              board
    (destructuring-bind (bw bh) (array-dimensions tile-grid)
      (when (and (<= 0 cx bw)
                 (<= 0 cy bh))
        ;; TODO: Select the correct tile to draw --GM
        ;; TODO: Make sure we get the right wall tile once we use those --GM
        (let* ((texture *gfx-tiles-floor001*))
          (with-pooled-rects ((d-rect px py *cell-w* *cell-h*)
                            (s-rect 0 0 *cell-w* *cell-h*))
            (sdl2:render-copy *renderer* texture
                              :source-rect s-rect
                              :dest-rect d-rect)))
        ))))

(defmethod draw-tile-at-upper ((board board) cx cy px py)
  (with-slots (tile-grid
               entities-grid
               )
              board
    (destructuring-bind (bw bh) (array-dimensions tile-grid)
      (when (and (<= 0 cx bw)
                 (<= 0 cy bh))
        (dolist (entity (reverse (aref entities-grid cx cy)))
          (draw-entity entity))
        ))))
