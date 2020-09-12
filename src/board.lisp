(in-package #:orions-furnace)

(standard-optimisation-settings)

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
(defgeneric board-tile-at (board cx cy)
  (:documentation "The tile at a given cell on a board."))
(defgeneric (setf board-tile-at) (tile board cx cy)
  (:documentation "Setter for the tile at a given cell on a board."))
(defgeneric tile-graphics (tile board cx cy)
  (:documentation "Returns (values texture src-cx src-cy) for a given tile."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod board-entities-at ((board board) cx cy)
  (with-slots (entities-grid) board
    (aref entities-grid cx cy)))
(defmethod (setf board-entities-at) (entities (board board) cx cy)
  (with-slots (entities-grid) board
    (setf (aref entities-grid cx cy) entities)))

(defmethod board-tile-at ((board null) cx cy) :space)
(defmethod board-tile-at ((board board) cx cy)
  (if (cell-on-board board cx cy)
    (with-slots (tile-grid) board
      (aref tile-grid cx cy))
    :space))
(defmethod (setf board-tile-at) (tile (board board) cx cy)
  (with-slots (tile-grid) board
    (setf (aref tile-grid cx cy) tile)))

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

      (setf (board-tile-at board 4 7) :wall)
      (setf (board-tile-at board 5 7) :wall)
      (setf (board-tile-at board 4 8) :wall)
      (setf (board-tile-at board 5 8) :wall)
      (setf (board-tile-at board 5 9) :wall)
      (setf (board-tile-at board 6 10) :wall)
      (setf (board-tile-at board 7 9) :wall)

      (setf (board-tile-at board 6 4) :closed-door)

      ;; Dummy entity
      (vector-push-extend
        (make-instance 'entity
                       :board board
                       :x 8
                       :y 4)
        (board-entities-array board))

      ;; Dummy player
      (vector-push-extend
        (make-instance 'player-entity
                       :board board
                       :x 10
                       :y 3)
        (board-entities-array board))
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
            ;; If it exists, check if we can enter into it
            (when (and (is-in-player-space entity)
                       (is-in-player-space other))
              ;; Nope!
              (nope!)))

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
        (multiple-value-bind (texture sx sy) (tile-graphics (board-tile-at board cx cy)
                                                            board cx cy)
          (with-pooled-rects ((d-rect px py *cell-w* *cell-h*)
                              (s-rect (* *cell-w* sx)
                                      (* *cell-h* sy)
                                      *cell-w* *cell-h*))
            (sdl2:render-copy *renderer* texture
                              :source-rect s-rect
                              :dest-rect d-rect)))
        ))))

(defmethod tile-graphics ((tile (eql :floor)) board cx cy)
  (values *gfx-tiles-floor001* 0 0))
(defmethod tile-graphics ((tile (eql :wall)) board cx cy)
  (let* ((sx 0) (sy 0))
    (when (eql tile (board-tile-at board (1+ cx) cy)) (incf sx 1))
    (when (eql tile (board-tile-at board (1- cx) cy)) (incf sx 2))
    (when (eql tile (board-tile-at board cx (1+ cy))) (incf sy 1))
    (when (eql tile (board-tile-at board cx (1- cy))) (incf sy 2))
    (values *gfx-tiles-wall001* sx sy)))

;; Fallback
(defmethod tile-graphics ((tile t) board cx cy)
  (values *gfx-unknown* 0 0))

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

(defmethod tick ((board board))
  ;; Tick entities
  (with-slots (entities-array) board
    (dotimes (i (fill-pointer entities-array))
      (tick (aref entities-array i))))
  )
