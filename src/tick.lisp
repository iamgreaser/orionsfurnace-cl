;;;; tick.lisp

(in-package #:orions-furnace)

(defparameter *ticks-per-second* 60)
(defparameter *last-tick-secs* nil)
(defparameter *last-tick-frames* nil)
(defun run-ticks ()
  (let* ((sec-current (/ (get-internal-real-time) internal-time-units-per-second))
         (frame-current (floor (* sec-current *ticks-per-second*))))
    ;; Do we have a previous time?
    (when *last-tick-secs*
      ;; Yes - take a delta.
      (let* ((frame-delta (- frame-current *last-tick-frames*)))
        (dotimes (reps frame-delta)
          (run-one-tick))))

    ;; Set last tick.
    (setf *last-tick-secs* sec-current)
    (setf *last-tick-frames* frame-current)))

(defun run-one-tick ()
  (let* ((walk-dx 0)
         (walk-dy 0))

    ;; Calculate walk direction
    (when *player-walk-key-xn* (decf walk-dx))
    (when *player-walk-key-xp* (incf walk-dx))
    (when *player-walk-key-yn* (decf walk-dy))
    (when *player-walk-key-yp* (incf walk-dy))
    (setf walk-dx (max -1 (min 1 walk-dx)))
    (setf walk-dy (max -1 (min 1 walk-dy)))

    ;; Calculate player sprite
    (cond
      ((< walk-dy 0) (setf *player-dir* :north))
      ((> walk-dy 0) (setf *player-dir* :south))
      ((< walk-dx 0) (setf *player-dir* :west))
      ((> walk-dx 0) (setf *player-dir* :east)))

    ;; Tick movement cooldown
    (when (> *player-move-cooldown* 0)
      (decf *player-move-cooldown*))

    ;; Can we move again?
    (when (<= *player-move-cooldown* 0)
      ;; Yes - are we wanting to move?
      (unless (and (= 0 walk-dx)
                   (= 0 walk-dy))
        ;; Yes - can we move there?
        (let* ((new-x (+ *player-tile-x* walk-dx))
               (new-y (+ *player-tile-y* walk-dy)))
          (when (can-enter-cell new-x new-y)
            ;; Yes - move!
            (setf *player-tile-x* new-x)
            (setf *player-tile-y* new-y)
            (setf *player-move-cooldown* *player-move-cooldown-time*)))))
    ))

(defun can-enter-cell (cx cy)
  (block result
    (unless (<= 0 cx (1- *board-w*))
      (return-from result nil))
    (unless (<= 0 cy (1- *board-h*))
      (return-from result nil))

    ;; TODO: Check against some sort of grid
    t))
