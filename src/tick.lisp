;;;; tick.lisp

(in-package #:orions-furnace)

(standard-optimisation-settings)

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

    (setf (entity-walk-dx *player-entity*) walk-dx)
    (setf (entity-walk-dy *player-entity*) walk-dy)

    ;; Tick it!
    (tick *player-entity*)
    ))
