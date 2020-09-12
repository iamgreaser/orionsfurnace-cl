;;;; run.lisp

(in-package #:orions-furnace)

(standard-optimisation-settings)

(defun main ()
  (sdl2:with-init (:video)
    (with-overall-rects-pool ()
      (sdl2:with-window (*window* :title "Orion's Furnace (Lisp version)"
                                  :w (* *base-screen-width* *screen-scale*)
                                  :h (* *base-screen-height* *screen-scale*))
        (sdl2:with-renderer (*renderer* *window* :flags '())
          (with-image-lib ()
            (with-ttf-lib ()
              (with-game-assets ()
                (with-texture (*backbuf* *renderer*
                                       sdl2:+pixelformat-bgra8888+
                                       :target
                                       *base-screen-width*
                                       *base-screen-height*)
                (core-event-loop)
                )))))))))

(defun run ()
  (sdl2:make-this-thread-main #'main))

(defmacro with-game-state (() &body body)
  `(let* ((*board* (generate-board))
          (*player-entity* (make-instance 'player-entity
                                          :board *board*
                                          :x 3 :y 2)))
     (vector-push-extend
       *player-entity*
       (board-entities-array *board*))
     ,@body))

(defun core-event-loop ()
  (with-game-state ()
    (sdl2:with-event-loop ()
      (:keydown (:keysym keysym) (handle-key keysym t))
      (:keyup   (:keysym keysym) (handle-key keysym nil))
      (:quit () t)
      (:idle () (handle-idle))
      )))

(defun handle-key (keysym pressed)
  (keysym-case keysym
    ;; Movement keys
    (:a (setf *player-walk-key-xn* pressed))
    (:d (setf *player-walk-key-xp* pressed))
    (:w (setf *player-walk-key-yn* pressed))
    (:s (setf *player-walk-key-yp* pressed))

    ;; G: Collect garbage
    (:g (when pressed (let () #+sbcl (sb-ext:gc :full t))))

    ;; R: Show "room" (memory stats)
    (:r (when pressed (room t)))

    ;; Escape: Quit
    (:escape
      (when (not pressed)
        (sdl2:push-quit-event)))
    ))

(defun handle-idle ()
  ;; TODO!
  (run-ticks)
  (draw-frame)
  (sleep 1/1000)
  )
