;;;; run.lisp

(in-package #:orions-furnace)

(defun run ()
  (sdl2:with-init (:video)
    (sdl2:with-window (*window* :title "Orion's Furnace (Lisp version)"
                                :w (* *base-screen-width* *screen-scale*)
                                :h (* *base-screen-height* *screen-scale*))
      (sdl2:with-renderer (*renderer* *window* :flags '())
        (with-image-lib ()
          (with-game-assets ()
            (with-texture (*backbuf* *renderer*
                                   sdl2:+pixelformat-bgra8888+
                                   :target
                                   *base-screen-width*
                                   *base-screen-height*)
            (core-event-loop)
            )))))))

(defun core-event-loop ()
  (sdl2:with-event-loop ()
    (:keydown (:keysym keysym) (handle-key keysym t))
    (:keyup   (:keysym keysym) (handle-key keysym nil))
    (:quit () t)
    (:idle () (handle-idle))
    ))

(defun handle-key (keysym pressed)
  (keysym-case keysym
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
