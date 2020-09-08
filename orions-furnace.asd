;;;; orions-furnace.asd

(asdf:defsystem #:orions-furnace
  :description "Describe orions-furnace here"
  ;; :author "Your Name <your.name@example.com>"
  :license "AGPLv3+"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:cffi
               #:sdl2
               #:sdl2-image
               #:sdl2-ttf
               #:trivial-gray-streams)
  :pathname "src"
  :components ((:file "package")
               (:file "assets")
               (:file "macros")
               (:file "classes")
               (:file "orions-furnace")

               (:file "entity")

               (:file "draw")
               (:file "tick")
               (:file "run")
               ))
