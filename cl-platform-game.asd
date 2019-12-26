(asdf:defsystem #:cl-platform-game
  :description "2d platform game in Common Lisp"
  :author "Andrea Di Persio <me@andreadipersio.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2
	       :sdl2-image
	       :bordeaux-threads
	       :usocket
	       :cl-ecs
	       :graph
	       :livesupport)
  :pathname "src"
  :components ((:file "package")
               (:file "game")
	       (:file "time")
	       (:file "ecs")
	       (:file "camera")
	       (:file "animation")
	       (:file "animation-assets")
	       (:file "animation-helper")))
