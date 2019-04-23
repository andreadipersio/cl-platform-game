;;;; toor.asd

(asdf:defsystem #:toor
  :description "Toor - Hackform Game"
  :author "Andrea Di Persio <me@andreadipersio.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2
	       :bordeaux-threads
	       :usocket
	       :cl-ecs
	       :graph)
  :components ((:file "package")
               (:file "toor")
	       (:file "ecs")))
