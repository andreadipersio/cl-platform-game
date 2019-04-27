;;;; toor.lisp

(in-package #:toor)

(defun test ()
  (cl-ecs:add-entity nil
	      (coords :x 10 :y 0)
	      (visibility :w 100 :h 100 :rgba '(255 255 0 1)))
  (cl-ecs:add-entity nil
	      (coords :x 0 :y 400)
	      (visibility :w 120 :h 90 :rgba '(255 0 0 1))
	      (velocity :x 0.5 :y 0))
  (cl-ecs:add-component 1 'velocity '(:velocity/x 0.5 :velocity/y 0)))

(defun game-loop (current-game-time)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Toor" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(init-movement-sys current-game-time)
	(init-render-sys renderer)
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:idle
	   ()
	   (update-game-time current-game-time)
	   (sdl2:set-render-draw-color renderer 0 0 0 255)
	   (sdl2:render-clear renderer)
	   (cl-ecs:do-system 'movement)
	   (cl-ecs:do-system 'render)
	   (sdl2:render-present renderer)
	   (sdl2:delay 5)
	   (format t "dt ~A fps ~A~%"
		   (game-time-delta current-game-time)
		   (fps current-game-time)))
	  (:quit () t))))))
  
(defun main ()
  (test)
  (game-loop (make-game-time)))
