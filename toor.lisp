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

(defun print-fps (current-game-time)
  (format t "dt ~A fps ~A~%"
	  (game-time-delta current-game-time)
	  (fps current-game-time)))

(defun move-entity (direction entity)
  (cond
    ((eq direction :top)
     (setf (velocity/x entity) 0)
     (setf (velocity/y entity) -0.5))
    ((eq direction :left)
     (setf (velocity/x entity) 0.5)
     (setf (velocity/y entity) 0))
    ((eq direction :bottom)
     (setf (velocity/x entity) 0)
     (setf (velocity/y entity) 0.5))
    ((eq direction :right)
     (setf (velocity/x entity) -0.5)
     (setf (velocity/y entity) 0))))

(defun reset-movement (entity &rest axis)
  (cond
    ((member :x axis) (setf (velocity/x entity) 0))
    ((member :y axis) (setf (velocity/y entity) 0))))

(defun game-loop (current-game-time)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Toor" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(init-movement-sys current-game-time)
	(init-render-sys renderer)
	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym))
		 (sym (sdl2:sym-value keysym))
		 (mod-value (sdl2:mod-value keysym)))
	     (cond
	       ((sdl2:scancode= scancode :scancode-w) (move-entity :top 1))
	       ((sdl2:scancode= scancode :scancode-a) (move-entity :right 1))
	       ((sdl2:scancode= scancode :scancode-s) (move-entity :bottom 1))
	       ((sdl2:scancode= scancode :scancode-d) (move-entity :left 1))
	       ((sdl2:scancode= scancode :scancode-i) (print-fps current-game-time))
	       
	       )))

	  (:keyup
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym))
		 (sym (sdl2:sym-value keysym))
		 (mod-value (sdl2:mod-value keysym)))
	     (cond
	       ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))

	       ((sdl2:scancode= scancode :scancode-w) (reset-movement 1 :y))
	       ((sdl2:scancode= scancode :scancode-a) (reset-movement 1 :x))
	       ((sdl2:scancode= scancode :scancode-s) (reset-movement 1 :y))
	       ((sdl2:scancode= scancode :scancode-d) (reset-movement 1 :x))

	       )))

	  (:idle
	   ()
	   (update-game-time current-game-time)
	   (sdl2:set-render-draw-color renderer 0 0 0 255)
	   (sdl2:render-clear renderer)
	   (cl-ecs:do-system 'movement)
	   (cl-ecs:do-system 'render)
	   (sdl2:render-present renderer)
	   (sdl2:delay 5))
	  (:quit () t))))))

(defun main ()
  (test)
  (game-loop (make-game-time)))
