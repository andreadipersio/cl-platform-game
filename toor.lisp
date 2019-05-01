;;;; toor.lisp

(in-package #:toor)

(defparameter *player-entity* nil)

(defun test ()
  (let ((player-entity
	 (cl-ecs:add-entity nil
			    (coords :x 10 :y 0)
			    (visibility :w 100 :h 100 :rgba '(255 255 0 1))
			    (velocity :x 0 :y 0)))
	(npc-entity
	 (cl-ecs:add-entity nil
			    (coords :x 0 :y 400)
			    (visibility :w 120 :h 90 :rgba '(255 0 0 1))
			    (velocity :x 0.5 :y 0))))
    (setq *player-entity* player-entity)
    (format t "*player-entity* is ~A~%" *player-entity*)))

(defun print-fps (current-game-time)
  (format t "delta-time ~A fps ~A~%"
	  (game-time-delta current-game-time)
	  (fps current-game-time)))

(defun move-entity (direction entity)
  (cond
    ((eq direction :top)
     (setf (velocity/y entity) -0.5))
    ((eq direction :left)
     (setf (velocity/x entity) 0.5))
    ((eq direction :bottom)
     (setf (velocity/y entity) 0.5))
    ((eq direction :right)
     (setf (velocity/x entity) -0.5))))

(defun reset-movement (entity &rest axis)
  (cond
    ((member :x axis) (setf (velocity/x entity) 0))
    ((member :y axis) (setf (velocity/y entity) 0))))

(defun game-loop (current-game-time renderer)
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (when (sdl2:keyboard-state-p :scancode-w) (move-entity :top *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-a) (move-entity :right *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-s) (move-entity :bottom *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-d) (move-entity :left *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-i) (print-fps current-game-time)))

    (:keyup (:keysym keysym)
	    (when (not (or
			(sdl2:keyboard-state-p :scancode-w)
			(sdl2:keyboard-state-p :scancode-s))) (reset-movement *player-entity* :y))
	    (when (not (or
			(sdl2:keyboard-state-p :scancode-a)
			(sdl2:keyboard-state-p :scancode-d))) (reset-movement *player-entity* :x)))

    (:idle
     ()
     (update-game-time current-game-time)
     (sdl2:set-render-draw-color renderer 0 0 0 255)
     (sdl2:render-clear renderer)
     (cl-ecs:do-system 'movement)
     (cl-ecs:do-system 'render)
     (sdl2:render-present renderer)
     (sdl2:delay 5))
    (:quit () t)))

(defun main ()
  (reset-ecs)
  (test)
  (let ((current-game-time (make-game-time)))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Toor" :flags '(:shown))
	(sdl2:with-renderer (renderer win :flags '(:accelerated))
	  (init-movement-sys current-game-time)
	  (init-render-sys renderer)
	  (continuable (game-loop current-game-time renderer)))))))
