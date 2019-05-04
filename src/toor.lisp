;;;; toor.lisp

(in-package #:toor)

(defparameter *player-entity* nil)

(defparameter *background-filepath* (asdf:system-relative-pathname 'toor "assets/bg-100.png"))
(defparameter *background-surface* (sdl2-image:load-image *background-filepath*))
(defparameter *background-texture* nil)

(defun init-textures (renderer)
  (setq *background-texture* (sdl2:create-texture-from-surface renderer *background-surface*)))

(defun render-background (renderer)
  (sdl2:render-copy renderer *background-texture*))

(defun test ()
  (let ((player-entity
	 (cl-ecs:add-entity nil
	   (player)
	   (coords :x 10 :y 0)
	   (visibility :w 100 :h 100 :rgba '(255 255 0 1))
	   (velocity :x 0 :y 0)))
	(npc-entity
	 (cl-ecs:add-entity nil
	   (coords :x 0 :y 400)
	   (visibility :w 120 :h 90 :rgba '(255 0 0 1))
	   (velocity :x 0.1 :y 0))))
    (setq *player-entity* player-entity)
    (format t "*player-entity* is ~A~%" *player-entity*)))

(defun print-fps (game-time)
  (format t "delta-time ~A fps ~A~%"
	  (game-time-delta game-time)
	  (fps game-time)))

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

(defun game-loop (game-time renderer)
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (when (sdl2:keyboard-state-p :scancode-w) (move-entity :top *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-a) (move-entity :right *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-s) (move-entity :bottom *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-d) (move-entity :left *player-entity*))
	      (when (sdl2:keyboard-state-p :scancode-i) (print-fps game-time)))

    (:keyup (:keysym keysym)
	    (when (not (or
			(sdl2:keyboard-state-p :scancode-w)
			(sdl2:keyboard-state-p :scancode-s))) (reset-movement *player-entity* :y))
	    (when (not (or
			(sdl2:keyboard-state-p :scancode-a)
			(sdl2:keyboard-state-p :scancode-d))) (reset-movement *player-entity* :x)))

    (:idle
     ()
     (update-game-time game-time)
     (sdl2:set-render-draw-color renderer 0 0 0 255)
     (sdl2:render-clear renderer)
     (cl-ecs:do-system 'movement)
     (cl-ecs:do-system 'camera)     
     (cl-ecs:do-system 'render)
     (sdl2:render-present renderer)
     (sdl2:delay 1))
    (:quit () t)))

(defun main ()
  (reset-ecs)
  (test)
  (sdl2-image:init '(:png))
  (let ((game-time (make-game-time))
	(camera (make-camera)))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Toor" :flags '(:shown))
	(sdl2:with-renderer (renderer win :flags '(:accelerated))
	  (init-movement-sys game-time)
	  (init-camera-sys renderer camera *level-1*)
	  (init-render-sys renderer camera)
	  (init-textures renderer)
	  (continuable (game-loop game-time renderer)))))))
