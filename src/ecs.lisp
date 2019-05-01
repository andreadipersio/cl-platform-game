(in-package #:toor)

(cl-ecs:init-ecs)

(cl-ecs:defcomponent coords (x y))
(cl-ecs:defcomponent visibility (w h rgba))
(cl-ecs:defcomponent velocity (x y))

(defun reset-ecs ()
  (mapcar #'cl-ecs:remove-entity (cl-ecs:all-entities)))

(defun init-movement-sys (current-game-time)
  (cl-ecs:defsys movement ((coords velocity) (e))
    (let* ((delta-time (game-time-delta current-game-time))
	   (velocity-x (* (velocity/x e) delta-time))
	   (velocity-y (* (velocity/y e) delta-time))
	   (new-x (+ (coords/x e) velocity-x))
	   (new-y (+ (coords/y e) velocity-y)))
      (if (>= new-x 800) (setq new-x 0))
      (if (>= new-y 600) (setq new-y 0))
      (setf (coords/x e) new-x)
      (setf (coords/y e) new-y))))

(defun init-render-sys (renderer)
  (cl-ecs:defsys render ((coords visibility) (e))
    (let ((x (truncate (coords/x e)))
	  (y (truncate (coords/y e))))
      (apply #'sdl2:set-render-draw-color (cons renderer (visibility/rgba e)))
      (sdl2:render-draw-rect renderer (sdl2:make-rect x y
				       (visibility/w e) (visibility/h e))))))
