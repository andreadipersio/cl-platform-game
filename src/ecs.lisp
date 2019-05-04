(in-package #:toor)

(cl-ecs:init-ecs)

(cl-ecs:defcomponent coords (x y))
(cl-ecs:defcomponent visibility (w h rgba))
(cl-ecs:defcomponent velocity (x y))
(cl-ecs:defcomponent player ())

(defun reset-ecs ()
  (mapcar #'cl-ecs:remove-entity (cl-ecs:all-entities)))

(defun init-movement-sys (game-time level)
  (cl-ecs:defsys movement ((coords velocity) (e))
    (let* ((delta-time (game-time-delta game-time))
	   (velocity-x (* (velocity/x e) delta-time))
	   (velocity-y (* (velocity/y e) delta-time))
	   (new-x (+ (coords/x e) velocity-x))
	   (new-y (+ (coords/y e) velocity-y))
	   (width (visibility/w e))
	   (height (visibility/h e)))

      ;; Prevent entity to move out of a level
      (when (or (minusp new-x)
		(> (+ new-x (if width width 0))
		   (level-width level))) (decf new-x velocity-x))
      (when (or (minusp new-y)
		(> (+ new-y (if height height 0))
		   (level-height level))) (decf new-y velocity-y))

      (setf (coords/x e) new-x)
      (setf (coords/y e) new-y))))

(defun init-camera-sys (renderer camera level)
  (cl-ecs:defsys camera ((player coords visibility) (e))
    (let ((rect (make-rect-from-entity e)))
      (center-camera camera level rect)
      (sdl2:render-copy renderer *background-texture* :source-rect (make-sdl-rect-from-camera camera)))))

(defun init-render-sys (renderer camera)
  (cl-ecs:defsys render ((coords visibility) (e))
    (apply #'sdl2:set-render-draw-color (cons renderer (visibility/rgba e)))
    (let ((x (truncate (- (coords/x e) (camera-x camera))))
	  (y (truncate (- (coords/y e) (camera-y camera))))
	  (w (visibility/w e))
	  (h (visibility/h e)))
      (sdl2:render-draw-rect renderer (sdl2:make-rect x y w h)))))
