(in-package #:toor)

(cl-ecs:init-ecs)

(cl-ecs:defcomponent coords (x y))
(cl-ecs:defcomponent visibility (w h rgba))
(cl-ecs:defcomponent velocity (x y))

(cl-ecs:defsys movement ((coords velocity) (e))
  (let ((new-x (+ (coords/x e) (velocity/x e)))
	(new-y (+ (coords/y e) (velocity/y e))))
    (setf (coords/x e) new-x)
    (setf (coords/y e) new-y)))

(defun init-render-sys (renderer)
  (cl-ecs:defsys render ((coords visibility) (e))
    (apply #'sdl2:set-render-draw-color (cons renderer (visibility/rgba e)))
    (sdl2:render-draw-rect renderer (sdl2:make-rect
				(coords/x e) (coords/y e)
				(visibility/w e) (visibility/h e)))))
