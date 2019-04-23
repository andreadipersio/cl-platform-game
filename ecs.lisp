(in-package #:toor)

(cl-ecs:init-ecs)

(cl-ecs:defcomponent position (x y))
(cl-ecs:defcomponent visibility (w h rgba))

(defun init-render-sys (renderer)
  (cl-ecs:defsys render ((position visibility) (e))
    (apply #'sdl2:set-render-draw-color (cons renderer (visibility/rgba e)))
    (sdl2:render-draw-rect renderer (sdl2:make-rect
				(position/x e) (position/y e)
				(visibility/w e) (visibility/h e)))))
