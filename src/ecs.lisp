(in-package #:toor)

(cl-ecs:init-ecs)

(cl-ecs:defcomponent coords (x y))
(cl-ecs:defcomponent visibility (w h rgba))
(cl-ecs:defcomponent velocity (x y))
(cl-ecs:defcomponent player ())
(cl-ecs:defcomponent collision-behaviour (behaviour))

(cl-ecs:defcomponent animation (width height row-size index key frame time))

(defparameter *player-animation-index* '(:run (8 13)
					 :still (0 0)))

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
		   (level-width level)))
	(decf new-x velocity-x))
      (when (or (minusp new-y)
		(> (+ new-y (if height height 0))
		   (level-height level)))
	(decf new-y velocity-y))

      (setf (coords/x e) new-x)
      (setf (coords/y e) new-y))))

(defun init-animation-sys (game-time)
    (cl-ecs:defsys animation ((animation) (e))
      (let* ((index (getf (animation/index e) (animation/key e)))
	     (current-frame (animation/frame e))
	     (first-frame (first index))
	     (last-frame (second index)))
	(when (>= (animation/time e) 100)
	  (decf (animation/time e) 100)
	  (incf (animation/frame e))
	  (when (< (animation/frame e) first-frame) (setf (animation/frame e) first-frame))
	  (when (> (animation/frame e) last-frame) (setf (animation/frame e) first-frame)))
	(incf (animation/time e))
	(format t "Action ~A Frame ~A (~A to ~A) t ~A ~%"
		(animation/key e)
		(animation/frame e)
		first-frame
		last-frame
		(animation/time e)))))

(defun init-collision-sys (game-time)
  (cl-ecs:defsys collision ((coords collision-behaviour) (e1 e2))
    (let ((delta-time (game-time-delta game-time))
	  (rect1 (make-rect-from-entity e1))
	  (rect2 (make-rect-from-entity e2)))
      (when (rect-collision-p rect1 rect2)
	(cond ((eql (collision-behaviour/behaviour e1) :stop)
	       (when (velocity/x e2)
		 (decf (coords/x e2) (* (velocity/x e2) delta-time))
		 (decf (coords/y e2) (* (velocity/y e2) delta-time)))))
	(cond ((eql (collision-behaviour/behaviour e2) :stop)
	       (when (velocity/x e1)
		 (decf (coords/x e1) (* (velocity/x e1) delta-time))
		 (decf (coords/y e1) (* (velocity/y e1) delta-time)))))))))

(defun init-camera-sys (renderer camera level)
  (cl-ecs:defsys camera ((player coords visibility) (e))
    (let ((rect (make-rect-from-entity e)))
      (center-camera camera level rect)
      (sdl2:render-copy renderer *background-texture* :source-rect (make-sdl-rect-from-camera camera)))))

(defun init-render-sys (renderer camera)
  (cl-ecs:defsys render ((coords visibility) (e))
    (apply #'sdl2:set-render-draw-color (cons renderer (visibility/rgba e)))
    (let* ((x (truncate (- (coords/x e) (camera-x camera))))
	   (y (truncate (- (coords/y e) (camera-y camera))))
	   (w (visibility/w e))
	   (h (visibility/h e))
	   (sdl-rect (sdl2:make-rect x y w h)))

      ;;
      ;; crappy animation routine
      ;;
      (if (animation/frame e)
	  (let* ((a-frame (animation/frame e))
		 (a-width (animation/width e))
		 (a-height (animation/height e))
		 (a-row-size (animation/row-size e))
		 (a-row (values (truncate (/ a-frame a-row-size))))
		 (a-col (values (mod a-frame a-row-size)))
		 (a-x-offset (* a-col a-width))
		 (a-y-offset (* a-row a-height)))
	    (format t "x ~A y ~A row ~A~%" a-x-offset a-y-offset a-row)
	    (sdl2:render-copy renderer *player-texture*
			      :source-rect (sdl2:make-rect
					    a-x-offset
					    a-y-offset
					    (animation/width e)
					    (animation/height e))
			      :dest-rect sdl-rect))

	  (sdl2:render-draw-rect renderer (sdl2:make-rect x y w h))))))
