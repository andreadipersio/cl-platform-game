(in-package #:toor)

(defstruct camera
  (x 0)
  (y 0)
  (width 800)
  (height 600))

(defstruct level
  (width (* 800 2))
  (height 800))

(defstruct rect
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defun rect-collision-p (rect1 rect2)
  (let ((rect1-x-side (+ (rect-x rect1) (rect-width rect1)))
	(rect2-x-side (+ (rect-x rect2) (rect-width rect2)))
	(rect1-y-side (+ (rect-y rect1) (rect-height rect1)))
	(rect2-y-side (+ (rect-y rect2) (rect-height rect2))))
    (and (>= rect1-x-side (rect-x rect2))
	 (<= (rect-x rect1) rect2-x-side)
	 (>= rect1-y-side (rect-y rect2))
	 (<= (rect-y rect1) rect2-y-side))))

(defun make-rect-from-entity (e)
  (make-rect :x (coords/x e)
	     :y (coords/y e)
	     :width (visibility/w e)
	     :height (visibility/h e)))

(defun make-sdl-rect-from-camera (camera)
  (sdl2:make-rect (truncate (camera-x camera))
		  (truncate (camera-y camera))
		  (camera-width camera)
		  (camera-height camera)))

(defun make-sdl-rect-from-entity (e)
  (sdl2:make-rect (truncate (coords/x e))
		  (truncate (coords/y e))
		  (visibility/w e)
		  (visibility/h e)))

(defun center-camera (camera level rect)
  "Center a camera in a level in respect to rect."
  (let* ((level-width (level-width level))
	 (level-height (level-height level))
	 (camera-width (camera-width camera))
	 (camera-height (camera-height camera))
	 (x (- (+ (rect-x rect) (/ (rect-width rect) 2)) (/ camera-width 2)))
	 (y (- (+ (rect-y rect) (/ (rect-height rect) 2)) (/ camera-height 2))))
    (when (< x 0) (setq x 0))
    (when (< y 0) (setq y 0))
    (when (> x (- level-width camera-width)) (setq x (- level-width camera-width)))
    (when (> y (- level-height camera-height)) (setq y (- level-height camera-height)))
    (setf (camera-x camera) x)
    (setf (camera-y camera) y)
    camera))

(defparameter *level-1* (make-level))
