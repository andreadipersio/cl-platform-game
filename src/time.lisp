(in-package #:toor)

(defstruct game-time
  (now (sdl2:get-performance-counter))
  (last 0)
  (delta 0.0d0))

(defun update-game-time (current-game-time)
  (setf (game-time-last current-game-time) (game-time-now current-game-time))
  (setf (game-time-now current-game-time) (sdl2:get-performance-counter))
  (setf (game-time-delta current-game-time) (float (delta-time current-game-time) 0.0d0))
  current-game-time)

(defun delta-time (current-game-time)
  (let ((now (game-time-now current-game-time))
	(last (game-time-last current-game-time))
	(frequency (sdl2:get-performance-frequency)))
    (/ (* (- now last) 1000) frequency)))

(defun fps (current-game-time)
  (/ 1000 (game-time-delta current-game-time)))
