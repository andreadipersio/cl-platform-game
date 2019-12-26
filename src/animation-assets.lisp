;;;; animation-assets.lisp

(in-package #:cl-platform-game)

(defparameter *animation-sheets*
  (list (list :player (make-spritesheet
		       :filepath (asdf:system-relative-pathname 'cl-platform-game "assets/player-sheet.png")
		       :width 50
		       :height 37
		       :frames-per-row 7))))

(defparameter *player-animation-sequences*
  (list (list :run (make-animation-sequence :first-frame 8
					    :last-frame 13
					    :time 100))

	(list :still (make-animation-sequence :first-frame 0
					      :last-frame 0
					      :time 0))

	(list :primary-attack (make-animation-sequence :first-frame 42
						       :last-frame 46
						       :time 100))))
