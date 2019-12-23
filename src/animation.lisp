;;;; animation.lisp

(in-package #:toor)

(defstruct spritesheet
  filepath
  sdl-texture
  width
  height
  frames-per-row)

(defstruct animation-state
  frame
  complete
  elapsed)

(defstruct animation-sequence
  first-frame
  last-frame
  time
  flip)

(defun load-spritesheet (renderer sheet)
  (let* ((sdl-surface (sdl2-image:load-image (spritesheet-filepath sheet)))
	 (sdl-texture (sdl2:create-texture-from-surface renderer sdl-surface)))
    (format t "loading spritesheet: ~S~%" (spritesheet-filepath sheet))
    (setf (spritesheet-sdl-texture sheet) sdl-texture)))

(defun preload-spritesheets (renderer sheets)
  (mapc (lambda (sheet-alist) (load-spritesheet renderer (cadr sheet-alist))) sheets))

(defun animation-sdl-rect (sheet frame sequence)
  (let* ((a-width (spritesheet-width sheet))
	 (a-height (spritesheet-height sheet))
	 (a-row-size (spritesheet-frames-per-row sheet))
	 (a-row (values (truncate (/ frame a-row-size))))
	 (a-col (mod frame a-row-size))
	 (a-x-offset (* a-col a-width))
	 (a-y-offset (* a-row a-height))
	 (flip (cond ((not (animation-sequence-flip sequence)) '(0))
		     ((eql (animation-sequence-flip sequence) :x) '(1))
		     ((eql (animation-sequence-flip sequence) :y) '(2))))
	 (sdl-rect (sdl2:make-rect
		    a-x-offset
		    a-y-offset
		    a-width
		    a-height)))
    (values sdl-rect flip)))
