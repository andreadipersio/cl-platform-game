;;;; animation-helper.lisp

(in-package #:toor)

(defun animation-helper-draw (win renderer sheet sequence)
  (let* ((sdl-texture (animation-sheet-sdl-texture sheet))
	 (texture-w (sdl2:texture-width sdl-texture))
	 (texture-h (sdl2:texture-height sdl-texture)))
    (sdl2:set-window-size win texture-w texture-h)
    (sdl2:render-copy renderer (animation-sheet-sdl-texture sheet))
    (sdl2:set-render-draw-color renderer 255 255 255 100)

    (let ((first-frame (animation-sequence-first-frame sequence))
	  (last-frame (animation-sequence-last-frame sequence)))
      (loop for frame from first-frame to last-frame
	    do (sdl2:render-draw-rect renderer (animation-sdl-rect sheet frame sequence))))))

(defun ring-iterator (sequence)
  (let ((i 0)
	(n (- (length sequence) 1)))
    (lambda ()
      (let ((current (nth i sequence)))
	(if (= i n)
	    (setf i 0)
	    (incf i))
	current))))

(defun animation-helper (sheets sequences)
  (let ((sheets-iterator (ring-iterator sheets))
	(sequences-iterator (ring-iterator sequences)))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Animation Helper" :flags '(:shown))
	(sdl2:with-renderer (renderer win :flags '(:accelerated))
	  (continuable
	    (preload-animation-sheets renderer sheets)
	    (let ((sheet (funcall sheets-iterator))
		  (sequence (funcall sequences-iterator)))

	      (sdl2:with-event-loop (:method :poll)
		(:keyup
		 (:keysym keysym)
		 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		   (sdl2:push-event :quit))
		 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-n)
		   (setf sequence (funcall sequences-iterator))))
		(:quit () t)
		(:idle
		 ()
		 (sdl2:set-window-title win (format nil "~S ~S" (car sheet) (car sequence)))
		 (sdl2:set-render-draw-color renderer 0 0 0 100)
		 (sdl2:render-clear renderer)

		 (animation-helper-draw win renderer (cadr sheet) (cadr sequence))

		 (sdl2:render-present renderer)
		 (sdl2:delay 1000))))))))))
