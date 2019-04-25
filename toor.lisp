;;;; toor.lisp

(in-package #:toor)

(defvar *command-thread-name* "Command Thread")
(defvar *command-queue* '())
(defvar *command-queue-lock* (bt:make-lock))

(defun command-pop ()
  (bt:with-lock-held (*command-queue-lock*)
    (pop *command-queue*)))

(defun command-push (cmd)
  (bt:with-lock-held (*command-queue-lock*)
    (setf *command-queue* (cons cmd *command-queue*))))

(defun command-thread ()
  (loop
     (let ((cmd (command-pop)))
       (eval cmd)
       (sdl2:delay 2000))))

(defun create-command-thread ()
  (bt:make-thread 'command-thread :name *command-thread-name*))

(defun test ()
  (cl-ecs:add-entity nil
	      (coords :x 10 :y 20)
	      (visibility :w 100 :h 100 :rgba '(255 255 0 1)))
  (cl-ecs:add-entity nil
	      (coords :x 50 :y 0)
	      (visibility :w 120 :h 90 :rgba '(255 0 0 1)))
  (cl-ecs:add-entity nil
	      (coords :x 50 :y 0)
	      (visibility :w 120 :h 90 :rgba '(255 0 0 1))
	      (velocity :x 100 :y 200))
  (cl-ecs:add-component 1 'velocity '(:velocity/x 10 :velocity/y 20)))

(defun main ()
  (test)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Toor" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(init-render-sys renderer)
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:idle
	   ()
	   (sdl2:set-render-draw-color renderer 0 0 0 255)
	   (sdl2:render-clear renderer)
	   (cl-ecs:do-system 'movement)
	   (cl-ecs:do-system 'render)
	   (sdl2:render-present renderer)
	   (sdl2:delay 250))
	  (:quit () t))))))
