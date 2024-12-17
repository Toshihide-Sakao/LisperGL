(in-package :cl-user)

(require :sdl2)
(require :cl-opengl)

(defun tri (color x y h w)
  (let* ((x1 x) ; top
               (y1 (+ y (/ h 2))) ; top
               (x2 (- x (/ w 2)))
               (y2 (- y (/ h 2)))
               (x3 (+ x (/ w 2)))
               (y3 (- y (/ h 2)))
               (color color))
    (gl:begin :triangles)
    (gl:color (first color) (second color) (third color))
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:vertex x3 y3)
    (gl:end)))


(defun main-loop (win)
  (format t "Beginning main loop.~%")
  (finish-output)
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (format t "Key ~A pressed.~%" (sdl2:scancode-value keysym))
              (finish-output))

    (:keyup (:keysym keysym)
            (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                  (sdl2:push-event :quit)))

    (:idle ()
           (gl:clear :color-buffer)
           (tri '(1.0 0.0 0.0) 0 0 2 2)
           (gl:flush)
           (sdl2:gl-swap-window win))

    (:quit () t)))

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
      sdl2-ffi:+sdl-major-version+
      sdl2-ffi:+sdl-minor-version+
      sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; basic window/gl setup
        (format t "Setting up window/gl.~%")
        (finish-output)

        (sdl2:gl-make-current win gl-context)
        (gl:viewport 0 0 800 600)
        (gl:matrix-mode :projection)
        (gl:ortho -2 2 -2 2 -2 2)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:clear-color 0.0 0.5 0.5 1.0)
        (gl:clear :color-buffer)

        ;; main loop
        (main-loop win)

        ; (format t "Closing opened game controllers.~%")
        (finish-output)
        ;; close any game controllers that were opened as well as any haptics
          ))))