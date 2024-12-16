(in-package :cl-user)

(require :sdl2)
(require :cl-opengl)

(defun main-loop (win)
  (format t "Beginning main loop.~%")
  (finish-output)
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
           (gl:clear :color-buffer)
           (gl:begin :triangles)
           (gl:color 1.0 0.0 0.0)
           (gl:vertex 0.0 1.0)
           (gl:vertex -1.0 -1.0)
           (gl:vertex 1.0 -1.0)
           (gl:end)
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
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          ;; main loop
          (main-loop win)

          (format t "Closing opened game controllers.~%")
          (finish-output)
          ;; close any game controllers that were opened as well as any haptics
          ))))