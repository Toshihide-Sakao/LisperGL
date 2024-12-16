(in-package :cl-user)

(require :sdl2)
(require :cl-opengl)

(defvar *gl-triangle* nil)
(defvar *vao* nil)
(defvar *gl-buffer-address* nil)
(defvar *shader-program-id* nil)

(defparameter *vertex-shader-source*
"#version 330 core
layout (location = 0) in vec3 position;
void main()
{
gl_Position = vec4(position.x, position.y, position.z, 1.0);
}")

(defparameter *fragment-shader-source*
"#version 330 core
out vec4 color;
void main()
{
color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}")

(defun assert-no-shader-errors (shader-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-shader-iv shader-id :compile-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-shader-info-log shader-id))))
      (cffi:foreign-free success))))

(defun assert-no-program-errors (program-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-program-iv program-id :link-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-program-info-log program-id))))
      (cffi:foreign-free success))))

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (progn
      ;; https://wiki.libsdl.org/SDL_GLattr
      ;; https://wiki.libsdl.org/SDL_GLprofile
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask
                        sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:gl-set-attr :doublebuffer 1)
      #+darwin
      (sdl2:gl-set-attr :context-forward-compatible-flag
                        sdl2-ffi:+sdl-gl-context-forward-compatible-flag+))

    (sdl2:with-window (win :w 800 :h 600
                       :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (format t "Setting up window/gl.~%")
        (progn
          (sdl2:gl-make-current win gl-context)
          ;; https://wiki.libsdl.org/SDL_GL_SetSwapInterval#Remarks
          ;; first try for adaptive vsync
          ;; note: using the ffi directly to bypass the rc assertion in wrapper library
          (gl:viewport 0 0 800 600)

          ;; shaders
          (let ((vertex-shader (gl:create-shader :vertex-shader))
                (fragment-shader (gl:create-shader :fragment-shader)))
            (gl:shader-source vertex-shader *vertex-shader-source*)
            (gl:compile-shader vertex-shader)
            (assert-no-shader-errors vertex-shader)

            (gl:shader-source fragment-shader *fragment-shader-source*)
            (gl:compile-shader fragment-shader)
            (assert-no-shader-errors fragment-shader)

            (setf *shader-program-id* (gl:create-program))
            (gl:attach-shader *shader-program-id* vertex-shader)
            (gl:attach-shader *shader-program-id* fragment-shader)
            (gl:link-program *shader-program-id*)
            (assert-no-program-errors *shader-program-id*)

            (gl:delete-shader vertex-shader)
            (gl:delete-shader fragment-shader))

          (let ((vec #(-0.5 -0.5 0.0
                       0.5 -0.5 0.0
                       0.0  0.5 0.0)))
            (setf *gl-triangle*
                  (loop :with gl-array = (gl:alloc-gl-array :float (length vec))
                     :for i :from 0 :below (length vec) :do
                       (setf (gl:glaref gl-array i)
                             (elt vec i))
                     :finally (return gl-array))))

          (setf *vao* (gl:gen-vertex-array))
          (setf *gl-buffer-address* (gl:gen-buffer))

          (gl:bind-vertex-array *vao*)

          (gl:bind-buffer :array-buffer *gl-buffer-address*)
          (gl:buffer-data :array-buffer
                          :static-draw
                          *gl-triangle*)
          (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) 0)
          (gl:enable-vertex-attrib-array 0)

          (gl:bind-buffer :array-buffer 0)
          (gl:bind-vertex-array 0))

        (gl:clear-color 0.0 1.0 1.0 1.0)

        ;; main loop
        (format t
                "Beginning main loop.~%  shader-program=~A~%  vao=~A~%  doublebuffers=~A~%"
                *shader-program-id*
                *vao*
                (sdl2:gl-get-attr :doublebuffer))
        (sdl2:with-event-loop (:method :poll)
          (:keydown (:keysym keysym)
                    (let ((scancode (sdl2:scancode-value keysym))
                          (sym (sdl2:sym-value keysym))
                          (mod-value (sdl2:mod-value keysym)))
                      (declare (ignore sym mod-value))
                      (cond
                        ((sdl2:scancode= scancode :scancode-q) (sdl2:push-event :quit)))))
          (:idle ()
                 (gl:clear :depth-buffer-bit :color-buffer-bit)
                 (gl:use-program *shader-program-id*)
                 (gl:bind-vertex-array *vao*)
                 (gl:draw-arrays :triangles 0 3)
                 (gl:bind-vertex-array 0)
                 (sdl2:gl-swap-window win)
                 (sleep 0.100))
          (:quit () t))
        (progn
          (gl:use-program 0)
          (gl:bind-vertex-array 0)
          (gl:delete-vertex-arrays (list *vao*))
          (setf *vao* nil)
          (gl:delete-buffers (list *gl-buffer-address*))
          (setf *gl-buffer-address* nil)
          (gl:free-gl-array *gl-triangle*)
          (setf *gl-triangle* nil)
          (gl:delete-program *shader-program-id*)
          (setf *shader-program-id* 0))
        (format t "Done!~%")))))