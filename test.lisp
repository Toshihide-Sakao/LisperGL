(require :sdl2)

(sdl2:init)
(sdl2:create-window-and-renderer 640 480)
(sdl2:delay 2000)  ; Wait 2 seconds
(sdl2:quit)
