#lang typed/racket/base

(require pict3d)

(provide (struct-out State))

(struct State
   ; Delta time from the last frame.
  ([dt               : Flonum]

   ; The most recent screenspace position of the mouse.
   [mouse-pos-last   : (Pairof Integer Integer)]

   ; The number of elapsed ticks
   [n                : Natural]

   ; The last rendered picture. Used for raytracing.
   [pict-last        : (Boxof Pict3D)]

   ; Set of all currently-pressed keys (mouse + keyboard).
   [pressed          : (Setof String)]

   ; Total elapsed time.
   [t                : Flonum]

   ; The most recent mouse trace. Will be unset if the mouse stops hovering
   ; something raytraceable.
   [trace-mouse      : (U #f Surface-Data)]

   ; The last successful mouse trace. Will be held until a new, non-#f value for
   ; trace-mouse is set.
   [trace-mouse/last : (U #f Surface-Data)]

   ; The location of the current mousedown, if any.
   [trace-mouse-down : (U #f Surface-Data)]

   ; Dimensions of the render window, in pixels.
   [window-dims      : (Pairof Index Index)])
  #:transparent)