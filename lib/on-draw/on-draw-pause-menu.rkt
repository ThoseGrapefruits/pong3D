#lang typed/racket/base

(require (only-in pict3d Pict3D)
         (only-in pict3d
                  affine-compose
                  combine
                  current-color
                  current-material
                  dir
                  empty-pict3d
                  material
                  origin
                  rectangle
                  rgba
                  transform)
         "./camera.rkt"
         "./menu.rkt"
         "./on-char-jiggle.rkt"
         "./position-screen-space.rkt"
         "./text.rkt"
         "../state/menu.rkt"
         "../state/state.rkt")

(provide on-draw-pause-menu)

(: on-draw-pause-menu : State -> Pict3D)
(define (on-draw-pause-menu s)
  (cond
    [(State-Pause-Menu? s)
     (combine
      (camera s)
      (render-background s)
      (render-menu s (State-Pause-Menu-menu s)))]
    [else empty-pict3d]))

(: render-background : State-Pause-Menu -> Pict3D)
(define (render-background s)
  (transform
   (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.5 0.5 0.5 0.8)])
     (rectangle origin (dir 1.0 1.0 0.01)))
   (affine-compose
    (position-screen-space-relative s 0.0 0.0 1.0))))
