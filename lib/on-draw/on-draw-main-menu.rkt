#lang typed/racket/base

(require pict3d
         "./camera.rkt"
         "./menu.rkt"
         "./position-screen-space.rkt"
         "../state/state.rkt")

(provide on-draw-main-menu)

(: on-draw-main-menu : State -> Pict3D)
(define (on-draw-main-menu s)
  (cond
    [(State-Main-Menu? s)
     (combine
      (camera s)
      (render-background s)
      (render-menu s (State-Main-Menu-menu s)))]
    [else empty-pict3d]))

(: render-background : State-Main-Menu -> Pict3D)
(define (render-background s)
  (transform
   (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.5 0.5 0.5 1.0)])
     (rectangle origin (dir 1.0 1.0 0.01)))
   (affine-compose
    (position-screen-space-relative s 0.0 0.0 1.0))))
