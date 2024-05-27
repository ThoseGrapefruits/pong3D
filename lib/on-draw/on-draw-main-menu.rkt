#lang typed/racket/base

(require pict3d
         racket/list
         "./camera.rkt"
         "./on-char-jiggle.rkt"
         "./position-screen-space.rkt"
         "./render-player-score.rkt"
         "./text.rkt"
         "../config.rkt"
         "../state/state.rkt")

(provide on-draw-main-menu)

(: on-draw-main-menu : State -> Pict3D)
(define (on-draw-main-menu s)
  (cond
    [(State-Main-Menu? s)
     (combine
      (render-background s)
      (render-menu s))]
    [else empty-pict3d]))

(: render-background : State-Pause-Menu -> Pict3D)
(define (render-background s)
  (transform
   (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.5 0.5 0.5 0.5)])
     (rectangle origin (dir 1.0 1.0 0.01)))
   (affine-compose
    (position-screen-space-relative s 0.0 0.0 0.6))))

(: render-menu : State-Pause-Menu -> Pict3D)
(define (render-menu s)
  (define menu (State-Pause-Menu-menu s))
  (define root (Menu-root menu))
  (combine
    (for/list : (Listof Pict3D)
      ([menu-item (Menu-Item-children root)])
      ((Menu-Item-draw menu-item) menu-item))))
