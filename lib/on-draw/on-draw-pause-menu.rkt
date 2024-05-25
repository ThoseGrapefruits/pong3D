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

(provide on-draw-pause-menu)

(: on-draw-pause-menu : State -> Pict3D)
(define (on-draw-pause-menu s)
  (cond
    [(State-Pause-Menu? s)
     (combine
      (render-menu s))]
    [else empty-pict3d]))

(: render-menu : State-Pause-Menu -> Pict3D)
(define (render-menu s) empty-pict3d)
