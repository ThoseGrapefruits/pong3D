#lang typed/racket/base

(require
  "./camera.rkt"
  "./position-screen-space.rkt"
  "./text.rkt"
  "../state/state.rkt"
  pict3d)

(: on-draw-game-over : State -> Pict3D)
(define (on-draw-game-over s)
  (cond
    [(State-Game-Over? s) (combine (camera s)
                                   (render-game-over-background s)
                                   (render-game-over-message s)
                                   (render-game-over-score s))]
    [else                 empty-pict3d]))

(: render-game-over-background : State-Game-Over -> Pict3D)
(define (render-game-over-background s)
  (transform (with-emitted (emitted "red" 0.2)
               (rotate-z (move-z (cube origin 0.5) 1.0) 45))
             (position-screen-space-relative s 0.0 0.0 1.0)))

(: render-game-over-message : State-Game-Over -> Pict3D)
(define (render-game-over-message s)
  (with-emitted (emitted "oldlace" 1.5)
    (transform (text "GAME OVER"
                     #:wrap 15.0
                     #:onchar (get-on-char-jiggle s))
               (affine-compose
                (position-screen-space-relative s -0.8 -0.85 0.9)
                (scale 0.06)))))

(: render-game-over-score : State-Game-Over -> Pict3D)
(define (render-game-over-score s)
  (define score (Player-score (State-Play-player (State-Game-Over-end-state s))))
  (define score-text (string-pad (number->string score) (length SCORE-SECTIONS) #\0))
  (combine
   (transform (render-player-score score)
              (position-screen-space-relative s 0.0 0.0 0.9))
   (with-emitted (emitted "oldlace" 1.5)
     (transform (text score-text
                      #:wrap 15.0
                      #:onchar (get-on-char-jiggle s)
                      #:ondraw (get-on-draw-game-score s))
                (affine-compose
                 (position-screen-space-relative s 0.0 -0.5 0.9)
                 (scale 0.06))))))
