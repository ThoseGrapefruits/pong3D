#lang typed/racket/base

(require pict3d
         "./config.rkt"
         "./on-draw/camera.rkt"
         "./state/state.rkt"
         "./util/pid.rkt"
         "./util/pos.rkt")

(provide on-mouse)

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  (cond [(State-Play? s) (on-mouse-play s n t x y e)]
        [(State-Main-Menu? s) (on-mouse-main-menu s n t x y e)]
        [else s]))

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "move") 
         (define items (State-Main-Menu-items s))
         (define hovered-surface (findf (get-mouse-within-bounds? s x y) items))
         (if hovered-surface
             s
             s)]
        [else s]))

(: on-mouse-play : State-Play Natural Flonum Integer Integer String -> State)
(define (on-mouse-play s n t x y e)
  (cond [(string=? e "move")
         (define player (State-Play-player s))
         (define player-pid (Player-y-pid player))
         (or (Player-y-desired player)
             (pid-reset! player-pid))
         (struct-copy
          State-Play s
          [player (struct-copy
                   Player (State-Play-player s)
                   [y-desired (- (* 1.6 (/ (exact->inexact x) SCREEN-WIDTH)) 0.8)])])]
        [else s]))

; UTIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-mouse-within-bounds? : State Integer Integer -> (-> Menu-Item Boolean))
(define (get-mouse-within-bounds? s x y)
  (λ (item)
    (define bounds (Menu-Item-bounding-rectangle item))
    (define pict-last (State-pict-last s))
    (define angle FOV)
    (define direction CAMERA-DIR) ; TODO modify based on x & y
    (define traced-pos (trace (unbox pict-last) CAMERA-POS direction))
    (and bounds
         traced-pos
         (within-bounds? traced-pos bounds))))