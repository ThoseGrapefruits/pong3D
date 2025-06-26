#lang typed/racket/base

(require "./text.rkt"
         "../state/state.rkt"
         (only-in pict3d move-z rotate-x/center rotate-y/center)
         (only-in racket/math pi))

(provide get-on-char)

(define QUARTER-PI (/ pi 4))
(define EIGHTH-PI  (/ pi 8))

; Get an approximately [50, 150] range integer that can be used as a consistent
; factor for the rotation speed of a given character.
(: hash-char : Char Integer -> Integer)
(define (hash-char char index)
  (define charint (modulo (* 677 (+ 13 (char->integer char))) 1259))
  (define indexmod (modulo (* 827 (+ 7 index)) 587))
  (+ 100 (modulo (* 677 charint indexmod) 53)))

(define-type Motion (Union 'jiggle 'wave))

(: get-on-char : State Motion -> font:On-Char-Handler)
(define (get-on-char s motion)
  (cond [(eq? motion 'jiggle) (get-on-char-jiggle s)]
        [(eq? motion 'wave) (get-on-char-wave s)]))

(: get-on-char-jiggle : State -> font:On-Char-Handler)
(define (get-on-char-jiggle s)
  (define t (unbox (State-t s)))
  (define ts (* 0.00001 t))
  (λ (pict char index)
    (define hash-x (hash-char char (- index)))
    (define hash-y (hash-char char index))
    ; purposefully flip the intensity and time factors to give some more
    ; character without having to generate 4 values
    (define rot-x (* hash-y (cos (+ hash-x (* hash-x ts))) 0.06)) ; pitch (up-down)
    (define rot-y (* hash-x (sin (+ hash-y (* hash-y ts))) 0.12)) ; yaw   (left-right)
    (rotate-y/center (rotate-x/center pict rot-x) rot-y)))

(: get-on-char-wave : State -> font:On-Char-Handler)
(define (get-on-char-wave s)
  (define t (unbox (State-t s)))
  (define ts (* 0.000007 t))
  (λ (pict char index)
    (define hash-x (/ (+ 400 (modulo (* index 6) 203)) 4.0))
    (define rot-x (* hash-x (cos (+ 0.0        (* hash-x ts))) 0.01))   ; pitch (up-down)
    (define rot-y (* hash-x (cos (+ 0.0        (* hash-x ts))) 0.12))   ; yaw   (left-right)
    (define mov-z (* hash-x (sin (+ 0.0        (* hash-x ts))) 0.0007)) ; rise
    (rotate-y/center (rotate-x/center (move-z pict mov-z) rot-x) rot-y)))
