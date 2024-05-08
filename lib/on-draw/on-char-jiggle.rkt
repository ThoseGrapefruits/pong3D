#lang typed/racket/base

(require "./text.rkt"
         "../state/state.rkt"
         pict3d)

; Get an approximately [50, 100] range integer that can be used as a consistent
; factor for the rotation speed of a given character.
(: hash-char : Char Integer -> Integer)
(define (hash-char char index)
  (define charint (modulo (* 677 (+ 13 (char->integer char))) 1259))
  (define indexmod (modulo (* 827 (+ 7 index)) 587))
  (+ 100 (modulo (* 677 charint indexmod) 53)))

(: get-on-char-jiggle : State -> font:On-Char-Handler)
(define (get-on-char-jiggle s)
  (define t (State-t s))
  (define ts (* 0.00001 t))
  (λ (pict char index)
    (define hash-x (hash-char char (- index)))
    (define hash-y (hash-char char index))
    ; purposefully flip the intensity and time factors to give some more
    ; character without having to generate 4 values
    (define rot-x (* hash-y (cos (* hash-x ts)) 0.1))
    (define rot-y (* hash-x (sin (* hash-y ts)) 0.2))
    (rotate-y/center (rotate-x/center pict rot-x) rot-y)))