#lang typed/racket

(require pict3d)

(provide (all-defined-out))

; This file contains a "font" made up of primitive 3D shapes. See text.rkt for rendering text.

(define-struct Char-3D
  ([width : Flonum]
   [drawn : Pict3D]))

; CONSTANTS

(: EM-HEIGHT : Flonum)
(define EM-HEIGHT 1.0)

(: EM-WIDTH : Flonum)
(define EM-WIDTH 0.7)

; MISC

(define unknown (Char-3D 0.5 (cube origin 1)))

; WHITESPACE

(define ws-space (Char-3D EM-WIDTH empty-pict3d))

; NUMBERS

(define num-0 (Char-3D 0.5
                         (sphere origin 1)))
(define num-1 (Char-3D 0.5
                         (cube origin 1)))
(define num-2 (Char-3D 0.5
                         (cube origin 1)))
(define num-3 (Char-3D 0.5
                         (cube origin 1)))
(define num-4 (Char-3D 0.5
                         (cube origin 1)))
(define num-5 (Char-3D 0.5
                         (cube origin 1)))
(define num-6 (Char-3D 0.5
                         (cube origin 1)))
(define num-7 (Char-3D 0.5
                         (cube origin 1)))
(define num-8 (Char-3D 0.5
                         (cube origin 1)))
(define num-9 (Char-3D 0.5
                         (cube origin 1)))

; ALPHA LOWER

(define a (Char-3D 0.5 (cube origin 1)))
(define b (Char-3D 0.5 (cube origin 1)))
(define c (Char-3D 0.5 (cube origin 1)))
(define d (Char-3D 0.5 (cube origin 1)))
(define e (Char-3D 0.5 (cube origin 1)))
(define f (Char-3D 0.5 (cube origin 1)))
(define g (Char-3D 0.5 (cube origin 1)))
(define h (Char-3D 0.5 (cube origin 1)))
(define i (Char-3D 0.5 (cube origin 1)))
(define j (Char-3D 0.5 (cube origin 1)))
(define k (Char-3D 0.5 (cube origin 1)))
(define l (Char-3D 0.5 (cube origin 1)))
(define m (Char-3D 0.5 (cube origin 1)))
(define n (Char-3D 0.5 (cube origin 1)))
(define o (Char-3D 0.5 (cube origin 1)))
(define p (Char-3D 0.5 (cube origin 1)))
(define q (Char-3D 0.5 (cube origin 1)))
(define r (Char-3D 0.5 (cube origin 1)))
(define s (Char-3D 0.5 (cube origin 1)))
(define t (Char-3D 0.5 (cube origin 1)))
(define u (Char-3D 0.5 (cube origin 1)))
(define v (Char-3D 0.5 (cube origin 1)))
(define w (Char-3D 0.5 (cube origin 1)))
(define x (Char-3D 0.5 (cube origin 1)))
(define y (Char-3D 0.5 (cube origin 1)))
(define z (Char-3D 0.5 (cube origin 1)))
