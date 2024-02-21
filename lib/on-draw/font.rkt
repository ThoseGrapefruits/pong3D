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

(: EM-HEIGHT-1/2 : Flonum)
(define EM-HEIGHT-1/2 (/ EM-HEIGHT 2.0))

(: EM-WIDTH : Flonum)
(define EM-WIDTH 1.0)

(: EM-WIDTH-3/4 : Flonum)
(define EM-WIDTH-3/4 (* 3.0 (/ EM-WIDTH 4.0)))

(: EM-WIDTH-1/2 : Flonum)
(define EM-WIDTH-1/2 (/ EM-WIDTH 2.0))

(: EM-WIDTH-1/4 : Flonum)
(define EM-WIDTH-1/4 (/ EM-WIDTH 4.0))

(: POSITION-CENTER : Pos)
(define POSITION-CENTER origin)

(: POSITION-BASELINE : Pos)
(define POSITION-BASELINE (pos+ POSITION-CENTER (dir-scale +y EM-HEIGHT-1/2)))

; MISC

(define unknown (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))

; WHITESPACE

(define ws-space (Char-3D EM-WIDTH-1/2 empty-pict3d))

; NUMBERS

(define num-0 (Char-3D EM-WIDTH
                         (scale-x (cylinder POSITION-CENTER EM-WIDTH-1/2) 0.8)))
(define num-1 (Char-3D EM-WIDTH-3/4
                         (scale-x (cube POSITION-CENTER EM-WIDTH-1/2) 0.4)))
(define num-2 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-3 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-4 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-5 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-6 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-7 (Char-3D EM-WIDTH
                         (scale-x (cylinder POSITION-CENTER EM-WIDTH-1/2) 0.8)))
(define num-8 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))
(define num-9 (Char-3D EM-WIDTH
                         (cube POSITION-CENTER EM-WIDTH-1/2)))

; ALPHA LOWER

(define a (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define b (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define c (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define d (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define e (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define f (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define g (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define h (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define i (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define j (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define k (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define l (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define m (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define n (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define o (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define p (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define q (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define r (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define s (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define t (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define u (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define v (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define w (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define x (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define y (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define z (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))

; ALPHA UPPER

(define A (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define B (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define C (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define D (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define E (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define F (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define G (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define H (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define I (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define J (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define K (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define L (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define M (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define N (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define O (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define P (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define Q (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define R (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define S (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define T (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define U (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define V (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define W (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define X (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define Y (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define Z (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))

; SYMBOLS

(define symbol-? (Char-3D EM-WIDTH (cube POSITION-CENTER EM-WIDTH-1/2)))
(define symbol-dot (Char-3D EM-WIDTH (sphere POSITION-BASELINE EM-WIDTH-1/4)))
