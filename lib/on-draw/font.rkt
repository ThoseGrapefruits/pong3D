#lang typed/racket

(require pict3d)

(provide (all-defined-out))

; This file contains a "font" made up of primitive 3D shapes. See text.rkt for rendering text.

(define-struct Char-3D
  ([width : Flonum]
   [drawn : Pict3D]))


; START   MIDDLE   END
; ╎       ╎        ╎      S  M  E
; ╎       ╎        ╎      ╎  ╎  ╎
; ╎       ╎        ╎      ╎  ╎  ╎
; ╶══───┬────┬───╤╤╴ ╌╌╌╌╌╎╌╌╎╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌  CAPLINE
; ╎     ╰╮  ╭╯   ││       ╎  ╎  ╎                             ╎
; ╎      ╰╮╭╯    ││       ╎  ╎  ╎                             ╎
; ╎       ║║     ║║       ╎  ╎  ╎                             ╎ HEIGHT-CAP
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║║ ╌╌╌╌╌╌┼╌╌┼╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌  MIDLINE
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║╠═════╮ ╭═════╮ ╥ ╌╌╌ ╥ ╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ MEANLINE
; ╎       ║║     ║║     ║ ║     ║ ║     ║     ╎               ╎
; ╎       ║║     ║║     ║ ╟─═══─╯ ║     ║     ╎ HEIGHT-MEAN   ╎
; ╎      ╭╯╰╮   ╭╯╰╮   ╭╯ ╰╮   ╭  ╰───╮╭╯     ╎               ╎
; ╎╌╌╌╌╌ ┴──┴ ╌ ┴──┴ ╌ ┴ ╌ ╰───┘ ╌╌╌╌ ╰╢ ╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ BASELINE
; ╎                                ╮   ║
; ╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ └───╯ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ DESCLINE


; CONSTANTS — RELATIVE MEASUREMENTS

(: X-HEIGHT : Flonum)
(define X-HEIGHT 0.45)

(: EM-HEIGHT : Flonum)
(define EM-HEIGHT 1.0)

(: EM-HEIGHT-1/2 : Flonum)
(define EM-HEIGHT-1/2 (/ EM-HEIGHT 2.0))

(: EM-HEIGHT-3/8 : Flonum)
(define EM-HEIGHT-3/8 (* 3.0 (/ EM-HEIGHT 8.0)))

(: EM-WIDTH : Flonum)
(define EM-WIDTH 1.0)

(: EM-WIDTH-1/8 : Flonum)
(define EM-WIDTH-1/8 (* 1.0 (/ EM-WIDTH 8.0)))

(: EM-WIDTH-1/4 : Flonum)
(define EM-WIDTH-1/4 (/ EM-WIDTH 4.0))

(: EM-WIDTH-3/8 : Flonum)
(define EM-WIDTH-3/8 (* 3.0 (/ EM-WIDTH 8.0)))

(: EM-WIDTH-1/2 : Flonum)
(define EM-WIDTH-1/2 (/ EM-WIDTH 2.0))

(: EM-WIDTH-5/8 : Flonum)
(define EM-WIDTH-5/8 (* 5.0 (/ EM-WIDTH 8.0)))

(: EM-WIDTH-3/4 : Flonum)
(define EM-WIDTH-3/4 (* 3.0 (/ EM-WIDTH 4.0)))

(: EM-WIDTH-7/8 : Flonum)
(define EM-WIDTH-7/8 (* 7.0 (/ EM-WIDTH 8.0)))

; CONSTANTS — GUIDE LINES
; These are some maybe-helpful constants of different positions relative to the
; *LINE things in the diagram above. We define the intersections between { START, MIDDLE, END } and
; { CAPLINE, MIDLINE, MEANLINE, BASELINE, DESCLINE } for common character widths.

; CAPLINE (top)
(: CAPLINE/START : Pos)
(define CAPLINE/START origin)

(: CAPLINE/CENTER : Pos)
(define CAPLINE/CENTER (pos+ CAPLINE/START +x EM-WIDTH-1/2))

(: CAPLINE/CENTER-1/4 : Pos)
(define CAPLINE/CENTER-1/4 (pos+ CAPLINE/START +x EM-WIDTH-1/8))

(: CAPLINE/CENTER-1/2 : Pos)
(define CAPLINE/CENTER-1/2 (pos+ CAPLINE/START +x EM-WIDTH-1/4))

(: CAPLINE/CENTER-3/4 : Pos)
(define CAPLINE/CENTER-3/4 (pos+ CAPLINE/START +x EM-WIDTH-3/8))

(: CAPLINE/END : Pos)
(define CAPLINE/END (pos+ CAPLINE/START +x EM-WIDTH))

(: CAPLINE/END-1/4 : Pos)
(define CAPLINE/END-1/4 (pos+ CAPLINE/START +x EM-WIDTH-1/4))

(: CAPLINE/END-1/2 : Pos)
(define CAPLINE/END-1/2 (pos+ CAPLINE/START +x EM-WIDTH-1/2))

(: CAPLINE/END-3/4 : Pos)
(define CAPLINE/END-3/4 (pos+ CAPLINE/START +x EM-WIDTH-3/4))

; BASELINE (bottom of most characters)
(: BASELINE/START : Pos)
(define BASELINE/START (pos+ origin +y EM-HEIGHT))

(: BASELINE/CENTER : Pos)
(define BASELINE/CENTER (pos+ BASELINE/START +x EM-WIDTH-1/2))

(: BASELINE/CENTER-1/4 : Pos)
(define BASELINE/CENTER-1/4 (pos+ BASELINE/START +x EM-WIDTH-1/8))

(: BASELINE/CENTER-1/2 : Pos)
(define BASELINE/CENTER-1/2 (pos+ BASELINE/START +x EM-WIDTH-1/4))

(: BASELINE/CENTER-3/4 : Pos)
(define BASELINE/CENTER-3/4 (pos+ BASELINE/START +x EM-WIDTH-3/8))

(: BASELINE/END : Pos)
(define BASELINE/END (pos+ BASELINE/START +x EM-WIDTH))

(: BASELINE/END-1/4 : Pos)
(define BASELINE/END-1/4 (pos+ BASELINE/START +x EM-WIDTH-1/4))

(: BASELINE/END-1/2 : Pos)
(define BASELINE/END-1/2 (pos+ BASELINE/START +x EM-WIDTH-1/2))

(: BASELINE/END-3/4 : Pos)
(define BASELINE/END-3/4 (pos+ BASELINE/START +x EM-WIDTH-3/4))

; MEANLINE (top of most lowercase characters)
(: MEANLINE/START : Pos)
(define MEANLINE/START (pos+ BASELINE/START -y X-HEIGHT))

(: MEANLINE/CENTER : Pos)
(define MEANLINE/CENTER (pos+ MEANLINE/START +x EM-WIDTH-1/2))

(: MEANLINE/CENTER-1/4 : Pos)
(define MEANLINE/CENTER-1/4 (pos+ MEANLINE/START +x EM-WIDTH-1/8))

(: MEANLINE/CENTER-1/2 : Pos)
(define MEANLINE/CENTER-1/2 (pos+ MEANLINE/START +x EM-WIDTH-1/4))

(: MEANLINE/CENTER-3/4 : Pos)
(define MEANLINE/CENTER-3/4 (pos+ MEANLINE/START +x EM-WIDTH-3/8))

(: MEANLINE/END : Pos)
(define MEANLINE/END (pos+ MEANLINE/START +x EM-WIDTH))

(: MEANLINE/END-1/4 : Pos)
(define MEANLINE/END-1/4 (pos+ MEANLINE/START +x EM-WIDTH-1/4))

(: MEANLINE/END-1/2 : Pos)
(define MEANLINE/END-1/2 (pos+ MEANLINE/START +x EM-WIDTH-1/2))

(: MEANLINE/END-3/4 : Pos)
(define MEANLINE/END-3/4 (pos+ MEANLINE/START +x EM-WIDTH-3/4))

; MIDLINE (midway between CAPLINE and BASELINE, useful for positioning shapes that use a center pos)
(: MIDLINE/START : Pos)
(define MIDLINE/START (pos+ origin +y EM-HEIGHT-1/2))

(: MIDLINE/CENTER : Pos)
(define MIDLINE/CENTER (pos+ MIDLINE/START +x EM-WIDTH-1/2))

(: MIDLINE/CENTER-1/4 : Pos)
(define MIDLINE/CENTER-1/4 (pos+ MIDLINE/START +x EM-WIDTH-1/8))

(: MIDLINE/CENTER-1/2 : Pos)
(define MIDLINE/CENTER-1/2 (pos+ MIDLINE/START +x EM-WIDTH-1/4))

(: MIDLINE/CENTER-3/4 : Pos)
(define MIDLINE/CENTER-3/4 (pos+ MIDLINE/START +x EM-WIDTH-3/8))

(: MIDLINE/END : Pos)
(define MIDLINE/END (pos+ MIDLINE/START +x EM-WIDTH))

(: MIDLINE/END-1/4 : Pos)
(define MIDLINE/END-1/4 (pos+ MIDLINE/START +x EM-WIDTH-1/4))

(: MIDLINE/END-1/2 : Pos)
(define MIDLINE/END-1/2 (pos+ MIDLINE/START +x EM-WIDTH-1/2))

(: MIDLINE/END-3/4 : Pos)
(define MIDLINE/END-3/4 (pos+ MIDLINE/START +x EM-WIDTH-3/4))

; DESCLINE (bottom of characters like "y" that go below the BASELINE)
(: DESCLINE/START : Pos)
(define DESCLINE/START (pos+ BASELINE/START +y EM-HEIGHT-3/8))

(: DESCLINE/CENTER : Pos)
(define DESCLINE/CENTER (pos+ DESCLINE/START +x EM-WIDTH-1/2))

(: DESCLINE/CENTER-1/4 : Pos)
(define DESCLINE/CENTER-1/4 (pos+ DESCLINE/START +x EM-WIDTH-1/8))

(: DESCLINE/CENTER-1/2 : Pos)
(define DESCLINE/CENTER-1/2 (pos+ DESCLINE/START +x EM-WIDTH-1/4))

(: DESCLINE/CENTER-3/4 : Pos)
(define DESCLINE/CENTER-3/4 (pos+ DESCLINE/START +x EM-WIDTH-3/8))

(: DESCLINE/END : Pos)
(define DESCLINE/END (pos+ DESCLINE/START +x EM-WIDTH))

(: DESCLINE/END-1/4 : Pos)
(define DESCLINE/END-1/4 (pos+ DESCLINE/START +x EM-WIDTH-1/4))

(: DESCLINE/END-1/2 : Pos)
(define DESCLINE/END-1/2 (pos+ DESCLINE/START +x EM-WIDTH-1/2))

(: DESCLINE/END-3/4 : Pos)
(define DESCLINE/END-3/4 (pos+ DESCLINE/START +x EM-WIDTH-3/4))

; MISC

(define unknown (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))

; WHITESPACE

(define ws-space (Char-3D EM-WIDTH-1/2 empty-pict3d))

; NUMBERS

(define num-0 (Char-3D EM-WIDTH
                         (scale-x (cylinder MIDLINE/CENTER EM-WIDTH-1/2) 0.8)))
(define num-1 (Char-3D EM-WIDTH-3/4
                         (scale-x (cube MIDLINE/CENTER EM-WIDTH-1/2) 0.4)))
(define num-2 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-3 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-4 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-5 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-6 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-7 (Char-3D EM-WIDTH
                         (scale-x (cylinder MIDLINE/CENTER EM-WIDTH-1/2) 0.8)))
(define num-8 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define num-9 (Char-3D EM-WIDTH
                         (cube MIDLINE/CENTER EM-WIDTH-1/2)))

; ALPHA LOWER

(define a (Char-3D EM-WIDTH-3/4
                   (rotate-x (cone (pos+ BASELINE/END +z EM-WIDTH-1/4)
                                   (pos+ MEANLINE/START -z EM-WIDTH-1/4)) -90.0)))
(define b (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define c (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define d (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define e (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define f (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define g (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define h (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define i (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define j (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define k (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define l (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define m (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define n (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define o (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define p (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define q (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define r (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define s (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define t (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define u (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define v (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define w (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define x (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define y (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define z (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))

; ALPHA UPPER

(define A (Char-3D EM-WIDTH (cone MIDLINE/CENTER EM-WIDTH-1/2)))
(define B (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define C (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define D (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define E (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define F (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define G (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define H (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define I (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define J (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define K (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define L (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define M (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define N (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define O (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define P (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define Q (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define R (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define S (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define T (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define U (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define V (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define W (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define X (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define Y (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define Z (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))

; SYMBOLS

(define symbol-? (Char-3D EM-WIDTH (cube MIDLINE/CENTER EM-WIDTH-1/2)))
(define symbol-dot (Char-3D EM-WIDTH (sphere BASELINE/CENTER-1/4 EM-WIDTH-1/4)))
