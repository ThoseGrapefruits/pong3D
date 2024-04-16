#lang typed/racket/base

(require pict3d)

(provide (all-defined-out))

; START   MIDDLE   END
; ╎       ╎        ╎      S  M  E
; ╎       ╎        ╎      ╎  ╎  ╎
; ╎       ╎        ╎      ╎  ╎  ╎
; ╶══───┬────┬───╤╤╴ ╌╌╌╌ ╎  ╎  ╎ ╌╌╌╌╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/CAP
; ╎     ╰╮  ╭╯   ││       ╎  ╎  ╎             ╎ HEIGHT-Y      ╎ HEIGHT-CAP
; ╎      ╰╮╭╯    ││       ╎  ╎  ╎             ╎               ╎
; ╎╌╌╌╌╌╌ ││ ╌╌╌ ││ ╌╌╌╌╌ ╎  ╎  ╎ ╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/MID-Y
; ╎       ║║     ║║       ╎  ╎  ╎             ╎               ╎
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║║ ╌╌╌╌╌ ╎  ╎  ╎ ╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/MID
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║╠═════╮ ╭═════╮ ╥ ╌╌╌ ╥ ╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/MEAN
; ╎       ║║     ║║     ║ ║     ║ ║     ║     ╎ HEIGHT-X      ╎
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║║ ╌╌╌ ║ ╟─═══─╯ ║ ╌╌╌ ║ ╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌ ╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/MID-X
; ╎      ╭╯╰╮   ╭╯╰╮   ╭╯ ╰╮   ╭  ╰───╮╭╯     ╎               ╎
; ╎╌╌╌╌╌ ┴──┴ ╌ ┴──┴ ╌ ┴ ╌ ╰───┘ ╌╌╌╌ ╰╢ ╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/BASE
; ╎╌╌╌╌╌╌ ├┘ ╌╌ ├──┘ ╌╌╌╌╌╌╌╌╌╌╌╌╌ ╮ ╌ ║ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/MID-DESC
; ╎╌╌╌╌╌╌╌│╌╌╌╌╌│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ └───╯ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ LINE/DESC
;         │     │                  ├───┘
;         │                        │
;         │     WIDTH-BASE/NARROW  │
;                                   
;         WIDTH-STROKE             WIDTH-BASE/WIDE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONSTANTS — RELATIVE MEASUREMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How far curved bottoms go past their linear counterparts. For example, on "d",
; how much further does the bottom of the circular-ish part go than the end of
; the straight line on the right. As hooman, we perceive these curves as not
; going as far as the sharply-terminating things next to them so we
; overcompensate and push them down until they feel right. This is meant to be
; blindly used with LINE/MID-X constants to push both the curved-LINE/MEAN up
; and the curved-LINE/BASE down relative to their flat counterparts.

(: CURVE-OVERSCALE-FACTOR : Flonum)
(define CURVE-OVERSCALE-FACTOR 1.05)

; HEIGHT-CAP
; The height between the LINE/BASE and the LINE/CAP — or the bottom of most
; uppercase characters to the top of most uppercase characters. Probably will
; remain 1.0 forever — it's our unit scale.

(: HEIGHT-CAP : Flonum)
(define HEIGHT-CAP 1.0)

(: HEIGHT-CAP-1/2 : Flonum)
(define HEIGHT-CAP-1/2 (/ HEIGHT-CAP 2.0))

(: HEIGHT-CAP-1/4 : Flonum)
(define HEIGHT-CAP-1/4 (/ HEIGHT-CAP 4.0))

(: HEIGHT-CAP-3/8 : Flonum)
(define HEIGHT-CAP-3/8 (* 3.0 (/ HEIGHT-CAP 8.0)))

(: WIDTH-EM : Flonum)
(define WIDTH-EM 1.0)

(: WIDTH-EM-1/32 : Flonum)
(define WIDTH-EM-1/32 (/ WIDTH-EM 32.0))

(: WIDTH-EM-1/16 : Flonum)
(define WIDTH-EM-1/16 (/ WIDTH-EM 16.0))

(: WIDTH-EM-3/32 : Flonum)
(define WIDTH-EM-3/32 (* 3.0 WIDTH-EM-1/32))

(: WIDTH-EM-1/8 : Flonum)
(define WIDTH-EM-1/8 (/ WIDTH-EM 8.0))

(: WIDTH-EM-5/32 : Flonum)
(define WIDTH-EM-5/32 (* 5.0 WIDTH-EM-1/32))

(: WIDTH-EM-3/16 : Flonum)
(define WIDTH-EM-3/16 (* 3.0 WIDTH-EM-1/16))

(: WIDTH-EM-1/4 : Flonum)
(define WIDTH-EM-1/4 (/ WIDTH-EM 4.0))

(: WIDTH-EM-5/16 : Flonum)
(define WIDTH-EM-5/16 (* 5.0 WIDTH-EM-1/16))

(: WIDTH-EM-3/8 : Flonum)
(define WIDTH-EM-3/8 (* 3.0 WIDTH-EM-1/8))

(: WIDTH-EM-7/16 : Flonum)
(define WIDTH-EM-7/16 (* 7.0 WIDTH-EM-1/16))

(: WIDTH-EM-1/2 : Flonum)
(define WIDTH-EM-1/2 (/ WIDTH-EM 2.0))

(: WIDTH-EM-9/16 : Flonum)
(define WIDTH-EM-9/16 (* 9.0 WIDTH-EM-1/16))

(: WIDTH-EM-5/8 : Flonum)
(define WIDTH-EM-5/8 (* 5.0 WIDTH-EM-1/8))

(: WIDTH-EM-11/16 : Flonum)
(define WIDTH-EM-11/16 (* 11.0 WIDTH-EM-1/16))

(: WIDTH-EM-3/4 : Flonum)
(define WIDTH-EM-3/4 (* 3.0 WIDTH-EM-1/4))

(: WIDTH-EM-13/16 : Flonum)
(define WIDTH-EM-13/16 (* 13.0 WIDTH-EM-1/16))

(: WIDTH-EM-7/8 : Flonum)
(define WIDTH-EM-7/8 (* 7.0 WIDTH-EM-1/8))

(: WIDTH-EM-17/16 : Flonum)
(define WIDTH-EM-17/16 (* 17.0 (/ WIDTH-EM 16.0)))

(: WIDTH-EM-9/8 : Flonum)
(define WIDTH-EM-9/8 (* 9.0 (/ WIDTH-EM 8.0)))

(: WIDTH-EM-2 : Flonum)
(define WIDTH-EM-2 (* 2.0 WIDTH-EM))

; WIDTH-STROKE
; The width of the standard stroke

(: WIDTH-STROKE : Flonum)
(define WIDTH-STROKE (/ WIDTH-EM 10.0))

(: WIDTH-STROKE-1/8 : Flonum)
(define WIDTH-STROKE-1/8 (/ WIDTH-STROKE 8.0))

(: WIDTH-STROKE-1/4 : Flonum)
(define WIDTH-STROKE-1/4 (/ WIDTH-STROKE 4.0))

(: WIDTH-STROKE-3/8 : Flonum)
(define WIDTH-STROKE-3/8 (* 3.0 WIDTH-STROKE-1/8))

(: WIDTH-STROKE-1/2 : Flonum)
(define WIDTH-STROKE-1/2 (/ WIDTH-STROKE 2.0))

(: WIDTH-STROKE-3/4 : Flonum)
(define WIDTH-STROKE-3/4 (* 3.0 WIDTH-STROKE-1/4))

(: WIDTH-STROKE-3/2 : Flonum)
(define WIDTH-STROKE-3/2 (* 3.0 WIDTH-STROKE-1/2))

; WIDTH-DIAGONAL-BASE
; The width of the base of any diagonal strokes close to 45º. This is slightly larger than
; WIDTH-STROKE to give the diagonal about the same weight as WIDTH-STROKE.

(: WIDTH-DIAGONAL-BASE : Flonum)
(define WIDTH-DIAGONAL-BASE (* WIDTH-STROKE (sqrt 2.0)))

(: WIDTH-DIAGONAL-BASE-1/2 : Flonum)
(define WIDTH-DIAGONAL-BASE-1/2 (/ WIDTH-DIAGONAL-BASE 2.0))

(: WIDTH-DIAGONAL-BASE-1/4 : Flonum)
(define WIDTH-DIAGONAL-BASE-1/4 (/ WIDTH-DIAGONAL-BASE 4.0))

; The width of the base of any diagonal strokes close to 60º.

(: WIDTH-DIAGONAL-SLIGHT-BASE : Flonum)
(define WIDTH-DIAGONAL-SLIGHT-BASE (* WIDTH-STROKE (sqrt 1.6)))

(: WIDTH-DIAGONAL-SLIGHT-BASE-1/2 : Flonum)
(define WIDTH-DIAGONAL-SLIGHT-BASE-1/2 (/ WIDTH-DIAGONAL-SLIGHT-BASE 2.0))

(: WIDTH-DIAGONAL-SLIGHT-BASE-1/4 : Flonum)
(define WIDTH-DIAGONAL-SLIGHT-BASE-1/4 (/ WIDTH-DIAGONAL-SLIGHT-BASE 4.0))

; WIDTH-BASE
; The width of the base of characters that have extended bases / serif marks

(: WIDTH-BASE/NARROW : Flonum)
(define WIDTH-BASE/NARROW (* WIDTH-STROKE 2.0))

(: WIDTH-BASE/NARROW-1/2 : Flonum)
(define WIDTH-BASE/NARROW-1/2 (/ WIDTH-BASE/NARROW 2.0))

(: WIDTH-BASE/WIDE : Flonum)
(define WIDTH-BASE/WIDE (* WIDTH-STROKE 3.0))

(: WIDTH-BASE/WIDE-1/2 : Flonum)
(define WIDTH-BASE/WIDE-1/2 (/ WIDTH-BASE/WIDE 2.0))

; DEPTH-Z
; Depth of characters on the z-axis

(: DEPTH-Z : Flonum)
(define DEPTH-Z (* WIDTH-STROKE 0.85))

(: DEPTH-Z-1/2 : Flonum)
(define DEPTH-Z-1/2 (/ DEPTH-Z 2.0))

; HEIGHT-X & HEIGHT-Y
; Heights defining the space below and above the LINE/MEAN.

(: HEIGHT-X : Flonum)
(define HEIGHT-X 0.4)

(: HEIGHT-X-1/4 : Flonum)
(define HEIGHT-X-1/4 (/ HEIGHT-X 4.0))

(: HEIGHT-X-3/8 : Flonum)
(define HEIGHT-X-3/8 (* 3.0 (/ HEIGHT-X 8.0)))

(: HEIGHT-X-7/16 : Flonum)
(define HEIGHT-X-7/16 (* 7 (/ HEIGHT-X 16.0)))

(: HEIGHT-X-1/2 : Flonum)
(define HEIGHT-X-1/2 (/ HEIGHT-X 2.0))

(: HEIGHT-Y : Flonum)
(define HEIGHT-Y (- HEIGHT-CAP HEIGHT-X))

(: HEIGHT-Y-1/8 : Flonum)
(define HEIGHT-Y-1/8 (/ HEIGHT-Y 8.0))

(: HEIGHT-Y-1/4 : Flonum)
(define HEIGHT-Y-1/4 (/ HEIGHT-Y 4.0))

(: HEIGHT-Y-3/8 : Flonum)
(define HEIGHT-Y-3/8 (* 3.0 HEIGHT-Y-1/8))

(: HEIGHT-Y-1/2 : Flonum)
(define HEIGHT-Y-1/2 (/ HEIGHT-Y 2.0))

; HEIGHT-DESC
; Height of descender reach below the LINE/BASE

(: HEIGHT-DESC : Flonum)
(define HEIGHT-DESC 0.45)

(: HEIGHT-DESC-1/4 : Flonum)
(define HEIGHT-DESC-1/4 (/ HEIGHT-DESC 4.0))

(: HEIGHT-DESC-5/16 : Flonum)
(define HEIGHT-DESC-5/16 (* 5.0 (/ HEIGHT-DESC 16.0)))

(: HEIGHT-DESC-3/8 : Flonum)
(define HEIGHT-DESC-3/8 (* 3.0 (/ HEIGHT-DESC 8.0)))

(: HEIGHT-DESC-1/2 : Flonum)
(define HEIGHT-DESC-1/2 (/ HEIGHT-DESC 2.0))

; HEIGHT-FULL
; HEIGHT-CAP + HEIGHT-DESC

(: HEIGHT-FULL : Flonum)
(define HEIGHT-FULL (+ HEIGHT-CAP HEIGHT-DESC))

(: HEIGHT-FULL-1/4 : Flonum)
(define HEIGHT-FULL-1/4 (+ HEIGHT-CAP-1/4 HEIGHT-DESC-1/4))

(: HEIGHT-FULL-1/2 : Flonum)
(define HEIGHT-FULL-1/2 (+ HEIGHT-CAP-1/2 HEIGHT-DESC-1/2))
