#lang typed/racket

(require pict3d)

; TODO make explicit. would be cool to have separate modules for each chunk of
; the font but that wasn't working like the examples in racket docs seemed to
; work so I gave up for now.
(provide (all-defined-out))

; This file contains a "font" made up of primitive 3D shapes. See text.rkt for
; rendering strings of text.

(define-struct Char-3D
  ([width : Flonum]
   ; This has to be a function so that we can parameterize color, material,
   ; emitted, etc when drawing.
   [draw : (-> Pict3D)]))


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

(: HEIGHT-CAP-3/8 : Flonum)
(define HEIGHT-CAP-3/8 (* 3.0 (/ HEIGHT-CAP 8.0)))

(: WIDTH-EM : Flonum)
(define WIDTH-EM 1.0)

(: WIDTH-EM-1/16 : Flonum)
(define WIDTH-EM-1/16 (/ WIDTH-EM 16.0))

(: WIDTH-EM-1/8 : Flonum)
(define WIDTH-EM-1/8 (/ WIDTH-EM 8.0))

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

; WIDTH-STROKE
; The width of the standard stroke

(: WIDTH-STROKE : Flonum)
(define WIDTH-STROKE (/ WIDTH-EM 10.0))

(: WIDTH-STROKE-1/2 : Flonum)
(define WIDTH-STROKE-1/2 (/ WIDTH-STROKE 2.0))

(: WIDTH-STROKE-1/4 : Flonum)
(define WIDTH-STROKE-1/4 (/ WIDTH-STROKE 4.0))

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
(define DEPTH-Z (* WIDTH-STROKE 3/4))

; greater-than-EM widths for sizing the boxes in which to place EM-width characters

(: WIDTH-EM-17/16 : Flonum)
(define WIDTH-EM-17/16 (* 17.0 (/ WIDTH-EM 16.0)))

(: WIDTH-EM-9/8 : Flonum)
(define WIDTH-EM-9/8 (* 9.0 (/ WIDTH-EM 8.0)))

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

(: HEIGHT-Y-1/2 : Flonum)
(define HEIGHT-Y-1/2 (/ HEIGHT-Y 2.0))

(: HEIGHT-Y-1/4 : Flonum)
(define HEIGHT-Y-1/4 (/ HEIGHT-Y 4.0))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONSTANTS — GUIDE LINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These are some maybe-helpful constants of different positions relative to the
; *LINE things in the diagram above. We define the intersections between
; { START, MIDDLE, END } and
; { LINE/CAP, LINE/MID, LINE/MEAN, LINE/MID-X, LINE/BASE, LINE/DESC }
; for common character widths.


; LINE/CAP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Top of most characters. We might support a RISELINE at some point but not now.

(: LINE/CAP/START : Pos)
(define LINE/CAP/START origin)

(: LINE/CAP/CENTER : Pos)
(define LINE/CAP/CENTER (pos+ LINE/CAP/START +x WIDTH-EM-1/2))

(: LINE/CAP/CENTER-1/4 : Pos)
(define LINE/CAP/CENTER-1/4 (pos+ LINE/CAP/START +x WIDTH-EM-1/8))

(: LINE/CAP/CENTER-1/2 : Pos)
(define LINE/CAP/CENTER-1/2 (pos+ LINE/CAP/START +x WIDTH-EM-1/4))

(: LINE/CAP/CENTER-5/8 : Pos)
(define LINE/CAP/CENTER-5/8 (pos+ LINE/CAP/START +x WIDTH-EM-5/16))

(: LINE/CAP/CENTER-3/4 : Pos)
(define LINE/CAP/CENTER-3/4 (pos+ LINE/CAP/START +x WIDTH-EM-3/8))

(: LINE/CAP/END : Pos)
(define LINE/CAP/END (pos+ LINE/CAP/START +x WIDTH-EM))

(: LINE/CAP/END-1/4 : Pos)
(define LINE/CAP/END-1/4 (pos+ LINE/CAP/START +x WIDTH-EM-1/4))

(: LINE/CAP/END-1/2 : Pos)
(define LINE/CAP/END-1/2 (pos+ LINE/CAP/START +x WIDTH-EM-1/2))

(: LINE/CAP/END-5/8 : Pos)
(define LINE/CAP/END-5/8 (pos+ LINE/CAP/START +x WIDTH-EM-5/8))

(: LINE/CAP/END-3/4 : Pos)
(define LINE/CAP/END-3/4 (pos+ LINE/CAP/START +x WIDTH-EM-3/4))


; LINE/BASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bottom of most characters, or at least bottom of some part of them.

(: LINE/BASE/START : Pos)
(define LINE/BASE/START (pos+ origin +y HEIGHT-CAP))

(: LINE/BASE/CENTER : Pos)
(define LINE/BASE/CENTER (pos+ LINE/BASE/START +x WIDTH-EM-1/2))

(: LINE/BASE/CENTER-1/4 : Pos)
(define LINE/BASE/CENTER-1/4 (pos+ LINE/BASE/START +x WIDTH-EM-1/8))

(: LINE/BASE/CENTER-1/2 : Pos)
(define LINE/BASE/CENTER-1/2 (pos+ LINE/BASE/START +x WIDTH-EM-1/4))

(: LINE/BASE/CENTER-5/8 : Pos)
(define LINE/BASE/CENTER-5/8 (pos+ LINE/BASE/START +x WIDTH-EM-5/16))

(: LINE/BASE/CENTER-3/4 : Pos)
(define LINE/BASE/CENTER-3/4 (pos+ LINE/BASE/START +x WIDTH-EM-3/8))

(: LINE/BASE/END : Pos)
(define LINE/BASE/END (pos+ LINE/BASE/START +x WIDTH-EM))

(: LINE/BASE/END-1/4 : Pos)
(define LINE/BASE/END-1/4 (pos+ LINE/BASE/START +x WIDTH-EM-1/4))

(: LINE/BASE/END-1/2 : Pos)
(define LINE/BASE/END-1/2 (pos+ LINE/BASE/START +x WIDTH-EM-1/2))

(: LINE/BASE/END-5/8 : Pos)
(define LINE/BASE/END-5/8 (pos+ LINE/BASE/START +x WIDTH-EM-5/8))

(: LINE/BASE/END-3/4 : Pos)
(define LINE/BASE/END-3/4 (pos+ LINE/BASE/START +x WIDTH-EM-3/4))

(: LINE/BASE/END-7/8 : Pos)
(define LINE/BASE/END-7/8 (pos+ LINE/BASE/START +x WIDTH-EM-7/8))


; LINE/MEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Top of many lowercase characters, or at least top of some part of them.

(: LINE/MEAN/START : Pos)
(define LINE/MEAN/START (pos+ LINE/BASE/START -y HEIGHT-X))

(: LINE/MEAN/CENTER : Pos)
(define LINE/MEAN/CENTER (pos+ LINE/MEAN/START +x WIDTH-EM-1/2))

(: LINE/MEAN/CENTER-1/4 : Pos)
(define LINE/MEAN/CENTER-1/4 (pos+ LINE/MEAN/START +x WIDTH-EM-1/8))

(: LINE/MEAN/CENTER-3/8 : Pos)
(define LINE/MEAN/CENTER-3/8 (pos+ LINE/MEAN/START +x WIDTH-EM-3/16))

(: LINE/MEAN/CENTER-1/2 : Pos)
(define LINE/MEAN/CENTER-1/2 (pos+ LINE/MEAN/START +x WIDTH-EM-1/4))

(: LINE/MEAN/CENTER-5/8 : Pos)
(define LINE/MEAN/CENTER-5/8 (pos+ LINE/MEAN/START +x WIDTH-EM-5/16))

(: LINE/MEAN/CENTER-3/4 : Pos)
(define LINE/MEAN/CENTER-3/4 (pos+ LINE/MEAN/START +x WIDTH-EM-3/8))

(: LINE/MEAN/END : Pos)
(define LINE/MEAN/END (pos+ LINE/MEAN/START +x WIDTH-EM))

(: LINE/MEAN/END-1/4 : Pos)
(define LINE/MEAN/END-1/4 (pos+ LINE/MEAN/START +x WIDTH-EM-1/4))

(: LINE/MEAN/END-1/2 : Pos)
(define LINE/MEAN/END-1/2 (pos+ LINE/MEAN/START +x WIDTH-EM-1/2))

(: LINE/MEAN/END-5/8 : Pos)
(define LINE/MEAN/END-5/8 (pos+ LINE/MEAN/START +x WIDTH-EM-5/8))

(: LINE/MEAN/END-3/4 : Pos)
(define LINE/MEAN/END-3/4 (pos+ LINE/MEAN/START +x WIDTH-EM-3/4))


; LINE/MID ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Midway between LINE/CAP and LINE/BASE. Useful for positioning shapes that use
; a center pos.

(: LINE/MID/START : Pos)
(define LINE/MID/START (pos+ origin +y HEIGHT-CAP-1/2))

(: LINE/MID/CENTER : Pos)
(define LINE/MID/CENTER (pos+ LINE/MID/START +x WIDTH-EM-1/2))

(: LINE/MID/CENTER-1/4 : Pos)
(define LINE/MID/CENTER-1/4 (pos+ LINE/MID/START +x WIDTH-EM-1/8))

(: LINE/MID/CENTER-1/2 : Pos)
(define LINE/MID/CENTER-1/2 (pos+ LINE/MID/START +x WIDTH-EM-1/4))

(: LINE/MID/CENTER-5/8 : Pos)
(define LINE/MID/CENTER-5/8 (pos+ LINE/MID/START +x WIDTH-EM-5/16))

(: LINE/MID/CENTER-3/4 : Pos)
(define LINE/MID/CENTER-3/4 (pos+ LINE/MID/START +x WIDTH-EM-3/8))

(: LINE/MID/END : Pos)
(define LINE/MID/END (pos+ LINE/MID/START +x WIDTH-EM))

(: LINE/MID/END-1/4 : Pos)
(define LINE/MID/END-1/4 (pos+ LINE/MID/START +x WIDTH-EM-1/4))

(: LINE/MID/END-1/2 : Pos)
(define LINE/MID/END-1/2 (pos+ LINE/MID/START +x WIDTH-EM-1/2))

(: LINE/MID/END-5/8 : Pos)
(define LINE/MID/END-5/8 (pos+ LINE/MID/START +x WIDTH-EM-5/8))

(: LINE/MID/END-3/4 : Pos)
(define LINE/MID/END-3/4 (pos+ LINE/MID/START +x WIDTH-EM-3/4))


; LINE/MID-X ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Midway between LINE/MEAN and LINE/BASE. Useful for positioning lowercase
; shapes that use a center pos.

(: LINE/MID-X/START : Pos)
(define LINE/MID-X/START (pos+ LINE/BASE/START -y HEIGHT-X-1/2))

(: LINE/MID-X/CENTER : Pos)
(define LINE/MID-X/CENTER (pos+ LINE/MID-X/START +x WIDTH-EM-1/2))

(: LINE/MID-X/CENTER-1/4 : Pos)
(define LINE/MID-X/CENTER-1/4 (pos+ LINE/MID-X/START +x WIDTH-EM-1/8))

(: LINE/MID-X/CENTER-3/8 : Pos)
(define LINE/MID-X/CENTER-3/8 (pos+ LINE/MID-X/START +x WIDTH-EM-3/16))

(: LINE/MID-X/CENTER-1/2 : Pos)
(define LINE/MID-X/CENTER-1/2 (pos+ LINE/MID-X/START +x WIDTH-EM-1/4))

(: LINE/MID-X/CENTER-5/8 : Pos)
(define LINE/MID-X/CENTER-5/8 (pos+ LINE/MID-X/START +x WIDTH-EM-5/16))

(: LINE/MID-X/CENTER-3/4 : Pos)
(define LINE/MID-X/CENTER-3/4 (pos+ LINE/MID-X/START +x WIDTH-EM-3/8))

(: LINE/MID-X/END : Pos)
(define LINE/MID-X/END (pos+ LINE/MID-X/START +x WIDTH-EM))

(: LINE/MID-X/END-1/4 : Pos)
(define LINE/MID-X/END-1/4 (pos+ LINE/MID-X/START +x WIDTH-EM-1/4))

(: LINE/MID-X/END-3/8 : Pos)
(define LINE/MID-X/END-3/8 (pos+ LINE/MID-X/START +x WIDTH-EM-3/8))

(: LINE/MID-X/END-7/16 : Pos)
(define LINE/MID-X/END-7/16 (pos+ LINE/MID-X/START +x WIDTH-EM-7/16))

(: LINE/MID-X/END-1/2 : Pos)
(define LINE/MID-X/END-1/2 (pos+ LINE/MID-X/START +x WIDTH-EM-1/2))

(: LINE/MID-X/END-5/8 : Pos)
(define LINE/MID-X/END-5/8 (pos+ LINE/MID-X/START +x WIDTH-EM-5/8))

(: LINE/MID-X/END-3/4 : Pos)
(define LINE/MID-X/END-3/4 (pos+ LINE/MID-X/START +x WIDTH-EM-3/4))


; LINE/MID-Y ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Midway between LINE/MEAN and LINE/BASE. Useful for positioning lowercase
; shapes that use a center pos.

(: LINE/MID-Y/START : Pos)
(define LINE/MID-Y/START (pos+ LINE/BASE/START -y (+ HEIGHT-X HEIGHT-Y-1/2)))

(: LINE/MID-Y/CENTER : Pos)
(define LINE/MID-Y/CENTER (pos+ LINE/MID-Y/START +x WIDTH-EM-1/2))

(: LINE/MID-Y/CENTER-1/4 : Pos)
(define LINE/MID-Y/CENTER-1/4 (pos+ LINE/MID-Y/START +x WIDTH-EM-1/8))

(: LINE/MID-Y/CENTER-3/8 : Pos)
(define LINE/MID-Y/CENTER-3/8 (pos+ LINE/MID-Y/START +x WIDTH-EM-3/16))

(: LINE/MID-Y/CENTER-1/2 : Pos)
(define LINE/MID-Y/CENTER-1/2 (pos+ LINE/MID-Y/START +x WIDTH-EM-1/4))

(: LINE/MID-Y/CENTER-5/8 : Pos)
(define LINE/MID-Y/CENTER-5/8 (pos+ LINE/MID-Y/START +x WIDTH-EM-5/16))

(: LINE/MID-Y/CENTER-3/4 : Pos)
(define LINE/MID-Y/CENTER-3/4 (pos+ LINE/MID-Y/START +x WIDTH-EM-3/8))

(: LINE/MID-Y/END : Pos)
(define LINE/MID-Y/END (pos+ LINE/MID-Y/START +x WIDTH-EM))

(: LINE/MID-Y/END-1/4 : Pos)
(define LINE/MID-Y/END-1/4 (pos+ LINE/MID-Y/START +x WIDTH-EM-1/4))

(: LINE/MID-Y/END-1/2 : Pos)
(define LINE/MID-Y/END-1/2 (pos+ LINE/MID-Y/START +x WIDTH-EM-1/2))

(: LINE/MID-Y/END-5/8 : Pos)
(define LINE/MID-Y/END-5/8 (pos+ LINE/MID-Y/START +x WIDTH-EM-5/8))

(: LINE/MID-Y/END-3/4 : Pos)
(define LINE/MID-Y/END-3/4 (pos+ LINE/MID-Y/START +x WIDTH-EM-3/4))

; LINE/MID-DESC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Middle of descender area of characters that go below LINE/BASE.

(: LINE/MID-DESC/START : Pos)
(define LINE/MID-DESC/START (pos+ LINE/BASE/START +y (/ HEIGHT-DESC 2.0)))

(: LINE/MID-DESC/CENTER : Pos)
(define LINE/MID-DESC/CENTER (pos+ LINE/MID-DESC/START +x WIDTH-EM-1/2))

(: LINE/MID-DESC/CENTER-1/4 : Pos)
(define LINE/MID-DESC/CENTER-1/4 (pos+ LINE/MID-DESC/START +x WIDTH-EM-1/8))

(: LINE/MID-DESC/CENTER-3/8 : Pos)
(define LINE/MID-DESC/CENTER-3/8 (pos+ LINE/MID-DESC/START +x WIDTH-EM-3/16))

(: LINE/MID-DESC/CENTER-1/2 : Pos)
(define LINE/MID-DESC/CENTER-1/2 (pos+ LINE/MID-DESC/START +x WIDTH-EM-1/4))

(: LINE/MID-DESC/CENTER-5/8 : Pos)
(define LINE/MID-DESC/CENTER-5/8 (pos+ LINE/MID-DESC/START +x WIDTH-EM-5/16))

(: LINE/MID-DESC/CENTER-3/4 : Pos)
(define LINE/MID-DESC/CENTER-3/4 (pos+ LINE/MID-DESC/START +x WIDTH-EM-3/8))

(: LINE/MID-DESC/END : Pos)
(define LINE/MID-DESC/END (pos+ LINE/MID-DESC/START +x WIDTH-EM))

(: LINE/MID-DESC/END-1/4 : Pos)
(define LINE/MID-DESC/END-1/4 (pos+ LINE/MID-DESC/START +x WIDTH-EM-1/4))

(: LINE/MID-DESC/END-1/2 : Pos)
(define LINE/MID-DESC/END-1/2 (pos+ LINE/MID-DESC/START +x WIDTH-EM-1/2))

(: LINE/MID-DESC/END-5/8 : Pos)
(define LINE/MID-DESC/END-5/8 (pos+ LINE/MID-DESC/START +x WIDTH-EM-5/8))

(: LINE/MID-DESC/END-3/4 : Pos)
(define LINE/MID-DESC/END-3/4 (pos+ LINE/MID-DESC/START +x WIDTH-EM-3/4))

; LINE/DESC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bottom of characters (e.g. "y") that go below LINE/BASE.

(: LINE/DESC/START : Pos)
(define LINE/DESC/START (pos+ LINE/BASE/START +y HEIGHT-DESC))

(: LINE/DESC/CENTER : Pos)
(define LINE/DESC/CENTER (pos+ LINE/DESC/START +x WIDTH-EM-1/2))

(: LINE/DESC/CENTER-1/4 : Pos)
(define LINE/DESC/CENTER-1/4 (pos+ LINE/DESC/START +x WIDTH-EM-1/8))

(: LINE/DESC/CENTER-1/2 : Pos)
(define LINE/DESC/CENTER-1/2 (pos+ LINE/DESC/START +x WIDTH-EM-1/4))

(: LINE/DESC/CENTER-5/8 : Pos)
(define LINE/DESC/CENTER-5/8 (pos+ LINE/DESC/START +x WIDTH-EM-5/16))

(: LINE/DESC/CENTER-3/4 : Pos)
(define LINE/DESC/CENTER-3/4 (pos+ LINE/DESC/START +x WIDTH-EM-3/8))

(: LINE/DESC/END : Pos)
(define LINE/DESC/END (pos+ LINE/DESC/START +x WIDTH-EM))

(: LINE/DESC/END-1/4 : Pos)
(define LINE/DESC/END-1/4 (pos+ LINE/DESC/START +x WIDTH-EM-1/4))

(: LINE/DESC/END-1/2 : Pos)
(define LINE/DESC/END-1/2 (pos+ LINE/DESC/START +x WIDTH-EM-1/2))

(: LINE/DESC/END-5/8 : Pos)
(define LINE/DESC/END-5/8 (pos+ LINE/DESC/START +x WIDTH-EM-5/8))

(: LINE/DESC/END-3/4 : Pos)
(define LINE/DESC/END-3/4 (pos+ LINE/DESC/START +x WIDTH-EM-3/4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FONT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; SHARED SHAPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CIRQUE
; The cirque is a part or whole circle shape. Right now, the arc of a given
; width is really the arc that will best fit that width, rather than the arc of
; that width (mainly due to centered positioning). Probably a good idea to
; change that in the future.

(: cirque : Pos Flonum Flonum [#:arc Arc] -> Pict3D)
(define (cirque p radius-x radius-y #:arc [arc circle-arc])
  (pipe p
        (dir radius-x
             (* CURVE-OVERSCALE-FACTOR radius-y)
             DEPTH-Z)
        #:arc arc
        #:bottom-radii (interval (/ (- radius-x WIDTH-STROKE) radius-x) 1.0)))

(: arc-3/8 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (arc-3/8 p radius-y #:arc [arc circle-arc])
  (cirque p WIDTH-EM-1/8 radius-y #:arc arc))

(: arc-1/2 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (arc-1/2 p radius-y #:arc [arc circle-arc])
  (cirque p WIDTH-EM-3/16 radius-y #:arc arc))

(: arc-y-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-y-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-Y/CENTER-3/8 HEIGHT-Y-1/2 #:arc arc))

(: arc-y-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-y-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-Y/CENTER-1/2 HEIGHT-Y-1/2 #:arc arc))

(: arc-x-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-x-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-X/CENTER-3/8 HEIGHT-X-1/2 #:arc arc))

(: arc-x-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-x-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-X/CENTER-1/2 HEIGHT-X-1/2 #:arc arc))

(: arc-desc-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-desc-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-DESC/CENTER-3/8 HEIGHT-DESC-1/2 #:arc arc))

(: arc-desc-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-desc-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-DESC/CENTER-1/2 HEIGHT-DESC-1/2 #:arc arc))

; WHITESPACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ws:space (Char-3D WIDTH-EM-1/2 (λ () empty-pict3d)))


; NUMBERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define num:0 (Char-3D WIDTH-EM
                       (λ () (scale-x (pipe LINE/MID/CENTER WIDTH-EM-1/2) 0.8))))
(define num:1 (Char-3D WIDTH-EM-3/4
                       (λ () (scale-x (cube LINE/MID/CENTER WIDTH-EM-1/2) 0.4))))
(define num:2 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:3 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:4 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:5 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:6 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:7 (Char-3D WIDTH-EM
                       (λ () (scale-x (cylinder LINE/MID/CENTER WIDTH-EM-1/2) 0.8))))
(define num:8 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:9 (Char-3D WIDTH-EM
                       (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))


; ALPHA LOWER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char:a (Char-3D WIDTH-EM-5/8
                        (λ () (combine
                               (arc-x-1/2)
                               (rectangle (pos+ LINE/MID-X/START +x WIDTH-EM-7/16)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:b (Char-3D WIDTH-EM-1/2
                        (λ () (combine (arc-x-1/2)
                                       (rectangle (pos+ LINE/MID/START +x WIDTH-EM-1/16)
                                                  (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:c (Char-3D WIDTH-EM-9/16 (λ () (arc-x-1/2 #:arc (arc 40.0 -40.0)))))
(define char:d (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (arc-x-1/2)
                               (rectangle (pos+ LINE/MID/START +x WIDTH-EM-7/16)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:e (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (arc-x-1/2 #:arc (arc 60.0 0.0))
                               (rectangle LINE/MID-X/CENTER-1/2
                                          (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z))))))
(define char:f (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; curve
                               (move-x (arc-y-1/2 #:arc (arc -180.0 -60.0))
                                       (- WIDTH-EM-1/8 WIDTH-STROKE-1/2))
                               ; ascender upper
                               (rectangle (pos+ LINE/MEAN/CENTER-3/8 -y HEIGHT-Y-1/4)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z))
                               ; cross
                               (rectangle LINE/MEAN/CENTER-3/8
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender lower
                               (rectangle LINE/MID-X/CENTER-3/8
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:g (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               ; top accent
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2
                                                (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; top curve
                               (arc-x-1/2)
                               ; connector
                               (arc-1/2 LINE/BASE/CENTER-1/2 HEIGHT-DESC-3/8 #:arc (arc 135.0 225.0))
                               ; bottom curve
                               (arc-1/2 LINE/MID-DESC/CENTER-1/2 HEIGHT-DESC-5/16)))))
(define char:h (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; ascender
                               (rectangle LINE/MID/START
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                               ; curve
                               (move-x (arc-x-1/2 #:arc (arc 180.0 0)) (- WIDTH-STROKE))
                               ; end ascender
                               (rectangle (pos+ LINE/BASE/END-5/8
                                                (dir (- 0.0 WIDTH-EM-3/16 WIDTH-STROKE-1/2 WIDTH-STROKE)
                                                     (- HEIGHT-X-1/4)
                                                     0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:i (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               ; dot
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* 2.0 WIDTH-STROKE))
                                          (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; cap
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2 +x (- WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2))
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender
                               (rectangle LINE/MID-X/CENTER-1/2
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
                               ; base
                               (move-y (rectangle LINE/BASE/CENTER-1/2
                                                  (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z))
                                       (- WIDTH-STROKE-1/2))))))
(define char:j (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; dot
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* 2.0 WIDTH-STROKE))
                                          (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; cap
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2 -x
                                                (- WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2))
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender
                               (rectangle LINE/MID-X/CENTER-1/2
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
                               (rectangle LINE/BASE/CENTER-1/2
                                          (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/2 DEPTH-Z))
                               ; descender
                               (move-x (arc-desc-1/2 #:arc (arc 0.0 90.0))
                                       (- WIDTH-STROKE WIDTH-EM-1/4))))))
(define char:k (Char-3D WIDTH-EM-5/8 (λ () (combine
                               ; ascender
                               (rectangle LINE/MID/START
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                               (move-x
                                (combine
                                 ; dialogal upper
                                 (move-y (arc-x-1/2 #:arc (arc 0.0 90.0)) (- HEIGHT-X-1/2))
                                 ; dialogal lower
                                 (move-y (arc-x-1/2 #:arc (arc -90.0 0.0)) HEIGHT-X-1/2))
                                (- WIDTH-STROKE-1/2 WIDTH-EM-1/4))))))
(define char:l (Char-3D WIDTH-EM-9/16
                        (λ () (combine
                               ; cap
                               (rectangle (pos+ LINE/CAP/CENTER-1/2 -x
                                                (- WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2))
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender
                               (rectangle LINE/MID/CENTER-1/2
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                               ; base
                               (move-y (rectangle LINE/BASE/CENTER-1/2
                                                  (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z))
                                       (- WIDTH-STROKE-1/2))))))
(define char:m (Char-3D WIDTH-EM-7/8
                        (λ () (combine
                               ; ascender left
                               (rectangle (pos+ LINE/BASE/START
                                                (dir WIDTH-STROKE (- HEIGHT-X-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; curve left
                               (arc-x-1/2 #:arc (arc 180.0 0))
                               ; ascender middle
                               (rectangle (pos+ LINE/BASE/CENTER-3/4
                                                (dir 0.0 (- HEIGHT-X-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; curve right
                               (move-x (arc-x-1/2 #:arc (arc 180.0 0))
                                       WIDTH-EM-1/4)
                               ; ascender end
                               (rectangle (pos+ LINE/BASE/START
                                                (dir (- WIDTH-EM-3/4 WIDTH-STROKE)
                                                     (- HEIGHT-X-1/4)
                                                     0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:n (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; curve
                               (move-x (arc-x-1/2 #:arc (arc 180.0 0)) (- WIDTH-STROKE))
                               ; ascender start
                               (rectangle (pos+ LINE/BASE/START
                                                (dir 0.0 (- HEIGHT-X-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; ascender end
                               (rectangle (pos+ LINE/BASE/END-5/8
                                                (dir (- 0.0 WIDTH-EM-3/16 WIDTH-STROKE-1/2 WIDTH-STROKE)
                                                     (- HEIGHT-X-1/4)
                                                     0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:o (Char-3D WIDTH-EM-1/2 (λ () (arc-x-1/2))))
(define char:p (Char-3D WIDTH-EM-5/8
                        (λ () (combine (arc-x-1/2)
                                       (rectangle (pos+ LINE/BASE/START +x WIDTH-STROKE-1/2)
                                                  (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z))))))
(define char:q (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:r (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               ; ascender
                               (rectangle (pos+ LINE/MID-X/START +x WIDTH-EM-1/16)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
                               ; curve
                               (cirque (pos+ LINE/BASE/CENTER-1/2 -x WIDTH-STROKE-1/2)
                                       WIDTH-EM-3/16
                                       HEIGHT-X
                                       #:arc (arc -180.0 -90.0))))))
                                       
(define char:s (Char-3D WIDTH-EM-1/2
                        (λ ()
                        (define arc-radius-y HEIGHT-X-3/8)
                        (define top
                          (cirque (pos+ LINE/MID-X/CENTER-1/2
                                        -y
                                        (- arc-radius-y WIDTH-STROKE-1/2))
                                  WIDTH-EM-3/16
                                  arc-radius-y
                                  #:arc (arc  90.0  270.0)))
                        (define bottom
                          (cirque (pos+ LINE/MID-X/CENTER-1/2
                                        +y
                                        arc-radius-y)
                                  WIDTH-EM-3/16
                                  arc-radius-y
                                  #:arc (arc -90.0 -270.0)))
                          (rotate-z/center (combine top bottom) 45.0))))
(define char:t (Char-3D WIDTH-EM-5/8
                        (λ () (combine
                               ; ascender upper
                               (rectangle LINE/MID-Y/CENTER-1/2
                                          (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z))
                               ; cross
                               (rectangle LINE/MEAN/CENTER-1/2
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender lower
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2 +y HEIGHT-X-1/4)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; curve
                               (move-x (arc-x-1/2 #:arc (arc 60.0 180.0))
                                       (- WIDTH-EM-3/16 WIDTH-STROKE-1/2))))))
(define char:u (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (rectangle (pos+ LINE/MEAN/START
                                                (dir WIDTH-EM-1/8 HEIGHT-X-1/4 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               (rectangle (pos+ LINE/MEAN/END-5/8
                                                (dir (- 0.0 WIDTH-EM-3/16 WIDTH-STROKE-1/2)
                                                     HEIGHT-X-1/4
                                                     0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               (arc-x-1/2 #:arc (arc 0.0 180.0))))))
(define char:v (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (move-x (cirque LINE/BASE/CENTER-1/2
                                               WIDTH-EM-3/16
                                               HEIGHT-X
                                               #:arc (arc  -90.0   0.0))
                                       (- WIDTH-STROKE-1/2 WIDTH-EM-3/16))
                               (move-x (cirque LINE/BASE/CENTER-1/2
                                               WIDTH-EM-3/16
                                               HEIGHT-X
                                               #:arc (arc -180.0 -90.0))
                                       (- WIDTH-EM-3/16 WIDTH-STROKE-1/2))))))
(define char:w (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:x (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:y (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:z (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))


; ALPHA UPPER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char:A (Char-3D WIDTH-EM
                        (λ () (cone LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:B (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:C (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:D (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:E (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:F (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:G (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:H (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:I (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:J (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:K (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:L (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:M (Char-3D WIDTH-EM-9/8
                        (λ () (cube LINE/MID/CENTER WIDTH-EM))))
(define char:N (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:O (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:P (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Q (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:R (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
; TODO this needs (arc-x-5/8) and/or to just use a custom thing so it can add
; WIDTH-STROKE-1/4 to both arc radii.
(define char:S (Char-3D WIDTH-EM
                        (λ () (combine
                               (arc-y-1/2 #:arc (arc  90.0 270.0))
                               (arc-x-1/2 #:arc (arc -90.0 -270.0))))))
(define char:T (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:U (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:V (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:W (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:X (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Y (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Z (Char-3D WIDTH-EM
                        (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))


; SYMBOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol:? (Char-3D WIDTH-EM
                          (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define symbol:dot (Char-3D WIDTH-EM-1/4
                            (λ () (sphere LINE/BASE/CENTER-1/4 WIDTH-EM-1/16))))


; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define misc:unknown (Char-3D WIDTH-EM
                              (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
