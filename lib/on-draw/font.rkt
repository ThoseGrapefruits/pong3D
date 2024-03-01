#lang typed/racket

(require pict3d)

; TODO make explicit
(provide (all-defined-out))

; This file contains a "font" made up of primitive 3D shapes. See text.rkt for rendering text.

(define-struct Char-3D
  ([width : Flonum]
   [draw : (-> Pict3D)]))


; START   MIDDLE   END
; ╎       ╎        ╎      S  M  E
; ╎       ╎        ╎      ╎  ╎  ╎
; ╎       ╎        ╎      ╎  ╎  ╎
; ╶══───┬────┬───╤╤╴ ╌╌╌╌╌╎╌╌╎╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌      CAPLINE
; ╎     ╰╮  ╭╯   ││       ╎  ╎  ╎                             ╎ HEIGHT-CAP
; ╎      ╰╮╭╯    ││       ╎  ╎  ╎                             ╎
; ╎       ║║     ║║       ╎  ╎  ╎                             ╎
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║║ ╌╌╌╌╌╌╎╌╌╎╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌   MIDCAPLINE
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║╠═════╮ ╭═════╮ ╥ ╌╌╌ ╥ ╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌     MEANLINE
; ╎       ║║     ║║     ║ ║     ║ ║     ║     ╎ HEIGHT-X      ╎
; ╎╌╌╌╌╌╌ ║║ ╌╌╌ ║║ ╌╌╌ ║ ╟─═══─╯ ║ ╌╌╌ ║ ╌╌╌╌╎ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌  MIDMEANLINE
; ╎      ╭╯╰╮   ╭╯╰╮   ╭╯ ╰╮   ╭  ╰───╮╭╯     ╎               ╎
; ╎╌╌╌╌╌ ┴──┴ ╌ ┴──┴ ╌ ┴ ╌ ╰───┘ ╌╌╌╌ ╰╢ ╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┴╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌     BASELINE
; ╎                                ╮   ║
; ╎╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ └───╯ ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌     DESCLINE

(define current-emitted (emitted "oldlace" 1.0))

; CONSTANTS — RELATIVE MEASUREMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How far curved bottoms go past their linear counterparts. For example, on "d",
; how much further does the bottom of the circular-ish part go than the end of
; the straight line on the right. As hooman, we perceive these curves as not
; going as far as the sharply-terminating things next to them so we
; overcompensate and push them down until they feel right. This is meant to be
; blindly used with MIDMEANLINE constants to push both the curved-MEANLINE up
; and the curved-BASELINE down relative to their flat counterparts.
(: CURVE-OVERSCALE-FACTOR : Flonum)
(define CURVE-OVERSCALE-FACTOR 1.05)

(: HEIGHT-X : Flonum)
(define HEIGHT-X 0.45)

(: HEIGHT-X-1/2 : Flonum)
(define HEIGHT-X-1/2 (/ HEIGHT-X 2.0))

(: EM-HEIGHT : Flonum)
(define EM-HEIGHT 1.0)

(: EM-HEIGHT-1/2 : Flonum)
(define EM-HEIGHT-1/2 (/ EM-HEIGHT 2.0))

(: EM-HEIGHT-3/8 : Flonum)
(define EM-HEIGHT-3/8 (* 3.0 (/ EM-HEIGHT 8.0)))

(: EM-WIDTH : Flonum)
(define EM-WIDTH 1.0)

(: EM-WIDTH-1/16 : Flonum)
(define EM-WIDTH-1/16 (* 1.0 (/ EM-WIDTH 16.0)))

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

; CONSTANTS — GUIDE LINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These are some maybe-helpful constants of different positions relative to the
; *LINE things in the diagram above. We define the intersections between
; { START, MIDDLE, END } and
; { CAPLINE, MIDCAPLINE, MEANLINE, MIDMEANLINE, BASELINE, DESCLINE }
; for common character widths.

; CAPLINE
; Top of most characters. We might support a RISELINE at some point but not now.
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

; BASELINE
; Bottom of most characters, or at least bottom of some part of them.
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

; MEANLINE
; Top of many lowercase characters, or at least top of some part of them.
(: MEANLINE/START : Pos)
(define MEANLINE/START (pos+ BASELINE/START -y HEIGHT-X))

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

; MIDCAPLINE
; Midway between CAPLINE and BASELINE. Useful for positioning shapes that use a
; center pos.
(: MIDCAPLINE/START : Pos)
(define MIDCAPLINE/START (pos+ origin +y EM-HEIGHT-1/2))

(: MIDCAPLINE/CENTER : Pos)
(define MIDCAPLINE/CENTER (pos+ MIDCAPLINE/START +x EM-WIDTH-1/2))

(: MIDCAPLINE/CENTER-1/4 : Pos)
(define MIDCAPLINE/CENTER-1/4 (pos+ MIDCAPLINE/START +x EM-WIDTH-1/8))

(: MIDCAPLINE/CENTER-1/2 : Pos)
(define MIDCAPLINE/CENTER-1/2 (pos+ MIDCAPLINE/START +x EM-WIDTH-1/4))

(: MIDCAPLINE/CENTER-3/4 : Pos)
(define MIDCAPLINE/CENTER-3/4 (pos+ MIDCAPLINE/START +x EM-WIDTH-3/8))

(: MIDCAPLINE/END : Pos)
(define MIDCAPLINE/END (pos+ MIDCAPLINE/START +x EM-WIDTH))

(: MIDCAPLINE/END-1/4 : Pos)
(define MIDCAPLINE/END-1/4 (pos+ MIDCAPLINE/START +x EM-WIDTH-1/4))

(: MIDCAPLINE/END-1/2 : Pos)
(define MIDCAPLINE/END-1/2 (pos+ MIDCAPLINE/START +x EM-WIDTH-1/2))

(: MIDCAPLINE/END-3/4 : Pos)
(define MIDCAPLINE/END-3/4 (pos+ MIDCAPLINE/START +x EM-WIDTH-3/4))

; MIDMEANLINE
; Midway between MEANLINE and BASELINE. Useful for positioning lowercase shapes that use a center pos
(: MIDMEANLINE/START : Pos)
(define MIDMEANLINE/START (pos+ BASELINE/START -y HEIGHT-X-1/2))

(: MIDMEANLINE/CENTER : Pos)
(define MIDMEANLINE/CENTER (pos+ MIDMEANLINE/START +x EM-WIDTH-1/2))

(: MIDMEANLINE/CENTER-1/4 : Pos)
(define MIDMEANLINE/CENTER-1/4 (pos+ MIDMEANLINE/START +x EM-WIDTH-1/8))

(: MIDMEANLINE/CENTER-1/2 : Pos)
(define MIDMEANLINE/CENTER-1/2 (pos+ MIDMEANLINE/START +x EM-WIDTH-1/4))

(: MIDMEANLINE/CENTER-3/4 : Pos)
(define MIDMEANLINE/CENTER-3/4 (pos+ MIDMEANLINE/START +x EM-WIDTH-3/8))

(: MIDMEANLINE/END : Pos)
(define MIDMEANLINE/END (pos+ MIDMEANLINE/START +x EM-WIDTH))

(: MIDMEANLINE/END-1/4 : Pos)
(define MIDMEANLINE/END-1/4 (pos+ MIDMEANLINE/START +x EM-WIDTH-1/4))

(: MIDMEANLINE/END-1/2 : Pos)
(define MIDMEANLINE/END-1/2 (pos+ MIDMEANLINE/START +x EM-WIDTH-1/2))

(: MIDMEANLINE/END-3/4 : Pos)
(define MIDMEANLINE/END-3/4 (pos+ MIDMEANLINE/START +x EM-WIDTH-3/4))

; DESCLINE
; Bottom of characters (e.g. "y") that go below the BASELINE,
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


; FONT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SHARED SHAPES
(define (lowercase-shape-circle-1/2)
  (let ([CURVE-SCALE (* CURVE-OVERSCALE-FACTOR EM-WIDTH-1/4)])
    (scale-x (pipe MIDMEANLINE/CENTER-1/2 CURVE-SCALE) 0.8)))

; WHITESPACE
(define ws:space (Char-3D EM-WIDTH-1/2 (λ () empty-pict3d)))

; NUMBERS
(define num:0 (Char-3D EM-WIDTH
                         (λ () (scale-x (pipe MIDCAPLINE/CENTER EM-WIDTH-1/2) 0.8))))
(define num:1 (Char-3D EM-WIDTH-3/4
                         (λ () (scale-x (cube MIDCAPLINE/CENTER EM-WIDTH-1/2) 0.4))))
(define num:2 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:3 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:4 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:5 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:6 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:7 (Char-3D EM-WIDTH
                         (λ () (scale-x (cylinder MIDCAPLINE/CENTER EM-WIDTH-1/2) 0.8))))
(define num:8 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define num:9 (Char-3D EM-WIDTH
                         (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))

; ALPHA LOWER
(define char:a (Char-3D EM-WIDTH-5/8 (λ () (rotate-x/center
                                 (cone MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4)
                                 90.0))))
(define char:b (Char-3D EM-WIDTH-5/8
                        (λ () (with-emitted (emitted "oldlace" 1.0)
                          (combine (lowercase-shape-circle-1/2)
                                   (scale-x
                                    (cube (pos+ MEANLINE/START +x EM-WIDTH-1/8) EM-HEIGHT-1/2)
                                    1/8))))))
(define char:c (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:d (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:e (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:f (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:g (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:h (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:i (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:j (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:k (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:l (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:m (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:n (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:o (Char-3D EM-WIDTH-5/8 (λ () (lowercase-shape-circle-1/2))))
(define char:p (Char-3D EM-WIDTH-5/8 (λ () (combine (lowercase-shape-circle-1/2)
                                              (scale-x
                                               (cube (pos+ BASELINE/START +x EM-WIDTH-1/4) EM-WIDTH-1/4)
                                               0.2)))))
(define char:q (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:r (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:s (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:t (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:u (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:v (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:w (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:x (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:y (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))
(define char:z (Char-3D EM-WIDTH-5/8 (λ () (cube MIDMEANLINE/CENTER-1/2 EM-WIDTH-1/4))))

; ALPHA UPPER
(define char:A (Char-3D EM-WIDTH (λ () (cone MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:B (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:C (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:D (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:E (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:F (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:G (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:H (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:I (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:J (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:K (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:L (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:M (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:N (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:O (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:P (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:Q (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:R (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:S (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:T (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:U (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:V (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:W (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:X (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:Y (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define char:Z (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))

; SYMBOLS
(define symbol:? (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
(define symbol:dot (Char-3D EM-WIDTH-1/4 (λ () (sphere BASELINE/CENTER-1/4 EM-WIDTH-1/16))))

; MISC
(define misc:unknown (Char-3D EM-WIDTH (λ () (cube MIDCAPLINE/CENTER EM-WIDTH-1/2))))
