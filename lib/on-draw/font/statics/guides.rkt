#lang typed/racket/base

(require pict3d
         "./measurements.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONSTANTS — GUIDES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(: LINE/CAP/CENTER-7/8 : Pos)
(define LINE/CAP/CENTER-7/8 (pos+ LINE/CAP/START +x WIDTH-EM-7/16))

(: LINE/CAP/END : Pos)
(define LINE/CAP/END (pos+ LINE/CAP/START +x WIDTH-EM))

(: LINE/CAP/END-1/4 : Pos)
(define LINE/CAP/END-1/4 (pos+ LINE/CAP/START +x WIDTH-EM-1/4))

(: LINE/CAP/END-3/8 : Pos)
(define LINE/CAP/END-3/8 (pos+ LINE/CAP/START +x WIDTH-EM-3/8))

(: LINE/CAP/END-1/2 : Pos)
(define LINE/CAP/END-1/2 (pos+ LINE/CAP/START +x WIDTH-EM-1/2))

(: LINE/CAP/END-5/8 : Pos)
(define LINE/CAP/END-5/8 (pos+ LINE/CAP/START +x WIDTH-EM-5/8))

(: LINE/CAP/END-3/4 : Pos)
(define LINE/CAP/END-3/4 (pos+ LINE/CAP/START +x WIDTH-EM-3/4))

(: LINE/CAP/END-7/8 : Pos)
(define LINE/CAP/END-7/8 (pos+ LINE/CAP/START +x WIDTH-EM-7/8))


; LINE/BASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bottom of most characters, or at least bottom of some part of them.

(: LINE/BASE/START : Pos)
(define LINE/BASE/START (pos+ origin +y HEIGHT-CAP))

(: LINE/BASE/CENTER : Pos)
(define LINE/BASE/CENTER (pos+ LINE/BASE/START +x WIDTH-EM-1/2))

(: LINE/BASE/CENTER-1/4 : Pos)
(define LINE/BASE/CENTER-1/4 (pos+ LINE/BASE/START +x WIDTH-EM-1/8))

(: LINE/BASE/CENTER-3/8 : Pos)
(define LINE/BASE/CENTER-3/8 (pos+ LINE/BASE/START +x WIDTH-EM-3/16))

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

(: LINE/BASE/END-3/8 : Pos)
(define LINE/BASE/END-3/8 (pos+ LINE/BASE/START +x WIDTH-EM-3/8))

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

(: LINE/MEAN/CENTER-7/8 : Pos)
(define LINE/MEAN/CENTER-7/8 (pos+ LINE/MEAN/START +x WIDTH-EM-7/16))

(: LINE/MEAN/END : Pos)
(define LINE/MEAN/END (pos+ LINE/MEAN/START +x WIDTH-EM))

(: LINE/MEAN/END-1/4 : Pos)
(define LINE/MEAN/END-1/4 (pos+ LINE/MEAN/START +x WIDTH-EM-1/4))

(: LINE/MEAN/END-3/8 : Pos)
(define LINE/MEAN/END-3/8 (pos+ LINE/MEAN/START +x WIDTH-EM-3/8))

(: LINE/MEAN/END-1/2 : Pos)
(define LINE/MEAN/END-1/2 (pos+ LINE/MEAN/START +x WIDTH-EM-1/2))

(: LINE/MEAN/END-5/8 : Pos)
(define LINE/MEAN/END-5/8 (pos+ LINE/MEAN/START +x WIDTH-EM-5/8))

(: LINE/MEAN/END-3/4 : Pos)
(define LINE/MEAN/END-3/4 (pos+ LINE/MEAN/START +x WIDTH-EM-3/4))

(: LINE/MEAN/END-7/8 : Pos)
(define LINE/MEAN/END-7/8 (pos+ LINE/MEAN/START +x WIDTH-EM-7/8))


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

(: LINE/MID/CENTER-7/8 : Pos)
(define LINE/MID/CENTER-7/8 (pos+ LINE/MID/START +x WIDTH-EM-7/16))

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

(: LINE/MID/END-7/8 : Pos)
(define LINE/MID/END-7/8 (pos+ LINE/MID/START +x WIDTH-EM-7/8))


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
(define LINE/MID-DESC/START (pos+ LINE/BASE/START +y HEIGHT-DESC-1/2))

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

(: LINE/DESC/END-3/8 : Pos)
(define LINE/DESC/END-3/8 (pos+ LINE/DESC/START +x WIDTH-EM-3/8))

(: LINE/DESC/END-1/2 : Pos)
(define LINE/DESC/END-1/2 (pos+ LINE/DESC/START +x WIDTH-EM-1/2))

(: LINE/DESC/END-5/8 : Pos)
(define LINE/DESC/END-5/8 (pos+ LINE/DESC/START +x WIDTH-EM-5/8))

(: LINE/DESC/END-3/4 : Pos)
(define LINE/DESC/END-3/4 (pos+ LINE/DESC/START +x WIDTH-EM-3/4))

; LINE/MID-FULL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Middle of full-height characters (e.g. brackets) which go from LINE/CAP to
; LINE/DESC.

(: LINE/MID-FULL/START : Pos)
(define LINE/MID-FULL/START (pos+ LINE/DESC/START -y HEIGHT-FULL-1/2))

(: LINE/MID-FULL/CENTER : Pos)
(define LINE/MID-FULL/CENTER (pos+ LINE/MID-FULL/START +x WIDTH-EM-1/2))

(: LINE/MID-FULL/CENTER-1/4 : Pos)
(define LINE/MID-FULL/CENTER-1/4 (pos+ LINE/MID-FULL/START +x WIDTH-EM-1/8))

(: LINE/MID-FULL/CENTER-3/8 : Pos)
(define LINE/MID-FULL/CENTER-3/8 (pos+ LINE/MID-FULL/START +x WIDTH-EM-3/16))

(: LINE/MID-FULL/CENTER-1/2 : Pos)
(define LINE/MID-FULL/CENTER-1/2 (pos+ LINE/MID-FULL/START +x WIDTH-EM-1/4))

(: LINE/MID-FULL/CENTER-5/8 : Pos)
(define LINE/MID-FULL/CENTER-5/8 (pos+ LINE/MID-FULL/START +x WIDTH-EM-5/16))

(: LINE/MID-FULL/CENTER-3/4 : Pos)
(define LINE/MID-FULL/CENTER-3/4 (pos+ LINE/MID-FULL/START +x WIDTH-EM-3/8))

(: LINE/MID-FULL/END : Pos)
(define LINE/MID-FULL/END (pos+ LINE/MID-FULL/START +x WIDTH-EM))

(: LINE/MID-FULL/END-1/4 : Pos)
(define LINE/MID-FULL/END-1/4 (pos+ LINE/MID-FULL/START +x WIDTH-EM-1/4))

(: LINE/MID-FULL/END-1/2 : Pos)
(define LINE/MID-FULL/END-1/2 (pos+ LINE/MID-FULL/START +x WIDTH-EM-1/2))

(: LINE/MID-FULL/END-5/8 : Pos)
(define LINE/MID-FULL/END-5/8 (pos+ LINE/MID-FULL/START +x WIDTH-EM-5/8))

(: LINE/MID-FULL/END-3/4 : Pos)
(define LINE/MID-FULL/END-3/4 (pos+ LINE/MID-FULL/START +x WIDTH-EM-3/4))