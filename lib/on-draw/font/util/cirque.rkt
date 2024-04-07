#lang typed/racket/base

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt")

(provide (all-defined-out))

; CIRQUE
; The cirque is a part or whole donut shape.

(: cirque : Pos Flonum Flonum [#:arc Arc] -> Pict3D)
(define (cirque center radius-x radius-y #:arc [arc circle-arc])
  (pipe center
        (dir radius-x
             radius-y
             DEPTH-Z-1/2)
        #:arc arc
        #:bottom-radii (interval (/ (- radius-x WIDTH-STROKE) radius-x) 1.0)))

; width-based cirque constructors (help simplify calls below)

(: cirque-3/8 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (cirque-3/8 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-3/16)
          WIDTH-EM-3/16
          radius-y
          #:arc arc))

(: cirque-1/2 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (cirque-1/2 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-1/4)
          WIDTH-EM-1/4
          radius-y
          #:arc arc))

(: cirque-5/8 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (cirque-5/8 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-5/16)
          WIDTH-EM-5/16
          radius-y
          #:arc arc))

(: cirque-3/4 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (cirque-3/4 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-3/8)
          WIDTH-EM-3/8
          radius-y
          #:arc arc))

; CIRQUE-CAP — capital-letter-height cirques

(: cirque-cap-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-cap-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID/START HEIGHT-CAP-1/2 #:arc arc))

(: cirque-cap-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-cap-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID/START HEIGHT-CAP-1/2 #:arc arc))

(: cirque-cap-5/8 : [#:arc Arc] -> Pict3D)
(define (cirque-cap-5/8 #:arc [arc circle-arc])
  (cirque-5/8 LINE/MID/START HEIGHT-CAP-1/2 #:arc arc))

(: cirque-cap-3/4 : [#:arc Arc] -> Pict3D)
(define (cirque-cap-3/4 #:arc [arc circle-arc])
  (cirque-3/4 LINE/MID/START HEIGHT-CAP-1/2 #:arc arc))

; CIRQUE-Y — cirques taking up the entire y range

(: cirque-y-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-y-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

(: cirque-y-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-y-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

(: cirque-y-5/8 : [#:arc Arc] -> Pict3D)
(define (cirque-y-5/8 #:arc [arc circle-arc])
  (cirque-5/8 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

(: cirque-y-3/4 : [#:arc Arc] -> Pict3D)
(define (cirque-y-3/4 #:arc [arc circle-arc])
  (cirque-3/4 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

; CIRQUE-X — cirques taking up the entire x range

(: cirque-x-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-x-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: cirque-x-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-x-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: cirque-x-5/8 : [#:arc Arc] -> Pict3D)
(define (cirque-x-5/8 #:arc [arc circle-arc])
  (cirque-5/8 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: cirque-x-3/4 : [#:arc Arc] -> Pict3D)
(define (cirque-x-3/4 #:arc [arc circle-arc])
  (cirque-3/4 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

; CIRQUE-X-link — cirques taking up the entire x range, linking up with a cirque-y of the same size

(: cirque-x-link-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-x-link-3/8 #:arc [arc circle-arc])
  (cirque-3/8 (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)
              (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2)
              #:arc arc))

(: cirque-x-link-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-x-link-1/2 #:arc [arc circle-arc])
  (cirque-1/2 (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)
              (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2)
              #:arc arc))

(: cirque-x-link-5/8 : [#:arc Arc] -> Pict3D)
(define (cirque-x-link-5/8 #:arc [arc circle-arc])
  (cirque-5/8 (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)
              (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2)
              #:arc arc))

(: cirque-x-link-3/4 : [#:arc Arc] -> Pict3D)
(define (cirque-x-link-3/4 #:arc [arc circle-arc])
  (cirque-3/4 (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)
              (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2)
              #:arc arc))

; CIRQUE-DESC — cirques taking up the entire desc range

(: cirque-desc-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-desc-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))

(: cirque-desc-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-desc-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))


(: placeholder-x : Flonum -> Pict3D)
(define (placeholder-x half-width)
  (rectangle (pos+ LINE/MID-X/START +x half-width)
             (dir half-width HEIGHT-X-1/2 DEPTH-Z)))

(: placeholder-tall : Flonum -> Pict3D)
(define (placeholder-tall half-width)
  (rectangle (pos+ LINE/MID/START +x half-width)
             (dir half-width HEIGHT-CAP-1/2 DEPTH-Z)))