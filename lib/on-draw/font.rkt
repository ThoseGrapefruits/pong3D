#lang typed/racket

(require pict3d
         "./font/guides.rkt"
         "./font/measurements.rkt")
    
(provide (all-defined-out)
         Char-3D
         Char-3D-char
         Char-3D-draw
         Char-3D-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FONT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; SHARED SHAPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CIRQUE
; The cirque is a part or whole donut shape.

(: cirque : Pos Flonum Flonum [#:arc Arc] -> Pict3D)
(define (cirque center radius-x radius-y #:arc [arc circle-arc])
  (pipe center
        (dir radius-x
             (* CURVE-OVERSCALE-FACTOR radius-y)
             DEPTH-Z)
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

; CIRQUE-X — cirques taking up the entire x range

(: cirque-x-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-x-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: cirque-x-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-x-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

; CIRQUE-DESC — cirques taking up the entire desc range

(: cirque-desc-3/8 : [#:arc Arc] -> Pict3D)
(define (cirque-desc-3/8 #:arc [arc circle-arc])
  (cirque-3/8 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))

(: cirque-desc-1/2 : [#:arc Arc] -> Pict3D)
(define (cirque-desc-1/2 #:arc [arc circle-arc])
  (cirque-1/2 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))


; QUAD-THICC
; A quad with sides and a top that extend back on the z-axis by DEPTH-Z
(: quad-thicc : Pos Pos Pos Pos -> Pict3D)
(define (quad-thicc c1 c2 c3 c4)
  ; We treat the given positions as center positions to make alignment w/ other
  ; shapes easier
  (define c1-front (pos+ c1 -z DEPTH-Z-1/2))
  (define c2-front (pos+ c2 -z DEPTH-Z-1/2))
  (define c3-front (pos+ c3 -z DEPTH-Z-1/2))
  (define c4-front (pos+ c4 -z DEPTH-Z-1/2))
  (define c1-back (pos+ c1 +z DEPTH-Z-1/2))
  (define c2-back (pos+ c2 +z DEPTH-Z-1/2))
  (define c3-back (pos+ c3 +z DEPTH-Z-1/2))
  (define c4-back (pos+ c4 +z DEPTH-Z-1/2))

  (combine
   ; front
   (quad c1-front c2-front c3-front c4-front)
   ; sides
   ; note: order matters here, only one side is visible
   (quad c1-back c2-back c2-front c1-front)
   (quad c2-back c3-back c3-front c2-front)
   (quad c3-back c4-back c4-front c3-front)
   (quad c4-back c1-back c1-front c4-front)))
   

; WHITESPACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ws:space
  (Char-3D #\space WIDTH-EM-1/2 (λ () empty-pict3d)))


; NUMBERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define num:0
  (Char-3D #\0
           WIDTH-EM-7/8
           (λ () (combine
                  ; dot
                  (cylinder (pos+ LINE/MID/START +x WIDTH-EM-5/16)
                            (dir WIDTH-STROKE WIDTH-STROKE DEPTH-Z))
                  ; circle
                  (cirque-cap-5/8)))))
(define num:1
  (Char-3D #\1
           WIDTH-EM-3/4
           (λ () (scale-x (cube LINE/MID/CENTER WIDTH-EM-1/2) 0.4))))
(define num:2
  (Char-3D #\2
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:3
  (Char-3D #\3
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:4
  (Char-3D #\4
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:5
  (Char-3D #\5
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:6
  (Char-3D #\6
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:7
  (Char-3D #\7
           WIDTH-EM
           (λ () (scale-x (cylinder LINE/MID/CENTER WIDTH-EM-1/2) 0.8))))
(define num:8
  (Char-3D #\8
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define num:9
  (Char-3D #\9
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))


; ALPHA LOWER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char:a
  (Char-3D #\a
           WIDTH-EM-1/2
           (λ () (combine
                  (cirque-x-3/8)
                  (rectangle (pos+ LINE/MID-X/START +x (- WIDTH-EM-7/16 WIDTH-STROKE-1/2))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:b
  (Char-3D #\b
           WIDTH-EM-1/2
           (λ () (combine (cirque-x-3/8)
                          (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:c
  (Char-3D #\c
           WIDTH-EM-9/16
           (λ () (cirque-x-3/8 #:arc (arc 40.0 -40.0)))))
(define char:d
  (Char-3D #\d
           WIDTH-EM-1/2
           (λ () (combine
                  (cirque-x-3/8)
                  (rectangle (pos+ LINE/MID/START +x WIDTH-EM-3/8)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:e
  (Char-3D #\e
           WIDTH-EM-1/2
           (λ () (combine
                  (cirque-x-3/8 #:arc (arc 60.0 0.0))
                  (rectangle LINE/MID-X/CENTER-3/8
                             (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z))))))
(define char:f
  (Char-3D #\f
           WIDTH-EM-3/8
           (λ () (combine
                  ; curve
                  (cirque-y-3/8 #:arc (arc -180.0 -60.0))
                  ; ascender upper
                  (rectangle (pos+ LINE/MEAN/START
                                   (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z))
                  ; cross
                  (rectangle (pos+ LINE/MEAN/START +x WIDTH-BASE/NARROW-1/2)
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; ascender lower
                  (rectangle (pos+ LINE/MID-X/START +x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:g
  (Char-3D #\g
           WIDTH-EM-1/2
           (λ () (combine
                  ; top accent
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2
                                   (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; top curve
                  (cirque-x-3/8)
                  ; connector
                  (cirque-1/2 LINE/BASE/START HEIGHT-DESC-3/8 #:arc (arc 135.0 225.0))
                  ; bottom curve
                  (cirque-1/2 LINE/MID-DESC/START HEIGHT-DESC-5/16)))))
(define char:h
  (Char-3D #\h
           WIDTH-EM-1/2
           (λ () (combine
                  ; ascender
                  (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                  ; curve
                  (cirque-x-3/8 #:arc (arc 180.0 0))
                  ; end ascender
                  (rectangle (pos+ LINE/BASE/END-3/8
                                   (dir (- 0.0 WIDTH-STROKE-1/2)
                                        (- HEIGHT-X-1/4)
                                        0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:i
  (Char-3D #\i
           WIDTH-EM-1/2
           (λ () (combine
                  ; dot
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* WIDTH-STROKE 1.5))
                             (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; cap
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2
                                   (dir (- WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2)
                                        WIDTH-STROKE-1/2
                                        0.0))
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; ascender
                  (rectangle LINE/MID-X/CENTER-1/2
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
                  ; base
                  (move-y (rectangle LINE/BASE/CENTER-1/2
                                     (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z))
                          (- WIDTH-STROKE-1/2))))))
(define char:j
  (Char-3D #\j
           WIDTH-EM-3/8
           (λ () (combine
                  ; dot
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* WIDTH-STROKE 1.5))
                             (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; cap
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2
                                   (dir (- WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2)
                                        WIDTH-STROKE-1/2
                                        0.0))
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; ascender
                  (rectangle LINE/MID-X/CENTER-1/2
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
                  (rectangle LINE/BASE/CENTER-1/2
                             (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/2 DEPTH-Z))
                  ; descender
                  (move-x (cirque-desc-3/8 #:arc (arc 0.0 90.0))
                          (- WIDTH-STROKE))))))
(define char:k
  (Char-3D #\k
           WIDTH-EM-5/8
           (λ () (combine
                  ; ascender
                  (rectangle LINE/MID/START
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                  (move-x
                   (combine
                    ; dialogal upper
                    (move-y (cirque-x-3/8 #:arc (arc 0.0 90.0)) (- HEIGHT-X-1/2))
                    ; dialogal lower
                    (move-y (cirque-x-3/8 #:arc (arc -90.0 0.0)) HEIGHT-X-1/2))
                   (- WIDTH-STROKE-1/2 WIDTH-EM-3/16))))))
(define char:l
  (Char-3D #\l WIDTH-EM-9/16
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
(define char:m

  (Char-3D #\m
           WIDTH-EM-7/8
           (λ () (combine
                  ; ascender left
                  (rectangle (pos+ LINE/BASE/START
                                   (dir WIDTH-STROKE-1/2 (- HEIGHT-X-1/4) 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  ; curve left
                  (cirque-x-3/8 #:arc (arc 180.0 0))
                  ; ascender middle
                  (rectangle (pos+ LINE/BASE/CENTER-3/4
                                   (dir (- WIDTH-STROKE-1/2) (- HEIGHT-X-1/4) 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  ; curve right
                  (move-x (cirque-x-3/8 #:arc (arc 180.0 0))
                          WIDTH-EM-1/4)
                  ; ascender end
                  (rectangle (pos+ LINE/BASE/START
                                   (dir (- WIDTH-EM-3/4 WIDTH-STROKE WIDTH-STROKE-1/2)
                                        (- HEIGHT-X-1/4)
                                        0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:n
  (Char-3D #\n
           WIDTH-EM-3/8
           (λ () (combine
                  ; curve
                  (cirque-x-3/8 #:arc (arc 180.0 0))
                  ; ascender start
                  (rectangle (pos+ LINE/BASE/START
                                   (dir WIDTH-STROKE-1/2 (- HEIGHT-X-1/4) 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  ; ascender end
                  (rectangle (pos+ LINE/BASE/END-3/8
                                   (dir (- WIDTH-STROKE-1/2)
                                        (- HEIGHT-X-1/4)
                                        0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))))))
(define char:o
  (Char-3D #\o
           WIDTH-EM-1/2
           (λ () (cirque-x-3/8))))
(define char:p
  (Char-3D #\p
           WIDTH-EM-1/2
           (λ () (combine (cirque-x-3/8)
                          (rectangle (pos+ LINE/BASE/START +x WIDTH-STROKE-1/2)
                                     (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z))))))
(define char:q
  (Char-3D #\q
           WIDTH-EM-1/2
           (λ () (combine
                  (cirque-x-3/8)
                  (rectangle (pos+ LINE/BASE/END-3/8 -x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z))))))
(define char:r
  (Char-3D #\r
           WIDTH-EM-7/16
           (λ ()
             (combine
              ; ascender
              (rectangle (pos+ LINE/MID-X/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))
              ; curve
              (cirque (pos+ LINE/BASE/START +x WIDTH-EM-1/4)
                      WIDTH-EM-1/4
                      HEIGHT-X
                      #:arc (arc -180.0 -90.0))))))
(define char:s
  (Char-3D #\s
           WIDTH-EM-5/16
           (λ ()
             (define arc-radius-y HEIGHT-X-3/8)
             (define top
               (cirque (pos+ LINE/MID-X/START
                             (dir WIDTH-EM-1/8 (- WIDTH-STROKE-1/2 arc-radius-y) 0.0))
                       WIDTH-EM-3/16
                       arc-radius-y
                       #:arc (arc  90.0  270.0)))
             (define bottom
               (cirque (pos+ LINE/MID-X/START
                             (dir WIDTH-EM-1/8 arc-radius-y 0.0))
                       WIDTH-EM-3/16
                       arc-radius-y
                       #:arc (arc -90.0 -270.0)))
             (rotate-z/center (combine top bottom) 45.0))))
(define char:t
  (Char-3D #\t
           WIDTH-EM-5/8
           (λ () (combine
                  ; ascender upper
                  (rectangle LINE/MID-Y/CENTER-3/8
                             (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z))
                  ; cross
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8 +x WIDTH-BASE/NARROW-1/2)
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                  ; ascender lower
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8 +y HEIGHT-X-1/4)
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  ; curve
                  (move-x (cirque-x-3/8 #:arc (arc 60.0 180.0))
                          (- WIDTH-EM-3/16 WIDTH-STROKE-1/2))))))
(define char:u
  (Char-3D #\u
           WIDTH-EM-1/2
           (λ () (combine
                  (rectangle (pos+ LINE/MEAN/START
                                   (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  (rectangle (pos+ LINE/MEAN/END-3/8
                                   (dir (- 0.0 WIDTH-STROKE-1/2)
                                        HEIGHT-X-1/4
                                        0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                  (cirque-x-3/8 #:arc (arc 0.0 180.0))))))
(define char:v
  (Char-3D #\v
           WIDTH-EM-1/2
           (λ () (combine
                  (move-x (cirque LINE/BASE/START
                                  WIDTH-EM-3/16
                                  HEIGHT-X
                                  #:arc (arc  -90.0   0.0))
                          0.0)
                  (move-x (cirque LINE/BASE/START
                                  WIDTH-EM-3/16
                                  HEIGHT-X
                                  #:arc (arc -180.0 -90.0))
                          (- WIDTH-EM-3/8 WIDTH-STROKE-1/2))))))
(define char:w
  (Char-3D #\w
           WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:x
  (Char-3D #\x
           WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:y
  (Char-3D #\y
           WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:z
  (Char-3D #\z
           WIDTH-EM-1/2
           (λ ()
             (define cf-offset (* WIDTH-STROKE (sqrt 2.0)))
             (define cf-top-inner (pos+ LINE/MEAN/END-3/8
                                        (dir (- cf-offset) WIDTH-STROKE 0.0)))
             (define cf-top-outer (pos+ LINE/MEAN/END-3/8
                                        (dir 0.0 WIDTH-STROKE 0.0)))
             (define cf-bottom-inner (pos+ LINE/BASE/START
                                           (dir cf-offset (- WIDTH-STROKE) 0.0)))
             (define cf-bottom-outer (pos+ LINE/BASE/START
                                           (dir 0.0 (- WIDTH-STROKE) 0.0)))
             (combine
              ; top bar
              (rectangle (pos+ LINE/MEAN/CENTER-3/8
                               (dir 0.0 WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z))
              ; cross
              (quad-thicc cf-top-outer
                          cf-top-inner
                          cf-bottom-outer
                          cf-bottom-inner)
              ; bottom bar
              (rectangle (pos+ LINE/BASE/CENTER-3/8
                               (dir 0.0 (- WIDTH-STROKE-1/2) 0.0))
                         (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z))))))


; ALPHA UPPER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char:A
  (Char-3D #\A
           WIDTH-EM
           (λ () (cone LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:B
  (Char-3D #\B
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:C
  (Char-3D #\C
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:D
  (Char-3D #\D
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:E
  (Char-3D #\E
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:F
  (Char-3D #\F
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:G
  (Char-3D #\G
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:H
  (Char-3D #\H
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:I
  (Char-3D #\I
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:J
  (Char-3D #\J
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:K
  (Char-3D #\K
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:L
  (Char-3D #\L
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:M
  (Char-3D #\M
           WIDTH-EM-9/8
           (λ () (cube LINE/MID/CENTER WIDTH-EM))))
(define char:N
  (Char-3D #\N
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:O
  (Char-3D #\O
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:P
  (Char-3D #\P
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Q
  (Char-3D #\Q
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:R
  (Char-3D #\R
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
; TODO this needs (arc-x-5/8) and/or to just use a custom thing so it can add
; WIDTH-STROKE-1/4 to both arc radii.
(define char:S
  (Char-3D #\S
           WIDTH-EM-3/4
           (λ () (combine
                  (cirque-3/4 LINE/MID-Y/CENTER-1/2
                           HEIGHT-Y-1/2
                           #:arc (arc  90.0 270.0))
                  (cirque-3/4 LINE/MID-X/CENTER-1/2
                           (+ HEIGHT-X-1/2 WIDTH-STROKE)
                           #:arc (arc -90.0 -270.0))))))
(define char:T
  (Char-3D #\T
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:U
  (Char-3D #\U
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:V
  (Char-3D #\V
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:W
  (Char-3D #\W
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:X
  (Char-3D #\X
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Y
  (Char-3D #\Y
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define char:Z
  (Char-3D #\Z
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))


; SYMBOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol:?
  (Char-3D #\?
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
(define symbol:dot
  (Char-3D #\.
           WIDTH-EM-1/4
           (λ () (sphere LINE/BASE/CENTER-1/4 WIDTH-EM-1/16))))


; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define misc:unknown
  (Char-3D #\nul
           WIDTH-EM
           (λ () (cube LINE/MID/CENTER WIDTH-EM-1/2))))
