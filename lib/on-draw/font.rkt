#lang typed/racket

(require pict3d
         "./font/guides.rkt"
         "./font/measurements.rkt")
    
(provide (all-defined-out)
         Char-3D
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

(: arc-3/8 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (arc-3/8 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-3/16)
          WIDTH-EM-3/16
          radius-y
          #:arc arc))

(: arc-1/2 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (arc-1/2 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-1/4)
          WIDTH-EM-1/4
          radius-y
          #:arc arc))

(: arc-3/4 : Pos Flonum [#:arc Arc] -> Pict3D)
(define (arc-3/4 left radius-y #:arc [arc circle-arc])
  (cirque (pos+ left +x WIDTH-EM-3/8)
          WIDTH-EM-3/8
          radius-y
          #:arc arc))

(: arc-y-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-y-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

(: arc-y-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-y-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-Y/START HEIGHT-Y-1/2 #:arc arc))

(: arc-x-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-x-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: arc-x-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-x-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-X/START HEIGHT-X-1/2 #:arc arc))

(: arc-desc-3/8 : [#:arc Arc] -> Pict3D)
(define (arc-desc-3/8 #:arc [arc circle-arc])
  (arc-3/8 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))

(: arc-desc-1/2 : [#:arc Arc] -> Pict3D)
(define (arc-desc-1/2 #:arc [arc circle-arc])
  (arc-1/2 LINE/MID-DESC/START HEIGHT-DESC-1/2 #:arc arc))

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
                               (arc-x-3/8)
                               (rectangle (pos+ LINE/MID-X/START +x WIDTH-EM-7/16)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:b (Char-3D WIDTH-EM-1/2
                        (λ () (combine (arc-x-3/8)
                                       (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                                                  (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:c (Char-3D WIDTH-EM-9/16 (λ () (arc-x-3/8 #:arc (arc 40.0 -40.0)))))
(define char:d (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (arc-x-3/8)
                               (rectangle (pos+ LINE/MID/START +x WIDTH-EM-3/8)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))))))
(define char:e (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (arc-x-3/8 #:arc (arc 60.0 0.0))
                               (rectangle LINE/MID-X/CENTER-3/8
                                          (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z))))))
(define char:f (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; curve
                               (arc-y-3/8 #:arc (arc -180.0 -60.0))
                               ; ascender upper
                               (rectangle (pos+ LINE/MEAN/START
                                                (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z))
                               ; cross
                               (rectangle LINE/MEAN/CENTER-3/8
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender lower
                               (rectangle (pos+ LINE/MID-X/START +x WIDTH-STROKE-1/2)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z))))))
(define char:g (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               ; top accent
                               (rectangle (pos+ LINE/MEAN/CENTER-1/2
                                                (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; top curve
                               (arc-x-3/8)
                               ; connector
                               (arc-1/2 LINE/BASE/CENTER-1/2 HEIGHT-DESC-3/8 #:arc (arc 135.0 225.0))
                               ; bottom curve
                               (arc-1/2 LINE/MID-DESC/CENTER-1/2 HEIGHT-DESC-5/16)))))
(define char:h (Char-3D WIDTH-EM-3/8
                        (λ () (combine
                               ; ascender
                               (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                               ; curve
                               (arc-x-3/8 #:arc (arc 180.0 0))
                               ; end ascender
                               (rectangle (pos+ LINE/BASE/END-3/8
                                                (dir (- 0.0 WIDTH-STROKE-1/2)
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
                               (move-x (arc-desc-3/8 #:arc (arc 0.0 90.0))
                                       (- WIDTH-STROKE WIDTH-EM-1/4))))))
(define char:k (Char-3D WIDTH-EM-5/8 (λ () (combine
                               ; ascender
                               (rectangle LINE/MID/START
                                          (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z))
                               (move-x
                                (combine
                                 ; dialogal upper
                                 (move-y (arc-x-3/8 #:arc (arc 0.0 90.0)) (- HEIGHT-X-1/2))
                                 ; dialogal lower
                                 (move-y (arc-x-3/8 #:arc (arc -90.0 0.0)) HEIGHT-X-1/2))
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
                               (arc-x-3/8 #:arc (arc 180.0 0))
                               ; ascender middle
                               (rectangle (pos+ LINE/BASE/CENTER-3/4
                                                (dir 0.0 (- HEIGHT-X-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; curve right
                               (move-x (arc-x-3/8 #:arc (arc 180.0 0))
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
                               (move-x (arc-x-3/8 #:arc (arc 180.0 0)) (- WIDTH-STROKE))
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
(define char:o (Char-3D WIDTH-EM-1/2 (λ () (arc-x-3/8))))
(define char:p (Char-3D WIDTH-EM-5/8
                        (λ () (combine (arc-x-3/8)
                                       (rectangle (pos+ LINE/BASE/START +x WIDTH-STROKE-1/2)
                                                  (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z))))))
(define char:q (Char-3D WIDTH-EM-5/8 (λ () (cube LINE/MID-X/CENTER-1/2 WIDTH-EM-1/4))))
(define char:r (Char-3D WIDTH-EM-1/2
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
                               (rectangle LINE/MID-Y/CENTER-3/8
                                          (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z))
                               ; cross
                               (rectangle (pos+ LINE/MEAN/CENTER-3/8 +x WIDTH-BASE/NARROW-1/2)
                                          (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z))
                               ; ascender lower
                               (rectangle (pos+ LINE/MEAN/CENTER-3/8 +y HEIGHT-X-1/4)
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               ; curve
                               (move-x (arc-x-3/8 #:arc (arc 60.0 180.0))
                                       (- WIDTH-EM-3/16 WIDTH-STROKE-1/2))))))
(define char:u (Char-3D WIDTH-EM-1/2
                        (λ () (combine
                               (rectangle (pos+ LINE/MEAN/START
                                                (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               (rectangle (pos+ LINE/MEAN/END-3/8
                                                (dir (- 0.0 WIDTH-STROKE-1/2)
                                                     HEIGHT-X-1/4
                                                     0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z))
                               (arc-x-3/8 #:arc (arc 0.0 180.0))))))
(define char:v (Char-3D WIDTH-EM-3/8
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
(define char:S (Char-3D WIDTH-EM-3/4
                        (λ () (combine
                               (arc-3/4 LINE/MID-Y/CENTER-1/2
                                        HEIGHT-Y-1/2
                                        #:arc (arc  90.0 270.0))
                               (arc-3/4 LINE/MID-X/CENTER-1/2
                                        (+ HEIGHT-X-1/2 WIDTH-STROKE)
                                        #:arc (arc -90.0 -270.0))))))
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
