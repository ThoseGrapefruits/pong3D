#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

(: UPPER-ARC-OFFSET : Flonum)
(define UPPER-ARC-OFFSET 15.0)

(define char:A
  (Char-3D #\A
           WIDTH-EM-3/4
           (λ ()
            (define half-width (- WIDTH-EM-5/8 WIDTH-STROKE-1/2))
            (define cross-bottom-height (- HEIGHT-X WIDTH-STROKE))
            (define cross-inset (* cross-bottom-height (/ half-width HEIGHT-CAP)))
            (define cross-radius (- WIDTH-EM-5/16 cross-inset))
            (combine
             ; left ascender
             (quad-thicc (pos+ LINE/CAP/CENTER-5/8 +x WIDTH-STROKE-1/2)
                         (pos+ LINE/CAP/CENTER-5/8 -x WIDTH-STROKE-1/2)
                         (pos+ LINE/BASE/START     +x 0.0)
                         (pos+ LINE/BASE/START     +x WIDTH-STROKE))
             ; right ascender
             (quad-thicc (pos+ LINE/CAP/CENTER-5/8 +x WIDTH-STROKE-1/2)
                         (pos+ LINE/CAP/CENTER-5/8 -x WIDTH-STROKE-1/2)
                         (pos+ LINE/BASE/END-5/8   -x WIDTH-STROKE)
                         (pos+ LINE/BASE/END-5/8   +x 0.0))
             ; cross
             (rectangle (pos+ LINE/MEAN/CENTER-5/8 +y WIDTH-STROKE-1/2)
                        (dir cross-radius WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:B
  (Char-3D #\B
           WIDTH-EM-5/8
           (λ () 
             (define overangle 15.0)
             (combine
              ; ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; loops
              (move-x (combine (cirque-y-3/4 #:arc (arc (- -90.0 overangle) (+ 90.0 overangle)))
                               (cirque-x-link-3/4 #:arc (arc -90.0 (+ 90.0 overangle))))
                      (- WIDTH-STROKE WIDTH-EM-3/8))))))

(define char:C
  (Char-3D #\C
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; top arc
              (cirque-y-1/2 #:arc (arc -180.0 (- UPPER-ARC-OFFSET)))
              ; connector y
              (rectangle (pos+ LINE/MEAN/START
                               (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
              ; connector x
              (rectangle (pos+ LINE/MEAN/START
                               (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; bottom arc
              (cirque-x-1/2 #:arc (arc 15.0 180.0))))))

(define char:D
  (Char-3D #\D
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; left ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              (move-x (combine (cirque-y-3/4 #:arc (arc (- -90.0 UPPER-ARC-OFFSET) 0.0)) ; top arc
                               ; right connector y
                               (rectangle (pos+ LINE/MEAN/END-3/4
                                                (dir (- WIDTH-STROKE-1/2) (- HEIGHT-Y-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
                               ; right connector x
                               (rectangle (pos+ LINE/MEAN/END-3/4
                                                (dir (- WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
                               ; bottom arc
                               (cirque-x-3/4 #:arc (arc 0.0 (+ 90.0 UPPER-ARC-OFFSET))))
                      (- WIDTH-STROKE WIDTH-EM-3/8))))))

(define char:E
  (Char-3D #\E
           WIDTH-EM-9/16
           (λ ()
             (combine
              ; left ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; top cross
              (rectangle (pos+ LINE/CAP/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              (rectangle (pos+ LINE/MEAN/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) 0.0 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              (rectangle (pos+ LINE/BASE/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) (- WIDTH-STROKE-1/2) 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:F
  (Char-3D #\F
           WIDTH-EM-1/2
           (λ ()
             (combine
              ; left ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; top cross
              (rectangle (pos+ LINE/CAP/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              (rectangle (pos+ LINE/MEAN/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) 0.0 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:G
  (Char-3D #\G
           WIDTH-EM
           (λ ()
             (combine
              ; top arc
              (cirque-y-1/2 #:arc (arc -180.0 (- UPPER-ARC-OFFSET)))
              ; left connector y
              (rectangle (pos+ LINE/MEAN/START
                               (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
              ; left connector x
              (rectangle (pos+ LINE/MEAN/START
                               (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; bottom arc
              (cirque-x-1/2 #:arc (arc 0.0 180.0))
              ; right connector x
              (rectangle (pos+ LINE/MEAN/START
                               (dir (- WIDTH-EM-1/2 WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; center tab
              (rectangle (pos+ LINE/MEAN/START
                               (dir (- WIDTH-EM-1/2 WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2) WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:H
  (Char-3D #\H
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; left ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; right ascender
              (rectangle (pos+ LINE/MID/END-1/2 -x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; cross
              (rectangle (pos+ LINE/MEAN/CENTER-1/2 +y WIDTH-STROKE-1/2)
                         (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:I
  (Char-3D #\I
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; top cross
              (rectangle (pos+ LINE/CAP/CENTER-1/2 +y WIDTH-STROKE-1/2)
                         (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              ; ascender
              (rectangle LINE/MID/CENTER-1/2
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; bottom cross
              (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                         (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:J
  (Char-3D #\J
           WIDTH-EM-3/4
           (λ ()
             (combine
              ; top cross
              (rectangle (pos+ LINE/CAP/END-1/2
                               (dir (- 0.0 WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2) WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              ; ascender y
              (rectangle (pos+ LINE/MID-Y/END-1/2
                              (dir (- WIDTH-STROKE-1/2) 0.0 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
              ; ascender x
              (rectangle (pos+ LINE/MEAN/END-1/2
                              (dir (- WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; bottom arc
              (cirque-x-link-1/2 #:arc (arc 0.0 (- 180.0 UPPER-ARC-OFFSET)))))))

(define char:K
  (Char-3D #\K
           WIDTH-EM-3/4
           (λ () 
             (combine
              ; ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; arm
              (quad-thicc (pos+ LINE/CAP/END-3/8
                                (dir WIDTH-STROKE                         0.0 0.0))
                          (pos+ LINE/CAP/END-3/8
                                (dir 0.0 0.0 0.0))
                          (pos+ LINE/MEAN/START
                                (dir WIDTH-STROKE                         0.0 0.0))
                          (pos+ LINE/MEAN/START
                                (dir (+ WIDTH-STROKE WIDTH-STROKE) WIDTH-STROKE-1/4 0.0)))
              ; leg
              (move-x (combine (cirque-x-3/4 #:arc (arc -90.0 0.0))
                               (rectangle (pos+ LINE/BASE/END-3/4
                                                (dir (- WIDTH-STROKE-1/2) (- HEIGHT-X-1/4) 0.0))
                                          (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2)))
                      (- WIDTH-STROKE WIDTH-EM-3/8))))))

(define char:L
  (Char-3D #\L
           WIDTH-EM-1/2
           (λ ()
             (combine
              ; left ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; base
              (rectangle (pos+ LINE/BASE/START
                               (dir (+ WIDTH-STROKE-1/2 WIDTH-BASE/WIDE-1/2) (- WIDTH-STROKE-1/2) 0.0))
                         (dir WIDTH-BASE/WIDE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:M
  (Char-3D #\M
           WIDTH-EM-7/8
           (λ () (combine
                  ; ascender left
                  (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
                  ; left diagonal
                  (quad-thicc (pos+ LINE/MEAN/CENTER-3/4 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/CENTER-3/4 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/CAP/START   +x WIDTH-STROKE)
                              (pos+ LINE/CAP/START   +x 0.0))
                  ; right diagonal
                  (quad-thicc (pos+ LINE/MEAN/CENTER-3/4 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/CENTER-3/4 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/CAP/END-3/4     -x 0.0)
                              (pos+ LINE/CAP/END-3/4     -x WIDTH-STROKE))
                  ; ascender right
                  (rectangle (pos+ LINE/MID/END-3/4 -x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define char:N
  (Char-3D #\N
           WIDTH-EM-5/8
           (λ () (combine
                  ; ascender left
                  (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
                  ; diagonal
                  (quad-thicc (pos+ LINE/BASE/END-1/2 -x WIDTH-DIAGONAL-BASE)
                              (pos+ LINE/BASE/END-1/2 +x 0.0)
                              (pos+ LINE/CAP/START    +x WIDTH-DIAGONAL-BASE)
                              (pos+ LINE/CAP/START    +x 0.0))
                  ; ascender right
                  (rectangle (pos+ LINE/MID/END-1/2 -x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define char:O
  (Char-3D #\O
           WIDTH-EM-5/8
           (λ () 
            (combine 
             ; arc top
             (cirque-y-1/2 #:arc (arc -180.0 0.0))
             ; connector left y
             (rectangle (pos+ LINE/MEAN/START
                              (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
             ; connector left x
             (rectangle (pos+ LINE/MEAN/START
                              (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
             ; arc bottom
             (cirque-x-1/2 #:arc (arc 0.0 180.0))
             ; connector right x
             (rectangle (pos+ LINE/MEAN/END-1/2
                              (dir (- WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
             ; connector right y
             (rectangle (pos+ LINE/MEAN/END-1/2
                              (dir (- WIDTH-STROKE-1/2) (- HEIGHT-Y-1/4) 0.0))
                        (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))))))

(define char:P
  (Char-3D #\P
           WIDTH-EM-5/8
           (λ () 
             (define overangle 15.0)
             (combine
              ; ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; loops
              (move-x (cirque-y-3/4 #:arc (arc (- -90.0 overangle) (+ 90.0 overangle)))
                      (- WIDTH-STROKE WIDTH-EM-3/8))))))

(define char:Q
  (Char-3D #\Q
           WIDTH-EM-3/4
           (λ ()
           (combine
            ; arc top
            (cirque-y-1/2 #:arc (arc -180.0 0.0))
            ; connector left y
            (rectangle (pos+ LINE/MEAN/START
                             (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                       (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
            ; connector left x
            (rectangle (pos+ LINE/MEAN/START
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                       (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
            ; arc bottom
            (cirque-x-1/2 #:arc (arc 0.0 180.0))
            ; connector right x
            (rectangle (pos+ LINE/MEAN/END-1/2
                             (dir (- WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                       (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
            ; connector right y
            (rectangle (pos+ LINE/MEAN/END-1/2
                             (dir (- WIDTH-STROKE-1/2) (- HEIGHT-Y-1/4) 0.0))
                       (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
            ; tail
            (move (cirque-desc-1/2 #:arc (arc 90.0 180.0))
                  (dir (+ WIDTH-EM-1/4 WIDTH-STROKE) (- 0.0 HEIGHT-DESC-1/2 WIDTH-STROKE-1/2) 0.0))))))

(define char:R
  (Char-3D #\R
           WIDTH-EM-5/8
           (λ () 
             (define overangle 15.0)
             (combine
              ; ascender
              (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
              ; loop
              (move-x (cirque-y-3/4 #:arc (arc (- -90.0 overangle) (+ 90.0 overangle)))
                      (- WIDTH-STROKE WIDTH-EM-3/8))
              ; leg
              (quad-thicc (pos+ LINE/MEAN/START
                                (dir (+ WIDTH-STROKE WIDTH-STROKE) (- WIDTH-STROKE-1/4) 0.0))
                          (pos+ LINE/MEAN/START
                                (dir WIDTH-STROKE                         0.0 0.0))
                          (pos+ LINE/BASE/END-3/8
                                (dir (- WIDTH-STROKE WIDTH-DIAGONAL-BASE) 0.0 0.0))
                          (pos+ LINE/BASE/END-3/8
                                (dir WIDTH-STROKE                         0.0 0.0)))))))

(define char:S
  (Char-3D #\S
           WIDTH-EM-3/4
           (λ () (combine
                  ; upper
                  (cirque-y-1/2 #:arc (arc  90.0 330.0))
                  ; lower
                  (cirque-x-link-1/2 #:arc (arc -90.0 -210.0))))))

(define char:T
  (Char-3D #\T
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; top cross
              (rectangle (pos+ LINE/CAP/CENTER-1/2 +y WIDTH-STROKE-1/2)
                         (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              ; ascender
              (rectangle LINE/MID/CENTER-1/2
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define char:U
  (Char-3D #\U
           WIDTH-EM-3/4
           (λ () 
             (combine
              ; ascender left y
              (rectangle (pos+ LINE/MID-Y/START
                               (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
              ; ascender left x
              (rectangle (pos+ LINE/MEAN/START
                               (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; arc bottom
              (cirque-x-1/2 #:arc (arc 0.0 180.0))
              ; ascender right x
              (rectangle (pos+ LINE/MEAN/END-1/2
                               (dir (- WIDTH-STROKE-1/2) HEIGHT-X-1/4 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
              ; ascender right y
              (rectangle (pos+ LINE/MID-Y/END-1/2
                               (dir (- WIDTH-STROKE-1/2) WIDTH-STROKE-1/2 0.0))
                         (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))))))

(define char:V
  (Char-3D #\V
           WIDTH-EM-5/8
           (λ () (combine
                  ; left
                  (quad-thicc (pos+ LINE/BASE/CENTER-1/2 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-1/2 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/CAP/START       +x WIDTH-STROKE)
                              (pos+ LINE/CAP/START       +x 0.0))
                  ; right
                  (quad-thicc (pos+ LINE/BASE/CENTER-1/2 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-1/2 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/CAP/END-1/2     -x 0.0)
                              (pos+ LINE/CAP/END-1/2     -x WIDTH-STROKE))))))

(define char:W
  (Char-3D #\W
           WIDTH-EM-7/8
           (λ ()
             (define center-offset WIDTH-EM-3/8)
             (define bottom-offset WIDTH-BASE/NARROW)
             (define far (+ center-offset center-offset))
             (define farbottom (- far bottom-offset))
             (combine
              ; left
              (quad-thicc (pos+ LINE/CAP/START  +x WIDTH-STROKE)
                          (pos+ LINE/CAP/START  +x 0.0)
                          (pos+ LINE/BASE/START +x (- bottom-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/BASE/START +x (+ bottom-offset WIDTH-STROKE-1/2)))
              ; center left
              (quad-thicc (pos+ LINE/BASE/START +x (- bottom-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/BASE/START +x (+ bottom-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/MEAN/START +x (+ center-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/MEAN/START +x (- center-offset WIDTH-STROKE-1/2)))
              ; center right
              (quad-thicc (pos+ LINE/MEAN/START +x (+ center-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/MEAN/START +x (- center-offset WIDTH-STROKE-1/2))
                          (pos+ LINE/BASE/START +x (- farbottom WIDTH-STROKE-1/2))
                          (pos+ LINE/BASE/START +x (+ farbottom WIDTH-STROKE-1/2)))
              ; right
              (quad-thicc (pos+ LINE/BASE/START +x (- farbottom WIDTH-STROKE-1/2))
                          (pos+ LINE/BASE/START +x (+ farbottom WIDTH-STROKE-1/2))
                          (pos+ LINE/CAP/START  +x far)
                          (pos+ LINE/CAP/START  +x (- far WIDTH-STROKE)))))))

(define char:X
  (Char-3D #\X
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; bottom-left to top-right
              (quad-thicc (pos+ LINE/BASE/START   +x 0.0)
                          (pos+ LINE/BASE/START   +x WIDTH-STROKE)
                          (pos+ LINE/CAP/END-1/2 -x 0.0)
                          (pos+ LINE/CAP/END-1/2 -x WIDTH-STROKE))
              ; bottom-right to top-left
              (quad-thicc (pos+ LINE/BASE/END-1/2 -x WIDTH-STROKE)
                          (pos+ LINE/BASE/END-1/2 -x 0.0)
                          (pos+ LINE/CAP/START   +x WIDTH-STROKE)
                          (pos+ LINE/CAP/START   +x 0.0))))))

(define char:Y
  (Char-3D #\Y
           WIDTH-EM-5/8
           (λ ()
             (combine
              ; left
              (quad-thicc (pos+ LINE/MEAN/CENTER-1/2 -x WIDTH-STROKE-1/2)
                          (pos+ LINE/MEAN/CENTER-1/2 +x WIDTH-STROKE-1/2)
                          (pos+ LINE/CAP/START       +x WIDTH-STROKE)
                          (pos+ LINE/CAP/START       +x 0.0))
              ; right
              (quad-thicc (pos+ LINE/MEAN/CENTER-1/2 -x WIDTH-STROKE-1/2)
                          (pos+ LINE/MEAN/CENTER-1/2 +x WIDTH-STROKE-1/2)
                          (pos+ LINE/CAP/END-1/2     -x 0.0)
                          (pos+ LINE/CAP/END-1/2     -x WIDTH-STROKE))
              (rectangle LINE/MID-X/CENTER-1/2
                         (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))))))

(define char:Z
  (Char-3D #\Z
           WIDTH-EM-5/8
           (λ ()
            (combine
             ; top
             (rectangle (pos+ LINE/CAP/CENTER-1/2 +y WIDTH-STROKE-1/2)
                        (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
             ; diagonal
             (quad-thicc (pos+ LINE/BASE/START   (dir 0.0              (- WIDTH-STROKE) 0.0))
                         (pos+ LINE/BASE/START   (dir WIDTH-STROKE     (- WIDTH-STROKE) 0.0))
                         (pos+ LINE/CAP/END-1/2  (dir 0.0              WIDTH-STROKE     0.0))
                         (pos+ LINE/CAP/END-1/2  (dir (- WIDTH-STROKE) WIDTH-STROKE     0.0)))
             ; top
             (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                        (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))
