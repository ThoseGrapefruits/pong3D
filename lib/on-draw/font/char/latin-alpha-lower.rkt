#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

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
                  (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                             (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z))))))
(define char:j
  (Char-3D #\j
           WIDTH-EM-3/8
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
                  (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                             (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z))))))
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
           WIDTH-EM-3/8
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
                  ; left
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                              (pos+ LINE/MEAN/START      +x 0.0))
                  ; right
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/END-3/8    -x 0.0)
                              (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))))))
(define char:w
  (Char-3D #\w
           WIDTH-EM-3/4
           (λ ()
            (define v (combine
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                              (pos+ LINE/MEAN/START      +x 0.0))
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/END-3/8    -x 0.0)
                              (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))))
            (combine
              ; left
              v
              ; right
              (move-x v (- WIDTH-EM-3/8 WIDTH-STROKE))))))
(define char:x
  (Char-3D #\x
           WIDTH-EM-5/8
           (λ () (combine
                  ; bottom-left to top-right
                  (quad-thicc (pos+ LINE/BASE/START   +x 0.0)
                              (pos+ LINE/BASE/START   +x WIDTH-STROKE)
                              (pos+ LINE/MEAN/END-3/8 -x 0.0)
                              (pos+ LINE/MEAN/END-3/8 -x WIDTH-STROKE))
                  ; bottom-right to top-left
                  (quad-thicc (pos+ LINE/BASE/END-3/8 -x WIDTH-STROKE)
                              (pos+ LINE/BASE/END-3/8 -x 0.0)
                              (pos+ LINE/MEAN/START +x WIDTH-STROKE)
                              (pos+ LINE/MEAN/START +x 0.0))))))
(define char:y
  (Char-3D #\y
           WIDTH-EM-5/8
           (λ () (combine
                  ; left
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                              (pos+ LINE/MEAN/START      +x 0.0))
                  ; right
                  (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/END-3/8    -x 0.0)
                              (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))
                  ; descender
                  (move (cirque-desc-3/8 #:arc (arc 0.0 90.0))
                        (dir (- WIDTH-STROKE-1/2 WIDTH-EM-3/16) (- HEIGHT-DESC-1/2) 0.0))))))
(define char:z
  (Char-3D #\z
           WIDTH-EM-1/2
           (λ ()
             (define cf-top-inner (pos+ LINE/MEAN/END-3/8
                                        (dir (- WIDTH-DIAGONAL-BASE) WIDTH-STROKE 0.0)))
             (define cf-top-outer (pos+ LINE/MEAN/END-3/8
                                        (dir 0.0 WIDTH-STROKE 0.0)))
             (define cf-bottom-inner (pos+ LINE/BASE/START
                                           (dir WIDTH-DIAGONAL-BASE (- WIDTH-STROKE) 0.0)))
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
