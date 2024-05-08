#lang typed/racket/base

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

(: NUM-ARC-OFFSET : Flonum)
(define NUM-ARC-OFFSET 15.0)

(define num:0
  (make-Char-3D-memoized
#\0
           WIDTH-EM-5/8
           (λ () (combine
                  ; dot
                  (cylinder LINE/MID/CENTER-1/2
                            (dir WIDTH-STROKE-3/4 WIDTH-STROKE-3/4 DEPTH-Z-1/2))
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

(define num:1
  (make-Char-3D-memoized
#\1
           WIDTH-EM-5/8
           (λ () (combine
                  ; cap
                  (quad-thicc (pos+ LINE/CAP/CENTER-1/2
                                    (dir (- 0.0 WIDTH-STROKE-1/2 WIDTH-BASE/NARROW) WIDTH-STROKE-1/2 0.0))
                              (pos+ LINE/CAP/CENTER-1/2
                                    (dir (- 0.0 WIDTH-STROKE-1/2 WIDTH-BASE/NARROW) WIDTH-STROKE 0.0))
                              (pos+ LINE/CAP/CENTER-1/2
                                    (dir (- WIDTH-STROKE-1/2) WIDTH-STROKE 0.0))
                              (pos+ LINE/CAP/CENTER-1/2
                                    (dir (- WIDTH-STROKE-1/2) 0.0 0.0)))
                  ; ascender
                  (rectangle LINE/MID/CENTER-1/2
                             (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
                  ; base
                  (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                             (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define num:2
  (make-Char-3D-memoized
#\2
           WIDTH-EM-5/8
           (λ () (combine
                  ; top arc
                  (cirque-y-1/2 #:arc (arc -165.0 90.0))
                  ; mid arc
                  (cirque-x-link-1/2 #:arc (arc -180.0 -90.0))
                  ; lower arc connector
                  (rectangle (pos+ LINE/BASE/START
                                   (dir WIDTH-STROKE-1/2 (- 0.0 HEIGHT-X-1/4 WIDTH-STROKE-1/2) 0.0))
                             (dir WIDTH-STROKE-1/2 (+ HEIGHT-X-1/4 WIDTH-STROKE-1/2) DEPTH-Z-1/2))
                  ; base
                  (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                             (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define num:3
  (make-Char-3D-memoized
#\3
           WIDTH-EM-5/8
           (λ () (combine
                  ; top arc
                  (cirque-y-1/2 #:arc (arc (- NUM-ARC-OFFSET 180.0) 90.0))
                  ; mid arc
                  (cirque-x-link-1/2 #:arc (arc -90.0 (- 180.0 NUM-ARC-OFFSET)))))))

(define num:4
  (make-Char-3D-memoized
#\4
           WIDTH-EM-5/8
           (λ ()
             (define riser-pos (pos+ LINE/MID/END-5/8 -x (* WIDTH-STROKE 2.0)))
             (define riser-pos-connect (pos (- (pos-x riser-pos) WIDTH-STROKE-1/2)
                                            (pos-y LINE/CAP/START)
                                            (pos-z riser-pos)))
             (combine
              ; diagonal
              (quad-thicc (pos+ LINE/MEAN/START
                                (dir 0.0 (- WIDTH-STROKE-1/2) 0.0))
                          (pos+ LINE/MEAN/START
                                (dir WIDTH-DIAGONAL-BASE (- WIDTH-STROKE-1/2) 0.0))
                          (pos+ riser-pos-connect
                                (dir 0.0 WIDTH-DIAGONAL-BASE 0.0))
                          (pos+ riser-pos-connect
                                (dir 0.0 0.0 0.0)))
              ; crossbar
              (rectangle (pos+ LINE/MEAN/CENTER-5/8 -x WIDTH-STROKE-1/2)
                         (dir (- WIDTH-EM-5/16 WIDTH-STROKE-1/2) WIDTH-STROKE-1/2 DEPTH-Z-1/2))
              ; ascender
              (rectangle riser-pos
                         (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define num:5
  (make-Char-3D-memoized
#\5
           WIDTH-EM-5/8
           (λ () (combine
                  ; top bar
                  (rectangle (pos+ LINE/CAP/CENTER-1/2
                                   (dir 0.0 WIDTH-STROKE-1/2 0.0))
                             (dir (- WIDTH-EM-1/4 WIDTH-STROKE-1/4) WIDTH-STROKE-1/2 DEPTH-Z-1/2))
                  ; ascender
                  (rectangle (pos+ LINE/MID-Y/START
                                   (dir WIDTH-STROKE-3/4 0.0 0.0))
                             (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
                  ; mid arc connector
                  (rectangle (pos+ LINE/MEAN/START
                                   (dir WIDTH-BASE/NARROW (- WIDTH-STROKE-1/2) 0.0))
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
                  ; bottom arc
                  (cirque-x-link-1/2 #:arc (arc -90.0 (- 180.0 NUM-ARC-OFFSET)))))))

(define num:6
  (make-Char-3D-memoized
#\6
           WIDTH-EM-5/8
           (λ () (combine
                  ; top arc
                  (cirque-y-1/2 #:arc (arc -180.0 (- NUM-ARC-OFFSET)))
                  ; ascender
                  (quad-thicc LINE/MID-X/START
                              (pos+ LINE/MID-X/START +x WIDTH-STROKE)
                              (pos+ LINE/MID-Y/START +x WIDTH-STROKE)
                              LINE/MID-Y/START)
                  ; base
                  (cirque-x-link-1/2)))))

(define num:7
  (make-Char-3D-memoized
#\7
           WIDTH-EM-5/8
           (λ () (combine
                  ; top bar
                  (rectangle (pos+ LINE/CAP/CENTER-1/2
                                   (dir WIDTH-STROKE-1/4 WIDTH-STROKE-1/2 0.0))
                             (dir (- WIDTH-EM-1/4 WIDTH-STROKE-1/4) WIDTH-STROKE-1/2 DEPTH-Z-1/2))
                  ; ascender
                  (quad-thicc
                    (pos+ LINE/BASE/CENTER-1/2 -x WIDTH-STROKE)
                    (pos+ LINE/BASE/CENTER-1/2 +x 0.0)
                    (pos+ LINE/CAP/END-1/2 (dir 0.0 WIDTH-STROKE 0.0))
                    (pos+ LINE/CAP/END-1/2 (dir (- WIDTH-STROKE) WIDTH-STROKE 0.0)))))))

(define num:8
  (make-Char-3D-memoized
#\8
           WIDTH-EM-5/8
           (λ () (combine
                  ; top arc
                  (cirque-y-1/2)
                  ; base
                  (cirque-x-link-1/2)))))

(define num:9
  (make-Char-3D-memoized
#\9
           WIDTH-EM-5/8
           (λ () (combine
                  ; top arc
                  (cirque-y-1/2)
                  ; ascender
                  (quad-thicc LINE/MID-Y/END-1/2
                              (pos+ LINE/MID-Y/END-1/2 -x WIDTH-STROKE)
                              (pos+ LINE/MID-X/END-1/2 -x WIDTH-STROKE)
                              LINE/MID-X/END-1/2)
                  ; base
                  (cirque-x-link-1/2 #:arc (arc 0.0 (- 180.0 NUM-ARC-OFFSET)))))))
