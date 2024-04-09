#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

(define symbol:?
  (Char-3D #\?
           WIDTH-EM-3/4
           (λ () (combine
                  ; arc
                  (cirque-y-1/2 #:arc (arc -165.0 90.0))
                  ; arc
                  (rectangle (pos+ LINE/MEAN/CENTER-1/2 +y 0.0)
                             (dir WIDTH-STROKE-1/2 WIDTH-STROKE DEPTH-Z-1/2))
                  ; dot
                  (cylinder (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-3/8)
                            (dir WIDTH-STROKE-3/4 WIDTH-STROKE-3/4 DEPTH-Z-1/2))))))

(define symbol:dot
  (Char-3D #\.
           WIDTH-EM-1/4
           (λ () (cylinder (pos+ LINE/BASE/CENTER-1/4 -y WIDTH-STROKE-3/8)
                         WIDTH-STROKE-3/4))))

(define symbol:comma
  (Char-3D #\,
           WIDTH-EM-1/4
           (λ () (combine
                  ; circle
                  (cylinder (pos+ LINE/BASE/CENTER-1/4 -y WIDTH-STROKE-3/8)
                            WIDTH-STROKE-3/4)
                            ; tail
                  (quad-thicc (pos+ LINE/BASE/CENTER-1/4
                                    (dir (- WIDTH-STROKE-1/4) 0.0 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir (- WIDTH-STROKE-1/4) WIDTH-STROKE 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir WIDTH-STROKE-1/4     WIDTH-STROKE 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir WIDTH-STROKE-3/4     0.0          0.0)))))))