#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

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
                        (dir cross-radius WIDTH-STROKE-1/2 DEPTH-Z))))))
(define char:B
  (Char-3D #\B
           WIDTH-EM-3/4
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:C
  (Char-3D #\C
           WIDTH-EM-3/4
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:D
  (Char-3D #\D
           WIDTH-EM-3/4
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:E
  (Char-3D #\E
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:F
  (Char-3D #\F
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:G
  (Char-3D #\G
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:H
  (Char-3D #\H
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:I
  (Char-3D #\I
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:J
  (Char-3D #\J
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:K
  (Char-3D #\K
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:L
  (Char-3D #\L
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:M
  (Char-3D #\M
           WIDTH-EM-9/8
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:N
  (Char-3D #\N
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:O
  (Char-3D #\O
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:P
  (Char-3D #\P
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:Q
  (Char-3D #\Q
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:R
  (Char-3D #\R
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
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
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:U
  (Char-3D #\U
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:V
  (Char-3D #\V
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:W
  (Char-3D #\W
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:X
  (Char-3D #\X
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:Y
  (Char-3D #\Y
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
(define char:Z
  (Char-3D #\Z
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))
