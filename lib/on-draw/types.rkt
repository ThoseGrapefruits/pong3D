#lang typed/racket/base

(require
  (only-in pict3d Emitted)
  "./palette.rkt")

(provide (struct-out Score-Section)
         SCORE-SECTIONS)

(define-struct Score-Section
  ([color-emitted : Emitted]
   [y : Flonum]
   [place-low : Integer]
   [place-high : Integer]))

(define SCORE-SECTIONS : (Listof Score-Section)
  (list (Score-Section EMITTED-WHITE  0.0  10       1)   ; ones
        (Score-Section EMITTED-BLUE   0.03 100     10)   ; tens
        (Score-Section EMITTED-YELLOW 0.06 1000   100)   ; hundreds
        (Score-Section EMITTED-PURPLE 0.09 10000 1000))) ; thousands
