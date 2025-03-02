#lang typed/racket/base

(require "./rsound.rkt")

(provide
 rs-play-song
 (struct-out Song))

(define-struct Song
  ([loop? : (Union 'loop 'one-shot)]
   [tempo  : Positive-Integer]
   [tracks : (Listof (Listof Symbol))]))

(: rs-play-song : Song -> PStream)
(define (rs-play-song song)
  (define ps (ps:make-pstream))
  (define get-next-time (λ () (max (- (ps:current-frame ps) 50) 0)))
  (: load-next : (-> Void))
  (define load-next
    (λ ()
      (ps:queue-callback ps load-next (get-next-time))
      (void)))
  (ps:queue-callback ps load-next 0)
  ps)
