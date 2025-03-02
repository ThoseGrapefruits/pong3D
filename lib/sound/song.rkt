#lang typed/racket/base

(require "./rsound.rkt")

(provide
 rs-play-song
 (struct-out Song))

(define-struct Song
  ([loop? : (Union 'loop 'one-shot)]
   [tempo  : Positive-Integer]
   [tracks : (Listof (Listof Symbol))]))

(: rs-play-song : Song -> Thread)
(define (rs-play-song song)
  (define pstream (ps:make-pstream))
  (thread (λ () null)))
