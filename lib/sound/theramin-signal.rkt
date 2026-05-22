#lang racket/base
(require racket/math
         (except-in "rsound-wrapper.rkt" network network-init)
         "rsound-network.rkt")

(provide make-siggy)

(define (make-siggy song)
  (let* ([len (exact->inexact (rs:frames song))]
         [ctr-step (network-init (rs:loop-ctr len 1))]
         [reverb-step (network-init rs:reverb)])
    (lambda ()
      (reverb-step
       (rs:ith/left song (exact-round (ctr-step)))))))
