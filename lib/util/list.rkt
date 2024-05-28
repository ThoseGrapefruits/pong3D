#lang typed/racket/base

(require racket/list)

(provide swap-last)

(: swap-last : (All (A) (Listof A) A -> (Listof A)))
(define (swap-last lst val)
  (cond [(empty? lst) (error "cannot swap-last of empty list")]
        [(empty? (rest lst)) (list val)]
        [else (cons (first lst)
                    (swap-last (rest lst) val))]))
