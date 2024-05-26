#lang typed/racket/base

(require pict3d
         racket/bool)

(provide path=?)

(: path=? : (Listof Tag) (Listof Tag) -> Boolean)
(define (path=? tags1 tags2)
  (and (= (length tags1) (length tags2))
       (for/and ([t1 tags1]
                 [t2 tags2])
         (or (and (symbol? t1)
                  (symbol? t2)
                  (symbol=? t1 t2))
             (and (number? t1)
                  (number? t2)
                  (= t1 t2))))))