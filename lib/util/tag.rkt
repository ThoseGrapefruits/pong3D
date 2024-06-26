#lang typed/racket/base

(require pict3d
         racket/bool)

(provide path=?
         path-prefix?
         tag=?
         Tags)

(define-type Tags (Listof Tag))

(: path=? : Tags Tags -> Boolean)
(define (path=? tags1 tags2)
  (and (= (length tags1) (length tags2))
       (for/and ([tag1 tags1]
                 [tag2 tags2])
         (tag=? tag1 tag2))))

(: path-prefix? : Tags Tags -> Boolean)
(define (path-prefix? tags1 tags2)
  (and (>= (length tags1) (length tags2))
       (for/and ([tag1 tags1]
                 [tag2 tags2])
         (tag=? tag1 tag2))))

(: tag=? : Tag Tag -> Boolean)
(define (tag=? tag1 tag2)
(or (and (symbol?  tag1)
         (symbol?  tag2)
         (symbol=? tag1 tag2))
    (and (number? tag1)
         (number? tag2)
         (= tag1  tag2))))
