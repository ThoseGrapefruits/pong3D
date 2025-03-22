#lang typed/racket/base

(require (only-in pict3d Tag))

(provide path=?
         path-prefix?
         Tags)

(define-type Tags (Listof Tag))

(: path=? : Tags Tags -> Boolean)
(define (path=? tags1 tags2)
  (and (= (length tags1) (length tags2))
       (for/and ([tag1 tags1]
                 [tag2 tags2])
         (eq? tag1 tag2))))

(: path-prefix? : Tags Tags -> Boolean)
(define (path-prefix? tags1 tags2)
  (and (>= (length tags1) (length tags2))
       (for/and ([tag1 tags1]
                 [tag2 tags2])
         (eq? tag1 tag2))))
