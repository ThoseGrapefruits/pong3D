#lang typed/racket/base/optional

(require pict3d)

(module wrapper racket/base
  (require nested-hash)
  (provide
   nested-hash-ref*
   nested-hash-set*))

(provide nested-hash-ref*
         nested-hash-set*)

(define-type Nested-Storable (U Pict3D #f))

(require/typed 'wrapper
 [nested-hash-ref* (-> HashTableTop
                                (Listof Any)
                                [#:default Nested-Storable]
                                Nested-Storable)]
 [nested-hash-set* (-> HashTableTop
                       (Listof Any)
                       Nested-Storable
                       [#:hash (-> HashTableTop)]
                       HashTableTop)])
