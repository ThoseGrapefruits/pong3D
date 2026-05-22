#lang typed/racket/base

(require "./rsound.rkt")

(define-type Updater (-> Flonum Void))

(require/typed
  "./tone-lerp-untyped.rkt"
  [tone-lerp (-> Flonum Flonum (values RNetwork Updater))])

(provide tone-lerp)
