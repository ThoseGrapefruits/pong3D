#lang typed/racket/base

(require "./rsound.rkt")

(define-type Updater (-> Flonum Void))
(define-type Updater-Double (-> Flonum Flonum Void))

(require/typed
  "./tone-lerp-untyped.rkt"
  [tone-lerp (-> Flonum Flonum (values RNetwork Updater))]
  [tone-lerp-double (-> Flonum Flonum Flonum (values RNetwork Updater-Double))])

(provide tone-lerp tone-lerp-double)
