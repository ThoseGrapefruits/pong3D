#lang typed/racket/base

(require (only-in racket/math exact-floor)
         "../preferences/preferences.rkt")

(provide (struct-out Menu-Item-Type-Slider-Flonum)
         make-Menu-Item-Type-Slider-Flonum)

;; STRUCTS & TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Menu-Item-Type-Slider-Flonum
  ([format       : (-> Flonum String)]
   [key          : Pref-Key]
   [max          : Flonum]
   [min          : Flonum]
   [step         : Flonum]
   [value-getter : (-> Flonum)]
   [value-setter : (-> Flonum Void)])
  #:transparent)

;; CONSTRUCTORS & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-Menu-Item-Type-Slider-Flonum :
   #:key  Pref-Key
   #:max  Flonum
   #:min  Flonum
   #:step Flonum
   [#:format (-> Flonum String)]
   [#:value-getter (-> Flonum)]
   [#:value-setter (-> Flonum Void)]
   -> Menu-Item-Type-Slider-Flonum)
(define (make-Menu-Item-Type-Slider-Flonum
         #:format [format #f]
         #:value-getter [value-getter #f]
         #:key  key
         #:max  max-value
         #:min  min-value
         #:value-setter [value-setter #f]
         #:step step)
  (Menu-Item-Type-Slider-Flonum
   (or format (λ ([n : Flonum])
                (number->string (exact-floor (* n 10)))))
   key
   max-value
   min-value
   step
   (or value-getter (λ ()
                   (get-pref-flonum key (λ () 0.0))))
   (or value-setter (λ ([value : Flonum])
                      (put-pref key (max min-value (min max-value value)))))))
