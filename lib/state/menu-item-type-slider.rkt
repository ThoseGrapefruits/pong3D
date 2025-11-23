#lang typed/racket/base

(require (only-in racket/math exact-floor)
         "../preferences/preferences.rkt")

(provide (struct-out Menu-Item-Type-Slider-Flonum)
         make-Menu-Item-Type-Slider-Flonum)

;; STRUCTS & TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Menu-Item-Type-Slider-Flonum
  ([decimal-digits : Nonnegative-Integer]
   [format         : (-> Flonum String)]
   [key            : Pref-Key]
   [max            : Flonum]
   [min            : Flonum]
   [text-cache     : (Boxof (U (Pairof Flonum String) #f))]
   [value-getter   : (-> Flonum)]
   [value-setter   : (-> Flonum Void)])
  #:transparent)

;; CONSTRUCTORS & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-Menu-Item-Type-Slider-Flonum :
   #:decimal-digits Nonnegative-Integer
   #:key  Pref-Key
   [#:format (-> Flonum String)]
   #:max  Flonum
   #:min  Flonum
   [#:value-getter (-> Flonum)]
   [#:value-setter (-> Flonum Void)]
   -> Menu-Item-Type-Slider-Flonum)
(define (make-Menu-Item-Type-Slider-Flonum
         #:decimal-digits decimal-digits
         #:format [format #f]
         #:key key
         #:max max-value
         #:min min-value
         #:value-getter [value-getter #f]
         #:value-setter [value-setter #f])
  (Menu-Item-Type-Slider-Flonum
   decimal-digits
   (or format
       (λ ([n : Flonum])
         (real->decimal-string n decimal-digits)))
   key
   max-value
   min-value
   (box #f)
   (or value-getter
       (λ ()
         (get-pref-flonum key (λ () 0.0))))
   (or value-setter
       (λ ([value : Flonum])
         (put-pref key (real->decimal-string (max min-value (min max-value value))
                                             decimal-digits))))))
