#lang typed/racket/base

(require "../preferences/preferences.rkt")

(provide (struct-out Menu-Item-Type-Slider-Flonum)
         make-Menu-Item-Type-Slider-Flonum)

;; STRUCTS & TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Menu-Item-Type-Slider-Flonum
  ([key       : Pref-Key]
   [max       : Flonum]
   [min       : Flonum]
   [step      : Flonum])
  #:transparent)

;; CONSTRUCTORS & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-Menu-Item-Type-Slider-Flonum :
   #:key  Pref-Key
   #:max  Flonum
   #:min  Flonum
   #:step Flonum
   -> Menu-Item-Type-Slider-Flonum)
(define (make-Menu-Item-Type-Slider-Flonum
         #:key  key
         #:max  max
         #:min  min
         #:step step)
  (Menu-Item-Type-Slider-Flonum
   key
   max
   min
   step))
