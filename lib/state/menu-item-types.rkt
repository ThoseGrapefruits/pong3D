#lang typed/racket/base

(require "./menu-item-type-slider.rkt")

(provide Menu-Item-Type
         Menu-Item-Type-Slider
         Menu-Item-Type-Slider?
         make-Menu-Item-Type-Slider-Flonum
         (struct-out Menu-Item-Type-Slider-Flonum))

(define-type Menu-Item-Type (U 'text Menu-Item-Type-Slider))
(define-type Menu-Item-Type-Slider (U Menu-Item-Type-Slider-Flonum))

(: Menu-Item-Type-Slider? : Any -> Boolean : Menu-Item-Type-Slider)
(define (Menu-Item-Type-Slider? o)
  (or (Menu-Item-Type-Slider-Flonum? o)))
