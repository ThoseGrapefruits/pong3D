#lang typed/racket/base

(require
  (only-in pict3d
           Tag)
  (only-in racket/math exact-round)
  "../on-draw/palette.rkt"
  "./menu.rkt"
  "./menu-item-type-slider.rkt"
  "./state.rkt")

(provide get-menu-root-settings)

(: get-menu-root-settings : Tag -> Menu-Item)
(define (get-menu-root-settings root)
  (make-Menu-Item
            #:color-active EMITTED-PURPLE
            #:label "Settings"
            #:tag   'settings
            #:children
            (list (make-Menu-Item
                  #:color-active EMITTED-BLUE
                  #:label "Display"
                  #:tag   'display)
                  (make-Menu-Item
                  #:color-active EMITTED-PURPLE
                  #:label "Sound"
                  #:tag   'sound
                  #:children
                  (list
                    (make-Menu-Item
                    #:color-active EMITTED-BLUE
                    #:label "Main volume"
                    #:tag   'volume-main
                    #:type  (make-Menu-Item-Type-Slider-Flonum
                              #:format (λ ([n : Flonum])
                                        (format "~a" (exact-round (* n 10))))
                              #:key 'volume-main
                              #:decimal-digits 1
                              #:min 0.0
                              #:max 1.0))
                    (make-Menu-Item
                    #:color-active EMITTED-BLUE
                    #:label "Effects volume"
                    #:tag   'volume-effects
                    #:type  (make-Menu-Item-Type-Slider-Flonum
                              #:format (λ ([n : Flonum])
                                        (format "~a" (exact-round (* n 10))))
                              #:key 'volume-effects
                              #:decimal-digits 1
                              #:min 0.0
                              #:max 1.0))
                    (make-Menu-Item
                    #:color-active EMITTED-BLUE
                    #:label "Music volume"
                    #:tag   'volume-music
                    #:type  (make-Menu-Item-Type-Slider-Flonum
                              #:format (λ ([n : Flonum])
                                        (format "~a" (exact-round (* n 10))))
                              #:key 'volume-music
                              #:decimal-digits 1
                              #:min 0.0
                              #:max 1.0)))))))