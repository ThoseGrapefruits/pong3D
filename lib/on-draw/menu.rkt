#lang typed/racket/base

(require
  pict3d
  racket/match
  "../state/menu.rkt"
  "../state/state-base.rkt"
  "../util/number.rkt")

(provide ANIMATION-TIME
         EMITTED-MENU-ITEM-TEXT
         EMITTED-MENU-ITEM-BACKGROUND
         Menu-Item-color
         Menu-Item-scale)

(: SCALE-MAX : Flonum)
(define SCALE-MAX 0.05)

(: ANIMATION-TIME : Flonum)
(define ANIMATION-TIME 140.0)

(define EMITTED-MENU-ITEM-TEXT        (emitted "oldlace" 1.5))
(define EMITTED-MENU-ITEM-TEXT-ACTIVE (emitted "oldlace" 0.1))
(define EMITTED-MENU-ITEM-BACKGROUND  (emitted "oldlace" 0.0))

; Returns the text and background color of a menu item
(: Menu-Item-color : State Menu Menu-Item -> (Values Emitted Emitted))
(define (Menu-Item-color s menu menu-item)
  (define active-color (Menu-Item-color-active menu-item))
  (define active-start (unbox (Menu-Item-active-start menu-item)))
  (define active-end   (unbox (Menu-Item-active-end   menu-item)))
  (define t (State-t s))

  (: time-since : Flonum)
  (define time-since
    (cond [active-end
          ; If Menu-Item stopped being active, we scale back to default.
           (max 0.0            (- ANIMATION-TIME (- t active-end)))]
          [active-start
           (min ANIMATION-TIME (- t active-start))]
          [else 0.0]))

  ; First 2 branches are (theoretical) optimizations to only do lerping when we are mid-transition.
  (cond [(= time-since 0.0)
         (values EMITTED-MENU-ITEM-TEXT        EMITTED-MENU-ITEM-BACKGROUND)]
        [(= time-since ANIMATION-TIME)
         (values EMITTED-MENU-ITEM-TEXT-ACTIVE active-color)]
        [else
         (define ratio (/ time-since ANIMATION-TIME))
         (match-define (emitted text-0r text-0g text-0b text-0a) EMITTED-MENU-ITEM-TEXT)
         (match-define (emitted text-1r text-1g text-1b text-1a) EMITTED-MENU-ITEM-TEXT-ACTIVE)
         (match-define (emitted bg-0r bg-0g bg-0b bg-0a) EMITTED-MENU-ITEM-BACKGROUND)
         (match-define (emitted bg-1r bg-1g bg-1b bg-1a) active-color)
         (values (emitted (lerp text-0r text-1r ratio)
                          (lerp text-0g text-1g ratio)
                          (lerp text-0b text-1b ratio)
                          (lerp text-0a text-1a ratio))
                 (emitted (lerp   bg-0r   bg-1r ratio)
                          (lerp   bg-0g   bg-1g ratio)
                          (lerp   bg-0b   bg-1b ratio)
                          (lerp   bg-0a   bg-1a ratio)))]))

(: Menu-Item-scale : State Menu Menu-Item -> Flonum)
(define (Menu-Item-scale s menu menu-item)
  (define active-start (unbox (Menu-Item-active-start menu-item)))
  (define active-end   (unbox (Menu-Item-active-end   menu-item)))
  (define t (State-t s))

  (: time-since : Flonum)
  (define time-since
    (cond [active-end
          ; If Menu-Item stopped being active, we scale back to 1.0.
           (max 0.0            (- ANIMATION-TIME (- t active-end)))]
          [active-start
           (min ANIMATION-TIME (- t active-start))]
          [else 0.0]))

  (+ 1.0 (* SCALE-MAX (/ time-since ANIMATION-TIME))))
