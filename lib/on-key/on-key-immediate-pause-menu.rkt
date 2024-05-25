#lang typed/racket/base

(require "./util.rkt"
         "../state/state.rkt")

(provide on-key-immediate-pause-menu)

(: on-key-immediate-pause-menu : State-Pause-Menu Natural Flonum String -> State)
(define (on-key-immediate-pause-menu s n t k)
  (cond
    [(just-pressed? s k "escape") (menu-get-out s)]
    [(just-pressed? s k "return" "space") (menu-get-out s)]
    [else s]))

(: menu-get-out : State-Pause-Menu -> (U State-Pause-Menu State-Play))
(define (menu-get-out s)
  (define menu (State-Pause-Menu-menu s))
  (define active (Menu-active-item menu))
  (define parent (Menu-Item-parent active))
  (if (Menu-Item? parent)
      s
      (struct-copy State-Play (State-Pause-Menu-resume-state s)
                   [pause-state (struct-copy State-Pause-Menu s
                                             ; clear the old resume-state, will
                                             ; get reset if re-pause-menu
                                             [resume-state #f])])))
