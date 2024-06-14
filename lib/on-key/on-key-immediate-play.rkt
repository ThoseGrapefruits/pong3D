#lang typed/racket/base

(require
  "./util.rkt"
  "../on-draw/palette.rkt"
  "../state/menu.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt")

(provide on-key-immediate-play)

(: on-key-immediate-play : State-Play Natural Flonum String -> State-Any)
(define (on-key-immediate-play s n t k)
  (cond
    [(just-pressed? s k "escape")
     (define pause-state (State-Play-pause-state s))
     (define resume-state
       (struct-copy State-Play s  ; resume-state
                    [pause-state #f]))
     ; try to use cached state
     (or (and pause-state
              (struct-copy State-Pause-Menu pause-state
                           [resume-state resume-state]))
         ; fall back to making a new one
         (State-transition
          State-Pause-Menu s
          (make-Menu      ; menu
           (get-pause-menu-root))
          resume-state))] ; resume-state
    [else s]))

(define (get-pause-menu-root)
  (make-Menu-Item
   #:children (list (make-Menu-Item #:color-active EMITTED-BLUE
                                    #:label "Resume"
                                    #:tag 'resume)
                    (make-Menu-Item #:color-active EMITTED-PURPLE
                                    #:label "Main menu"
                                    #:tag 'main-menu)
                    (make-Menu-Item #:color-active EMITTED-YELLOW
                                    #:label "Exit"
                                    #:tag 'exit))
   #:label "Pause"
   #:tag 'root-pause))
