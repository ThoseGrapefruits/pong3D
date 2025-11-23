#lang typed/racket/base

(require
  "../on-draw/palette.rkt"
  "./menu.rkt"
  "./menu-root-settings.rkt")

(provide get-menu-root-pause)

(: get-menu-root-pause : -> Menu-Item)
(define (get-menu-root-pause)
  (make-Menu-Item
   #:children (list (make-Menu-Item #:color-active EMITTED-BLUE
                                    #:label "Resume"
                                    #:tag 'resume)
                    (get-menu-root-settings 'root-pause)
                    (make-Menu-Item #:color-active EMITTED-PURPLE
                                    #:label "Main menu"
                                    #:tag 'main-menu)
                    (make-Menu-Item #:color-active EMITTED-YELLOW
                                    #:label "Exit"
                                    #:tag 'exit))
   #:label "Pause"
   #:tag 'root-pause))
