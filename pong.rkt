#lang typed/racket

(require
  pict3d/universe
  typed/racket/gui

  "./lib/config.rkt"
  "./lib/on-draw/index.rkt"
  "./lib/on-frame/index.rkt"
  "./lib/on-key.rkt"
  "./lib/on-mouse.rkt"
  "./lib/state/init.rkt"
  "./lib/state/setters.rkt"
  "./lib/state/state.rkt"
  "./lib/state/validation.rkt")

;
;                                  ═══
;
;     ⊖
;    /                             ╭─╮
;   x                              ╰─╯
;  /                                                                        ⊕
; ⊕                                                                         |
;                               ╓───────╖                                   z 
;                               ╙───────╜                                   |
;                             ⊖ ——— y ——— ⊕                                 ⊖


(big-bang3d
 (state-start)
 #:frame-delay FRAME-DELAY-MILLIS
 #:name "Pong3D — Racket"
 #:on-draw on-draw
 #:on-frame on-frame
 #:on-key on-key
 #:on-mouse on-mouse
 #:on-release on-release
 #:valid-state? valid-state?
 #:width SCREEN-WIDTH
 #:height SCREEN-HEIGHT)

0
