#lang typed/racket/base

(require
  pict3d/universe
  typed/racket/gui

  "./lib/config.rkt"
  "./lib/on-draw/index.rkt"
  "./lib/on-frame/index.rkt"
  "./lib/on-key.rkt"
  "./lib/on-mouse.rkt"
  "./lib/sound/index.rkt"
  "./lib/state/init.rkt"
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

(rs-play SOUND-STARTUP)

(big-bang3d
 (state-start)
 #:cursor (make-object cursor% 'bullseye)
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

(rs-stop)
