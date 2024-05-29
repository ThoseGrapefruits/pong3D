#lang typed/racket/base

(require
  typed/racket/gui
  pict3d/universe

  "./lib/config.rkt"
  "./lib/on-draw/on-draw.rkt"
  "./lib/on-frame/on-frame.rkt"
  "./lib/on-key/on-key.rkt"
  "./lib/on-mouse/on-mouse.rkt"
  "./lib/sound/sound.rkt"
  "./lib/state/init.rkt"
  "./lib/state/stop.rkt"
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

(big-bang3d (state-start)
            #:frame-delay FRAME-DELAY-MILLIS
            #:name "Pong3D — Racket"
            #:on-draw on-draw
            #:on-frame on-frame
            #:on-key on-key
            #:on-mouse on-mouse
            #:on-release on-release
            #:stop-state? stop-state?
            #:valid-state? valid-state?
            #:width SCREEN-WIDTH
            #:height SCREEN-HEIGHT)

(rs-stop)
(exit 0)
