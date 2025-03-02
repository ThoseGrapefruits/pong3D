#lang typed/racket/base

(require
  typed/racket/gui
  pict3d/universe

  "./lib/config.rkt"
  "./lib/on-draw/on-draw.rkt"
  "./lib/on-frame/on-frame.rkt"
  "./lib/on-key/on-key.rkt"
  "./lib/on-mouse/on-mouse.rkt"
  "./lib/sound/song.rkt"
  "./lib/sound/song-background.rkt"
  "./lib/sound/sound.rkt"
  "./lib/sound/song-background.rkt"
  "./lib/state/init.rkt"
  "./lib/state/stop.rkt"
  "./lib/state/validation.rkt")

;
;             /                    ═══                    \
;            /                                             \
;     ⊖     /                                               \
;    /     /                       ╭─╮                       \
;   x     /                        ╰─╯                        \
;  /     /                                                     \            ⊕
; ⊕     /                                                       \           |
;      /                        ╓───────╖                        \          z 
;     /                         ╙───────╜                         \         |
;    /                        ⊖ ——— y ——— ⊕                        \        ⊖

; Names:
; - Bumper: the glowing rails along the sides of the arena, off of which the balls bounce
; - Paddle: the rectangular prisms controlled by the player and opponent

(define thread-sound-startup (rs-play SOUND-STARTUP))
(define song-stream-menu (rs-play-song SONG-MENU))

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
