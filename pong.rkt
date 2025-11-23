#lang typed/racket/base

(module profile-wrapper racket/base
  (require (only-in profile profile-thunk))
  (provide profile-thunk))

(require
  (only-in pict3d/universe big-bang3d)
  "./lib/config.rkt"
  "./lib/on-draw/on-draw.rkt"
  "./lib/on-frame/on-frame.rkt"
  "./lib/on-key/on-key.rkt"
  "./lib/on-mouse/on-mouse.rkt"
  "./lib/on-resize.rkt"
  "./lib/sound/song.rkt"
  "./lib/sound/song-background.rkt"
  "./lib/sound/sound.rkt"
  "./lib/sound/song-background.rkt"
  "./lib/state/init.rkt"
  "./lib/state/stop.rkt"
  "./lib/state/validation.rkt")

(require/typed
 'profile-wrapper
 [profile-thunk (-> (-> Any)
                    [#:delay Nonnegative-Real]
                    [#:repeat Exact-Nonnegative-Integer]
                    [#:threads Boolean]
                    [#:render Any]
                    [#:periodic-renderer Any]
                    [#:use-errortrace? Boolean]
                    [#:order Symbol]
                    Any)])

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

(define (run)
  (big-bang3d (state-start #f)
              #:frame-delay FRAME-DELAY-MILLIS
              #:name "Pong3D — Racket"
              #:on-draw on-draw
              #:on-frame on-frame
              #:on-key on-key
              #:on-mouse on-mouse
              #:on-release on-release
              #:on-resize on-resize
              #:stop-state? stop-state?
              #:valid-state? valid-state?
              #:width SCREEN-WIDTH-INIT
              #:height SCREEN-HEIGHT-INIT))

; To profile performance
; (profile-thunk run)
(run)

; (rs-stop)
(exit 0)
