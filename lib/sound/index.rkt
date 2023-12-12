#lang typed/racket

(require typed/racket/gui/base)

(module wrapper racket
  (require (prefix-in rs: rsound))
  (require (prefix-in rs: rsound))
  (provide
   rs:default-sample-rate
   rs:ding
   rs:play
   rs:rs-overlay
   rs:rs-append*
   rs:rs-write
   rs:rsound
   rs:rsound?
   rs:synth-note
   rs:stop))

(require/typed
 'wrapper
 [#:opaque RSound rs:rsound?]
 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:rs-append* (-> (Sequenceof RSound) RSound)]
 [rs:rs-overlay (-> RSound RSound RSound)]
 [rs:rs-write (-> RSound Path Void)]
;  [rs:rsound any/c]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)])

(provide
 rs-play
 rs-stop

 SOUND-BALL-BOUNCE-BUMPER
 SOUND-BALL-BOUNCE-WALL
 SOUND-DING
 SOUND-MUSIC)

;; PLAY/STOP

(: rs-play (->* (RSound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; (rs:check-below-threshold sound 2.0)
  (define cached-path (hash-ref path-cache sound #f))
  (define path (if cached-path cached-path (make-temporary-file "pong3d-sound-~a.wav")))
  (hash-set! path-cache sound path)
  (rs:rs-write sound path)
  (: play (-> Boolean))
  (define play (if loop
                   (lambda () (play-sound path #f) (play))
                   (lambda () (play-sound path #f))))
  (thread play))

(: rs-stop (-> Void))
(define rs-stop rs:stop)

;; SOUND CONSTANTS

(: SOUND-DING RSound)
(define SOUND-DING rs:ding)

(: SOUND-BALL-BOUNCE-BUMPER RSound)
(define SOUND-BALL-BOUNCE-BUMPER rs:ding)

(: SOUND-BALL-BOUNCE-WALL RSound)
(define SOUND-BALL-BOUNCE-WALL rs:ding)

(: path-cache (Mutable-HashTable RSound Path))
(define path-cache (make-hash))

(define dur 1/10)

(: SOUND-MUSIC RSound)
(define SOUND-MUSIC
  (rs:rs-overlay
   (rs:rs-append*
    (for*/list : (Listof RSound) ([i (in-range 20)]
                [j (in-list '(0 2 3 7))])
      (rs:synth-note "vgame" 49 (+ 60 j)
                  (exact-round
                   (* (rs:default-sample-rate) dur)))))
   (rs:rs-append*
    (for*/list : (Listof RSound) ([i (in-range 5)]
                                  [j (in-list '(8 3 4 3))])
      (rs:synth-note "vgame" 49 (+ 52 j)
                  (exact-round
                   (* (rs:default-sample-rate) (* 4 dur))))))))
