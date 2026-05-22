#lang typed/racket/base

(provide
 (for-syntax play)

 PStream
 (struct-out pstream)
 ps:andqueue
 ps:clear!
 ps:current-frame
 ps:make-pstream
 ps:play
 ps:queue
 ps:queue-callback
 ps:set-volume!

 RNetwork
 RNetwork/s
 network/s
 network-init

 RSignalSimple
 RSound
 (struct-out rsound)

 rs:append*
 rs:build-sound
 rs:clip&volume
 rs:default-sample-rate
 rs:ding
 rs:frames
 rs:ith/left
 rs:ith/right
 rs:loop-ctr
 rs:make-ding
 rs:make-tone
 rs:overlay
 rs:play/proc
 rs:reverb
 rs:scale
 rs:signal->rsound
 rs:silence
 rs:sine-wave
 rs:stop
 rs:synth-note
 rs:write)

(require (for-syntax (only-in "./rsound-wrapper.rkt" network play)))

;; STRUCTS & FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require/typed
 "./rsound-wrapper.rkt"
 [#:struct network/s ([ins : Natural]
                       [outs : Natural]
                       [maker : (-> Procedure)])
  #:type-name RNetwork/s])

(define-type RNetwork (U RNetwork/s Procedure))
(define-type RSignalSimple (-> Real))

(require/typed
 "./rsound-network.rkt"
 [network-init (-> RNetwork Procedure)]
 [signal? (-> Any Boolean)]
 [filter? (-> Any Boolean)]
 [loop-ctr (-> Real Real RNetwork/s)]
 [loop-ctr/variable (-> Real RNetwork/s)]
 [simple-ctr (-> Real Real RNetwork/s)]
 [frame-ctr RNetwork/s]
 [signal-samples (-> RNetwork Natural (Vectorof Any))]
 [signal-nth (-> RNetwork Natural Any)]
 [tap (-> Natural Any RNetwork/s)]
 [choose-net (-> RNetwork/s RNetwork/s RNetwork/s)])

(require/typed
 (except-in "./rsound-wrapper.rkt" network network/s play)
 [#:struct rsound ([data        : (Vectorof Real)]
                   [start       : Nonnegative-Real]
                   [stop        : Nonnegative-Real]
                   [sample-rate : Nonnegative-Real])
  #:type-name RSound]
 [#:struct pstream ([sound-heap    : Any]
                    [callback-heap : Any]
                    [time-checker  : Any]
                    [frame-rate    : Any]
                    [volume-box    : Any])
  #:type-name PStream]

 [ps:andqueue (All (T) (-> PStream RSound Natural T T))]
 [ps:clear! (-> PStream Void)]
 [ps:current-frame (-> PStream Natural)]
 [ps:play (-> PStream RSound PStream)]
 [ps:queue (-> PStream RSound Natural String)]
 [ps:queue-callback (-> PStream (-> Void) Natural String)]
 [ps:set-volume! (-> PStream Real PStream)]
 [ps:make-pstream (->* () (#:buffer-time Positive-Float) PStream)]

 [rs:append* (-> (Sequenceof RSound) RSound)]
 [rs:build-sound (-> Positive-Integer RNetwork RSound)]
 [rs:clip&volume (-> Real RSignalSimple RSignalSimple)]
 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:frames (-> RSound Nonnegative-Integer)]
 [rs:ith/left (-> RSound Nonnegative-Integer Real)]
 [rs:ith/right (-> RSound Nonnegative-Integer Real)]
 [rs:loop-ctr (-> Real Real RNetwork)]
 [rs:make-ding (-> Nonnegative-Integer RSound)]
 [rs:make-tone (-> Nonnegative-Real Nonnegative-Flonum Exact-Nonnegative-Integer RSound)]
 [rs:overlay (-> RSound RSound RSound)]
 [rs:play/proc (-> RSound Void)]
 [rs:reverb RNetwork/s]
 [rs:scale (-> Real RSound RSound)]
 [rs:signal->rsound (-> Nonnegative-Integer RNetwork RSound)]
 [rs:silence (-> Nonnegative-Integer RSound)]
 [rs:sine-wave RNetwork]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)]
 [rs:write (-> RSound Path Void)])
