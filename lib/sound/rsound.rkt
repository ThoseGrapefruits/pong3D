#lang typed/racket/base

(provide
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

 RSound
 (struct-out rsound)
 rs:append*
 rs:default-sample-rate
 rs:ding
 rs:make-ding
 rs:make-tone
 rs:overlay
 rs:scale
 rs:sine-wave
 rs:stop
 rs:synth-note
 rs:write)

(module wrapper racket/base
  (require rsound)
  (provide
   (struct-out pstream)
   (struct-out rsound)
   (struct-out network/s)
   (prefix-out rs: network)

   (rename-out [andqueue ps:andqueue]
               [make-pstream ps:make-pstream]
               [pstream-clear! ps:clear!]
               [pstream-current-frame ps:current-frame]
               [pstream-play ps:play]
               [pstream-queue ps:queue]
               [pstream-queue-callback ps:queue-callback]
               [pstream-set-volume! ps:set-volume!])

   (prefix-out rs: andplay)
   (prefix-out rs: build-sound)
   (prefix-out rs: default-sample-rate)
   (prefix-out rs: ding)
   (prefix-out rs: make-ding)
   (prefix-out rs: make-tone)
   (prefix-out rs: sine-wave)
   (prefix-out rs: synth-note)
   (prefix-out rs: stop)

   (rename-out [rs-overlay rs:overlay]
               [rs-append* rs:append*]
               [rs-write   rs:write]
               [rs-scale   rs:scale])))

(require/typed
 'wrapper
 [#:struct network/s ([ins : Positive-Integer]
                      [outs : Positive-Integer]
                      [maker : (-> RNetwork)])
  #:type-name RNetwork/s]
 [#:struct rsound ([data : (Vectorof Real)]
                   [start : Nonnegative-Real]
                   [stop : Nonnegative-Real]
                   [sample-rate : Nonnegative-Real])
  #:type-name RSound]
 [#:struct pstream ([sound-heap : Any]
                    [callback-heap : Any]
                    [time-checker : Any]
                    [frame-rate : Any]
                    [volume-box : Any])
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
 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:make-ding (-> Nonnegative-Integer RSound)]
 [rs:make-tone (-> Nonnegative-Real Nonnegative-Flonum Exact-Nonnegative-Integer RSound)]
 [rs:overlay (-> RSound RSound RSound)]
 [rs:scale (-> Real RSound RSound)]
 [rs:sine-wave RNetwork]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)]
 [rs:write (-> RSound Path Void)])

(define-type RNetwork (U RNetwork/s RSound))
