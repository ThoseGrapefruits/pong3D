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

 RSound
 (struct-out rsound)
 rs:append*
 rs:default-sample-rate
 rs:ding
 rs:make-ding
 rs:overlay
 rs:scale
 rs:stop
 rs:synth-note
 rs:write)

(module wrapper racket/base
  (require rsound)
  (provide
   (struct-out pstream)
   (struct-out rsound)

   (rename-out [andqueue ps:andqueue]
               [make-pstream ps:make-pstream]
               [pstream-clear! ps:clear!]
               [pstream-current-frame ps:current-frame]
               [pstream-play ps:play]
               [pstream-queue ps:queue]
               [pstream-queue-callback ps:queue-callback]
               [pstream-set-volume! ps:set-volume!])

   (prefix-out rs: andplay)
   (prefix-out rs: default-sample-rate)
   (prefix-out rs: ding)
   (prefix-out rs: make-ding)
   (prefix-out rs: make-tone)
   (prefix-out rs: play)
   (prefix-out rs: synth-note)
   (prefix-out rs: stop)

   (rename-out [rs-overlay rs:overlay]
               [rs-append* rs:append*]
               [rs-write   rs:write]
               [rs-scale   rs:scale])))

(require/typed
 'wrapper
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

 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:make-ding (-> Integer RSound)]
 [rs:append* (-> (Sequenceof RSound) RSound)]
 [rs:overlay (-> RSound RSound RSound)]
 [rs:write (-> RSound Path Void)]
 [rs:scale (-> Real RSound RSound)]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)])
