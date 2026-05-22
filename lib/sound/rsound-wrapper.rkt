#lang racket/base

(require rsound)
(provide
  ; STRUCTS
  (struct-out pstream)
  (struct-out rsound)
  (struct-out network/s)

  ; SYNTAX
  network
  play
  network-init

  ; FUNCTIONS
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
  (prefix-out rs: clip&volume)
  (prefix-out rs: default-sample-rate)
  (prefix-out rs: ding)
  (prefix-out rs: loop-ctr)
  (prefix-out rs: make-ding)
  (prefix-out rs: make-tone)
  (prefix-out rs: play/proc)
  (prefix-out rs: reverb)
  (prefix-out rs: signal->rsound)
  (prefix-out rs: silence)
  (prefix-out rs: sine-wave)
  (prefix-out rs: stop)
  (prefix-out rs: synth-note)

  (rename-out [rs-overlay   rs:overlay]
              [rs-append*   rs:append*]
              [rs-frames    rs:frames]
              [rs-ith/left  rs:ith/left]
              [rs-ith/right rs:ith/right]
              [rs-scale     rs:scale]
              [rs-write     rs:write]))