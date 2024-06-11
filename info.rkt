#lang info

(define name "pong-racket")
(define deps '("file-watchers"
               "nested-hash"
               ; Using fork for my own non-backwards-compatible changes
               "git@github.com:ThoseGrapefruits/pict3d.git:main"
               "pict3d"
               "rsound"
               "typed-compose"
               "typed-racket-stream"))
