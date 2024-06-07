#lang info

(define name "pong-racket")
(define deps '("file-watchers"
               "nested-hash"
               ; using fork pending merge of
               ; https://github.com/jeapostrophe/pict3d/pull/31
               "git@github.com:ThoseGrapefruits/pict3d.git"
               "rsound"
               "typed-compose"
               "typed-racket-stream"))
