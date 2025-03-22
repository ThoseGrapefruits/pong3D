#lang typed/racket/base

(require "./song.rkt")

(provide SONG-MENU)

(: SONG-MENU : Song)
(define SONG-MENU
  (Song
   'loop
   10
   2
   '(C_:-1 A_:+2
     B_:-1 B_:+2
     A_:-1 A_:+1)))

(: SONG-TEST : Song)
(define SONG-TEST
  (Song
   'loop
   10
   3
   '(C_:-1 E_:-1 G_:-1
     E_:-1 G_:-1 C_:+0
     G_:-1 C_:+0 E_:+0
     C_:+0 __:__ G_:+0
     E_:+0 G_:+0 C_:+1
     G_:+0 C_:+1 E_:+1
     C_:+1 E_:+1 G_:+1
     G_:+0 C_:+1 E_:+1)))
