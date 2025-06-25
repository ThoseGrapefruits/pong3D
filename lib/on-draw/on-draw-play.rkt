#lang typed/racket/base

(require (only-in pict3d Pict3D Pos Tag)
         (only-in pict3d
                  affine-compose
                  arc
                  combine
                  cube
                  current-color
                  current-emitted
                  current-material
                  cylinder
                  dir
                  dir-dx
                  emitted
                  empty-pict3d
                  freeze
                  group
                  light
                  map-group
                  material
                  move
                  move-x
                  move-y
                  move-z
                  origin
                  pos
                  pos-x
                  pos+
                  pos-
                  quad
                  rectangle
                  rgba
                  rotate-x
                  rotate-y
                  rotate-z
                  scale
                  scale-x
                  sphere
                  transform
                  with-emitted)
         racket/list
         (only-in racket/function identity)
         "./camera.rkt"
         "./on-char-jiggle.rkt"
         "./position-screen-space.rkt"
         "./render-player-score.rkt"
         "./text.rkt"
         "../config.rkt"
         "../preferences/preferences.rkt"
         "../state/state.rkt")

(provide on-draw-play)

(define BALL-RADIUS-PREVIEW (/ BALL-RADIUS 2.0))
(define PADDLE-SCALE-PREVIEW (dir (dir-dx PADDLE-SCALE) BALL-RADIUS-PREVIEW BALL-RADIUS-PREVIEW))
(define COLOR-OPPONENT-EMITTED (emitted 100 60 10 0.03))
(define COLOR-PLAYER-EMITTED (emitted "plum" 2))

(: get-group : Pict3D (Listof Tag) -> Pict3D)
(define (get-group pict tags)
  (: mydentity : Pict3D -> Pict3D)
  (define mydentity identity)
  (: results : (Listof Pict3D))
  (define results (map-group pict tags mydentity))
  (cond [(empty? results) empty-pict3d]
        [else             (first results)]))

(: on-draw-play : State -> Pict3D)
(define (on-draw-play s)
  (: state-play : (U #f State-Play))
  (define state-play
    (cond [(State-Play? s) s]
          [(State-Pause-Menu? s) (State-Pause-Menu-resume-state s)]
          [else #f]))
  (define drawn (if state-play
      (combine (render-game-play-opponent state-play)
            ;; (render-sample-text state-play)
               (render-game-play-player state-play)
               (render-game-play-ball state-play)
               (render-game-play-hud state-play)
               (render-game-play-lights+camera state-play)
               (render-game-play-arena state-play)
               (render-game-play-arena-bumpers state-play))
      empty-pict3d))
  (when state-play
    (define player (State-Play-player state-play))
    (define score-last-frame (unbox (Player-score-last-frame player)))
    (define score (Player-score player))
    (when (or (not score-last-frame)
              (not (= score score-last-frame)))
      (set-box! (Player-score-last-frame player) score)))
  drawn)

(define tqbf (string-append-immutable
              "the quick brown fox jumps over the lazy dog...\n"
              "i said THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG!\n"
              "abcdefghijklmnopqrstuvwxyz...\n"
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ!\n"
              "aA bB cC dD eE fF gG hH iI jJ kK lL mM nN oO pP qQ rR sS tT uU vV wW xX yY zZ "
              "{one} [two] (three) \"some\" 'thing' \n"
              "012345678910 +=!@$#%^&* ()[]{}<>><<> -—_? a/b\\c| :;., '\" 2+2=5"))

(define cor (string-append-immutable
             "EVERY MORNING I WAKE UP & OPEN PALM SLAM A VHS INTO THE SLOT. "
             "ITS CHRONICLES OF RIDDICK AND RIGHT THEN & THERE I START DOING "
             "THE MOVES ALONGSIDE WITH THE MAIN CHARACTER, RIDDICK. I DO EVERY "
             "MOVE AND I DO EVERY MOVE HARD. MAKIN WHOOSHING SOUNDS WHEN I SLAM "
             "DOWN SOME NECRO BASTARDS OR EVEN WHEN I MESS UP TECHNIQUE. NOT MANY "
             "CAN SAY THEY ESCAPED THE GALAXYS MOST DANGEROUS PRISON. I CAN. I "
             "SAY IT & I SAY IT OUTLOUD EVERYDAY TO PEOPLE IN MY COLLEGE CLASS "
             "AND ALL THEY DO IS PROVE PEOPLE IN COLLEGE CLASS CAN STILL BE "
             "IMMATURE JEKRS. AND IVE LEARNED ALL THE LINES AND IVE LEARNED HOW "
             "TO MAKE MYSELF & MY APARTMENT LESS LONELY BY SHOUTING EM ALL. 2 "
             "HOURS INCLUDING WIND DOWN EVERY MORNIng"))

(define wop (string-append-immutable
             "Why you have to do it like that?\n"
             "Got these ladies checkin' they bags\n"
             "Lookin' like they breakin' they back—breakin' they back\n"
             "Takin' it back-back-back wait\n"
             "Now I'm checkin' shawty with the microbraids (Braids)\n"
             "Hotter than a baby in a microwave\n"
             "Look at how she woppin', man, she might get paid\n"
             "She wop, wop, wop a little harder\n"
             "Look at how she wop, she a party starter\n"
             "I hope that security remembered to card her\n"
             "'Cause I ain't finna wop, but with you I'll holler\n"
             "Said I never drop dime, but I drop you a dollar\n"
             "Told her give me your number and maybe I call her\n"
             "She said that she knew me and she know I'm a baller\n"))

(: render-arena-bumper Pict3D)
(define render-arena-bumper
  (combine (with-emitted (emitted "oldlace" 2.0) (cylinder origin 1))
           (for/list : (Listof Pict3D) ([z (in-range 0 10)])
             (light (pos 0.0 0.0 (/ (exact->inexact z) 9.0))
                    (emitted "oldlace" 4.0)))))

(: render-sample-text : State-Play -> Pict3D)
(define (render-sample-text s)
  (combine
    empty-pict3d
    (render-sample-text-cor s)
    ; (render-sample-text-smol s "s")
    ; (render-sample-text-tqbf s)
    ; (render-sample-text-wop s)
    ))

(: render-sample-text-cor : State-Play -> Pict3D)
(define (render-sample-text-cor s)
  (define t (State-t s))
  (parameterize
      ([current-emitted (emitted 0.5 0.7 1.0 2.0)])
     (transform
      (text cor
            #:wrap 9.0
            #:onchar (get-on-char s 'jiggle))
      (affine-compose
       (move-x (+ 9.0 (* t -0.0002)))
       (rotate-y -90.0)
       (move-z 6.0)
       (move-y -0.8)
       (rotate-x -90.0)
       (rotate-y -90.0)
       (scale 0.18)))))

(: render-sample-text-smol : State-Play String -> Pict3D)
(define (render-sample-text-smol s str)
  (parameterize
      ([current-emitted (emitted "pink" 1.0)])
    (transform
     (text str #:onchar (get-on-char s 'jiggle))
     (affine-compose
      (position-screen-space-relative s 0.0 -3.5 0.6)
      (scale 0.8)))))

(: render-sample-text-tqbf : State-Play -> Pict3D)
(define (render-sample-text-tqbf s)
  (parameterize
      ([current-emitted (emitted 1.0 0.9 0.0 1.1)])
    (transform
     (text tqbf
           #:wrap 15.0
           #:onchar (get-on-char s 'jiggle))
     (affine-compose
      (position-screen-space-relative s -0.6 -0.85 0.8)
      (scale 0.05)))))

(: render-sample-text-wop : State-Play -> Pict3D)
(define (render-sample-text-wop s)
  (parameterize
      ([current-emitted (emitted 1.0 0.9 0.0 1.5)])
    (transform
     (text wop
           #:wrap 40.0
           #:spacing-paragraph 0.0
           #:onchar (get-on-char s 'jiggle))
     (affine-compose
      (position-screen-space-relative s -0.8 -0.85 0.7)
      (scale 0.04)))))

(: render-game-play-arena : State-Play -> Pict3D)
(define (render-game-play-arena s)
  (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.6 0.6 0.6)])
    (transform (combine (render-game-play-arena-tunnel s)
                        (render-game-play-arena-sun s)
                        (render-game-play-arena-sky s))
               (affine-compose (scale-x 10)
                               (move-z -0.1)
                               (rotate-x -90)
                               (rotate-y 90)))))

(: render-game-play-arena-sky : State-Play -> Pict3D)
(define (render-game-play-arena-sky s)
  (with-emitted (emitted 0 0 0.01 20)
    (cylinder origin 1
              #:arc (arc 180 360)
              #:inside? #t
              #:top-cap? #f
              #:bottom-cap? #t
              #:start-cap? #f
              #:end-cap? #f
              #:outer-wall? #f)))

(: render-game-play-arena-sun : State-Play -> Pict3D)
(define (render-game-play-arena-sun s)
  (transform (light origin (emitted "goldenrod" 0.0001))
             (affine-compose
              (rotate-z (- 135.0 (* 0.003 (State-t s))))
              (move-y -0.5)
              (move-z -0.99)
              (scale 40))))

(: render-game-play-arena-tunnel : State-Play -> Pict3D)
(define (render-game-play-arena-tunnel s)
  (cylinder origin 1
            #:arc (arc 180 360)
            #:inside? #t
            #:top-cap? #f
            #:bottom-cap? #f))

(: render-game-play-arena-bumpers : State-Play -> Pict3D)
(define (render-game-play-arena-bumpers s)
  (define ball (State-Play-ball s))
  (define y+ (+ WALL-Y BALL-RADIUS))
  (define y- (- 0.0 WALL-Y BALL-RADIUS))
  (define ball-pos (Ball-pos ball))
  (define oldlace-preview (emitted "oldlace" 1.0))
  (define should-draw-guides (get-pref-boolean 'gameplay-guides (λ () #f)))
  (define guides
    (if (not should-draw-guides)
        empty-pict3d
        (combine
         ; ball x-position
         (with-emitted oldlace-preview
           (sphere (pos (pos-x ball-pos) y+ 0.0) BALL-RADIUS-PREVIEW))
         (with-emitted oldlace-preview
           (sphere (pos (pos-x ball-pos) y- 0.0) BALL-RADIUS-PREVIEW))

         (freeze
          (combine
           ; opponent x-position
           (parameterize
               ([current-material (material #:ambient 0.0
                                            #:diffuse 0.0
                                            #:specular 0.0
                                            #:roughness 0.3)]
                [current-emitted oldlace-preview])
             (combine
              (rectangle (pos OPPONENT-X y+ 0.0) PADDLE-SCALE-PREVIEW)
              (rectangle (pos OPPONENT-X y- 0.0) PADDLE-SCALE-PREVIEW)
              (rectangle (pos PLAYER-X y+ 0.0) PADDLE-SCALE-PREVIEW)
              (rectangle (pos PLAYER-X y- 0.0) PADDLE-SCALE-PREVIEW)))
           )))))

  (combine
   guides
   (freeze
    (combine
     ; actual bumpers
     (transform render-arena-bumper
                (affine-compose
                 (move-y y+)
                 (scale (dir 10 1/256 1/256))))
     (transform render-arena-bumper
                (affine-compose
                 (move-y y-)
                 (scale (dir 10 1/256 1/256))))))))

(: render-game-play-ball : State-Play -> Pict3D)
(define (render-game-play-ball s)
  (define ball (State-Play-ball s))
  (combine
   (render-game-play-ball-predicted s)
   (light
    (Ball-pos ball)
    (emitted "oldlace" 0.1)
    #:range 1)
   (with-emitted (emitted "oldlace" 1.5)
     (sphere (Ball-pos ball) BALL-RADIUS))))

(: render-game-play-ball-predicted : State-Play -> Pict3D)
(define (render-game-play-ball-predicted s)
  (combine (for/list : (Listof Pict3D)
    ([predicted-pos (State-Play-ball-predicted-pos-ends s)])
    (define is-player (positive? (pos-x predicted-pos)))
    (cond [(null? predicted-pos) empty-pict3d]
          [else (with-emitted
                    (emitted "oldlace" 0.3)
                  (transform
                   (sphere origin BALL-RADIUS)
                   (affine-compose
                    (move (pos- predicted-pos origin))
                    (rotate-y (if is-player 90.0 -90.0)))))]))))

(: render-game-play-hud : State-Play -> Pict3D)
(define (render-game-play-hud s)
  (define player (State-Play-player s))
  (define score (Player-score player))
  (define score-last-frame (unbox (Player-score-last-frame player)))
  (define pict-last (unbox (State-pict-last s)))
  (define cached (and pict-last
                      score-last-frame
                      (= score score-last-frame)
                      (get-group pict-last '(hud))))
  (or cached
      (group (combine
              ; player score
              (transform (render-player-score score)
                         (position-screen-space-pixels s 100.0 100.0 1.0))
              ; player lives
              (transform
               (parameterize ([current-emitted COLOR-PLAYER-EMITTED])
                 (combine
                  (for/list : (Listof Pict3D)
                    ([n (range 0 (Player-lives player))])
                    (cube (pos (* (exact->inexact n) -0.08) 0.0 0.0) 0.02))))
               (position-screen-space-pixels s -100.0 100.0 1.0 #:wrap? #t)))
             'hud)))

(: render-game-play-opponent : State-Play -> Pict3D)
(define (render-game-play-opponent s)
  (parameterize
      ([current-material (material #:ambient 0.0
                                   #:diffuse 0.0
                                   #:specular 0.0
                                   #:roughness 0.3)]
       [current-emitted COLOR-OPPONENT-EMITTED])
    (rectangle (pos OPPONENT-X (Opponent-y (State-Play-opponent s)) 0.0)
               PADDLE-SCALE)))

(: render-game-play-player : State-Play -> Pict3D)
(define (render-game-play-player s)
  (parameterize
      ([current-material (material #:ambient 0.0
                                   #:diffuse 0.0
                                   #:specular 0.6
                                   #:roughness 0.3)]
       [current-emitted COLOR-PLAYER-EMITTED])
    (rectangle (pos PLAYER-X (Player-y (State-Play-player s)) 0.0)
               PADDLE-SCALE)))

(: render-game-play-lights+camera : State-Play -> Pict3D)
(define (render-game-play-lights+camera s)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (camera s)))