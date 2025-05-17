#lang typed/racket/base

(require
  (only-in pict3d
           Emitted
           Pict3D
           affine-compose
           bounding-rectangle
           combine
           cube
           current-emitted
           dir
           empty-pict3d
           emitted
           group
           origin
           pos+
           rectangle
           scale
           tessellate
           transform
           -y)
  (only-in racket/match match-define)
  "../state/menu.rkt"
  "../state/menu-item-types.rkt"
  "../state/state.rkt"
  "../util/number.rkt"
  "./on-char-jiggle.rkt"
  "./position-screen-space.rkt"
  "./text.rkt")

(provide ANIMATION-TIME
         EMITTED-MENU-ITEM-TEXT
         EMITTED-MENU-ITEM-BACKGROUND
         group-within
         Menu-Item-color
         Menu-Item-parents
         Menu-Item-scale
         render-menu
         State-Menu)

(: SCALE-MAX : Flonum)
(define SCALE-MAX 0.05)

(: ANIMATION-TIME : Flonum)
(define ANIMATION-TIME 140.0)

(define EMITTED-MENU-ITEM-TEXT        (emitted "oldlace" 1.5))
(define EMITTED-MENU-ITEM-TEXT-ACTIVE (emitted "oldlace" 0.1))
(define EMITTED-MENU-ITEM-BACKGROUND  (emitted "oldlace" 0.0))

; Recursively groups the given pict based on the tags in the given list of
; parents (produced by Menu-Item-parents).
(: group-within : Pict3D (Listof Menu-Item) -> Pict3D)
(define (group-within pict parents)
  (cond [(null? parents) pict]
        [else (group-within (group (combine pict empty-pict3d) (Menu-Item-tag (car parents)))
                            (cdr parents))]))

; Returns the text and background color of a menu item
(: Menu-Item-color : State Menu Menu-Item -> (Values Emitted Emitted))
(define (Menu-Item-color s menu menu-item)
  (define active-color (Menu-Item-color-active menu-item))
  (define active-start (unbox (Menu-Item-active-start menu-item)))
  (define active-end   (unbox (Menu-Item-active-end   menu-item)))
  (define t (State-t s))

  (: time-since : Flonum)
  (define time-since
    (cond [active-end
           ; If Menu-Item stopped being active, we scale back to default.
           (max 0.0            (- ANIMATION-TIME (- t active-end)))]
          [active-start
           (min ANIMATION-TIME (- t active-start))]
          [else 0.0]))

  ; First 2 branches are (theoretical) optimizations to only do lerping when we are mid-transition.
  (cond [(= time-since 0.0)
         (values EMITTED-MENU-ITEM-TEXT        EMITTED-MENU-ITEM-BACKGROUND)]
        [(= time-since ANIMATION-TIME)
         (values EMITTED-MENU-ITEM-TEXT-ACTIVE active-color)]
        [else
         (define ratio (/ time-since ANIMATION-TIME))
         (match-define (emitted text-0r text-0g text-0b text-0a) EMITTED-MENU-ITEM-TEXT)
         (match-define (emitted text-1r text-1g text-1b text-1a) EMITTED-MENU-ITEM-TEXT-ACTIVE)
         (match-define (emitted bg-0r bg-0g bg-0b bg-0a) EMITTED-MENU-ITEM-BACKGROUND)
         (match-define (emitted bg-1r bg-1g bg-1b bg-1a) active-color)
         (values (emitted (lerp text-0r text-1r ratio)
                          (lerp text-0g text-1g ratio)
                          (lerp text-0b text-1b ratio)
                          (lerp text-0a text-1a ratio))
                 (emitted (lerp   bg-0r   bg-1r ratio)
                          (lerp   bg-0g   bg-1g ratio)
                          (lerp   bg-0b   bg-1b ratio)
                          (lerp   bg-0a   bg-1a ratio)))]))

(: Menu-Item-parents : Menu-Item -> (Listof Menu-Item))
(define (Menu-Item-parents menu-item)
  (define parent (unbox (Menu-Item-parent menu-item)))
  (cond [(Menu-Item? parent) (cons parent (Menu-Item-parents parent))]
        [else                '()                                     ]))

(: Menu-Item-scale : State Menu Menu-Item -> Flonum)
(define (Menu-Item-scale s menu menu-item)
  (define active-start (unbox (Menu-Item-active-start menu-item)))
  (define active-end   (unbox (Menu-Item-active-end   menu-item)))
  (define t (State-t s))

  (: time-since : Flonum)
  (define time-since
    (cond [active-end
           ; If Menu-Item stopped being active, we scale back to 1.0.
           (max 0.0            (- ANIMATION-TIME (- t active-end)))]
          [active-start
           (min ANIMATION-TIME (- t active-start))]
          [else 0.0]))

  (+ 1.0 (* SCALE-MAX (/ time-since ANIMATION-TIME))))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: render-menu : State-Menu Menu -> Pict3D)
(define (render-menu s menu)
  (define active-path (unbox (Menu-active-path menu)))
  (define active-menu-item (and active-path (Menu-ref menu active-path)))
  (cond [(not active-menu-item) empty-pict3d]
        [else
         (define parents (Menu-Item-parents active-menu-item))
         (define rendered
           (combine
            (render-menu-header s menu active-menu-item parents)
            (render-menu-items  s menu active-menu-item parents)))
         ; render-menu-items will add their own direct parent, if it exists,
         ; since it can be smarter about whether it's the root node or not.
         (if (null? parents) rendered (group-within rendered (cdr parents)))]))

(define TEXT-SCALE 0.06)

(: render-menu-item : State-Menu Menu Menu-Item Integer (Listof Menu-Item) -> Pict3D)
(define (render-menu-item s menu menu-item i siblings)
  (define type (Menu-Item-type menu-item))
  (cond [(eq? 'text type)
         (render-menu-item-text   s menu menu-item i siblings type)]
        [(Menu-Item-Type-Slider? type)
         (render-menu-item-slider s menu menu-item i siblings type)]))

(: bounds-cache : (HashTable String Bounds))
(define bounds-cache (make-hasheq))

(: get-bounds : String Pict3D -> Bounds)
(define (get-bounds label label-rendered)
  (hash-ref
   bounds-cache label
   (λ ()
     (define-values (b1 b2) (bounding-rectangle (tessellate label-rendered)))
     (define bounds : Bounds (assert (cons b1 b2) bounds?))
     (hash-set! bounds-cache label bounds)
     bounds)))

(: render-menu-item-slider :
   State-Menu Menu Menu-Item Integer (Listof Menu-Item)
   Menu-Item-Type-Slider -> Pict3D)
(define (render-menu-item-slider s menu menu-item i siblings type)
  (define label (Menu-Item-label menu-item))
  (: value-getter : -> Any)
  (define value-getter
  (cond [(Menu-Item-Type-Slider-Flonum? type)
         (Menu-Item-Type-Slider-Flonum-value-getter type)]
        [else
         (λ () "")]))
  (define label-wrapped (format "~a <~a>" label (value-getter)))
  (define-values (emitted-text emitted-bg) (Menu-Item-color s menu menu-item))
  (define label-rendered
    (parameterize ([current-emitted emitted-text])
      (text label-wrapped #:onchar (get-on-char s 'wave))))

  (define bounds (get-bounds label-wrapped label-rendered))
  (match-define (cons bound1 bound2) bounds)

  (define y (+ -0.55 (* (exact->inexact i) 0.25)))
  (transform (group (combine
                     label-rendered
                     (parameterize ([current-emitted emitted-bg])
                       (if (and bound1 bound2)
                           (rectangle (pos+ bound1 (dir -0.2 -0.2 0.25))
                                      (pos+ bound2 (dir  0.2  0.2 0.5)))
                           empty-pict3d)))
                    (Menu-Item-tag menu-item))
             (affine-compose (position-screen-space-relative s -0.8 y 0.6)
                             (scale (* (Menu-Item-scale s menu menu-item)
                                       TEXT-SCALE)))))

(: render-menu-item-text :
   State-Menu Menu Menu-Item Integer (Listof Menu-Item)
   'text -> Pict3D)
(define (render-menu-item-text s menu menu-item i siblings type)
  (define-values (emitted-text emitted-bg) (Menu-Item-color s menu menu-item))
  (define label (Menu-Item-label menu-item))
  (define label-rendered
    (parameterize ([current-emitted emitted-text])
      (text label #:onchar (get-on-char s 'wave))))
  (define bounds (get-bounds label label-rendered))
  (match-define (cons bound1 bound2) bounds)

  (transform
   (group (combine
           label-rendered
           (parameterize ([current-emitted emitted-bg])
             (if (and bound1 bound2)
                 (rectangle (pos+ bound1 (dir -0.25 -0.25 0.25))
                            (pos+ bound2 (dir  0.25  0.25 0.5)))
                 empty-pict3d)))
          (Menu-Item-tag menu-item))
   (affine-compose
    (position-screen-space-relative s -0.8 (+ -0.55 (* (exact->inexact i) 0.25)) 0.6)
    (scale (* (Menu-Item-scale s menu menu-item) TEXT-SCALE)))))

(: render-menu-items : State-Menu Menu Menu-Item (Listof Menu-Item) -> Pict3D)
(define (render-menu-items s menu menu-item parents)
  (define parent (and (not (null? parents)) (car parents)))
  (define parent-usable (if (Menu-Item? parent) parent menu-item))
  (define siblings (Menu-Item-children parent-usable))
  (define rendered
    (parameterize
        ([current-emitted (emitted "oldlace" 2.0)])
      (combine
       ; header
       (transform
        (text (Menu-Item-label parent-usable)
              #:onchar (get-on-char s 'wave))
        (affine-compose
         (position-screen-space-relative s -0.8 -1.2 0.6)
         (scale (* 2.0 TEXT-SCALE))))
       ; items
       (for/list : (Listof Pict3D)
         ([menu-item siblings]
          [i (in-range 0 (length siblings))])
         (render-menu-item s menu menu-item i siblings)))))
  (group rendered (Menu-Item-tag parent-usable)))

(: render-menu-header : State-Menu Menu Menu-Item (Listof Menu-Item) -> Pict3D)
(define (render-menu-header s menu menu-item parents)
  (render-menu-header-parents menu-item parents))

(: render-menu-header-parents : Menu-Item (Listof Menu-Item) -> Pict3D)
(define (render-menu-header-parents menu-item parents)
  (define index-max (- (length parents) 1))
  (combine (for/list : (Listof Pict3D)
             ([index (in-range 0 (- (length parents) 1))])
             (define index-offset (- index-max index))
             (cube (pos+ origin -y (- 1.0 (exact->inexact index-offset))) 0.1))))
