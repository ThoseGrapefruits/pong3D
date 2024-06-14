#lang typed/racket/base

(require pict3d
         "./camera.rkt"
         "./menu.rkt"
         "./on-char-jiggle.rkt"
         "./position-screen-space.rkt"
         "./text.rkt"
         "../state/menu.rkt"
         "../state/state.rkt")

(provide on-draw-main-menu)

(: on-draw-main-menu : State -> Pict3D)
(define (on-draw-main-menu s)
  (cond
    [(State-Main-Menu? s)
     (combine
      (camera s)
      (render-background s)
      (render-menu s))]
    [else empty-pict3d]))

(: render-background : State-Main-Menu -> Pict3D)
(define (render-background s)
  (transform
   (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.5 0.5 0.5 1.0)])
     (rectangle origin (dir 1.0 1.0 0.01)))
   (affine-compose
    (position-screen-space-relative s 0.0 0.0 1.0))))

; TODO this needs to render the structure above the current active menu so the
; trace paths are correct
(: render-menu : State-Main-Menu -> Pict3D)
(define (render-menu s)
  (define menu (State-Main-Menu-menu s))
  (define active-path (unbox (Menu-active-path menu)))
  (define active-menu-item (and active-path (Menu-ref menu active-path)))
  (cond [(not active-menu-item) empty-pict3d]
        [else
         (combine
          (render-menu-header s menu active-menu-item)
          (render-menu-items  s menu active-menu-item))]))

(define TEXT-SCALE 0.06)

(: render-menu-items : State-Main-Menu Menu Menu-Item -> Pict3D)
(define (render-menu-items s menu menu-item)
  (define parents (get-parents menu-item))
  (define parent (and (not (null? parents)) (car parents)))
  (define parent-usable (if (Menu-Item? parent) parent menu-item))
  (define siblings (Menu-Item-children parent-usable))
  (define rendered (parameterize
      ([current-emitted (emitted "oldlace" 2.0)])
    (group
     (combine
      ; header
      (transform
       (text (Menu-Item-label parent-usable)
             #:onchar (get-on-char-jiggle s))
       (affine-compose
        (position-screen-space-relative s -0.8 -1.2 0.6)
        (scale (* 2.0 TEXT-SCALE))))
      ; items
      (for/list : (Listof Pict3D)
        ([menu-item siblings]
         [i (in-range 0 (length siblings))])
        (define label-rendered (text (Menu-Item-label menu-item)) )
        (define-values (bound1 bound2) (bounding-rectangle label-rendered))
        (define-values (emitted-text emitted-bg) (Menu-Item-color s menu menu-item))

        (transform
         (group (combine
                 (parameterize ([current-emitted emitted-text])
                   (text (Menu-Item-label menu-item)
                         #:onchar (get-on-char-jiggle s)))
                 (parameterize ([current-emitted emitted-bg])
                   (if (and bound1 bound2)
                       (rectangle (pos+ bound1 (dir -0.2 -0.2 0.2))
                                  (pos+ bound2 (dir  0.2  0.2 0.2)))
                       empty-pict3d)))
                (Menu-Item-tag menu-item))
         (affine-compose
          (position-screen-space-relative s -0.8 (+ -0.55 (* (exact->inexact i) 0.25)) 0.6)
          (scale (* (Menu-Item-scale s menu menu-item) TEXT-SCALE))))))
     (Menu-Item-tag parent-usable))))
     (foldr (λ ([parent : Menu-Item] [pict : Pict3D])
              (group pict (Menu-Item-tag parent)))
            rendered
            parents))

;; PARENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-parents (->* (Menu-Item) ((Listof Menu-Item)) (Listof Menu-Item)))
(define (get-parents menu-item [results '()])
  (define parent (unbox (Menu-Item-parent menu-item)))
  (cond [(Menu-Item? parent) (get-parents parent (cons parent results))]
        [else                results                                   ]))

(: render-menu-header : State-Main-Menu Menu Menu-Item -> Pict3D)
(define (render-menu-header s menu menu-item)
  (render-menu-header-parents menu-item))

(: render-menu-header-parents : Menu-Item -> Pict3D)
(define (render-menu-header-parents menu-item)
  (define parents (get-parents menu-item))
  (define index-max (- (length parents) 1))
  (combine (for/list : (Listof Pict3D)
             ([index (in-range 0 (- (length parents) 1))])
             (define index-offset (- index-max index))
             (cube (pos+ origin -y (- 1.0 (exact->inexact index-offset))) 0.1))))
