#lang typed/racket/base

(require pict3d
         "./camera.rkt"
         "./on-char-jiggle.rkt"
         "./position-screen-space.rkt"
         "./text.rkt"
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

(: render-menu : State-Main-Menu -> Pict3D)
(define (render-menu s)
  (define menu (State-Main-Menu-menu s))
  (define root (Menu-root menu))
  (define active (unbox (Menu-active-item menu)))
  (combine
    (render-menu-header      s active)
    (render-menu-item-active s active)))

(: render-menu-item-active : State-Main-Menu Menu-Item -> Pict3D)
(define (render-menu-item-active s menu-item)
  (define parent (unbox (Menu-Item-parent menu-item)))
  (define parent-usable (if (Menu-Item? parent) parent menu-item))
  (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-emitted (emitted "oldlace" 2.0)])
    (combine
     ; header
     (transform
      (combine (text (Menu-Item-label menu-item)) 
               (cube origin 0.1))
      (affine-compose
       (position-screen-space-relative s -0.8 -0.8 0.6)
       (scale 0.06)))
     (for/list : (Listof Pict3D)
       ([menu-item (Menu-Item-children parent-usable)])
       (text (Menu-Item-label menu-item))))))

; PARENTS

(: render-menu-header : State-Main-Menu Menu-Item -> Pict3D)
(define (render-menu-header s mi)
  (define-values (pict _) (render-menu-header-parents mi 0))
  pict)

(: render-menu-header-parents : (U Menu Menu-Item) Integer ->
   (Values Pict3D Integer))
(define (render-menu-header-parents mi index)
  (define parent (and (Menu-Item? mi) (unbox (Menu-Item-parent mi))))
  (define index-new (+ index 1))
  (define (draw [index-offset : Integer])
    (cube (pos+ origin -y (- 1.0 (exact->inexact index-offset))) 0.1))
  (cond [parent
         (define-values (pict-parents index-max)
           (render-menu-header-parents parent index-new))
         (define index-offset (- index-max index))
         (values (combine (draw index-offset) pict-parents) index-max)]
        [else
         (values (draw 0) index-new)]))
