#lang typed/racket/base

(require pict3d
         "./state-base.rkt")

(provide (struct-out Menu)
         make-Menu
         (struct-out Menu-Item)
         make-Menu-Item
         Menu-handle-activate)

(: make-Menu : Menu-Item -> Menu)
(define (make-Menu root)
  (define menu
    (Menu (box root)   ; active-item
          (box 0.0)    ; active-since
          (box #f)     ; hovered-item
          (box 0.0)    ; hovered-since
          root)) ; root
  (Menu-update-parents! menu)
  menu)

(: menu-item->pair : Menu-Item -> (Pairof Tag Menu-Item))
(define (menu-item->pair item)
  (cons (Menu-Item-tag item) item))

(: make-Menu-Item :
   [#:children (Listof Menu-Item)]
   [#:draw (-> Menu-Item Pict3D)]
   #:label String
   #:tag Tag
   -> Menu-Item)
(define (make-Menu-Item #:children [children '()]
                        #:draw [draw (λ (_) empty-pict3d)]
                        #:label label
                        #:tag tag)
  (Menu-Item
   children             ; children
   (make-hash           ; children-map
    (map menu-item->pair children))
   draw                 ; draw
   label                ; label
   (box #f)
   tag))

(struct Menu
   ; The active/focused menu item. Defaults to root.
  ([active-item   : (Boxof Menu-Item)]
   ; When the active menu item started being active
   [active-since  : (Boxof Flonum)]

   ; The hovered menu item, if any.
   [hovered-item  : (Boxof (U #f Menu-Item))]
   ; When the hovered menu item started being hovered.
   [hovered-since : (Boxof Flonum)]

   ; The root menu item, whose parent is this Menu
   [root          : Menu-Item]))

(struct Menu-Item
   ; The direct descendents of this Menu-Item in the tree.
  ([children : (Listof Menu-Item)]
   ; The direct descendants of this Menu-Item, organized by tag for fast access.
   [children-map : (HashTable Tag Menu-Item)]
   [draw : (-> Menu-Item Pict3D)]
   ; The text label displayed on this menu item.
   [label : String]
   ; The direct parent of this Menu-Item in the tree. If it is a Menu, this
   ; Menu-Item is the root of a Menu.
   [parent : (Boxof (U #f Menu Menu-Item))]
   ; The tag for this Menu-Item. Used to look up menu items from raytraces.
   [tag : Tag]))

(: Menu-handle-activate : State Menu -> Menu)
(define (Menu-handle-activate s m) m)

(: Menu-update-parents! : Menu -> Void)
(define (Menu-update-parents! menu)
  (define root (Menu-root menu))
   (set-box! (Menu-Item-parent root) menu)
   (Menu-Item-update-parents! root))

(: Menu-Item-update-parents! : Menu-Item -> Void)
(define (Menu-Item-update-parents! parent)
   (for ([child (Menu-Item-children parent)])
     (set-box! (Menu-Item-parent child) parent)
     (Menu-Item-update-parents! child))
)
