#lang typed/racket/base

(require pict3d
         racket/list)

(provide (struct-out Menu)
         make-Menu
         (struct-out Menu-Item)
         make-Menu-Item
         Menu-ref)

;; STRUCTS & TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Menu
   ; The active/focused menu item. Defaults to path to root.
  ([active-path   : (Boxof (Listof Tag))]
   ; When the active menu item started being active
   [active-since  : (Boxof Flonum)]

   ; The hovered menu item, if any.
   [hovered-path  : (Boxof (U #f (Listof Tag)))]
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


;; CONSTRUCTORS & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-Menu : Menu-Item -> Menu)
(define (make-Menu root)
  (define menu
    (Menu (box (list (Menu-Item-tag root))) ; active-path
          (box 0.0)    ; active-since
          (box #f)     ; hovered-path
          (box 0.0)    ; hovered-since
          root)) ; root
  (Menu-update-parents! menu)
  menu)

(: Menu-Item->Pair : Menu-Item -> (Pairof Tag Menu-Item))
(define (Menu-Item->Pair item)
  (cons (Menu-Item-tag item) item))

(: Menu-ref : Menu (Listof Tag) -> (U #f Menu-Item))
(define (Menu-ref menu path)
  (define root (Menu-root menu))
  (define root-tag (Menu-Item-tag root))
  (cond [(not (equal? root-tag (first path)))
         (error 'Menu-ref
                "got path starting at ~s for menu ~s"
                root-tag (first path))]
        [else
         (foldl (λ ([tag : Tag] [mi : (U #f Menu-Item)])
                  (and mi (hash-ref (Menu-Item-children-map mi) tag #f)))
                (Menu-root menu)
                (rest path))]))

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
    (map Menu-Item->Pair children))
   draw                 ; draw
   label                ; label
   (box #f)
   tag))

;; UTIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: Menu-update-parents! : Menu -> Void)
(define (Menu-update-parents! menu)
  (define root (Menu-root menu))
   (set-box! (Menu-Item-parent root) menu)
   (Menu-Item-update-parents! root))

(: Menu-Item-update-parents! : Menu-Item -> Void)
(define (Menu-Item-update-parents! parent)
   (for ([child (Menu-Item-children parent)])
     (set-box! (Menu-Item-parent child) parent)
     (Menu-Item-update-parents! child)))
