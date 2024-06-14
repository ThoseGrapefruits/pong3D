#lang typed/racket/base

(require pict3d
         racket/list
         "../util/tag.rkt")

(provide (struct-out Menu)
         make-Menu
         (struct-out Menu-Item)
         make-Menu-Item
         Menu-Item-active-transition!
         Menu-ref
         Path-Source)

;; STRUCTS & TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Path-Source (U 'active 'hover))

(struct Menu
   ; The active/focused menu item. Defaults to path to root.
  ([active-path   : (Boxof (U #f Tags))]

   ; The hovered menu item, if any.
   [hovered-path  : (Boxof (U #f Tags))]

   ; The root menu item, whose parent is this Menu
   [root          : Menu-Item])
  #:transparent)

(struct Menu-Item
   ; When this Menu-Item started being active or hovered.
  ([active-start : (Boxof (U #f Flonum))]
   ; When this Menu-Item stopped being active or hovered.
   [active-end : (Boxof (U #f Flonum))]
   ; The direct descendents of this Menu-Item in the tree.
   [children : (Listof Menu-Item)]
   ; The direct descendants of this Menu-Item, organized by tag for fast access.
   [children-map : (HashTable Tag Menu-Item)]
   ; The emitted color for this menu item when it's active.
   [color-active : Emitted]
   ; The custom draw function, if any.
   [draw : (-> Menu-Item Pict3D)]
   ; The text label displayed on this menu item.
   [label : String]
   ; The direct parent of this Menu-Item in the tree. If it is a Menu, this
   ; Menu-Item is the root of a Menu.
   [parent : (Boxof (U #f Menu Menu-Item))]
   ; The tag for this Menu-Item. Used to look up menu items from raytraces.
   [tag : Tag])
   #:property prop:custom-write (λ (menu-item out mode)
                                  (fprintf out "#0=#(struct:Menu-Item )"))
  #:transparent)

(: Menu-Item-custom-write : Menu-Item Output-Port (U Boolean 0 1) Positive-Integer -> Void)
(define (Menu-Item-custom-write menu-item out mode [depth 0])
  (fprintf out "~s#0=#(struct:Menu-Item )" (build-string depth (λ (_) #\space))))

;; CONSTRUCTORS & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-Menu : Menu-Item -> Menu)
(define (make-Menu root)
  (define root-tag (Menu-Item-tag root))
  (define menu
    (Menu (box (list root-tag)) ; active-path
          (box #f)              ; hovered-path
          root))                ; root
  (Menu-update-parents! menu)
  menu)

(: Menu-Item->Pair : Menu-Item -> (Pairof Tag Menu-Item))
(define (Menu-Item->Pair item)
  (cons (Menu-Item-tag item) item))

(: Menu-ref : Menu (Listof Tag) -> (U #f Menu-Item))
(define (Menu-ref menu path)
  (define root (Menu-root menu))
  (define root-tag (Menu-Item-tag root))
  (cond [(empty? path) #f]
        [(not (equal? root-tag (first path)))
         (error 'Menu-ref
                "got path starting at ~s for menu ~s"
                (first path) root-tag)]
        [else
         (foldl (λ ([tag : Tag] [mi : (U #f Menu-Item)])
                  (and mi (hash-ref (Menu-Item-children-map mi) tag #f)))
                (Menu-root menu)
                (rest path))]))

(: make-Menu-Item :
   [#:children (Listof Menu-Item)]
   [#:draw (-> Menu-Item Pict3D)]
   [#:color-active Emitted]
   #:label String
   #:tag Tag
   -> Menu-Item)
(define (make-Menu-Item #:children [children '()]
                        #:draw [draw (λ (_) empty-pict3d)]
                        #:color-active [color-active (emitted "oldlace" 1.5)]
                        #:label label
                        #:tag tag)
  (Menu-Item (box 0.0)    ; active-start
             (box 0.0)    ; active-end
             children     ; children
             (make-hash   ; children-map
              (map Menu-Item->Pair children))
             color-active ; color-active
             draw         ; draw
             label        ; label
             (box #f)     ; parent
             tag))        ; tag

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

(: Menu-Item-active-transition! : (U #f Menu-Item) Menu-Item Flonum -> Void)
(define (Menu-Item-active-transition! old new t)
  ; TODO use existing start and end values on new if within ANIMATION-TIME.
  ; Needs math that I can't think about right now, probably should draw it out.
  (and old (set-box! (Menu-Item-active-end   old) t))
  (set-box! (Menu-Item-active-start new) t)
  (set-box! (Menu-Item-active-end   new) #f))
