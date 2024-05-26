#lang typed/racket/base

(require pict3d
         "./state-base.rkt")

(provide (struct-out Menu)
         (struct-out Menu-Item)
         Menu-handle-activate)

(struct Menu
   ; The active/focused menu item. Defaults to Menu-root.
  ([active-item   : Menu-Item]
   ; When the active menu item started being active
   [active-since  : Flonum]

   ; The hovered menu item, if any.
   [hovered-item  : (U #f Menu-Item)]
   ; When the hovered menu item started being hovered.
   [hovered-since : Flonum]

   ; The root menu item, whose parent is this Menu
   [root          : Menu-Item]))

(struct Menu-Item
   ; The direct descendents of this Menu-Item in the tree.
  ([children : (Listof Menu-Item)]
   ; The direct descendants of this Menu-Item, organized by tag for fast access.
   [children-map : (Weak-HashTable Tag Menu-Item)]
   [draw : (-> Pict3D)]
   ; The text label displayed on this menu item.
   [label : String]
   ; The direct parent of this Menu-Item in the tree. If it is a Menu, this
   ; Menu-Item is the root of a Menu.
   [parent : (U Menu Menu-Item)]
   ; The tag for this Menu-Item. Used to look up menu items from raytraces.
   [tag : Tag]))

(: Menu-handle-activate : State Menu -> Menu)
(define (Menu-handle-activate s m) m)
