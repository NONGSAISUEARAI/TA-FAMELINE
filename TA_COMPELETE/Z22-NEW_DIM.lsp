; ;;;=========================================================================
;   ;;; Sort a list of 3d elements
;   ;;;=========================================================================
;   ;;; From: "Tony Tanzillo" <tony.tanzillo at caddzone dot com>
;   ;;; Newsgroups: autodesk.autocad.customization
;   ;;; Subject: Re: How can I sort a list of 3d points?
;   ;;; Date: Wed, 19 Mar 2003 10:37:20 -0500
;   ;;;
;   ;;; You can use vl-sort, with a comparison function that
;   ;;; weights the ordinates in whatever way you want.
;   ;;;
;   ;;; Here's an example that gives the greatest weight
;   ;;; to the X ordinate, and the least weight to the Z
;   ;;; ordinate:

;   ;;; sort on three keys (x, y, and z)
;   ;|
;   (defun-q compare-points (a b / fuzz)
;       (setq fuzz 1.0e-6) ;; comparison precision
;       (if (equal (car a) (car b) fuzz)
;         (if (equal (cadr a) (cadr b) fuzz)
;             (> (caddr a) (caddr b))
;             (> (cadr a) (cadr b))
;         )
;         (> (car a) (car b))
;       )
;   )

;   ;;; example  (vl-sort <list-of-points> 'compare-points)

;   ;;; If you search this newsgroup, you'll find a much
;   ;;; more powerful sorting function along with a good
;   ;;; discussion on why (vl-sort) can be very dangerous.
;   ;;; For that reason, I suggest you replace the built-in
;   ;;; vl-sort with this:

;   (defun-q vl-sort (lst func)
;     (mapcar
;       '(lambda (x) (nth x lst))
;       (vl-sort-i lst func)
;     )
;   )

;   |;
;   ;;; This will ensure that (vl-sort) does not remove
;   ;;; elements that it sees as equal.

;   ;;;=========================================================================
;   ;;; LSORT.LSP  Copyright 1992-98  Tony Tanzillo  all rights reserved
;   ;;;
;   ;;; ----------------------------------------------------------------
;   ;;; Merging complex list sort
;   ;;;
;   ;;; LSORT.LSP implements a modified version of the classic
;   ;;; merge sort algorithm that sorts arbitrarily-complex lists
;   ;;; using a caller-defined relational predicate function.
;   ;;;
;   ;;;  (lsort <list> <OnCompare>)
;   ;;;
;   ;;;  <OnCompare> is a function that takes two arguments,
;   ;;;  and returns non-nil if the first argument is greater
;   ;;;  than the second, or nil otherwise. The arguments are
;   ;;;  the elements of the list to be sorted. This argument
;   ;;;  must be quoted.
;   ;;;
;   ;;;  The default sort order is descending. To change the
;   ;;;  sort order to ascending, the <OnCompare> function can
;   ;;;  return the logical complement (not) of it's result.
;   ;;;
;   ;;; Examples:
;   ;;;
;   ;;;  1.  Sort a list of coordinates on the Y-component:
;   ;;;
;   ;;;      Assume unsorted data is in 'UNSORTED
;   ;;;
;   ;;;    (setq sorted
;   ;;;       (lsort unsorted
;   ;;;         '(lambda (a b)
;   ;;;             (> (cadr a) (cadr b))
;   ;;;          )
;   ;;;       )
;   ;;;    )
;   ;;;
;   ;;;
;   ;;;  2.  Sort a list of entity names by layer:
;   ;;;
;   ;;;    (setq sorted
;   ;;;       (lsort unsorted
;   ;;;         '(lambda (e1 e2)
;   ;;;            (> (cdr (assoc 8 (entget e1)))
;   ;;;               (cdr (assoc 8 (entget e2)))
;   ;;;            )
;   ;;;          )
;   ;;;       )
;   ;;;    )
;   ;;;
;   ;;;  3.  Sort a list of coordinates on multiple
;   ;;;      keys (first by the X ordinate, and then
;   ;;;      by the Y ordinate):
;   ;;;
;   ;;;     (setq epsilon 1e-6)
;   ;;;
;   ;;;     (defun-q compare-points (p1 p2)
;   ;;;        (cond
;   ;;;           (  (equal (car p1) (car p2) epsilon)  ; if x are equal,
;   ;;;              (> (cadr p1) (cadr p2)))           ; then compare y,
;   ;;;           (t (> (car p1) (car p2)))             ; else compare x
;   ;;;        )
;   ;;;     )
;   ;;;
;   ;;;     (setq sorted (lsort unsorted 'compare-points))
;   ;;;=========================================================================
;   (defun-q lsort (input OnCompare / fun)
;     (setq fun (cond (OnCompare) (t '>)))
;     (lsort-aux input)
;   )

;   (defun-q lsort-aux (input)
;     (if (cdr input)
;       (  (lambda (tlist)
;             (lsort-merge
;                 (lsort-aux (car tlist))
;                 (lsort-aux (cadr tlist))
;             )
;           )
;           (lsort-split input)
;       )
;       input
;     )
;   )

;   (defun-q lsort-split (right / left)
;     (repeat (/ (length right) 2)
;       (setq
;           left (cons (car right) left)
;           right (cdr right)
;       )
;     )
;     (list left right)
;   )

;   (defun-q lsort-merge (left right / out)
;     (while (and left right)
;       (if (apply fun (list (car left) (car right)))
;           (setq
;             out (cons (car left) out)
;             left (cdr left)
;           )
;           (setq
;             out (cons (car right) out)
;             right (cdr right)
;           )
;       )
;     )
;     (append (reverse out) left right)
;   )
; ; 
; (defun c:vc ()
;   (setq ssss (car (entsel)))
;   ; (setq ssss_x (cadr ssss))
;   ; (setq abx (entget (car (entsel "\n OK GO"))))
  
;   ; (setq ssss_Y (caddr ssss))
  
; )
; (defun c:vcc ()
;   ; (setq ssss (car (entsel)))
;   ; ; (setq ssss_x (cadr ssss))
;   (setq abx (entget (car (entsel "\n OK GO"))))
;   (setq abx_car (assoc -1 abx))
;   (setq abx_car2 (assoc 10 abx))
  
  
;   (setq abx_car1 (cdr abx_car))
  
  
;   (setq o_i (vlax-ename->vla-object abx_car1))
;   (princ abx)
;   (princ "\n =")
;   (vla-get-rotation o_i)
  
;   ; (setq ssss_Y (caddr ssss))
  
; )



(defun c:ucs_world()
  (command "ucs" "world")
)
(defun c:ucs_previ ()
  (command "ucs" "p")
)



(defun c:ddx ()
  (c:ucs_world)
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
    )
  )
  (setvar "osmode" 0)
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  (defun get-x-coordinate (entity)
    (cadr (assoc 10 (entget entity)))
  )
  (defun sort-entities-by-x (selection-set)
    (setq entity-list '())

    (setq num-entities (sslength selection-set))

    (setq i 0)
    (while (< i num-entities)
      (setq entity (ssname selection-set i))
      (setq x-coord (get-x-coordinate entity))
      (setq entity-list (cons (list entity x-coord) entity-list))
      (setq i (+ i 1))
    )

    (setq sorted-entity-list (vl-sort entity-list 
                                    '(lambda (a b) 
                                      (if (and (cdr a) (cdr b))
                                        (< (cadr a) (cadr b))
                                        t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                    )
                          )
    )

    (setq sorted-ename-list '())
    (foreach entity-entity sorted-entity-list
      (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
    )

    (reverse sorted-ename-list)
  )
  (setq my-selection-set (ssget  '((0 . "INSERT"))))
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 1)
  
          (setq total-entities (length sorted-enames))
          (while (and (< i total-entities) (< ii total-entities))
          ; (while (< i total-entities)
            ; part i
            (setq entity_i (nth i sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
            
            ; part ii
            (setq entity_ii (nth ii sorted-enames))
              (setq o_ii (vlax-ename->vla-object entity_ii))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
              (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
              (setq o_ii_ins_x (car o_ii_ins_xy))
              (setq o_ii_ins_y (cadr o_ii_ins_xy))
              (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
            (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
            
            ; part i+ii offset dim
            (setq o_ii3_ins_y (+ o_ii_ins_y_off OF_DIM))
            (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

            (command "dimstyle" "r" "DIM E-E")
            (setvar "DIMSCALE" sc)
            (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
            
            (setq i (+ i 1))
            (setq ii (+ i 1))
                     
          )
  (setvar "osmode" 1215)
  (c:ucs_previ)
)
(defun c:ddy ()
  (c:ucs_world)
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
    )
  )
  (setvar "osmode" 0)
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )

    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  (defun get-x-coordinate (entity)
    (caddr (assoc 10 (entget entity)))
  )
  (defun sort-entities-by-x (selection-set)
    (setq entity-list '())

    (setq num-entities (sslength selection-set))

    (setq i 0)
    (while (< i num-entities)
      (setq entity (ssname selection-set i))
      (setq x-coord (get-x-coordinate entity))
      (setq entity-list (cons (list entity x-coord) entity-list))
      (setq i (+ i 1))
    )

    (setq sorted-entity-list (vl-sort entity-list 
                                    '(lambda (b a) 
                                      (if (and (cdr b) (cdr a))
                                        (< (cadr b) (cadr a))
                                        t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                    )
                          )
    )

    (setq sorted-ename-list '())
    (foreach entity-entity sorted-entity-list
      (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
    )

    ; (reverse sorted-ename-list)
  )
  (setq my-selection-set (ssget  '((0 . "INSERT"))))
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 1)
  
          (setq total-entities (length sorted-enames))
          (while (and (< i total-entities) (< ii total-entities))
            ; (while (< i total-entities)
              ; part i
            (setq entity_i (nth i sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
            
            ; part ii
            (setq entity_ii (nth ii sorted-enames))
              (setq o_ii (vlax-ename->vla-object entity_ii))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
              (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
              (setq o_ii_ins_x (car o_ii_ins_xy))
              (setq o_ii_ins_y (cadr o_ii_ins_xy))
              (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
            (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
            
            ; part i+ii offset dim
            (setq o_ii3_ins_x (- o_ii_ins_x_off OF_DIM))
            (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
            
            (command "dimstyle" "r" "DIM E-E")
            (setvar "DIMSCALE" sc)
            (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
            
            (setq i (+ i 1))
            (setq ii (+ i 1))
                     
          )
  (setvar "osmode" 1215)
  (c:ucs_previ)
)
(defun c:hhx ()
  (c:ucs_world)
  
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
    )
  )
  (setvar "osmode" 0)
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  (defun get-x-coordinate (entity)
    (cadr (assoc 10 (entget entity)))
  )
  (defun sort-entities-by-x (selection-set)
    (setq entity-list '())

    (setq num-entities (sslength selection-set))

    (setq i 0)
    (while (< i num-entities)
      (setq entity (ssname selection-set i))
      (setq x-coord (get-x-coordinate entity))
      (setq entity-list (cons (list entity x-coord) entity-list))
      (setq i (+ i 1))
    )

    (setq sorted-entity-list (vl-sort entity-list 
                                    '(lambda (a b) 
                                      (if (and (cdr a) (cdr b))
                                        (< (cadr a) (cadr b))
                                        t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                    )
                          )
    )

    (setq sorted-ename-list '())
    (foreach entity-entity sorted-entity-list
      (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
    )

    (reverse sorted-ename-list)
  )
  (setq my-selection-set (ssget  '((0 . "INSERT"))))
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 1)
  
          (setq total-entities (length sorted-enames))
          (while (and (< i total-entities) (< ii total-entities))
            ; (while (< i total-entities)
            ; part i
            (setq entity_i (nth i sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
            
            ; part ii
            (setq entity_ii (nth ii sorted-enames))
              (setq o_ii (vlax-ename->vla-object entity_ii))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
              (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
              (setq o_ii_ins_x (car o_ii_ins_xy))
              (setq o_ii_ins_y (cadr o_ii_ins_xy))
              (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
            (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
            
            ; part i+ii offset dim
            (setq o_ii3_ins_y (+ o_ii_ins_y_off HI_DIM))
            (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

            (command "dimstyle" "r" "DIM E-E")
            (setvar "DIMSCALE" sc)
            (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
            
            (setq i (+ i 1))
            (setq ii (+ i 1))
                     
          )
  (setvar "osmode" 1215)
  (c:ucs_previ)
)

(defun c:hhy ()
  (c:ucs_world)
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
    )
  )
  (setvar "osmode" 0)
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  (defun get-x-coordinate (entity)
    (caddr (assoc 10 (entget entity)))
  )
  (defun sort-entities-by-x (selection-set)
    (setq entity-list '())

    (setq num-entities (sslength selection-set))

    (setq i 0)
    (while (< i num-entities)
      (setq entity (ssname selection-set i))
      (setq x-coord (get-x-coordinate entity))
      (setq entity-list (cons (list entity x-coord) entity-list))
      (setq i (+ i 1))
    )

    (setq sorted-entity-list (vl-sort entity-list 
                                    '(lambda (b a) 
                                      (if (and (cdr b) (cdr a))
                                        (< (cadr b) (cadr a))
                                        t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                    )
                          )
    )

    (setq sorted-ename-list '())
    (foreach entity-entity sorted-entity-list
      (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
    )

    ; (reverse sorted-ename-list)
  )
  (setq my-selection-set (ssget  '((0 . "INSERT"))))
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 1)
  
          (setq total-entities (length sorted-enames))
          (while (and (< i total-entities) (< ii total-entities))
          ; (while (< i total-entities)
            ; part i
            (setq entity_i (nth i sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
            
            ; part ii
            (setq entity_ii (nth ii sorted-enames))
              (setq o_ii (vlax-ename->vla-object entity_ii))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
              (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
              (setq o_ii_ins_x (car o_ii_ins_xy))
              (setq o_ii_ins_y (cadr o_ii_ins_xy))
              (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
            (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
            
            ; part i+ii offset dim
            (setq o_ii3_ins_x (- o_ii_ins_x_off HI_DIM))
            (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))

            (command "dimstyle" "r" "DIM E-E")
            (setvar "DIMSCALE" sc)
            (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
            
            (setq i (+ i 1))
            (setq ii (+ i 1))
                     
          )
  (setvar "osmode" 1215)
  (c:ucs_previ)
)


(defun c:mg_move_grid ()
  
  
  (setq mv_main (entget (car (entsel "\n OK GO"))))
  
  (setq mv_assoc_ename (assoc -1 mv_main))
  (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
  (setq mv_ename (cdr mv_assoc_ename))
  
  (setq mv_obj (vlax-ename->vla-object mv_ename))
  (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
  (setq mv_obj_ins_x (car mv_obj_ins_xy))
  (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
  (setq mv_obj_ins_z 0)
  
  (setq finding_xy (getpoint))
  (setq finding_x (car finding_xy))
  
  
  
  (setq fvc (strcat (rtos finding_x 2 8) "," (rtos mv_obj_ins_y 2 8) "," (rtos mv_obj_ins_z 2 8)))
  
  (command "copy" mv_ename "" mv_obj_ins_xy fvc)
  
    (c:mg_move_grid_last)
 
)
(defun c:mg_move_grid_last ()
  (setq p 1)
  (setq pp 10)
  (while 
    (< p pp)
    (setq mv_main_last (entlast))
    ; (setq mv_main (entget (car (entsel "\n OK GO"))))
    
    ; (setq mv_assoc_ename (assoc -1 mv_main))
    ; (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
    ; (setq mv_ename (cdr mv_assoc_ename))
    
    (setq mv_obj (vlax-ename->vla-object mv_main_last))
    (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
    (setq mv_obj_ins_x (car mv_obj_ins_xy))
    (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
    (setq mv_obj_ins_z 0)
    
    (setq finding_xy (getpoint))
    (setq finding_x (car finding_xy))
    
    
    
    (setq fvc (strcat (rtos finding_x 2 8) "," (rtos mv_obj_ins_y 2 8) "," (rtos mv_obj_ins_z 2 8)))
    
    (command "copy" mv_main_last "" mv_obj_ins_xy fvc "")
    (setq p (+ p 1))
  )  
)
(defun c:mvx_reset_x_grid ()
  
  (setq mv_main (entget (car (entsel "\n NEWJEAN MAIN"))))
  
  (setq mv_assoc_ename (assoc -1 mv_main))
  (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
  (setq mv_ename (cdr mv_assoc_ename))
  
  (setq mv_obj (vlax-ename->vla-object mv_ename))
  (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
  (setq mv_obj_ins_x (car mv_obj_ins_xy))
  (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
  (setq mv_obj_ins_z 0)
  
    (setq mySet (ssget (list 
                      ;  (cons 8 "0") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
    )
  (setq entityCount (sslength mySet))
    (setq i 0)
  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq ejc (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
    (setq o (vlax-ename->vla-object ejc))
    (setq o_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o))))
    (setq jx_x (car o_ins_xy))
    (setq jx_y (cadr o_ins_xy))
    (setq jx_z 0)

    (setq insertionPoint (vlax-3d-point jx_x mv_obj_ins_y jx_z))
    (vla-put-insertionpoint o insertionPoint)
    (setq i (+ i 1)) 
  ) 
)
(defun c:mvy_reset_y_grid ()
  
  (setq mv_main (entget (car (entsel "\n NEWJEAN MAIN"))))
  
  (setq mv_assoc_ename (assoc -1 mv_main))
  (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
  (setq mv_ename (cdr mv_assoc_ename))
  
  (setq mv_obj (vlax-ename->vla-object mv_ename))
  (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint mv_obj))))
  (setq mv_obj_ins_x (car mv_obj_ins_xy))
  (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
  (setq mv_obj_ins_z 0)
  
    (setq mySet (ssget (list 
                      ;  (cons 8 "0") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
    )
  (setq entityCount (sslength mySet))
    (setq i 0)
  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq ejc (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
    (setq o (vlax-ename->vla-object ejc))
    (setq o_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o))))
    (setq jx_x (car o_ins_xy))
    (setq jx_y (cadr o_ins_xy))
    (setq jx_z 0)

    (setq insertionPoint (vlax-3d-point mv_obj_ins_x jx_y jx_z))
    (vla-put-insertionpoint o insertionPoint)
    (setq i (+ i 1)) 
  ) 
)

(defun c:r90c_ROTALE_90+CPOY ()
  (setq grid_main (car (entsel)))
  (setq grid_main_obj (vlax-ename->vla-object grid_main))
   (setq grid_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint grid_main_obj))))
    (setq grid_x (car grid_ins_xy))
    (setq grid_y (cadr grid_ins_xy))
    (setq grid_z 0)

  (command "rotate" grid_main "" grid_ins_xy "co" 90)
)



