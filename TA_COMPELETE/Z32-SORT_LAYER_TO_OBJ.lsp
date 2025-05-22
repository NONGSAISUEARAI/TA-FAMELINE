(defun c:lay2obj ()
  ; LEE_MAC_PART
    (defun LM:vl-setattributevalue ( blk tag val )
        (setq tag (strcase tag))
        (vl-some
          '(lambda ( att )
                (if (= tag (strcase (vla-get-tagstring att)))
                    (progn (vla-put-textstring att val) val)
                )
            )
            (vlax-invoke blk 'getattributes)
        )
    )
  ; LEE_MAC_PART
  
  ;PART_LIST_AND_SORTING_LAYER
    (setq lay_list ())
    (setq lays (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
    (vlax-for lay lays
      (setq lay_list (append lay_list (list (vla-get-Name lay))))
    )
    (setq new_sort_lay_list (acad_strlsort lay_list))
    
    (setq total_lay_list (length lay_list))
    (setq ilay 0)
    (while
      (< ilay total_lay_list)
      (setq lay_list_nth (nth ilay new_sort_lay_list))
      (setq ilay (+ ilay 1))
      
    )
  ;PART_LIST_AND_SORTING_LAYER 
  
  ;PART_SSGET_AND_SORTING_OBJ
      (defun get-x-coordinate (entity)
        (caddr (assoc 10 (entget entity))) 
        ;cadr is func to indicate coord x 
        ;caddr is func to indicate coord y
      )

      (defun sort-entities-by-coord (selection-set)
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
                                            (> (cadr a) (cadr b)) ; > = max to min, < = min to max 
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
    ; start finding sssget for sorting coord x or y 
      (setq my-selection-set (ssget 
                    (list 
                      (cons 0 "INSERT")       ;type of object
                      (cons 8 "000 - GRID")   ;kind of layer 
                      ;  (cons 62 1)          ;kind of color call sign with color code index
                    )
                  )
      )
    ; end finding sssget for sorting coord x or y 

      (princ (setq sorted-enames (sort-entities-by-coord my-selection-set)))  ;CALL FUNC. SORTING OBJECT
      (setq total-entities (length sorted-enames))                            ;SUMMARY TOTAL OBJECT

      (princ (strcat "total =" (rtos total-entities 2 0)))   
  
    
    
  ;PART_SSGET_AND_SORTING_OBJ
  
  ; ที่เหลือก็คือใส่อะไรใส่เลย
      (setq sorted-enames (sort-entities-by-coord my-selection-set))                                              ;CALL FUNC. SORTING OBJECT
      (setq total-entities (length sorted-enames))                                                                ;SUMMARY TOTAL OBJECT
      (setq ienam 0)                                                                                                  ;setiing i=0 for make while loop
      (setq ilay 0)
    (while 
      (< ienam total-entities)                                                                                        ;indicate citeria for making while loop
      (setq array (nth ienam sorted-enames))                                                                          ;NTH FOR sorting list [0 = 1st object]
      (setq array_ename (vlax-ename->vla-object array))                                                           ;tran ENAME TO OBJECT NAME
      (setq array_ename_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint array_ename))))  ;method for array insertion point X Y Z [ref.from cad]
      
      (setq array_ename_ins_x (car array_ename_ins_xy))                                                           ;car caddr cadr ... etc. is c func. see http://www.lee-mac.com/programs.html
      (setq array_ename_ins_y (cadr array_ename_ins_xy))                                                          ;car caddr cadr ... etc. is c func. see http://www.lee-mac.com/programs.html
      (setq array_ename_ins_y 0)                                                                             ;(metthod to set number w/ varient
      (setq KK 50)
      
      (< ilay total_lay_list)
      (setq lay_list_nth (nth ilay new_sort_lay_list))
      
      
      (setq KK lay_list_nth)
      
      (LM:vl-setattributevalue array_ename "NAMEBLK" lay_list_nth)
      
      (command "chprop" array "" "LAYER") 
      
      (setq ienam (+ ienam 1))
      (setq ilay (+ ilay 1))
    )
  ; ที่เหลือก็คือใส่อะไรใส่เลย

)
