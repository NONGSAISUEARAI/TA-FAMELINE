;นี่คือ code ต้นแบบ การ เรียงลำดับ coord x หรือ coord  y 
;สำหรับ block dynblock att-block หรือ อะไร ก็ตามที่มันมี insertion point x y  ได้หมด

(defun c: ::::::::::::::::::::::::::::::::PLEASE_RENAME_YOUR_COOMMAD:::::::::::::::::::::::::::::::: ()
  ; start sub function for sorting
    (defun get-x-coordinate (entity) 
      (cadr (assoc 10 (entget entity)))
      ;cadr is func to indicate coord x
      ;caddr is func to indicate coord y
    )

    (defun sort-entities-by-coord (selection-set) 
      (setq entity-list '())
      (setq num-entities (sslength selection-set))
      (setq i-sor 0)
      (while (< i-sor num-entities) 
        (setq entity (ssname selection-set i-sor))
        (setq x-coord (get-x-coordinate entity))
        (setq entity-list (cons (list entity x-coord) entity-list))
        (setq i-sor (+ i-sor 1))
      )
      (setq sorted-entity-list (vl-sort entity-list 
                                        '(lambda (a b) 
                                          (if (and (cdr a) (cdr b)) 
                                            (> (cadr a) (cadr b)) ; > = max to min, < = min to max
                                            t
                                          ) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
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
                                (cons 0 "INSERT") ;type of object
                                ; (cons 8 "000 - GRID") ;kind of layer
                                ;  (cons 62 1)          ;kind of color call sign with color code index
                              )
                            )
      )
    ; end finding sssget for sorting coord x or y

    (princ (setq sorted-enames (sort-entities-by-coord my-selection-set))) ;CALL FUNC. SORTING OBJECT
    (setq total-entities (length sorted-enames)) ;SUMMARY TOTAL OBJECT

    (princ (strcat "total =" (rtos total-entities 2 0))) ;just princ to command line
  ; end sub function for sorting
  
  ; ที่เหลือก็คือใส่อะไรใส่เลย
  (setq sorted-enames (sort-entities-by-coord my-selection-set))
  ;CALL FUNC. SORTING OBJECT
  (setq total-entities (length sorted-enames))
  ;SUMMARY TOTAL OBJECT
  (setq i-xxx 0) ;setiing i=0 for make while loop
  (while (< i-xxx total-entities)  ;indicate citeria for making while loop
    (setq array (nth i-xxx sorted-enames)) ;NTH FOR sorting list [0 = 1st object]
    (setq array_ename (vlax-ename->vla-object array)) ;tran ENAME TO OBJECT NAME
    (setq array_ename_ins_xy (vlax-safearray->list 
                               (vlax-variant-value (vla-get-InsertionPoint array_ename))
                             )
    ) ;method for array insertion point X Y Z [ref.from cad]

    (setq array_ename_ins_x (car array_ename_ins_xy)) ;car caddr cadr ... etc. is c func. see http://www.lee-mac.com/programs.html
    (setq array_ename_ins_y (cadr array_ename_ins_xy)) ;car caddr cadr ... etc. is c func. see http://www.lee-mac.com/programs.html
    (setq array_ename_ins_y 0) ;(metthod to set number w/ varient
    (setq i-xxx (+ i-xxx 1))
  )
) 


(defun : ::::::::::::::::::::::::::::::::PLEASE_RENAME_YOUR_COOMMAD:::::::::::::::::::::::::::::::: ()
  ; sub_func_for_fillter_effectivename
    ;finding_effectivename_part
      (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
      (setq select_name_obj (vlax-ename->vla-object select_name))
      (setq finding_ef_name (LM:effectivename select_name_obj))
    ;
    ;fillter_effectivename_part
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name finding_ef_name)
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
              (progn
                (setq ename-list (cons blk_obj ename-list))
                ; ทำคำสั่งอื่น ๆ ที่ต้องการ
              )
              (princ "\n")
          )
          (setq ie (+ ie 1))
        )
        (princ "\n")
        (princ (setq total_ename-list (length ename-list)))
        (setq ff (ssadd))
        (foreach ename ename-list
          (ssadd ename ff)
        )
        (setq my-selection-set ff )
  ; 
    
  ; (command "pselect" my-selection-set "") ;for_testing_process

  ; sub_func_for_sorting
    (defun get-x-coordinate (entity) 
      (caddr (assoc 10 (entget entity)))
      ;cadr is func to indicate coord x
      ;caddr is func to indicate coord y
    )
    (defun sort-entities-by-coord (selection-set) 
      (setq entity-list '())
      (setq num-entities (sslength selection-set))
      (setq i-sor 0)
      (while (< i-sor num-entities) 
        (setq entity (ssname selection-set i-sor))
        (setq x-coord (get-x-coordinate entity))
        (setq entity-list (cons (list entity x-coord) entity-list))
        (setq i-sor (+ i-sor 1))
      )
      (setq sorted-entity-list (vl-sort entity-list 
                                        '(lambda (a b) 
                                          (if (and (cdr a) (cdr b)) 
                                            (> (cadr a) (cadr b)) ; > = max to min, < = min to max
                                            t
                                          ) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
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
      ; (setq my-selection-set (ssget 
      ;                         (list 
      ;                           (cons 0 "INSERT") ;type of object
      ;                           ; (cons 8 "000 - GRID") ;kind of layer
      ;                           ;  (cons 62 1)          ;kind of color call sign with color code index
      ;                         )
      ;                       )
      ; )
    ; end finding sssget for sorting coord x or y

    (princ (setq sorted-enames (sort-entities-by-coord my-selection-set))) ;CALL FUNC. SORTING OBJECT
    (setq total-entities (length sorted-enames)) ;SUMMARY TOTAL OBJECT

    (princ (strcat "total =" (rtos total-entities 2 0))) ;just princ to command line
  ; 
  ; main_functiion
  ; 
  
)
