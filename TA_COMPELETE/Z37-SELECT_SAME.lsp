
; ;selection set แบบง่ายๆ 
; (setq select_fillter (entget (car (entsel "\n OK GO"))))

; (set select_fillter (entsel "select fillter"))
; (set select_fillter_get (entget select_fillter))

; (setq select_group (ssget 
;                 (list 
;                   (cons 0 "INSERT")       ;type of object
;                   (cons 8 "000 - GRID")   ;kind of layer 
;                   ; (cons 2 "SSSS")       ;kind of nameblock
;                   ; (cons 62 1)           ;kind of color call sign with color code index
;                 )
;               )
; )


  ;sub_func_by_Lee_Mac
    (defun LM:effectivename ( obj )
        (vlax-get-property obj
            (if (vlax-property-available-p obj 'effectivename)
                'effectivename
                'name
            )
        )
    )
    ; LM:getdynprops for example multi dynamic
      (defun LM:getdynprops ( blk )
        (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
      )
    ; LM:getdynpropvalue for single dynamic
      (defun LM:getdynpropvalue (blk prp) 
        (setq prp (strcase prp))
        (vl-some 
          '(lambda (x) 
            (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
          )
          (vlax-invoke blk 'getdynamicblockproperties)
        )
      )
    ; LM:getdynpropvalue for single dynamic
      (defun LM:setdynpropvalue ( blk prp val )
        (setq prp (strcase prp))
        (vl-some
          '(lambda ( x )
                (if (= prp (strcase (vla-get-propertyname x)))
                    (progn
                        (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                        (cond (val) (t))
                    )
                )
            )
            (vlax-invoke blk 'getdynamicblockproperties)
        )
      )
  ;sub_func_by_Lee_Mac
;basic_func
  (defun c:ucs_world()
    (command "ucs" "world")
  )
  (defun c:ucs_previ()
    (command "ucs" "p")
  )
;basic_func
;pre_main_func
  (defun c:pre_main_func()
    (setvar "osmode" 0)
    (setq h (getint "specify scale"))
    (command "_-INSERT""LNAD - A4 TITLE BLOCK PART REV01""-50,-60" h 0)
    (setq ti_blk (entlast))
    (setq ti_blk_obj (vlax-ename->vla-object ti_blk ))
    (c:ucs_world)
    (c:z37-MK_V_DIM)
    (c:z37-MK_H_DIM)
    (command "ucs" "p")
    (setvar "osmode" 1215)
    (vla-delete ti_blk_obj)
  )
  
(defun c:z37-MK_V_DIM ()
  ; (c:ucs_world)
  ;tittle_block_ref_for_cpolygon
    (setq tittle_blk (entlast))
      (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
      (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
      (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
      (if (and (= ef_tillte_blk ef_blk) )
        (progn
          (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
          (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

          (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
          (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
          (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
          
          (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
          (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
          (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
          (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
          (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
          ; ทำคำสั่งอื่น ๆ ที่ต้องการ
        )
        (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
      )
     
    (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
    (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
  ;tittle_block_ref_for_cpolygon
  
  ; finding_grid_line part
    (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))

    ; (setq my-selection-set (ssget "C" 
    ;                           '(100 100) ;for insert x
    ;                           '(200 200) ;for insert y
    ;                           '(
    ;                             (0 . "INSERT") 
                                
    ;                             )
    ;                   )
    ; )
    (setq total_ssget (sslength my-selection-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my-selection-set ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (vla-get-rotation blk_obj_Set)
        (setq ss (LM:effectivename blk_obj_Set))

        (if (and (= ef_name ss) (= (vla-get-rotation blk_obj_Set) 0))
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
    
    
    ; (command "pselect" ff )
  ; finding_grid_line part
  
  ; sub_function part
    ;basic_func
      (defun c:ucs_world()
        (command "ucs" "world")
      )
      (defun c:ucs_previ ()
        (command "ucs" "p")
      )
    ;basic_func

    ;custom_func_for_grid_line
        (defun ddxc (/ scl)
          ; (c:ucs_world)
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
            ; (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
          
            (setq scl sc_tt_blk ) ; changing this Variable for dynamic data Excel Programing
            
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
          
          (setq my-selection-set ff) ; changing this Variable for dynamic data Excel Programing
          
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
        
                  ; part i
                    (setq iii (- total-entities 1))
                    (setq entity_1st (nth 0 sorted-enames))
                      (setq o_1st (vlax-ename->vla-object entity_1st))
                      (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_1st) "OFFSET"))
                      (setq o_1st_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_1st))))
                      (setq o_1st_ins_x (car o_1st_ins_xy))
                      (setq o_1st_ins_y (cadr o_1st_ins_xy))
                      (setq o_1st_ins_y_off (+ (cadr o_1st_ins_xy) OFFSET))
                    (setq o_1st_ins_xy_new (strcat (rtos o_1st_ins_x 2 8) "," (rtos o_1st_ins_y_off 2 8) ))
                  ; part last
                    (setq entity_last (nth iii sorted-enames))
                      (setq o_last (vlax-ename->vla-object entity_last))
                      (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_last) "OFFSET"))
                      (setq o_last_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_last))))
                      (setq o_last_ins_x (car o_last_ins_xy))
                      (setq o_last_ins_y (cadr o_last_ins_xy))
                      (setq o_last_ins_y_off (+ (cadr o_last_ins_xy) OFFSET))
                    (setq o_last_ins_xy_new (strcat (rtos o_last_ins_x 2 8) "," (rtos o_last_ins_y_off 2 8) ))   

                  ; part i+iii offset dim
                  (setq o_iii3_ins_y (+ o_last_ins_y_off HI_DIM))
                  (setq o_iii3_ins_xy_new (strcat (rtos o_last_ins_x 2 8) "," (rtos o_iii3_ins_y 2 8) ))          
                
                  (command "dimlinear" o_1st_ins_xy_new o_last_ins_xy_new o_iii3_ins_xy_new)
          
          (setvar "osmode" 1215)
          ; (c:ucs_previ)
        )


    ;custom_func_for_grid_line
  
      (ddxc) ;sub func for send to command line 

  
  ; sub_function part
)
(defun c:z37-MK_H_DIM ()
  ; (c:ucs_world)
  ;tittle_block_ref_for_cpolygon  
    ; (setq tittle_blk (entlast))
      (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
      (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
      (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
      (if (and (= ef_tillte_blk ef_blk) )
        (progn
          (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
          (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

          (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
          (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
          (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
          
          (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
          (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
          (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
          (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
          (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
          ; ทำคำสั่งอื่น ๆ ที่ต้องการ
        )
        (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
      )
     
    (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
    (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
  ;tittle_block_ref_for_cpolygon
  
  ; finding_grid_line part
    
    (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))
    ; (vla-delete tittle_blk_obj)

    ; (setq my-selection-set (ssget "C" 
    ;                           '(100 100) ;for insert x
    ;                           '(200 200) ;for insert y
    ;                           '(
    ;                             (0 . "INSERT") 
                                
    ;                             )
    ;                   )
    ; )
    (setq total_ssget (sslength my-selection-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my-selection-set ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (setq rotation (rtos (vla-get-rotation blk_obj_Set) 2 8))
        (setq rotation_set "1.57079633")
        (setq ss (LM:effectivename blk_obj_Set))

        (if (and (= ef_name ss) (not (= (vla-get-rotation blk_obj_Set) 0)))
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
    
    
    ;  (command "pselect" ff )
  ; finding_grid_line part
  
  ; sub_function part
    ;basic_func
      (defun c:ucs_world()
        (command "ucs" "world")
      )
      (defun c:ucs_previ ()
        (command "ucs" "p")
      )
    ;basic_func

    ;custom_func_for_grid_line
        (defun ddyc (/ scl)
          ; (c:ucs_world)
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
            ; (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
          
            (setq scl sc_tt_blk ) ; changing this Variable for dynamic data Excel Programing
            
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
          
          (setq my-selection-set ff) ; changing this Variable for dynamic data Excel Programing
          
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
        
                  ; part i
                    (setq iii (- total-entities 1))
                    (setq entity_1st (nth 0 sorted-enames))
                      (setq o_1st (vlax-ename->vla-object entity_1st))
                      (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_1st) "OFFSET"))
                      (setq o_1st_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_1st))))
                      (setq o_1st_ins_x (car o_1st_ins_xy))
                      (setq o_1st_ins_y (cadr o_1st_ins_xy))
                      (setq o_1st_ins_x_off (- (car o_ii_ins_xy) OFFSET))
                    (setq o_1st_ins_xy_new (strcat (rtos o_1st_ins_x_off 2 8) "," (rtos o_1st_ins_y 2 8) ))
                  ; part last
                    (setq entity_last (nth iii sorted-enames))
                      (setq o_last (vlax-ename->vla-object entity_last))
                      (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_last) "OFFSET"))
                      (setq o_last_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_last))))
                      (setq o_last_ins_x (car o_last_ins_xy))
                      (setq o_last_ins_y (cadr o_last_ins_xy))
                      (setq o_last_ins_x_off (- (car o_ii_ins_xy) OFFSET))
                    (setq o_last_ins_xy_new (strcat (rtos o_last_ins_x_off 2 8) "," (rtos o_last_ins_y 2 8) ))   

                  ; part i+iii offset dim
                  (setq o_iii3_ins_x (- o_last_ins_x_off HI_DIM))
                  (setq o_iii3_ins_xy_new (strcat (rtos o_iii3_ins_x 2 8) "," (rtos o_last_ins_y 2 8) ))          
                
                  (command "dimlinear" o_1st_ins_xy_new o_last_ins_xy_new o_iii3_ins_xy_new)
          
          (setvar "osmode" 1215)
          ; (c:ucs_previ)
        )


    ;custom_func_for_grid_line
  
      (ddyc) ;sub func for send to command line 

  
  ; sub_function part
)

(defun c:z37-DYN_GRIDLINE ()
  (setq grid_ins (entlast))
    (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
    (setq grid_ins_ef_name (LM:effectivename grid_ins_obj))
    (setq dyn_grid_blk "000-GRID_LINE_DYN")
      (if (and (= grid_ins_ef_name dyn_grid_blk))
        (progn
          (setq H (getreal "specify H"))
          (setq OFS (getreal "specify OFS"))
          (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
          
          (setq grid_ins_H (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "H" (rtos H 2 8)))
          (setq grid_ins_OFS (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "OFFSET" (rtos OFS 2 8)))
          ; ทำคำสั่งอื่น ๆ ที่ต้องการ
        )
        (princ "\n")
      )
)
(defun z37-DYN_GRIDLINE-FOR_EXCEL (H OFS / )
  (setq grid_ins (entlast))
    (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
    (setq grid_ins_ef_name (LM:effectivename grid_ins_obj))
    (setq dyn_grid_blk "000-GRID_LINE_DYN")
      (if (and (= grid_ins_ef_name dyn_grid_blk))
        (progn
          ; (setq H (getreal "specify H")) ;ถ้ามี agument ตามหลัง defun ตรงนี้ให้ comment
          ; (setq OFS (getreal "specify OFS")) ;ถ้ามี agument ตามหลัง defun ตรงนี้ให้ comment
          (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
          
          (setq grid_ins_H (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "H" (rtos H 2 8)))
          (setq grid_ins_OFS (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "OFFSET" (rtos OFS 2 8)))
          ; ทำคำสั่งอื่น ๆ ที่ต้องการ
        )
        (princ "\n")
      )
)
; (c:z37-DYN_GRIDLINE 50 5)
; (z37-DYN_GRIDLINE-FOR_EXCEL 50 5)







