;Dynamic Funcion
  ;; Get Dynamic Block Property Value  -  Lee Mac
      ;; Returns the value of a Dynamic Block property (if present)
      ;; blk - [vla] VLA Dynamic Block Reference object
      ;; prp - [str] Dynamic Block property name (case-insensitive)

      (defun LM:getdynpropvalue ( blk prp )
          (setq prp (strcase prp))
          (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
              (vlax-invoke blk 'getdynamicblockproperties)
          )
      )
  ;; Get Dynamic Block Property Value  -  Lee Mac
    ;; Returns the value of a Dynamic Block property (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)

    (defun LM:getdynpropvalue ( blk prp )
        (setq prp (strcase prp))
        (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Get Dynamic Block Property Value  -  Lee Mac
    ;; Returns the value of a Dynamic Block property (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)

    (defun LM:getdynpropvalue ( blk prp )
        (setq prp (strcase prp))
        (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Set Dynamic Block Properties  -  Lee Mac
    ;; Modifies values of Dynamic Block properties using a supplied association list.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; lst - [lst] Association list of ((<Property> . <Value>) ... )
    ;; Returns: nil

    (defun LM:setdynprops ( blk lst / itm )
        (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
        (foreach x (vlax-invoke blk 'getdynamicblockproperties)
            (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
                (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
            )
        )
    )
  ;; Get Dynamic Block Property Allowed Values  -  Lee Mac
    ;; Returns the allowed values for a specific Dynamic Block property.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)
    ;; Returns: [lst] List of allowed values for property, else nil if no restrictions

    (defun LM:getdynpropallowedvalues ( blk prp )
        (setq prp (strcase prp))
        (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Toggle Dynamic Block Flip State  -  Lee Mac
    ;; Toggles the Flip parameter if present in a supplied Dynamic Block.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Return: [int] New Flip Parameter value

    (defun LM:toggleflipstate ( blk )
        (vl-some
          '(lambda ( prp / rtn )
                (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                    (progn
                        (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                        rtn
                    )
                )
            )
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Get Visibility Parameter Name  -  Lee Mac
    ;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [str] Name of Visibility Parameter, else nil

    (defun LM:getvisibilityparametername ( blk / vis )  
        (if
            (and
                (vlax-property-available-p blk 'effectivename)
                (setq blk
                    (vla-item
                        (vla-get-blocks (vla-get-document blk))
                        (vla-get-effectivename blk)
                    )
                )
                (= :vlax-true (vla-get-isdynamicblock blk))
                (= :vlax-true (vla-get-hasextensiondictionary blk))
                (setq vis
                    (vl-some
                      '(lambda ( pair )
                            (if
                                (and
                                    (= 360 (car pair))
                                    (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                                )
                                (cdr pair)
                            )
                        )
                        (dictsearch
                            (vlax-vla-object->ename (vla-getextensiondictionary blk))
                            "ACAD_ENHANCEDBLOCK"
                        )
                    )
                )
            )
            (cdr (assoc 301 (entget vis)))
        )
    )
  ;; Get Dynamic Block Visibility State  -  Lee Mac
    ;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [str] Value of Visibility Parameter, else nil

    (defun LM:getvisibilitystate ( blk / vis )
        (if (setq vis (LM:getvisibilityparametername blk))
            (LM:getdynpropvalue blk vis)
        )
    )
  ;; Set Dynamic Block Visibility State  -  Lee Mac
;
;Effective Block Name Funcition
  ;; Effective Block Name  -  Lee Mac
    ;; obj - [vla] VLA Block Reference object
        (defun LM:effectivename ( obj )
            (vlax-get-property obj
                (if (vlax-property-available-p obj 'effectivename)
                    'effectivename
                    'name
                )
            )
        )
  ;; Effective Block Name  -  Lee Mac
    ;; ent - [ent] Block Reference entity

    (defun LM:al-effectivename ( ent / blk rep )
        (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
            (if
                (and
                    (setq rep
                        (cdadr
                            (assoc -3
                                (entget
                                    (cdr
                                        (assoc 330
                                            (entget
                                                (tblobjname "block" blk)
                                            )
                                        )
                                    )
                                  '("AcDbBlockRepBTag")
                                )
                            )
                        )
                    )
                    (setq rep (handent (cdr (assoc 1005 rep))))
                )
                (setq blk (cdr (assoc 2 (entget rep))))
            )
        )
        blk
    )
;
;Attribute Function
  ;; Get Attribute Value  -  Lee Mac
    ;; Returns the value held by the specified tag within the supplied block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; Returns: [str] Attribute value, else nil if tag is not found.

    (defun LM:vl-getattributevalue ( blk tag )
        (setq tag (strcase tag))
        (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Value  -  Lee Mac
    ;; Sets the value of the first attribute with the given tag found within the block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; val - [str] Attribute Value
    ;; Returns: [str] Attribute value if successful, else nil.

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
  ;; Get Attribute Values  -  Lee Mac
    ;; Returns an association list of attributes present in the supplied block.
    ;; blk - [vla] VLA Block Reference Object
    ;; Returns: [lst] Association list of ((<tag> . <value>) ... )

    (defun LM:vl-getattributevalues ( blk )
        (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Values  -  Lee Mac
    ;; Sets attributes with tags found in the association list to their associated values.
    ;; blk - [vla] VLA Block Reference Object
    ;; lst - [lst] Association list of ((<tag> . <value>) ... )
    ;; Returns: nil

    (defun LM:vl-setattributevalues ( blk lst / itm )
        (foreach att (vlax-invoke blk 'getattributes)
            (if (setq itm (assoc (vla-get-tagstring att) lst))
                (vla-put-textstring att (cdr itm))
            )
        )
    )
;
;sub_function_for_coding
  (defun c:ucs_world()
    (command "ucs" "world")
  )
  (defun c:ucs_previ ()
    (command "ucs" "p")
  )
;

(defun c:r90c_ROTALE_90+CPOY ()
  (setq grid_main (car (entsel)))
  (setq grid_main_obj (vlax-ename->vla-object grid_main))
   (setq grid_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint grid_main_obj))))
    (setq grid_x (car grid_ins_xy))
    (setq grid_y (cadr grid_ins_xy))
    (setq grid_z 0)

  (command "rotate" grid_main "" grid_ins_xy "co" 90)
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
(defun c:ddxx ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
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
  
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
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
  ;making_dim_
)
(defun c:ddyy ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my-efname-set ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
        (setq fixed_angle (fix rotation_to_angle) )
        
        (setq ss (LM:effectivename blk_obj_Set))

        (if (and (= ef_name ss) (= fixed_angle 90))
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
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
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
  ;making_dim_
)
(defun c:hhxx ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
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
  
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
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
  ;making_dim_
)
(defun c:hhyy ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my-efname-set ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
        (setq fixed_angle (fix rotation_to_angle) )
        
        (setq ss (LM:effectivename blk_obj_Set))

        (if (and (= ef_name ss) (= fixed_angle 90))
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
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
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
  ;making_dim_
)
(defun c:hmx ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
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
  
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
    (setq i1st 0)
    (setq iilast (- total-entities 1))
  
          (setq total-entities (length sorted-enames))
          
          ; (while (< i total-entities)
            ; part i
            (setq entity_i (nth i1st sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
            
            ; part ii
            (setq entity_ii (nth iilast sorted-enames))
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
          
    (setvar "osmode" 1215)
    (c:ucs_previ)
  ;making_dim_
)
(defun c:hmy ()
  ;sub_func
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
  ;sub_func
  
  ; เริ่ม part คำนวณ SCALE
    (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq sc (* scl 10)) ; 5 = 50 2 = 20
    (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
    (setq OF_DIM (* (* scl 1) of_point))
    (setq HI_DIM (* (* scl 1) hi_point))
  ; จบ part คำนวณ SCALE
  
  ;sub_func_for_fillter_effectivename
    (setq my-efname-set (ssget  '((0 . "INSERT"))))
    (setq total_ssget (sslength my-efname-set))
    (setq ie 0)
    (setq ename-list '())
    (setq ef_name "000-GRID_LINE_DYN")
      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my-efname-set ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
        (setq fixed_angle (fix rotation_to_angle) )
        
        (setq ss (LM:effectivename blk_obj_Set))

        (if (and (= ef_name ss) (= fixed_angle 90))
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
  ;sub_func_for_fillter_effectivename
  
  ;sub_func_for_sorting_part
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
  ;sub_func_for_sorting_part
  
  ;making_dim_
    (setq my-selection-set ff )
    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
    (setq i1st 0)
    (setq iilast (- total-entities 1))
  
          (setq total-entities (length sorted-enames))
          
            ; (while (< i total-entities)
              ; part i
            (setq entity_i (nth i1st sorted-enames))
              (setq o_i (vlax-ename->vla-object entity_i))
              (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
              (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
              (setq o_i_ins_x (car o_i_ins_xy))
              (setq o_i_ins_y (cadr o_i_ins_xy))
              (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
            (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
            
            ; part ii
            (setq entity_ii (nth iilast sorted-enames))
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
          
    (setvar "osmode" 1215)
    (c:ucs_previ)
  ;making_dim_
)

;Chaning Dim Part
  (defun c:DCCV ()
    (setq myset (ssget 
                  (list 
                    ;  (cons 0 "inset")
                    ;  (cons 8 "000 - GRID")
                    ;  (cons 62 1)
                  ) 
                ) 
    )
    (vl-load-com)
    (setq entityCount (sslength mySet))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq entity1 (ssname mySet i))
        (setq s0 (vlax-ename->vla-object entity1))
        (setq s0_dimst (vla-get-stylename s0))
        (setq s0_dimst (vla-get-scalefactor s0))
        

        (setq new_s0_dimst (vla-put-stylename s0 "DIM C-C"))
        (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
        
      (setq i (1+ i))
    )  
  )
  (defun c:DECV ()
    (setq myset (ssget (list 
                        
                        ;  (cons 0 "inset") 
                        ;  (cons 8 "000 - GRID") 
                        ;  (cons 62 1) 
                        
                      ) 
                ) 
    )
    (vl-load-com)
    (setq entityCount (sslength mySet))
      (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq entity1 (ssname mySet i))
        (setq s0 (vlax-ename->vla-object entity1))
        (setq s0_dimst (vla-get-stylename s0))
        (setq s0_dimst (vla-get-scalefactor s0))
        
        (setq new_s0_dimst (vla-put-stylename s0 "DIM E-C"))
        (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))    
      
      (setq i (1+ i))
    )  
  )
  (defun c:DCEV ()
    (setq myset (ssget (list 
                        
                        ;  (cons 0 "inset") 
                        ;  (cons 8 "000 - GRID") 
                        ;  (cons 62 1) 
                        
                      ) 
                ) 
    )
    (vl-load-com)
    (setq entityCount (sslength mySet))
      (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq entity1 (ssname mySet i))
        (setq s0 (vlax-ename->vla-object entity1))
        (setq s0_dimst (vla-get-stylename s0))
        (setq s0_dimst (vla-get-scalefactor s0))
        
        (setq new_s0_dimst (vla-put-stylename s0 "DIM C-E"))
        (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
      
      (setq i (1+ i))
    )  
  )
  (defun c:DEEV ()
    (setq myset (ssget (list 
                        
                        ;  (cons 0 "inset") 
                        ;  (cons 8 "000 - GRID") 
                        ;  (cons 62 1) 
                        
                      ) 
                ) 
    )
    (vl-load-com)
    (setq entityCount (sslength mySet))
      (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq entity1 (ssname mySet i))
        (setq s0 (vlax-ename->vla-object entity1))
        (setq s0_dimst (vla-get-stylename s0))
        (setq s0_dimst (vla-get-scalefactor s0))
        
        (setq new_s0_dimst (vla-put-stylename s0 "DIM E-E"))
        (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
      
      (setq i (1+ i))
    )  
  )
;