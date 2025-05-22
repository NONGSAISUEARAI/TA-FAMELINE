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
  ;; Set Dynamic Block Property Value  -  Lee Mac
    ;; Modifies the value of a Dynamic Block property (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)
    ;; val - [any] New value for property
    ;; Returns: [any] New value if successful, else nil

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
  ;; Get Dynamic Block Properties  -  Lee Mac
    ;; Returns an association list of Dynamic Block properties & values.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [lst] Association list of ((<prop> . <value>) ... )

    (defun LM:getdynprops ( blk )
        (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
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
    ;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; val - [str] Visibility State Parameter value
    ;; Returns: [str] New value of Visibility Parameter, else nil

    (defun LM:SetVisibilityState ( blk val / vis )
        (if
            (and
                (setq vis (LM:getvisibilityparametername blk))
                (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
            )
            (LM:setdynpropvalue blk vis val)
        )
    )
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


  ;sub_func
    (defun LM:round ( n )
      (fix (+ n (if (minusp n) -0.5 0.5)))
    )
    (defun TA:sub_startpt_func_XY (vla-object)
      (strcat 
        (rtos (car(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
        ","
        (rtos (cadr(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
      )
    )
    (defun TA:sub_entpt_func_XY (vla-object)
      (strcat 
        (rtos (car(vlax-safearray->list (vlax-variant-value (vla-get-endpoint vla-object)))) 2 8)
        ","
        (rtos (cadr(vlax-safearray->list (vlax-variant-value (vla-get-endpoint vla-object)))) 2 8)
      )
    )

  ;

  ;sub_func_for_sorting_part
    (defun get-x-coordinate (entity)
      (cadr (assoc 10 (entget entity)))
      ;cadr is func to indicate coord x
      ;caddr is func to indicate coord y
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

(defun c:Z48_ins_to_point ()

    (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
    (setq select_name_obj (vlax-ename->vla-object select_name))
    (setq ef_name (LM:effectivename select_name_obj))
    (setq mode-v (cond ( (getint (strcat "\nSpecify object \nmode 1 = startpt \nmode 2 = endpt \n<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : \nไม่มี MODE 3 นะคนดีย์"  ) ) ) (mode-v) ) )
     
  ;

  ;main_func_for_ssget_specify_obj
    (setq my-selection-set (ssget 
                            (list 
                              (cons 0 "LINE,POLYLINE,LWPOLYLINE,") ;type of object
                              ; (cons 8 "000 - GRID") ;kind of layer
                              ;  (cons 62 1)          ;kind of color call sign with color code index
                            )
                          )
    )

    (setq sorted-enames (sort-entities-by-x my-selection-set))
    (setq total-entities (length sorted-enames))
  ;
  
  
    (setq i_line 0)

  
    ;loop_mode
      (while
        (< i_line total-entities)
        ;main_func_for_insertion_point
          (setq my_line_set (nth i_line sorted-enames))
          (setq my_line_set_obj (vlax-ename->vla-object my_line_set))
        ;
        ;insertion_point_part
          ;get_mode_start-end-point
            (defun get_mode_start-end-point (mode)
              (if (= mode 1)
                  (TA:sub_startpt_func_XY my_line_set_obj)
                (if (= mode 2) 
                  (TA:sub_entpt_func_XY my_line_set_obj)
                )
              )
            )
            (setq insertion_point (get_mode_start-end-point mode-v))
          ;
        ;length_part
          (setq l_length (rtos (vla-get-length my_line_set_obj) 2 8))
        ;
        ;angle_part
          (setq _pi  3.14)
          (setq new_angle (LM:round (*(/ (vla-get-angle my_line_set_obj) _pi) 180)))
          (setq radius_angle (vla-get-angle my_line_set_obj))
        ;
        ; get_prototye_angle
          
          (setq select_name_rotation (vla-get-rotation (vlax-ename->vla-object select_name)))
        ; 
        ; ;commad_line_mode
          (command "insert" ef_name insertion_point 1 0)
          (setq ins_dyn_block (entlast))
          (setq ins_dyn_block_obj (vlax-ename->vla-object ins_dyn_block))
          (setq TOTAL_LENGTH (LM:setdynpropvalue  ins_dyn_block_obj "H" l_length))
          (setq put_new_angle (vla-put-rotation ins_dyn_block_obj radius_angle))
          (LM:setdynpropvalue  ins_dyn_block_obj "W" 25)
        ;
        (setq i_line (+ 1 i_line))
      )
    ;
    
)

(defun c:rr1_reset_rotation_line ()
  (setq my_line_set (ssget  '((0 . "LINE"))))
  (setq total_my_line_set (sslength my_line_set))

    (setq i_line_ro 0)
    (while ;reset_rotation_line
      (setq line_ename (ssname my_line_set i_line_ro))
      (setq line_ename_obj (vlax-ename->vla-object line_ename))

      (setq line_ename_obj_start_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-startpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_end_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-endpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_start_x (car line_ename_obj_start_xyz))
      (setq line_ename_obj_start_y (cadr line_ename_obj_start_xyz))
      (setq line_ename_obj_end_y (cadr line_ename_obj_end_xyz))
      (setq line_ename_obj_length (vla-get-length line_ename_obj))
      (setq line_ename_obj_length_Divide (/ line_ename_obj_length 2))


        
        (if 
          (or 
            (> line_ename_obj_start_y line_ename_obj_end_y)
            ; (= (vla-get-rotate line_ename_obj) 90.0)
          )
          (progn
            (setq new_line_ename_obj_start_y (- line_ename_obj_start_y line_ename_obj_length_Divide))
            (setq new_line_ename_obj_start_xy (strcat (rtos line_ename_obj_start_x 2 8) "," (rtos new_line_ename_obj_start_y 2 8) ))
            (command "rotate" line_ename "" new_line_ename_obj_start_xy 180)
          )
        )
        
        (setq i_line_ro (+ i_line_ro 1))
    )
    (setq i_line_ins 0)
    (while ;ins_dynamic_block
      (setq line_ename (ssname my_line_set i_line_ins))
      (setq line_ename_obj (vlax-ename->vla-object line_ename))
      (setq existing_rotate 
        (LM:round 
          (* (/ (vla-get-angle line_ename_obj) 3.14) 180)
        )
      )

      (setq line_ename_obj_start_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-startpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_end_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-endpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_start_x (car line_ename_obj_start_xyz))
      (setq line_ename_obj_start_y (cadr line_ename_obj_start_xyz))
      (setq line_ename_obj_end_y (cadr line_ename_obj_end_xyz))
      (setq line_ename_obj_length (vla-get-length line_ename_obj))
      (setq line_ename_obj_length_Divide (/ line_ename_obj_length 2))
      (setq line_ename_obj_start_xy (strcat (rtos line_ename_obj_start_x 2 8) "," (rtos line_ename_obj_start_y 2 8)))

        
        (if 
          (or 
            ; (> line_ename_obj_start_y line_ename_obj_end_y)
            (= existing_rotate 90.0)
          )
          (progn
            (setq dynamic_name "001TYP_S_LT50x150mm._T3")
            (command "insert" dynamic_name line_ename_obj_start_xy 1 0)
            (setq dynamic_last (entlbeast))
            (setq dynamic_last_obj (vlax-ename->vla-object dynamic_last))
        
            (setq H (LM:setdynpropvalue dynamic_last_obj "Distance1" line_ename_obj_length))
            ; (setq W (LM:setdynpropvalue dynamic_last_obj "W" 25))
          )
        )
        
        
        (setq i_line_ins (+ i_line_ins 1))
    )
)

(defun c:rr2_reset_rotation_line ()
  (setq my_line_set (ssget  '((0 . "LINE"))))
  (setq total_my_line_set (sslength my_line_set))

    (setq i_line_ro 0)
    (while ;reset_rotation_line
      (setq line_ename (ssname my_line_set i_line_ro))
      (setq line_ename_obj (vlax-ename->vla-object line_ename))

      (setq line_ename_obj_start_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-startpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_end_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-endpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_start_x (car line_ename_obj_start_xyz))
      (setq line_ename_obj_start_y (cadr line_ename_obj_start_xyz))
      (setq line_ename_obj_end_y (cadr line_ename_obj_end_xyz))
      (setq line_ename_obj_length (vla-get-length line_ename_obj))
      (setq line_ename_obj_length_Divide (/ line_ename_obj_length 2))
      
      
      (setq Dist1st  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getStartPoint line_ename_obj)))
      (setq Dist2nd  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getEndPoint line_ename_obj)))
      (setq lineLen (- Dist2nd Dist1st))
      (setq PointMid (vlax-curve-getPointAtDist line_ename_obj (+ Dist1st (* 0.5 lineLen))))
      
      


        
        (if 
          (or 
            (> line_ename_obj_start_y line_ename_obj_end_y)
            (= (angtos (vla-get-angle line_ename_obj)) (rtos 90.0 2 8))
          )
          (progn
            (setq new_line_ename_obj_start_y (- line_ename_obj_start_y line_ename_obj_length_Divide))
            (setq new_line_ename_obj_start_xy (strcat (rtos line_ename_obj_start_x 2 8) "," (rtos new_line_ename_obj_start_y 2 8) ))
            ; (command "rotate" line_ename "" new_line_ename_obj_start_xy 180)
            (command "rotate" line_ename "" PointMid 180)
          )
        )
        
        (setq i_line_ro (+ i_line_ro 1))
    )
)






