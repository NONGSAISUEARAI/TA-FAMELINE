;Dynamic Funcion Lee Mac
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
;Method Selection Set Ta Trai
  (defun TA:standard_func_selection_set_cond_case_1 () ;วิธีเก็บข้อมูล selection set แบบเดียวกับ pickfirst variable
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq ss_pre_filter_set_xx (ssget 
                                        (list 
                                          (cons 0 "INSERT") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
  )
  (defun TA:standard_func_entsel_case_1 () ;วิธีเก็บข้อมูลแบบตัวแปรเดี่ยวตามเงื่อนไข (condition rule) จนกว่าจะถูกต้อง
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Attribute Block\n"))))
            (if (and ;condition rule
                  (/= get_effectivename_ nil)
                  (= (vla-get-hasattributes (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                  ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
    ;user_input_get_block_(not loop)
      ;block_data_for insertion
        (setq blk_ename_ (car (entsel "specify_block_")))
        (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
        (if (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference") ;For Check objectname
          (progn
            (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
            (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
          )
          (alert "Object is not block.")
        )
      ;
    ;
    ;get_data_dynamic_block_object
      (if (= 1 1) 
        (progn
          ;user_input_get_block_(not loop)
            ;block_data_for insertion
              (if (/= (setq blk_ename_ (car (entsel "specify_block_"))) nil)
                (progn
                  (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
                )
                (alert "Object is blank.")
              )         
              (if (and
                    (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference");For Check objectname
                    (= (vla-get-isdynamicblock blk_ename_obj_) :vlax-true );For Check objectname is dynamicblock
                  ) 
                (progn
                  (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
                  (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
                )
                (alert "Object is not block.")
              )
            ;
          ;
        )
      )
    ;
  )
;
;Sorting Coordinate Function Ta Trai
  (defun CO:sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car (cadr a)) (car (cadr b)))))))
  )
  (defun CO:sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))))
  )
  (defun CO:sort_by_val_ (val_ list_ )  ;เรียงชุดข้อมูลตามแนวแกน
    (if (< val_ (length (car list_)))
      (progn
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth val_ a) (nth val_ b))))))
      )
      (setq error_ "The number of val is less than list")
    )
  )
  (defun TA:standard_list_croodinate_sorting (ss_post_filter_set_ sequence ) ;เรียง object ตามแนวแกน ใน selection set
    ; ;selection_set
      ;   (if  ;pre_select_ssget_or_post_select_ssget
      ;     (= 
      ;       (setq ss_pre_filter_set_xx (ssget "I" 
      ;                                         (list 
      ;                                           (cons 0 "INSERT") ;type of object
      ;                                           ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                           ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                           ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                         )
      ;                                   )
      ;       )
      ;       nil
      ;     )
      ;     (progn 
      ;       (setq ss_pre_filter_set_xx (ssget 
      ;                                     (list 
      ;                                       (cons 0 "INSERT") ;type of object
      ;                                       ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                       ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
      ;                                       ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                     )
      ;                                   )
      ;       )
      ;     )
      ;     (sslength ss_pre_filter_set_xx)
      ;   )
    ; ;
    ; ;user_input
      ;   (setq effectivename_val (LM:effectivename (vlax-ename->vla-object (car (entsel )))))
    ; ;
    ; ;filter_via_sub_func
      ;   (setq ss_post_filter_set_ (TA:Filter_ss_set_ ss_pre_filter_set_xx  effectivename_val))
    ; ;
    ;preloop_and_while get_ename_and_add_ins_pt
      (setq sum_data_ ())
      (setq ss_post_filter_set_i 0)
      (while  (< ss_post_filter_set_i (sslength ss_post_filter_set_))
        ;get_data
          (setq ss_post_filter_set_ename (ssname ss_post_filter_set_ ss_post_filter_set_i))
          (setq ss_post_filter_set_obj (vlax-ename->vla-object ss_post_filter_set_ename))
        ;
        ;sum_data_via_data
          (setq sum_data (TA:ename+vla-get-insertionpoint ss_post_filter_set_obj))
          (setq sum_data_ (cons sum_data sum_data_))
        ;
        (setq ss_post_filter_set_i (+ ss_post_filter_set_i 1))
      )
    ;
    ;sub_user_input_
      (cond
        ((= sequence "X") ;correct case
          (progn
            (princ "XXX")
            (setq sort_x (CO:sort_by_X sum_data_))
          )
        )
        ((= sequence "Y") ;correct case
          (progn
            (princ "YYY")
            (setq sort_y (CO:sort_by_Y sum_data_))
          )
        )
        ((and ;incorrect case
            (/= sequence "X")
            (/= sequence "Y")
          )
          (progn
            (princ "error_sorting_ename please specify argument X or Y")
            ; (setq sort_y (sort_by_y sum_data_))
          )
        )
      )
    ;
  )
;
;Filter Selection Set by Effectivename Ta Trai
  (defun TA:Filter_ss_set_ (ss_pre_filter_set_  effectivename) ;กรอง block object ด้วย LM:effectivename
    (setq ss_post_filter_set_ (ssadd))
    ; (setq effectivename "12a")
    ;preloop_and_while
      (setq ss_pre_filter_set_i 0)
      (while 
        ;get_data
          (setq ss_pre_filter_set_ename (ssname  ss_pre_filter_set_ ss_pre_filter_set_i) )
          (setq ss_pre_filter_set_obj (vlax-ename->vla-object ss_pre_filter_set_ename))
          (setq ss_pre_filter_set_obj_efname_ (LM:effectivename ss_pre_filter_set_obj))
        ;
        ;filter_data
          (if (= ss_pre_filter_set_obj_efname_ effectivename)
            (progn
            (ssadd ss_pre_filter_set_ename ss_post_filter_set_)
            )
            (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_pre_filter_set_i (/ 100 (float (sslength ss_pre_filter_set_)))) 2 0) "%\n"))
          )
        ;
        (setq ss_pre_filter_set_i (+ ss_pre_filter_set_i 1))
      )
    ;
    (sslength ss_post_filter_set_)
    (princ ss_post_filter_set_)
    (setq ss_post_filter_set_ ss_post_filter_set_)
  )
  (defun TA:Prop_Filter_ss_set_ (Selection_list_ nameprop_ propval_)
    (setq temp_ssget_ (ssadd))
    (setq Selection_list_i 0)
    (while (< Selection_list_i (sslength Selection_list_))
      (setq Selection_list_ename_ (ssname Selection_list_ Selection_list_i))
      (setq Selection_list_obj_ (vlax-ename->vla-object Selection_list_ename_))
        (if
          (= (vlax-get-property Selection_list_obj_ nameprop_) propval_ )
          (progn
            (setq temp_ssget_ (ssadd Selection_list_ename_ temp_ssget_))
          )
          (princ "\n")
        )
      (setq Selection_list_i (+ Selection_list_i 1))
    )
    (sslength temp_ssget_)
    (setq temp_ssget_ temp_ssget_)
  )
  (defun TA:member (str_name_ member_list_)
      (setq member_list_i 0)
      (setq found nil)
      (while (and
              (< member_list_i (length member_list_))
              (/= found T)
            )
        (setq member_list_val (nth member_list_i member_list_))
        (if (= str_name_ member_list_val)
          (progn
            (setq found T)  
          )
            (setq found nil)
        )
        (setq member_list_i (+ member_list_i 1))
      )
      (setq result found)
  )
  (defun TA:member_position (str_name_ pos member_list_)
      (setq member_list_i 0)
      (setq result_found_val ())
      (setq found nil)
      (while (and
              (< member_list_i (length member_list_))
              ; (/= found T)
            )
        (setq member_list_val (nth member_list_i member_list_))
        (if (= str_name_ (nth pos member_list_val))
          (progn
            ; (setq found T)
            (setq result_found_val (cons member_list_val result_found_val))          
          )
            (setq found nil)
        )
        (setq member_list_i (+ member_list_i 1))
      )
      (setq result result_found_val)
  )
  (defun remove_member_list_ (num_ point_list_)
    (setq new_list_ ())
    ; (setq num_ 2)
    (setq point_list_i 0)
    (while (< point_list_i (length point_list_))
      
      (setq point_list_set_ (nth point_list_i point_list_))
      (setq point_list_set_remove (vl-remove (nth num_ point_list_set_) point_list_set_))
      (setq new_list_ (cons point_list_set_remove new_list_))
      (setq point_list_i (+ point_list_i 1))
      
    )
    (setq result_ (reverse new_list_))
  )
  (defun remove_single_list_ (num_ point_list_)
    (setq new_list_ ())
      (setq point_list_set_ point_list_)
      (setq point_list_set_remove (vl-remove (nth num_ point_list_set_) point_list_set_))

    (setq result_ point_list_set_remove)
  )
  (defun TA:remove_val_list_ (val_list_ target_list_)
    ;example
      ; (setq target_list_ (list 1 2 3 4 5 6 7 8 9 nil))
      ; (setq target_list_ (list "ปรับปรุงอาตาร_DERMA_HEALTH_คลอ" "ง9_DB_STUDIO" ""))
      ; (setq val_list_ nil) 
      ; (TA:remove_val_list_ 8 target_list_)
    ;
    ;preloop_and_while
      (setq target_list_i 0)
      (while (< target_list_i (length target_list_))
        (setq target_list_ename_ (nth  target_list_i target_list_))
        (if (or (= target_list_ename_ nil) (= target_list_ename_ "") )
          (progn
            (setq target_list_ (vl-remove (nth target_list_i target_list_) target_list_))
          )
        )
        (setq target_list_i (+ target_list_i 1))
      )
      (setq target_list_ target_list_)
    ; 
  )

;
;CREATE_OBJECT_FUNC
  ;text
    (defun create-text-vla (pt textString height rotation / acadDoc textObj) 
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))
      (setq textObj (vla-addtext modelSpace textString pt height))
      (vla-put-Height textObj height)
      (vla-put-Rotation textObj rotation)
      textObj
    )
    (defun create-Mtext-vla (pt_ width text)
    ;; This example creates an MText object in model space.
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    ;; Define the multiline text object
    (setq corner (vlax-3d-point pt_)
          width 500
    )
    ;; Creates the mtext Object
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq MTextObj (vla-AddMText modelSpace corner width text))
    ; (vla-ZoomAll acadObj)
  )
  ;
  ;add+pline_data
    (defun TA:vla-AddLightWeightPolyline_(point_list_)
        ;; This example creates a lightweight polyline in model space.
        (setq acadObj (vlax-get-acad-object))
        (setq doc (vla-get-ActiveDocument acadObj))
        
        ;; Define the intersection points
        

        ;; Convert 3D points to a flat 2D list
        (setq points (apply 'append (mapcar '(lambda (pt) (list (car pt) (cadr pt))) point_list_)))

        ;; Convert to safe array
        (setq pointArray (vlax-make-safearray vlax-vbDouble (cons 0 (- (length points) 1))))
        (vlax-safearray-fill pointArray points)
            
        ;; Create a lightweight Polyline object in model space
        (setq modelSpace (vla-get-ModelSpace doc))
        (setq plineObj (vla-AddLightWeightPolyline modelSpace pointArray))
        (vla-ZoomAll acadObj)
    )
    (defun create-pline (coords)
      (if (= (length coords) 0)
          nil
        (progn
          ; สร้างพอลีไนจากค่าพิกัด
          (command "pline" (car coords) )
          
          ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
          (foreach coord (cdr coords)
            (command coord )
          )
        )
      )
      (command "")
    )
    (defun create-plines (coords close_loop_)
      (if (= (length coords) 0)
          nil
        (progn
          ; สร้างพอลีไนจากค่าพิกัด
          (command "pline" (car coords) )
          
          ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
          (foreach coord (cdr coords)
            (command coord )
          )
        )
      )
      (command "")
      (cond
        ( (= close_loop_ "close") 
          (progn 
            (setq new_line_data_ (vlax-ename->vla-object (entlast)))
            (vla-put-closed new_line_data_ :vlax-true)
          )
        )
        ((= close_loop_ "open") 
          (progn 
            (setq new_line_data_ (vlax-ename->vla-object (entlast)))
            (vla-put-closed new_line_data_ :vlax-false)
          )
        )
      )
    )
    (defun TA:vla-addvertex_ (pline_obj_ vertex_num_ point_list_)
      (setq point_list_new (remove_single_list_ 2 point_list_))
      (setq newVertex (vlax-make-safearray vlax-vbDouble '(0 . 1)))
      (vlax-safearray-fill newVertex point_list_new)
      (vla-addvertex pline_obj_ vertex_num_ newVertex)
    )
    (defun TA:add_vertex_point_pline_via_breakpt_ (ename_cut_edge_ ename_intersection_obj_)
      
      ;get_data_main_cut_and_main_intersection_
        (setq bb_obj_ (vlax-ename->vla-object ename_cut_edge_))     
        (setq ss_obj_ (vlax-ename->vla-object ename_intersection_obj_))     
        (setq intersec_ (LM:intersections+0 
                          bb_obj_
                          ss_obj_
                          acextendnone
                        )
        )
        (setq vertex_list_ (TA:Get_Pline_vertext_angle_case1 
                              ename_intersection_obj_
                            )
        )
        (setq intersec_i 0)
      ;
      ;preloop_and_while adding_vertex_point_
        (setq ename_list_ ())
        (setq ssadd_list_ (ssadd))
        (while   (< intersec_i (length intersec_))
          (setq intersec_val_ (nth intersec_i intersec_))
          (command "breakatpoint" ename_intersection_obj_ intersec_val_ "")
          (setq new_E_ (entlast))
          ; (command "pselect" new_E_)
          ; (command "pselect" ss)
          (setq ename_list_ (cons new_E_ ename_list_))
          (setq ssadd_list_ (ssadd new_E_ ssadd_list_))
          (setq ssadd_list_ (ssadd ename_intersection_obj_ ssadd_list_))
          (sslength ssadd_list_)
          (command "_.pedit" "_m" ssadd_list_ "" "_j" "" "")
          (setq ename_intersection_obj_ (entlast))
          (setq intersec_i (+ intersec_i 1))
          
        )
      ;
    )
    (defun TA:add_vertex_point_pline_via_breakpt_1 (ename_cut_edge_ ename_intersection_obj_)
      
          
          ;get_data_main_cut_and_main_intersection_
            (setq bb_obj_ (vlax-ename->vla-object ename_cut_edge_))     
            (setq ss_obj_ (vlax-ename->vla-object ename_intersection_obj_))     
            (setq intersec_ (LM:intersections+0 
                              bb_obj_
                              ss_obj_
                              acextendnone
                            )
            )
            (command "pedit" ename_intersection_obj_ "" "c" "" )
            (setq vertex_list_ (TA:Get_line_ins_point_ 
                                  ename_intersection_obj_
                                )
            )
            (setq intersec_i 0)
          ;
          ;preloop_and_while adding_vertex_point_
            (setq ename_list_ ())   
            (setq ssadd_list_ (ssadd))
            (while   (< intersec_i (length intersec_))
              (setq intersec_val_ (nth intersec_i intersec_))
              (command "breakatpoint" ename_intersection_obj_ intersec_val_ "")
              
              (setq new_E_ (entlast))
              ; (command "pselect" new_E_)
              ; (command "pselect" ss)
              (setq ename_list_ (cons new_E_ ename_list_))
              (setq ssadd_list_ (ssadd new_E_ ssadd_list_))
              (setq ssadd_list_ (ssadd ename_intersection_obj_ ssadd_list_))
              (sslength ssadd_list_)
              (command "_.pedit" "_m" ssadd_list_ "y" "" "_j" "" "")
              (command "_.pedit" "_m" ssadd_list_ "" "y" )
              (setq ename_intersection_obj_ (entlast))
              (setq intersec_i (+ intersec_i 1))
              
            )
          ;
    )
  ;
  ;add_circle
    (defun TA:vla-addarc_ (center_ rad_ point_list_ )
      ; point_list_ must be two coord set in list
      ; do not vlax-3d_point w/ agument
      ;sub_func_
        (defun CO:sort_by_val_ (val_ list_ )  ;เรียงชุดข้อมูลตามแนวแกน
          (if (< val_ (length (car list_)))
            (progn
              (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth val_ a) (nth val_ b))))))
            )
            (setq error_ "The number of val is less than list")
          )
        )
      ;
      ;basic_get_data_
        (setq acadObj (vlax-get-acad-object))
        (setq doc (vla-get-ActiveDocument acadObj))
        (setq modelSpace (vla-get-ModelSpace doc))
      ;
      ;advance_get_data
        (setq point_list_ (reverse (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_)))))
      ;
      ;preloop_and_while sorting arc_line
        (setq angle_list_ ())
        (setq point_list_i 0)
        (setq point_list_ii 1)
        (while (< point_list_i (- (length point_list_) 1))
          (setq point_list_set_1 (nth point_list_i point_list_))
          (setq angle_ (atof (angtos (angle center_ point_list_set_1))))
          (setq sum (list
                      point_list_set_1
                      angle_
                    )
          )
          (setq angle_list_ (cons sum angle_list_))
          (setq point_list_i (+ point_list_i 1))
          (setq point_list_ii (+ point_list_ii 1))
        )
      ;
      (setq point_list_ (append (CO:sort_by_val_ 1 angle_list_) (list (car (CO:sort_by_val_ 1 angle_list_) ))) )
      (setq point_list_i 0)
      (setq point_list_ii 1)
      (while (< point_list_i (- (length point_list_) 1))
        (setq point_list_set_1 (car (nth point_list_i point_list_)))
        (setq point_list_set_2 (car (nth point_list_ii point_list_)))
        (setq start_ang_ (angle center_ point_list_set_1))
        (setq end_ang_ (angle center_ point_list_set_2))
        ; (setq rad_ (distance center_ point_list_set_1))
        
        (vla-addarc modelSpace (vlax-3d-point center_) rad_  start_ang_ end_ang_)
        (setq point_list_i (+ point_list_i 1))
        (setq point_list_ii (+ point_list_ii 1))
      )
      
    )
  ;
  ;add-point
    (defun vla-addpoint_ (point) 
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))
      (vla-addpoint modelSpace (vlax-3d-point point))
    )
  ;
  ;get_line
    (defun TA:Get_line_ins_point_ (ename_)
      
      ; (setq ename_ (car (entsel)))
      (setq ename_obj_ (vlax-ename->vla-object ename_))
      (setq ename_obj_str_pt_ (vlax-safearray->list (vlax-variant-value (vla-get-startpoint ename_obj_))))
      (setq ename_obj_end_pt_ (vlax-safearray->list (vlax-variant-value (vla-get-endpoint ename_obj_))))
      (setq sum_ (list
                   ename_obj_str_pt_
                   ename_obj_end_pt_
                 )
      )
    )
    
  ;
  ;offset
    (defun TA:Offset_line (ename num_ )
      ; (setq 1-PL_en_ (car (entsel)))
      (setq 1-PL_en_obj (vlax-ename->vla-object ename))
      (vla-Offset 1-PL_en_obj num_)
    )
    (defun TA:Offset_Pline_multi_vertex_ (ename num_ mode)
      ; ;for_testing_mode
      ;   (setq ename (car (entsel)))
      ;   (setq num_ 0.01)
      ;   (setq mode "min")
      ; ;
      ;test_vla-get-objectname_process
        (if (= (vla-get-objectname (vlax-ename->vla-object ename)) "AcDbPolyline")
          (progn
            (setq Get_Pline_ (entget ename))
            (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
            (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
          )
        )
      ;
      ;test_length_vertex_process
        (if 
          (and
            (= (vla-get-objectname (vlax-ename->vla-object ename)) "AcDbPolyline")
            (> (length Get_Pline_vtx_pt_) 2)
          )
          (progn
            (setq num_plus_ (* num_ 1))
            (setq num_minus_ (* num_ -1))
            (setq 1-PL_en_obj (vlax-ename->vla-object ename))
          )
          (princ "s")
        )
      ;
      ;plus mode process
        (if 
          (and
            (= (vla-get-objectname (vlax-ename->vla-object ename)) "AcDbPolyline")
            (> (length Get_Pline_vtx_pt_) 2)
          )
          (progn
            (setq result_num_plus_ ())
            (vla-Offset 1-PL_en_obj num_plus_)
            (setq num_plus_obj_ (entlast))
            (setq num_plus_area_ (vla-get-area (vlax-ename->vla-object num_plus_obj_)))
            (setq result_num_plus_ (cons num_plus_area_ result_num_plus_))
            (setq result_num_plus_ (cons num_plus_obj_ result_num_plus_))
          )
          (princ "s")
        )
      ;
      ;minus mode process
        (if 
          (and
            (= (vla-get-objectname (vlax-ename->vla-object ename)) "AcDbPolyline")
            (> (length Get_Pline_vtx_pt_) 2)
          )
          (progn
            (setq result_num_minus_ ())
            (vla-Offset 1-PL_en_obj num_minus_)
            (setq num_minus_obj_ (entlast))
            (setq num_minus_area_ (vla-get-area (vlax-ename->vla-object num_minus_obj_)))
            (setq result_num_minus_ (cons num_minus_area_ result_num_minus_))
            (setq result_num_minus_ (cons num_minus_obj_ result_num_minus_))
          )
          (princ "s")
        )
      ;
      ;result_data
        (setq resul_overall_ (append (list result_num_minus_) (list result_num_plus_)))
      ;
      (cond 
        ((and
           (= mode "max")
         )
         (progn
          (setq max_data_ (CO:sort_by_val_ 1 resul_overall_  ))
          (vla-delete (vlax-ename->vla-object (car (nth 0 max_data_))))
          (setq new_data_ (car (nth 1 max_data_)))
         )
        )
        ((= mode "min")
         (progn
          (setq min_data_ (reverse (CO:sort_by_val_ 1 resul_overall_  )))
          (vla-delete (vlax-ename->vla-object (car (nth 0 min_data_))))
          (setq new_data_ (car (nth 1 min_data_)))
          
         )
        )
        
      )
    )
  ;
;
;sub_function
  (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
  )
  (defun sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (caddr a) (caddr b))))))
  )
  (defun sort_by_list_2nd_car (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car (nth 1 a)) (car (nth 1 b)))))))
  )
  (defun sort_by_list_2nd_cadr (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr (nth 1 a)) (cadr (nth 1 b)))))))
  )
  (defun sort_by_higest (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 4 a) (nth 4 b))))))
  )
  (defun sort_by_lowest (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 6 a) (nth 6 b))))))
  )
  (defun sort_by_higest_hon (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 3 a) (nth 3 b))))))
  )
  (defun sort_by_lowest_hon (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 6 a) (nth 6 b))))))
  )
  (defun sort_by_higest_ver (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 5 a) (nth 5 b))))))
  )
  (defun sort_by_lowest_ver (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 3 a) (nth 3 b))))))
  )

  (defun LM:RemoveNth (n l / i) 
    (setq i -1)
   (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
  )
  (defun LM:str->lst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
  )
  (defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
  )
;
;sub_command
  (defun c:z63A_text2att () 
    (setq abx_ (entget (car (entsel))))
    (setq abx_ename (cdr (assoc -1 abx_)))
    (setq abx_obj (vlax-ename->vla-object abx_ename))
    ; (setq abx_dump (vlax-dump-object abx_obj))
    (setq abx_obj_text (vla-get-textstring abx_obj))
    (setq abx_obj_text_+val (strcat abx_obj_text "_value"))
    (setq att_xyz-ins (getpoint))

    (command "-attdef" "L" "" abx_obj_text_+val "" "----" "S" "TA" "J" "MC" att_xyz-ins 0)
    (setq att_last_ (entlast))
    (setq att_last_obj (vlax-ename->vla-object att_last_))
    (vla-put-height att_last_obj 1.8)
  )
  (defun c:z63B_export_exelText_to_cad ()
    (setq text_sc 1.8)
    (setq excelText_to_cad_ (getstring "specify text" ))
    (setq ptpt_ (vlax-3d-point (getpoint "specify point")))
    (create-text-vla ptpt_ excelText_to_cad_ text_sc 0 )
    (setq new_ename_text (entlast))
    (setq new_ename_text_obj_ (vlax-ename->vla-object new_ename_text))
    (setq new_ename_text_obj_get_color (vla-put-color new_ename_text_obj_ 0)) ;0 = bylock, 256 = bylayer
    (setq new_ename_text_obj_get_layer (vla-put-layer new_ename_text_obj_ "0")) ;by_string_name_layer
    (setq new_ename_text_obj_get_linetype (vla-put-linetype new_ename_text_obj_ "byblock")) ;by_string_name_linetype
  )
  

    ; (setq new_ename_text (entlast))
    ; (setq new_ename_text_obj_ (vlax-ename->vla-object new_ename_text))
    ; (vla-get-textstring new_ename_text_obj_)
    ; (vla-put-textstring new_ename_text_obj_   "NUM.PART\\PSYM.PART\\PMATT_VAL\\PMATT_THK_VAL\\PMATT_SUBFIX_1\\PMATT_SUBFIX_2\\Pkind of usage matterial\\PPART-W1\\PPART-H1\\PPART-W2\\PPART-W3\\PPART-W4\\PPART-LEN\\P") ;กำลังดูเรื่องการรวม text ใน บรรทัด ของ excel ก่อนนำไปรวมกับ data  

    ; (setq new_ename_text_obj_get_color (vla-put-color new_ename_text_obj_ 0)) ;0 = bylock, 256 = bylayer
    ; (setq new_ename_text_obj_get_layer (vla-put-layer new_ename_text_obj_ "0")) ;by_string_name_layer
    ; (setq new_ename_text_obj_get_linetype (vla-put-linetype new_ename_text_obj_ "byblock")) ;by_string_name_linetype
;
;Z44_changing_prop
  ; all line_layer_color
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints")
    ;
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-byblock "byblock") 
    ;
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 

      (setq li-TA_L_000-10 "TA_L_000.10.LIN") 
      (setq li-TA_L_000-25 "TA_L_000.25.LIN") 
      (setq li-TA_L_000-50 "TA_L_000.50.LIN") 
      (setq li-TA_L_001-00 "TA_L_001.00.LIN") 
      (setq li-TA_L_002-00 "TA_L_002.00.LIN") 
      (setq li-TA_L_003-00 "TA_L_003.00.LIN") 
      (setq li-TA_L_005-00 "TA_L_005.00.LIN") 
      (setq li-TA_L_025-00 "TA_L_025.00.LIN") 
      (setq li-TA_L_050-00 "TA_L_050.00.LIN") 
      (setq li-TA_L_100-00 "TA_L_100.00.LIN") 
    ;
  ;
  ;grid line group
    (defun c:G001()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-001_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G002()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-002_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G005()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-005_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G010()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-010_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G015()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-015_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G020()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-020_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G025()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-025_GG_TA_LINE "")
        (command "pselect" select_group "")    
      ;
    )
    (defun c:G030()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-030_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G035()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-035_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G050()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-050_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G075()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-075_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
    (defun c:G100()
      ; selection part 
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
        (command "CHPROP" select_group "" "LA" LY-000-GRID "")
        (command "CHPROP" select_group "" "C" c-grey08 "")
        (command "CHPROP" select_group "" "LT" li-100_GG_TA_LINE "") 
        (command "pselect" select_group "")
      ;
    )
  ;
  ;hidden line group
    (defun c:L00010 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_000-10 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L00025 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_000-25 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L00050 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_000-50 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L001 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_001-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L002 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_002-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L003 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_003-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L005 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_005-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L025 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_025-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L050 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_050-00 "")
      (command "pselect" select_group "")
      ;
    )
    (defun c:L100 () 
      ; selection part
        (setq select_group (ssget 
                                  ;  (list
                                  ;    (cons 0 "INSERT") ;type of object
                                  ;    (cons 8 "000 - GRID") ;kind of layer
                                  ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                  ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                  ;  )
                          )
        )
      ;
      ; command part
      (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
      (command "CHPROP" select_group "" "C" c-bylayer "")
      (command "CHPROP" select_group "" "LT" li-TA_L_100-00 "")
      (command "pselect" select_group "")
      ;
    )
  ;
  ;reset linetype layer color
    (defun c:SZ_setzero_layer ()
      (command "-layer" "s" LY-0 "")
      (command "-color" c-bylayer "")
      (command "-LINETYPE" "s" li-bylayer "")
    )
    (defun c:A04_TUBESERIES ()
      (command "-layer" "s" LY-A04_TUBESERIES "")
      (command "-color" c-green "")
      (command "-LINETYPE" "s" li-bylayer "")
    )
    (defun c:LY-001-ASCESSORIES ()
      (command "-layer" "s" LY-001-ASCESSORIES "")
      (command "-color" c-red "")
      (command "-LINETYPE" "s" li-bylayer "")
    )
    (defun c:LY-000-GRID ()
      (command "-layer" "s" LY-000-GRID "")
      (command "-color" c-bylayer "")
      (command "-LINETYPE" "s" li-bylayer "")
    )
    
  ;
; 

;DATA_BASE_product_name_
  (defun c:z63C_ADD_PRODUCT_TO_LV_DATA_FROM_EXCEL_ ()
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a block named 'LV_DATA_FROM_EXCEl'.
    ;The operation of the command will add dimension Acad's object to the block's attribute values and generate a set of text.     
    ;
    ;condition_product_name_
      (setq product_number_ (cond 
                              ((getint 
                                  (strcat "\nuser_input_for_product_number
                                            \nALU.TUBE_10x50mm._AT_PROFILE_   = 1
                                            \nALU.TUBE_10x50mm._RT_PROFILE_   = 2
                                            \nALU.TUBE_10x100mm._RT_PROFILE_  = 3
                                            \nALU.TUBE_20x50mm._AT_PROFILE_   = 4
                                            \nALU.TUBE_20x50mm._RT_PROFILE_   = 5
                                            \nALU.TUBE_1\"x1\"x1.2mm._        = 6
                                            \nALU.TUBE_1\"x2\"x1.2mm._        = 7
                                            \nALU.TUBE_1\"x3\"x1.2mm._        = 8
                                            \nALU.TUBE_1\"x4\"x1.2mm._        = 9
                                            \nALU.TUBE_2\"x2\"x1.2mm._        = 10
                                            \nALU.TUBE_2\"x4\"x1.2mm._        = 11
                                            \nALU.TUBE_2\"x6\"x1.5mm._        = 12
                                            \nALU.TUBE_2\"x8\"x3.0mm._        = 13
                                            \nALU.TUBE_3\"x4\"x4.5mm._        = 14
                                            \n<" 
                                          (rtos 
                                            (setq product_number_ (cond 
                                                                    (product_number_)
                                                                    (1.0)
                                                                  )
                                            )
                                          )
                                          "> : "
                                  )
                                )
                              )
                              (product_number_)
                            )
      ) 
      (cond ;product_name_case_
        (;product_name_case_1
          (and
              (= product_number_ 1)
          )
          (progn
            (setq W_LV_VALUE 10 )
            (setq H_LV_VALUE 50 )
            (setq THK_LV_VALUE 1.0 )
            (setq matterial_case_ "ALU.TUBE_10x50mm._AT_PROFILE_")
            (setq matterial_code_case_ "1050AT")
            (princ "product_name_case_1")
          )
        )
        (;product_name_case_2
          (and
              (= product_number_ 2)
          )
          (progn
            (setq W_LV_VALUE 10 )
            (setq H_LV_VALUE 50 )
            (setq THK_LV_VALUE 1.1 )
            (setq matterial_case_ "ALU.TUBE_10x50mm._RT_PROFILE_")
            (setq matterial_code_case_ "1050RT")
            (princ "product_name_case_2")
          )
        )
        (;product_name_case_3
          (and
              (= product_number_ 3)
          )
          (progn
            (setq W_LV_VALUE 10 )
            (setq H_LV_VALUE 100 )
            (setq THK_LV_VALUE 1.1 )
            (setq matterial_case_ "ALU.TUBE_10x100mm._RT_PROFILE_")
            (setq matterial_code_case_ "10100AT")
            (princ "product_name_case_3")
          )
        )
        (;product_name_case_4
          (and
              (= product_number_ 4)
          )
          (progn 
            (setq W_LV_VALUE 20)
            (setq H_LV_VALUE 50)
            (setq THK_LV_VALUE 1.0)
            (setq matterial_case_ "ALU.TUBE_20x50mm._AT_PROFILE_")
            (setq matterial_code_case_ "2050AT")
            (princ "product_name_case_4")
          )
        )
        (;product_name_case_5
          (and
              (= product_number_ 5)
          )
          (progn 
            (setq W_LV_VALUE 20)
            (setq H_LV_VALUE 50)
            (setq THK_LV_VALUE 1.1)
            (setq matterial_case_ "ALU.TUBE_20x50mm._RT_PROFILE_")
            (setq matterial_code_case_ "2050RT")
            (princ "product_name_case_5")
          )
        )
        (;product_name_case_6
          (and
              (= product_number_ 6)
          )
          (progn 
            (setq W_LV_VALUE 25)
            (setq H_LV_VALUE 25)
            (setq THK_LV_VALUE 1.2)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_6")
          )
        )
        (;product_name_case_7
          (and
              (= product_number_ 7)
          )
          (progn 
            (setq W_LV_VALUE 25)
            (setq H_LV_VALUE 25)
            (setq THK_LV_VALUE 1.2)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_7")
          )
        )
        (;product_name_case_8
          (and
              (= product_number_ 8)
          )
          (progn 
            (setq W_LV_VALUE 25)
            (setq H_LV_VALUE 75)
            (setq THK_LV_VALUE 1.2)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_8")
          )
        )
        (;product_name_case_9
          (and
              (= product_number_ 9)
          )
          (progn 
            (setq W_LV_VALUE 25)
            (setq H_LV_VALUE 100)
            (setq THK_LV_VALUE 1.2)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_9")
          )
        )
        (;product_name_case_10
          (and
              (= product_number_ 10)
          )
          (progn 
            (setq W_LV_VALUE 50)
            (setq H_LV_VALUE 50)
            (setq THK_LV_VALUE 1.2)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_10")
          )
        )
        (;product_name_case_11
          (and
              (= product_number_ 11)
          )
          (progn 
            (setq W_LV_VALUE 50)
            (setq H_LV_VALUE 100)
            (setq THK_LV_VALUE 2.0)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_11")
          )
        )
        (;product_name_case_12
          (and
              (= product_number_ 12)
          )
          (progn 
            (setq W_LV_VALUE 50)
            (setq H_LV_VALUE 150)
            (setq THK_LV_VALUE 2.0)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_12")
          )
        )
        (;product_name_case_13
          (and
              (= product_number_ 13)
          )
          (progn 
            (setq W_LV_VALUE 50)
            (setq H_LV_VALUE 200)
            (setq THK_LV_VALUE 3.0)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_13")
          )
        )
        (;product_name_case_14
          (and
              (= product_number_ 14)
          )
          (progn 
            (setq W_LV_VALUE 50)
            (setq H_LV_VALUE 200)
            (setq THK_LV_VALUE 4.5)
            (setq matterial_case_ "ALU.TUBE")
            (setq matterial_code_case_ (strcat 
                                                  (rtos (/ W_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos (/ H_LV_VALUE 25) 2 0) "\""
                                                  "x"
                                                  (rtos THK_LV_VALUE 2 1) "mm."
                                                )
            )
            (princ "product_name_case_14")
          )
        )
      )
    ;
    ;user_input_selection_blk_object_
      ;REF_blk_for_LV_DATA_FROM_EXCEl  
        (setq REF_block_ nil)
        (while (not REF_block_)
            (setq REF_block_ (entsel "\nSelect a First BLOCK "))
            (if 
              (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
                (setq REF_block_ (car REF_block_))
                
                (progn
                  (setq REF_block_ nil)
                  (alert "Please select a BLOCK")
                )
            )
            (if
              (/= REF_block_ nil)
              (progn 
                (setq REF_block_obj (vlax-ename->vla-object REF_block_))
              )

              (princ "/n")
            )
            (if (= REF_block_ nil)
              (progn
                (alert "Please select a LV_DATA_FROM_EXCEl \n")
              )
              (cond
                ((and 
                  (= (LM:effectivename REF_block_obj) 
                      "LV_DATA_FROM_EXCEl"
                  )
                  (= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
                ) 
                  (progn 
                    (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                    (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                  )
                  (princ "\n")
                )
                ((or 
                  (/= (LM:effectivename REF_block_obj) 
                      "LV_DATA_FROM_EXCEl"
                  )
                  (/= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
                ) 
                  (progn 
                    (setq REF_block_ nil)
                    (alert "Please select a LV_DATA_FROM_EXCEl \n")
                  )
                  (princ "/n")
                )
              )
            )
        )
      ;
    ;
    ;input_attribute_data_
      (LM:vl-setattributevalue REF_block_obj "W_LV_VALUE" W_LV_VALUE )
      (LM:vl-setattributevalue REF_block_obj "H_LV_VALUE" H_LV_VALUE )
      (LM:vl-setattributevalue REF_block_obj "THK._LV_VALUE" THK_LV_VALUE )
      (LM:vl-setattributevalue REF_block_obj "MATTERIAL_VALUE" matterial_case_ )
      (LM:vl-setattributevalue REF_block_obj "MATTERIAL_CODE_VALUE" matterial_code_case_ )
    ;
    ;repeart_command
      (C:z630re_excel_LV_MAIN_DATA_refresh_data_630refresh)
    ;
  )
;

(defun c:z630_excel_LV_MAIN_DATA_to_ATT_CAD ()
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named 'LV_DATA_FROM_EXCEl'.
  ;The operation of the command will read and send range cell data in excel to the block attribute block data and generate a set of text in dynamic block.
  ;
  ;insert_data_from_excel
    (command "insert" "LV_DATA_FROM_EXCEl" "0,0" 1 0)
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (setq ref_LV_data_obj_att_total (length ref_LV_data_obj_att_))
  ;
  ;preloop_and_while 
    (setq ref_LV_data_obj_att_i 0)
    (while
      (< ref_LV_data_obj_att_i ref_LV_data_obj_att_total)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_i ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_i (+ ref_LV_data_obj_att_i 1))
    )
    (setq ucs_world (vlax-3d-point 0 0 0))
    (setq new_ins_pt (getpoint ))
    (setq new_ins_pt1 (vlax-3d-point new_ins_pt))
    (vla-move ref_LV_data_obj ucs_world new_ins_pt1)
    (vla-put-xscalefactor ref_LV_data_obj 5)
  ;
)
(defun C:z630re_excel_LV_MAIN_DATA_refresh_data_630refresh ()
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a block named 'LV_DATA_FROM_EXCEl'.
    ;The operation of the command will refresh data in attribute block .
    ;Attribute block is dimension of matterial and generate a set of text in dynamic block 
    ;
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "i" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq ss_pre_filter_set_xx_ (ssget ":s"
                                        (list 
                                          (cons 0 "INSERT") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;get_data_process_

      (if 
        (and 
            (= (sslength ss_pre_filter_set_xx_) 1)
            (= (vla-get-objectname (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0))) "AcDbBlockReference")
            (= (LM:effectivename (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0)) ) "LV_DATA_FROM_EXCEl" )
        )
        (progn
          (setq LV_DATA_FROM_EXCEl_list_ (LM:vl-getattributevalues (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0))))
        )
      )
    ;
    ;generate_data_in_box
     ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          ;get BLK-NAME_
            (setq BLOCK_NAME_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"))
          ;
          ;get_MATTERIAL_
            (setq MATTERIAL_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "MATTERIAL_VALUE"))
          ;
          ;get_MOCKUP SIZE
            (setq WIDTH_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "WIDTH_VALUE"))
            (setq HEIGHT_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "HEIGHT_VALUE"))
            (setq DIRECTION_DISTANCE_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "DIRECTION DISTANCE_VALUE"))
            (setq EDGETOCENTRICLV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO CENTRIC.LV_VALUE"))
            (setq EDGETOSURFACELV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO SURFACE.LV_VALUE"))
          ;
          ;get_LOUVER_DATA
            (setq W_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "W_LV_VALUE"))
            (setq H_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "H_LV_VALUE"))
            (setq THK_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "THK._LV_VALUE"))
            (setq QTYLV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "QTY.LV_VALUE"))
            (setq DISTANCE_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "DISTANCE_LV_VALUE"))
            (setq TOTAL_L_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_L_VALUE"))
          ;
          ;get_SUB_FAME_
            (setq SUB_FAME_DIST_C-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST.C-C_VALUE"))
            (setq SUB_TOTAL\\PC-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_TOTAL\\PC-C_VALUE"))
            (setq SUB_FAME_DIST_QTY_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME_DIST._QTY._VALUE"))
            (setq SUB_FAME_DIST_E-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST. E-C_VALUE"))
          ;
          ;put-matterial_process
            ;pre_set_var
            (setq matterial_case_ "AL.TUBE" )
            (setq matterial_case_ "" )
            (setq matterial_code_case_ "" )
            ;
            (cond ;LTEIWOOD_CASE_
              (;litewood_case_1
                 (and
                    (= (atof W_LV_VALUE) 12)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 0.7)
                 )
                 (progn
                   (setq matterial_case_ "LITEWOOD_PANEL")
                   (setq matterial_code_case_ "LIT-PN-01")
                   (princ "matterial_case_1")
                 )
              )
              (;litewood_case_2
                 (and
                    (= (atof W_LV_VALUE) 20)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 0.7)
                 )
                 (progn
                   (setq matterial_case_ "LITEWOOD_PANEL")
                   (setq matterial_code_case_ "LIT-PN-04")
                   (princ "matterial_case_2")
                 )
              )
              (;litewood_case_3
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 100)
                    (= (atof THK_LV_VALUE) 0.7)
                 )
                 (progn
                   (setq matterial_case_ "LITEWOOD_PANEL")
                   (setq matterial_code_case_ "LIT-PN-02")
                   (princ "matterial_case_3")
                 )
              )
              (;litewood_case_4
                 (and
                    (= (atof W_LV_VALUE) 50)
                    (= (atof H_LV_VALUE) 150)
                    (= (atof THK_LV_VALUE) 0.7)
                 )
                 (progn
                   (setq matterial_case_ "LITEWOOD_PANEL")
                   (setq matterial_code_case_ "LIT-PN-03")
                   (princ "matterial_case_4")
                 )
              )
              
            )
            (cond ;ALU.TUBE_SERIES_CASE_
              (;ALTUBESERIES_case_1
                 (and
                    (= (atof W_LV_VALUE) 10)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.0)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE_10x50mm._AT_PROFILE_")
                   (setq matterial_code_case_ "1050AT")
                   (princ "ALTUBESERIES_case_1")
                 )
              )
              (;ALTUBESERIES_case_2
                 (and
                    (= (atof W_LV_VALUE) 10)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.1)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE_10x50mm._RT_PROFILE_")
                   (setq matterial_code_case_ "1050RT")
                   (princ "ALTUBESERIES_case_2")
                 )
              )
              (;ALTUBESERIES_case_3
                 (and
                    (= (atof W_LV_VALUE) 10)
                    (= (atof H_LV_VALUE) 100)
                    (= (atof THK_LV_VALUE) 1.0)
                 )
                 (progn 
                   (setq matterial_case_ "ALU.TUBE_10x100mm._AT_PROFILE_")
                   (setq matterial_code_case_ "10100AT")
                   (princ "ALTUBESERIES_case_3")
                 )
              )
              (;ALTUBESERIES_case_4
                 (and
                    (= (atof W_LV_VALUE) 20)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.0)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE_20x50mm._AT_PROFILE_")
                   (setq matterial_code_case_ "2050AT")
                   (princ "ALTUBESERIES_case_4")
                 )
              )
              (;ALTUBESERIES_case_5
                 (and
                    (= (atof W_LV_VALUE) 20)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.1)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE_20x50mm._RT_PROFILE_")
                   (setq matterial_code_case_ "2050RT")
                   (princ "ALTUBESERIES_case_5")
                 )
              )
            )
            (cond ;AL.TUBE_case_
              (;AL.TUBE_case_1
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 25)
                    (= (atof THK_LV_VALUE) 1.2)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_1") 
                 )
              )
              (;AL.TUBE_case_2
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.2)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_2")
                 )
              )
              (;AL.TUBE_case_3
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 75)
                    (= (atof THK_LV_VALUE) 1.2)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_3")
                 )
              )
              (;AL.TUBE_case_4
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 100)
                    (= (atof THK_LV_VALUE) 1.2)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_4")
                 )
              )
              (;AL.TUBE_case_5
                 (and
                    (= (atof W_LV_VALUE) 25)
                    (= (atof H_LV_VALUE) 150)
                    (= (atof THK_LV_VALUE) 1.5)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_5")
                 )
              )
              (;AL.TUBE_case_6
                 (and
                    (= (atof W_LV_VALUE) 50)
                    (= (atof H_LV_VALUE) 50)
                    (= (atof THK_LV_VALUE) 1.2)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_6")
                 )
              )
              (;AL.TUBE_case_7
                 (and
                    (= (atof W_LV_VALUE) 50)
                    (= (atof H_LV_VALUE) 100)
                    (= (atof THK_LV_VALUE) 2.0)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_7")
                 )
              )
              (;AL.TUBE_case_8
                 (and
                    (= (atof W_LV_VALUE) 50)
                    (= (atof H_LV_VALUE) 150)
                    (= (atof THK_LV_VALUE) 2.0)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_8")
                 )
              )
              (;AL.TUBE_case_9
                 (and
                    (= (atof W_LV_VALUE) 50)
                    (= (atof H_LV_VALUE) 200)
                    (= (atof THK_LV_VALUE) 3.0)
                 )
                 (progn
                   (setq matterial_case_ "ALU.TUBE")
                   (setq W_LV_VALUE_inch (/ (atof W_LV_VALUE) 25))
                   (setq H_LV_VALUE_inch (/ (atof H_LV_VALUE) 25))
                   (setq matterial_code_case_ (strcat 
                                                (rtos W_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                (rtos H_LV_VALUE_inch 2 0) "\""
                                                "x"
                                                THK_LV_VALUE "mm."
                                              ) 
                   )
                   (princ "AL.TUBE_case_9")
                 )
              )
            )
            
          
          

            ;put_result_
              (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "MATTERIAL_VALUE" matterial_case_ )
              (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "MATTERIAL_CODE_VALUE" matterial_code_case_ )
            ;
          ;
          ;put_MOCKUP SIZE
            (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO CENTRIC.LV_VALUE"
              (cond
                (;direction_case_1
                   (and
                      (= DIRECTION_DISTANCE_VALUE  "VERTICAL_LINE")
                      
                   )
                   (progn
                     (princ "direction_case_1\n")
                     (/ (- (atof WIDTH_VALUE) (* (- (atoi QTYLV_VALUE) 1) (atof DISTANCE_LV_VALUE))) 2)
                     
                   )
                )
                (;direction_case_2
                   (and
                      (= DIRECTION_DISTANCE_VALUE  "HONRIZONTAL_LINE")
                   )
                   (progn
                     (princ "direction_case_2\n")
                     (/ (- (atof HEIGHT_VALUE) (* (- (atoi QTYLV_VALUE) 1) (atof DISTANCE_LV_VALUE))) 2)
                     
                   )
                )
              )
            )
            (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO SURFACE.LV_VALUE"
              (cond
                (;direction_case_1
                   (and
                      (= DIRECTION_DISTANCE_VALUE  "VERTICAL_LINE")
                      
                   )
                   (progn
                     (princ "direction_case_1\n")
                     (- (/ (- (atof WIDTH_VALUE) (* (- (atoi QTYLV_VALUE) 1) (atof DISTANCE_LV_VALUE))) 2) (/ (min (atof W_LV_VALUE) (atof W_LV_VALUE)) 2))
                   )
                )
                (;direction_case_2
                   (and
                      (= DIRECTION_DISTANCE_VALUE  "HONRIZONTAL_LINE")
                   )
                   (progn
                     (princ "direction_case_2\n")
                     (- (/ (- (atof HEIGHT_VALUE) (* (- (atoi QTYLV_VALUE) 1) (atof DISTANCE_LV_VALUE))) 2) (/ (min (atof W_LV_VALUE) (atof W_LV_VALUE)) 2))
                   )
                )
              )
            )
          ;
          ;put_SUB_FAME_
            (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "SUB_TOTAL\\PC-C_VALUE"
              (* (atof SUB_FAME_DIST_C-C_VALUE) (atof SUB_FAME_DIST_QTY_VALUE))
            )
            (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST. E-C_VALUE"
              (/ (- (atof HEIGHT_VALUE) (* (atof SUB_FAME_DIST_C-C_VALUE) (atof SUB_FAME_DIST_QTY_VALUE)) ) 2)
            )
            (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST. C-E_VALUE"
              (/ (- (atof HEIGHT_VALUE) (* (atof SUB_FAME_DIST_C-C_VALUE) (atof SUB_FAME_DIST_QTY_VALUE)) ) 2)
            )
          ;
          ;put_BLK-name   
            (cond ;BLK-name_case
              (;wcmatch_case_1
                 (and 
                   (wcmatch matterial_case_ "*AT*")
                 )
                 (progn
                    (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"
                      (strcat "000-TYP_LV_" matterial_case_  "@" DISTANCE_LV_VALUE  "_" "V-LINE"  )
                    )
                   (princ "wcmatch_case_1")
                 )
              )
              (;wcmatch_case_2
                 (and 
                   (wcmatch matterial_case_ "*RT*")
                 )
                 (progn
                    (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"
                      (strcat "000-TYP_LV_" matterial_case_  "@" DISTANCE_LV_VALUE  "_" "V-LINE"  )
                    )
                    (princ "wcmatch_case_2")
                 )
              )
              (;wcmatch_case_3
                 (and 
                   (not (wcmatch matterial_case_ "*AT*"))
                   (not (wcmatch matterial_case_ "*RT*"))
                 )
                 (progn
                    (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"
                      (strcat "000-TYP_LV_" MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "@" DISTANCE_LV_VALUE "mm." "_" "V-LINE"  )
                    )
                    (princ "wcmatch_case_3")
                 )
              )
              (;wcmatch_case_4
                 (and 
                   (not (wcmatch matterial_case_ "*AT*"))
                   (not (wcmatch matterial_case_ "*RT*"))
                 )
                 (progn
                    (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"
                      (strcat "000-TYP_LV_" MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "@" DISTANCE_LV_VALUE "mm." "_" "V-LINE"  )
                    )
                    (princ "wcmatch_case_4")
                 )
              )
              (;wcmatch_case_5
                 (and 
                   (wcmatch matterial_case_ "*ALU.TUBE*")
                   
                 )
                 (progn
                    (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"
                      (strcat "000-TYP_LV_" MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "@" DISTANCE_LV_VALUE "mm." "_" "V-LINE"  )
                    )
                    (princ "wcmatch_case_5")
                 )
              )
              
            )
            
             
          ;
         (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
       )
     ;
  
  
    ;
        
)

(defun c:z631_excel_LV_PART_DATA_to_ATT_CAD () ;LOUVER_MOCKUP_DATA_
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named '001 - LV_PATTERN DATA CUSTOM6'.
  ;The operation of the command will read range cell data in excel  and send the range cell data to attribute block  
  ;
  ;insert_data_from_excel
    ; (setq ref_point1 (getpoint))
    (command "insert" "001 - LV_PATTERN DATA CUSTOM6" "0,0" 1 0)
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (setq ref_LV_data_obj_att_total (length ref_LV_data_obj_att_))
  ;
  ;preloop_and_while excel_input_cotent_data
    (setq ref_LV_data_obj_att_i 0)
    (setq ref_LV_data_obj_att_ia 5)
    (setq ACESSORIES_count 0)
    (while ;excel_input_string_cotent_data
      (< ref_LV_data_obj_att_i ref_LV_data_obj_att_total)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_i ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_i (+ ref_LV_data_obj_att_i 1))
    )
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (while ;excel_dynamic_lookup_data
      (< ref_LV_data_obj_att_ia 14)
      (setq ref_LV_data_obj_att_name (cdr (nth ref_LV_data_obj_att_ia ref_LV_data_obj_att_)))
      (if (/= ref_LV_data_obj_att_name "")
        (progn
          (setq ACESSORIES_count (+ ACESSORIES_count 1) )
          
        )
        (princ "\n")
      )
      (setq ref_LV_data_obj_att_ia (+ ref_LV_data_obj_att_ia 1) )
    )
    (LM:setdynpropvalue ref_LV_data_obj "ACESSORIES_LINE_LOOKUP" ACESSORIES_count)
    (setq ucs_world (vlax-3d-point 0 0 0))
    (setq new_ins_pt (getpoint ))
    (setq new_ins_pt1 (vlax-3d-point new_ins_pt))
    (vla-move ref_LV_data_obj ucs_world new_ins_pt1)
    (vla-put-xscalefactor ref_LV_data_obj 5)
  ;
)
(defun c:z631_excel_LV_PART_DATA_refresh_data_631refresh ()
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a block named '001 - LV_PATTERN DATA CUSTOM6'.
    ;The operation of the command will refresh data in attribute block .
    ;Attribute block is dimension of matterial and generate a set of text in dynamic block 
    ;
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "i" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq ss_pre_filter_set_xx_ (ssget ":s"
                                        (list 
                                          (cons 0 "INSERT") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;get_data_process_

      (if 
        (and 
            (= (sslength ss_pre_filter_set_xx_) 1)
            (= (vla-get-objectname (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0))) "AcDbBlockReference")
            (= (LM:effectivename (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0)) ) "LV_DATA_FROM_EXCEl" )
        )
        (progn
          (setq LV_DATA_FROM_EXCEl_list_ (LM:vl-getattributevalues (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ 0))))
        )
      )
    ;
    ;generate_data_in_box
     ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          ;get BLK-NAME_
            (setq BLOCK_NAME_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "BLOCK_NAME_VALUE"))
          ;
          ;GET_MATTERIAL_
            (setq MATTERIAL_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "MATTERIAL_VALUE"))
            (setq MATTERIAL_CODE_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "MATTERIAL_CODE_VALUE"))
          ;
          ;get_MOCKUP SIZE
            
            (setq WIDTH_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "WIDTH_VALUE"))
            (setq HEIGHT_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "HEIGHT_VALUE"))
            (setq DIRECTION_DISTANCE_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "DIRECTION DISTANCE_VALUE"))
            (setq EDGETOCENTRICLV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO CENTRIC.LV_VALUE"))
            (setq EDGETOSURFACELV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "EDGE TO SURFACE.LV_VALUE"))
          ;
          ;get_LOUVER_DATA
            (setq W_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "W_LV_VALUE"))
            (setq H_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "H_LV_VALUE"))
            (setq DIRECTION_DISTANCE_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "DIRECTION DISTANCE_VALUE"))
            (setq THK_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "THK._LV_VALUE"))
            (setq QTYLV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "QTY.LV_VALUE"))
            (setq DISTANCE_LV_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "DISTANCE_LV_VALUE"))
            (setq TOTAL_L_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_L_VALUE"))
          ;
          ;get_SUB_FAME_
            (setq SUB_FAME_DIST_C-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST.C-C_VALUE"))
            (setq SUB_TOTAL\\PC-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_TOTAL\\PC-C_VALUE"))
            (setq SUB_FAME_DIST_QTY_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME_DIST._QTY._VALUE"))
            (setq SUB_FAME_DIST_E-C_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SUB_FAME DIST. E-C_VALUE"))
          ;

          ;put_data_procress
            (setq MOCKUP_PART_CODE_1 (strcat MATTERIAL_CODE_VALUE "@"  DISTANCE_LV_VALUE "mm."  ))
            (setq MOCKUP_PART_CODE_2 (strcat 
                                        (if (= DIRECTION_DISTANCE_VALUE "VERTICAL_LINE")
                                          (progn
                                            (setq DIRECTION_DISTANCE_VALUE_ "V_LINE_MOCKUP_PANEL")
                                          )
                                        )
                                        "_"
                                        WIDTH_VALUE
                                        "x"
                                        HEIGHT_VALUE
                                       "mm."
                                     )
            )
            (setq MOCKUP_PART_NAME_1 (strcat MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "@" DISTANCE_LV_VALUE "mm." ))
            (setq MOCKUP_PART_NAME_2 (strcat 
                                        (if (= DIRECTION_DISTANCE_VALUE "VERTICAL_LINE")
                                          (progn
                                            (setq DIRECTION_DISTANCE_VALUE_ "V_LINE_MOCKUP_PANEL")
                                          )
                                        )
                                        "_"
                                        WIDTH_VALUE
                                        "x"
                                        HEIGHT_VALUE
                                        "mm."
                                     )
            )

            (setq MATTERIAL_VALUE (strcat MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "_" "thk." THK_LV_VALUE "mm."))
            (setq SIZING_VALUE (strcat  WIDTH_VALUE "x" HEIGHT_VALUE "mm. " "(TOTAL_AREA_"  (rtos (* (/ (atof WIDTH_VALUE) 1000)  (/ (atof HEIGHT_VALUE) 1000)) 2 2) "sq.m.)"))
            (setq DISTANCE_LV_VALUE (strcat DISTANCE_LV_VALUE "mm."))
            (setq TOTAL_LOUVERS_VALUE1 (strcat QTYLV_VALUE " pc.""(TOTAL_LENGTH = " (rtos (* (atof QTYLV_VALUE) (/ (atof HEIGHT_VALUE) 1000) ) 2 2 ) "m.)" ))
            (setq ACESSORIES_VALUE1 (strcat "AL.U-PROFILE (VS-26)"))
            (setq TOTAL_SET_VALUE (strcat "1 set"))
            
          ;
          
         (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
       )
     ;
  
  
    ;
    ;REF_blk_for_GV2  
      (setq REF_block_ nil)
      (while (not REF_block_)
          (setq REF_block_ (entsel "\nSelect a First BLOCK "))
          (if 
            (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
              (setq REF_block_ (car REF_block_))
              
              (progn
                (setq REF_block_ nil)
                (alert "Please select a BLOCK")
              )
          )
          (if
            (/= REF_block_ nil)
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
            )

            (princ "/n")
          )
          (if (= REF_block_ nil)
            (progn
              (alert "Please select a 001 - LV_PATTERN DATA CUSTOM6 \n")
            )
            (cond
              ((and 
                (= (LM:effectivename REF_block_obj) 
                    "001 - LV_PATTERN DATA CUSTOM6"
                )
                (= (LM:effectivename REF_block_obj) "001 - LV_PATTERN DATA CUSTOM6")
              ) 
                (progn 
                  (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                  (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                )
                (princ "\n")
              )
              ((or 
                (/= (LM:effectivename REF_block_obj) 
                    "001 - LV_PATTERN DATA CUSTOM6"
                )
                (/= (LM:effectivename REF_block_obj) "001 - LV_PATTERN DATA CUSTOM6")
              ) 
                (progn 
                  (setq REF_block_ nil)
                  (alert "Please select a 001 - LV_PATTERN DATA CUSTOM6 \n")
                )
                (princ "/n")
              )
            )
          )
      )
    ;
    ;PUT_ATT_process 

  
      (LM:vl-setattributevalue REF_block_obj "CODE_VALUE_1" MOCKUP_PART_CODE_1)
      (LM:vl-setattributevalue REF_block_obj "CODE_VALUE_2" MOCKUP_PART_CODE_2)

      (LM:vl-setattributevalue REF_block_obj "PART_NAME_VALUE_1" MOCKUP_PART_NAME_1)
      (LM:vl-setattributevalue REF_block_obj "PART_NAME_VALUE_2" MOCKUP_PART_NAME_2)
  
      (LM:vl-setattributevalue REF_block_obj "SIZING_VALUE" SIZING_VALUE)

      (LM:vl-setattributevalue REF_block_obj "MATTERIAL_VALUE" MATTERIAL_VALUE)
      (LM:vl-setattributevalue REF_block_obj "DISTANCE_LOUVERS_VALUE" DISTANCE_LV_VALUE)
      (LM:vl-setattributevalue REF_block_obj "TOTAL_LOUVERS_VALUE1" TOTAL_LOUVERS_VALUE1)
      (LM:vl-setattributevalue REF_block_obj "ACESSORIES_VALUE1" ACESSORIES_VALUE1)
      (LM:vl-setattributevalue REF_block_obj "TOTAL_SET_VALUE" TOTAL_SET_VALUE)
      (LM:setdynpropvalue REF_block_obj "ACESSORIES_LINE_LOOKUP" 1)

    ; 
)
(defun c:z631A_add_part_code_to_mockup_data_addcodemck ()
  ;user_input_for_selection_set_blk_object_
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq ss_pre_filter_set_xx_ (ssget 
                                        (list 
                                          (cons 0 "INSERT") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
  ;
  ;user_input_for_indicate_corrdinate_axis)
    ;preloop_and_while
      (setq corrdinate_axis_value_ nil)
      (while (= corrdinate_axis_value_ _nil)
        (setq corrdinate_axis_ (cond ( (getint (strcat "\nuser_input_for_indicate_corrdinate_axis \n1 = X \n2 = Y\n<" (rtos (setq corrdinate_axis_ (cond (corrdinate_axis_) (1.0) ) ) ) "> : " ) ) ) (corrdinate_axis_) ) )
        (if 
          (or
            (= corrdinate_axis_ 1)
            (= corrdinate_axis_ 2)
          )
          (progn
            (setq corrdinate_axis_value_ corrdinate_axis_)
            (cond
              (;corrdinate_case_1
                 (and
                    (= corrdinate_axis_ 1)
                 )
                 (progn
                   (setq corrdinate_axis_value_ "X")
                   (princ "corrdinate_case_1")
                 )
              )
              (;corrdinate_case_2
                 (and
                    (= corrdinate_axis_ 2)

                 )
                 (progn
                   (setq corrdinate_axis_value_ "Y")
                   (princ "corrdinate_case_2")
                 )
              )
            )
            
          )
          (setq corrdinate_axis_value_ nil)
        )
        
      )
    ;
  ;
  
  ;filter_and_sorting_process
    (setq filter_blk_object_ (TA:Filter_ss_set_ ss_pre_filter_set_xx_ "001 - PART DATA CUSTOM 2025" ))
    (setq sorted_blk_object_ (TA:standard_list_croodinate_sorting filter_blk_object_ corrdinate_axis_value_))
  ;
  ;get_data_part_code_
    ;preloop_and_while
      (setq get_part_CODE_VALUE_1_list_ nil)
      (setq sorted_blk_object_i 0)
      (while (< sorted_blk_object_i (length sorted_blk_object_))
        (setq sorted_blk_object_ename_ (car (nth sorted_blk_object_i sorted_blk_object_)))
        (setq sorted_blk_object_ename_obj_ (vlax-ename->vla-object sorted_blk_object_ename_))
        (setq get_part_CODE_VALUE_1 (LM:vl-getattributevalue sorted_blk_object_ename_obj_ "CODE_VALUE_1"))
        (setq get_part_CODE_VALUE_1_list_ (cons get_part_CODE_VALUE_1 get_part_CODE_VALUE_1_list_) )
        
        (setq sorted_blk_object_i (+ sorted_blk_object_i 1))
      )
    ;
  ;
  ;user_intput_
    ;REF_blk_for_GV2  
      (setq REF_block_ nil)
      (while (not REF_block_)
          (setq REF_block_ (entsel "\nSelect a First BLOCK "))
          (if 
            (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
              (setq REF_block_ (car REF_block_))
              
              (progn
                (setq REF_block_ nil)
                (alert "Please select a BLOCK")
              )
          )
          (if
            (/= REF_block_ nil)
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
            )

            (princ "/n")
          )
          (if (= REF_block_ nil)
            (progn
              (alert "Please select a 001 - LV_PATTERN DATA CUSTOM6 \n")
            )
            (cond
              ((and 
                (= (LM:effectivename REF_block_obj) 
                    "001 - LV_PATTERN DATA CUSTOM6"
                )
                (= (LM:effectivename REF_block_obj) "001 - LV_PATTERN DATA CUSTOM6")
              ) 
                (progn 
                  (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                  (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                )
                (princ "\n")
              )
              ((or 
                (/= (LM:effectivename REF_block_obj) 
                    "001 - LV_PATTERN DATA CUSTOM6"
                )
                (/= (LM:effectivename REF_block_obj) "001 - LV_PATTERN DATA CUSTOM6")
              ) 
                (progn 
                  (setq REF_block_ nil)
                  (alert "Please select a 001 - LV_PATTERN DATA CUSTOM6 \n")
                )
                (princ "/n")
              )
            )
          )
      )
    ;
  ;
  ;preloop_and_while
    
    (setq get_part_CODE_VALUE_1_list_i 0)
    (setq get_part_CODE_VALUE_1_list_ii 1)
    (while (< get_part_CODE_VALUE_1_list_i (length get_part_CODE_VALUE_1_list_))
      (setq ACESSORIES_VALUE_val_ (strcat "ACESSORIES_VALUE" (rtos get_part_CODE_VALUE_1_list_ii 2 0) ))
      (setq get_part_CODE_VALUE_1_list_ename_ (nth  get_part_CODE_VALUE_1_list_i get_part_CODE_VALUE_1_list_))
      (LM:vl-setattributevalue REF_block_obj ACESSORIES_VALUE_val_ get_part_CODE_VALUE_1_list_ename_)
      (setq get_part_CODE_VALUE_1_list_i (+ get_part_CODE_VALUE_1_list_i 1))
      (LM:setdynpropvalue REF_block_obj "ACESSORIES_LINE_LOOKUP" (rtos (length get_part_CODE_VALUE_1_list_) 2 0))
      (setq get_part_CODE_VALUE_1_list_ii (+ get_part_CODE_VALUE_1_list_ii 1))
    )
  ;
)

(defun c:z632_excel_LV_PART_DATA_to_ATT_CAD ()
  ;insert_data_from_excel
    ; (setq ref_point1 (getpoint))
    ; (command "insert" "001 - PART DATA CUSTOM 2023_REV01" "0,0" 1 0)
    (command "insert" "001 - PART DATA CUSTOM 2025" "0,0" 1 0)
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    ;get_ins_data
      (setq ref_lv_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ref_LV_data_obj))))
      (setq ref_lv_re_ins_ (list
                          (+ (car ref_lv_ins_) 2.5)
                          (- (cadr ref_lv_ins_) 2.5)
                          0
                        )
      )
    ; 
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (setq ref_LV_data_obj_att_total (length ref_LV_data_obj_att_))
  ;
  ;preloop_and_while excel_input_cotent_data
    (setq ref_LV_data_obj_att_i 0)
    (setq ref_LV_data_obj_att_ia 5)
    (setq ACESSORIES_count 0)
    (while ;excel_input_string_cotent_data
      (< ref_LV_data_obj_att_i ref_LV_data_obj_att_total)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_i ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_i (+ ref_LV_data_obj_att_i 1))
    )
    ; (while ;excel_dynamic_lookup_data
    ;   (< ref_LV_data_obj_att_ia 14)
    ;   (setq ref_LV_data_obj_att_name (cdr (nth ref_LV_data_obj_att_ia ref_LV_data_obj_att_)))
    ;   (if (/= ref_LV_data_obj_att_name "")
    ;     (progn
    ;       (setq ACESSORIES_count (+ ACESSORIES_count 1) )
          
    ;     )
    ;     (princ "\n")
    ;   )
    ;   (setq ref_LV_data_obj_att_ia (+ ref_LV_data_obj_att_ia 1) )
    ; )
    ; (LM:setdynpropvalue ref_LV_data_obj "ACESSORIES_LINE_LOOKUP" ACESSORIES_count)
    (setq ucs_world (vlax-3d-point 0 0 0))
    (setq new_ins_pt (getpoint ))
    (setq new_ins_pt1 (vlax-3d-point new_ins_pt))
    (vla-move ref_LV_data_obj ucs_world new_ins_pt1)
    (vla-put-xscalefactor ref_LV_data_obj 5)
  ;
)
(defun c:z634_excel_LAYOUT_DATA_to_ATT_CAD ()
  ;insert_data_from_excel
    (command "insert" "LNAD - A4 TITLE BLOCK PART REV01" "0,0" 1 0)
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (setq ref_LV_data_obj_att_total (length ref_LV_data_obj_att_))
  ;
  ;preloop_and_while user_input_data_part1
    (setq ref_LV_data_obj_att_i 0)
    (while
      (< ref_LV_data_obj_att_i 22)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_i ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_i (+ ref_LV_data_obj_att_i 1))
    )
  ;
  ;preloop_and_while user_input_data_part2
    (setq ref_LV_data_obj_att_ib 30)
    (while
      (< ref_LV_data_obj_att_ib 32)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_ib ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_ib (+ ref_LV_data_obj_att_ib 1))
    )
  ;
  ;insert_data
    (setq ucs_world (vlax-3d-point 0 0 0))
    (setq new_ins_pt (getpoint ))
    (setq new_ins_pt1 (vlax-3d-point new_ins_pt))
    (vla-move ref_LV_data_obj ucs_world new_ins_pt1)
    (LM:setdynpropvalue ref_LV_data_obj "TYPE_BLOCK" "PART_BLOCK____SC__5")
  ;
)
(defun c:z635_MK_MCK_LV_Front_view ()

  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "i" 
                                          (list 
                                            (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn 
        (setq ss_pre_filter_set_xx_ (ssget ":s"
                                      (list 
                                        (cons 0 "INSERT") ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;sub_command_for_refresh_data
  (C:z630re_excel_LV_MAIN_DATA_refresh_data_630refresh)
  ;
  ;mk_scale
    (setq mk_scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
  ;
  ;mk_lv_and_subf_object
    ;sub_func
      (defun TA:MK-RECTANG (ref_point width_val height_val) 
        (setq mk_ref_mck (list 
                          (+ (car ref_point) (atof width_val))
                          (+ (cadr ref_point) (atof height_val))
                          0
                        )
        )
        (command "_.RECTANG" ref_point mk_ref_mck)
      )
    ;
    ;REF_blk_for_GV2  
      (setq REF_block_ nil)
      (while (not REF_block_)
          (if (= ss_pre_filter_set_xx_ nil)
            (progn
              (setq REF_block_ (entsel "\nSelect a First BLOCK "))
            )
            (setq REF_block_ (list (ssname ss_pre_filter_set_xx_ 0) "notthing"))
          )
          (if 
            (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
              (setq REF_block_ (car REF_block_))
              
              (progn
                (setq REF_block_ nil)
                (alert "Please select a BLOCK")
              )
          )
          (if
            (/= REF_block_ nil)
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
            )

            (princ "/n")
          )
          (if (= REF_block_ nil)
            (progn
              (alert "Please select a LV_DATA_FROM_EXCEl\n")
            )
            (cond
              ((and 
                (= (LM:effectivename REF_block_obj) 
                    "LV_DATA_FROM_EXCEl"
                )
                (= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
              ) 
                (progn 
                  (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                  (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                )
                (princ "\n")
              )
              ((or 
                (/= (LM:effectivename REF_block_obj) 
                    "LV_DATA_FROM_EXCEl"
                )
                (/= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
              ) 
                (progn 
                  (setq REF_block_ nil)
                  (alert "Please select a LV_DATA_FROM_EXCEl\n")
                )
                (princ "/n")
              )
            )
          )
      )
    ;
    ;make_rectangle_boundary
      ;ref-excel-data 
        (setq WIDTH_VALUE (LM:vl-getattributevalue REF_block_obj "WIDTH_VALUE"))
        (setq HEIGHT_VALUE (LM:vl-getattributevalue REF_block_obj "HEIGHT_VALUE"))
        (setq W_LV_VALUE (LM:vl-getattributevalue REF_block_obj "W_LV_VALUE" ))
        (setq H_LV_VALUE (LM:vl-getattributevalue REF_block_obj "H_LV_VALUE" ))
        (setq W_SUBF_VALUE (LM:vl-getattributevalue REF_block_obj "W_SUBF._VALUE" ))
        (setq H_SUBF_VALUE (LM:vl-getattributevalue REF_block_obj "H_SUBF._VALUE" ))
      ;
      ;make-border
        (setq ref_point1 (getpoint "specify point to make object"))
        (TA:MK-RECTANG ref_point1 WIDTH_VALUE HEIGHT_VALUE)
        (setvar "osmode" 0)
        (setq rec_get (entget (entlast)))
        (setq rec_obj (vlax-ename->vla-object (entlast)))
        (setq rec_obj_vertex_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) rec_get))
        (setq rec_obj_vertex_pt (mapcar 'cdr rec_obj_vertex_pt))
      ;
    ;
    ;setvar
      (setvar "osmode" 0)
    ;
    ;insertion_gridline
      ;preloop_and_while
        (c:LY-000-GRID)
        (setq rec_obj_vtex_i 0)
        (while ;make_vertical_and_honrizontal_GLINE_at_border
          (< rec_obj_vtex_i 1)
          (setq ins_pt_V-start (nth 3 rec_obj_vertex_pt))
          (setq ins_pt_V-end (nth 2 rec_obj_vertex_pt))
          (setq ins_pt_H-start (nth 3 rec_obj_vertex_pt))
          (setq ins_pt_H-end (nth 0 rec_obj_vertex_pt))
          ;V-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_V-start 1 0 )
            (setq ins_pt_V-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-start_obj "H" (atoi HEIGHT_VALUE))
            (LM:setdynpropvalue ins_pt_V-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-start_obj "LOCATION" "LV_LINE")
          ;
          ;V-end_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_V-end 1 0 )
            (setq ins_pt_V-end_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-end_obj "H" (atoi HEIGHT_VALUE))
            (LM:setdynpropvalue ins_pt_V-end_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-end_obj "LOCATION" "LV_LINE")
          ;
          ;H-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_H-start 1 90)
            (setq ins_pt_H-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_H-start_obj "H" (atoi WIDTH_VALUE))
            (LM:setdynpropvalue ins_pt_H-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_H-start_obj "LOCATION" "SUBF_LINE")
          ;
          ;H-end_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_H-end 1 90)
            (setq ins_pt_H-end_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_H-end_obj "H" (atoi WIDTH_VALUE))
            (LM:setdynpropvalue ins_pt_H-end_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_H-end_obj "LOCATION" "SUBF_LINE")
          ;
          (setq rec_obj_vtex_i (+ rec_obj_vtex_i 1))
        )
      ;
      ;preloop_and_while_make_vertical_LV-grid_in_boundary
        ;get-lv-data
          (setq GL_TOTAL (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "QTY.LV_VALUE"))))
          (setq GL_i 0)
          (setq GL_offset (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "EDGE TO CENTRIC.LV_VALUE"))))
          (setq GL_dist (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "DISTANCE_LV_VALUE"))))
        ;
        (while ;V_lv_GLine
          (< GL_i GL_total)
          (setq ins_offset_lv (list 
                        (+ (car (nth 3 rec_obj_vertex_pt)) GL_offset)
                        (cadr (nth 3 rec_obj_vertex_pt))
                        0
                      )
          )
          ;offset_V-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_offset_lv 1 0 )
            (setq ins_pt_V-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-start_obj "H" (atoi HEIGHT_VALUE))
            (LM:setdynpropvalue ins_pt_V-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-start_obj "LOCATION" "LV_LINE")
          ;
          (setq GL_i (+ GL_i 1))
          (setq GL_offset (+ GL_offset GL_dist))
        )
      ;
      ;preloop_and_while_make_vertical_SUBF-grid_in_boundary
        ;get-lv-data
          (setq SUBF_TOTAL (+ (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "SUB_FAME_DIST._QTY._VALUE"))) 1))
          (setq SUBF_i 0)
          (setq SUBF_offset (atoi (setq SUBF-E-C_VAL (LM:vl-getattributevalue REF_block_obj "SUB_FAME DIST. E-C_VALUE"))))
          (setq SUBF_dist (atoi (setq SUBF-C-C_VAL (LM:vl-getattributevalue REF_block_obj "SUB_FAME DIST.C-C_VALUE"))))
        ;
        (while ;V_lv_GLine
          (< SUBF_i SUBF_TOTAL)
          (setq ins_offset_subf (list 
                        (car (nth 0 rec_obj_vertex_pt))
                        (+ (cadr (nth 0 rec_obj_vertex_pt)) SUBF_offset)
                        0
                      )
          )
          ;offset_V-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_offset_subf 1 90)
            (setq ins_subf_h-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_subf_h-start_obj "H" (atoi WIDTH_VALUE))
            (LM:setdynpropvalue ins_subf_h-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_subf_h-start_obj "LOCATION" "SUBF_LINE")
          ;
          (setq SUBF_i (+ SUBF_i 1))
          (setq SUBF_offset (+ SUBF_offset SUBF_dist))
        )
      ;
    ;
    ;selection_set_fillter_grid_line_direction
      (setq rec_obj_bound (vla-getboundingbox rec_obj 'nn 'mm))
      (setq ins_low (vlax-safearray->list nn))
      (setq ins_top (vlax-safearray->list mm))
      (setq ss_GL_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
      ; (command "pselect" ss_GL_set_ "" )
    ;
    ;preloop_and_while filter_selection_set_SUBF
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_GL_set_i 0)
      (setq FILT_GLINE_set_H ())
      (while
        (< ss_GL_set_i (sslength ss_GL_set_))
        (setq ss_GL_set_ename (ssname ss_GL_set_ ss_GL_set_i))
        (setq ss_GL_set_obj (vlax-ename->vla-object ss_GL_set_ename))
        (setq ss_GL_set_efname (LM:Effectivename ss_GL_set_obj))
          (if 
            (and 
              (= ss_GL_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_GL_set_obj))) 90)
            )
            (progn
              ;get_ins_datastart
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_GL_set_obj))))
                (setq mid_ins (list
                                (+ (car base_ins) (/ (LM:getdynpropvalue ss_GL_set_obj "H") 2))  
                                (cadr base_ins)  
                              )
                )
                (setq end_ins (list
                                (+ (car base_ins) (LM:getdynpropvalue ss_GL_set_obj "H"))  
                                (cadr base_ins)  
                              )
                )            
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_GL_set_ename
                        base_ins
                        mid_ins
                        end_ins
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_H (cons ename+ins FILT_GLINE_set_H))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_GL_set_i (/ 100 (float (sslength ss_GL_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_H_total (length FILT_GLINE_set_H))
              ;
            )
            (princ "\n")
          )
        (setq ss_GL_set_i (+ ss_GL_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
      (setq SRT_FILT_GLINE_set_subf_ (reverse (sort_by_list_2nd_cadr FILT_GLINE_set_H)))
    ;
    ;insertion_SUBF_object
      ;preloop_and_while SUBF_object
        (c:LY-001-ASCESSORIES)
        (setq SRT_FILT_GLINE_set_subf_i 1)
        (while ;insert_SUBF
          (< SRT_FILT_GLINE_set_subf_i (- (length SRT_FILT_GLINE_set_subf_) 1))
          (setq SRT_FILT_GLINE_set_subf_list (nth SRT_FILT_GLINE_set_subf_i SRT_FILT_GLINE_set_subf_))
          (command "insert" "001 - DYNAMIC subfame" (nth 2 SRT_FILT_GLINE_set_subf_list) 1 0)
          (setq subf_list_obj (vlax-ename->vla-object (entlast)))
          (LM:setdynpropvalue subf_list_obj "H" (atoi W_SUBF_VALUE))
          (LM:setdynpropvalue subf_list_obj "W" (atoi WIDTH_VALUE))
          (setq SRT_FILT_GLINE_set_subf_i (+ SRT_FILT_GLINE_set_subf_i 1))
        )
      ;
    ;
    ;preloop_and_while filter_selection_set_LV
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_GL_set_i 0)
      (setq FILT_GLINE_set_V ())
      (while
        (< ss_GL_set_i (sslength ss_GL_set_))
        (setq ss_GL_set_ename (ssname ss_GL_set_ ss_GL_set_i))
        (setq ss_GL_set_obj (vlax-ename->vla-object ss_GL_set_ename))
        (setq ss_GL_set_efname (LM:Effectivename ss_GL_set_obj))
          (if 
            (and 
              (= ss_GL_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_GL_set_obj))) 0)
            )
            (progn
              ;get_ins_datastart
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_GL_set_obj))))
                (setq mid_ins (list
                                (car base_ins)
                                (- (cadr base_ins) (/ (LM:getdynpropvalue ss_GL_set_obj "H") 2))  
                              )
                )
                (setq end_ins (list
                                (car base_ins)
                                (- (cadr base_ins) (LM:getdynpropvalue ss_GL_set_obj "H"))  
                              )
                )            
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_GL_set_ename
                        base_ins
                        mid_ins
                        end_ins
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_V (cons ename+ins FILT_GLINE_set_V))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_GL_set_i (/ 100 (float (sslength ss_GL_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_V_total (length FILT_GLINE_set_V))
              ;
            )
            (princ "\n")
          )
        (setq ss_GL_set_i (+ ss_GL_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
      (setq SRT_FILT_GLINE_set_lv_ (sort_by_list_2nd_car FILT_GLINE_set_V))
    ;
    ;insertion_LV_object
      ;preloop_and_while LV_object
        (c:A04_TUBESERIES)
        (setq SRT_FILT_GLINE_set_lv_i 1)
        (while ;insert_LV
          (< SRT_FILT_GLINE_set_lv_i (- (length SRT_FILT_GLINE_set_lv_) 1))
          (setq SRT_FILT_GLINE_set_lv_list (nth SRT_FILT_GLINE_set_lv_i SRT_FILT_GLINE_set_lv_))
          (command "insert" "001 - DYNAMIC LV" (nth 3 SRT_FILT_GLINE_set_lv_list) 1 0)  
          (setq ins_pt_V-end_obj (vlax-ename->vla-object (entlast)))
          (LM:setdynpropvalue ins_pt_V-end_obj "H" (atoi HEIGHT_VALUE))
          (LM:setdynpropvalue ins_pt_V-end_obj "W" (atoi W_LV_VALUE))
          (setq SRT_FILT_GLINE_set_lv_i (+ SRT_FILT_GLINE_set_lv_i 1))
        )
      ;
    ;
  ;
  ;mk_dim_mode_sub-H-line
    ;get_previous_var
    (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_sub-V-line
    ;get_previous_var
    (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 2 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl)
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_main-H-line
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl)
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_main-V-line
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 2 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl)
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
)
(defun c:z636_MK_MCK_LV_PLAN_view ()
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "i" 
                                          (list 
                                            (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn 
        (setq ss_pre_filter_set_xx_ (ssget ":s"
                                      (list 
                                        (cons 0 "INSERT") ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;sub_command_for_refrersh_data_
  (C:z630re_excel_LV_MAIN_DATA_refresh_data_630refresh)
  ;
  ;mk_lv_and_subf_object
    ;mk_scale
      (setq mk_scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    ;
    ;sub_func
      (defun TA:MK-RECTANG (ref_point width_val height_val) 
        (setq mk_ref_mck (list 
                          (+ (car ref_point) (atof width_val))
                          (+ (cadr ref_point) (atof height_val))
                          0
                        )
        )
        (command "_.RECTANG" ref_point mk_ref_mck)
      )
    ;
    ;REF_blk_for_GV2  
      (setq REF_block_ nil)
      (while (not REF_block_)
          (if (= ss_pre_filter_set_xx_ nil)
            (progn
              (setq REF_block_ (entsel "\nSelect a First BLOCK "))
            )
            (setq REF_block_ (list (ssname ss_pre_filter_set_xx_ 0) "notthing"))
          )
          (if 
            (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
              (setq REF_block_ (car REF_block_))
              
              (progn
                (setq REF_block_ nil)
                (alert "Please select a BLOCK")
              )
          )
          (if
            (/= REF_block_ nil)
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
            )

            (princ "/n")
          )
          (if (= REF_block_ nil)
            (progn
              (alert "Please select a LV_DATA_FROM_EXCEl\n")
            )
            (cond
              ((and 
                (= (LM:effectivename REF_block_obj) 
                    "LV_DATA_FROM_EXCEl"
                )
                (= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
              ) 
                (progn 
                  (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                  (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                )
                (princ "\n")
              )
              ((or 
                (/= (LM:effectivename REF_block_obj) 
                    "LV_DATA_FROM_EXCEl"
                )
                (/= (LM:effectivename REF_block_obj) "LV_DATA_FROM_EXCEl")
              ) 
                (progn 
                  (setq REF_block_ nil)
                  (alert "Please select a LV_DATA_FROM_EXCEl\n")
                )
                (princ "/n")
              )
            )
          )
      )
    ;
    ;make_rectangle_boundary
      ;ref-excel-data 
        (setq WIDTH_VALUE (LM:vl-getattributevalue REF_block_obj "WIDTH_VALUE"))
        (setq HEIGHT_VALUE (LM:vl-getattributevalue REF_block_obj "HEIGHT_VALUE"))
        (setq W_LV_VALUE (LM:vl-getattributevalue REF_block_obj "W_LV_VALUE" ))
        (setq H_LV_VALUE (LM:vl-getattributevalue REF_block_obj "H_LV_VALUE" ))
        (setq W_SUBF_VALUE (LM:vl-getattributevalue REF_block_obj "W_SUBF._VALUE" ))
        (setq H_SUBF_VALUE (LM:vl-getattributevalue REF_block_obj "H_SUBF._VALUE" ))
      ;
      ;make-border
        (setq ref_point1 (getpoint))
        (TA:MK-RECTANG ref_point1 WIDTH_VALUE (rtos (+ (atoi H_LV_VALUE) (atoi H_SUBF_VALUE))))
        (setq rec_get (entget (entlast)))
        (setq rec_obj (vlax-ename->vla-object (entlast)))
        (setq rec_obj_vertex_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) rec_get))
        (setq rec_obj_vertex_pt (mapcar 'cdr rec_obj_vertex_pt))
      ;
    ;
    ;setvar
      (setvar "osmode" 0)
    ;
    ;insertion_gridline
      ;preloop_and_while
        (c:LY-000-GRID)
        (setq rec_obj_vtex_i 0)
        (while ;make_vertical_and_honrizontal_GLINE_at_border
          (< rec_obj_vtex_i 1)
          (setq ins_pt_V-start (nth 3 rec_obj_vertex_pt))
          (setq ins_pt_V-end (nth 2 rec_obj_vertex_pt))
          (setq ins_pt_H-start (nth 3 rec_obj_vertex_pt))
          (setq ins_pt_H-end (nth 0 rec_obj_vertex_pt))
          ;V-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_V-start 1 0 )
            (setq ins_pt_V-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-start_obj "H" (+ (atoi H_LV_VALUE) (atoi H_SUBF_VALUE)))
            (LM:setdynpropvalue ins_pt_V-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-start_obj "LOCATION" "LV_LINE")
          ;
          ;V-end_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_V-end 1 0 )
            (setq ins_pt_V-end_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-end_obj "H" (+ (atoi H_LV_VALUE) (atoi H_SUBF_VALUE)))
            (LM:setdynpropvalue ins_pt_V-end_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-end_obj "LOCATION" "LV_LINE")
          ;
          ;H-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_H-start 1 90)
            (setq ins_pt_H-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_H-start_obj "H" (atoi WIDTH_VALUE))
            (LM:setdynpropvalue ins_pt_H-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_H-start_obj "LOCATION" "SUBF_LINE")
          ;
          ;H-end_position
            (command "insert" "000-GRID_LINE_DYN" ins_pt_H-end 1 90)
            (setq ins_pt_H-end_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_H-end_obj "H" (atoi WIDTH_VALUE))
            (LM:setdynpropvalue ins_pt_H-end_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_H-end_obj "LOCATION" "SUBF_LINE")
          ;
          (setq rec_obj_vtex_i (+ rec_obj_vtex_i 1))
        )
      ;
      ;preloop_and_while_make_vertical_LV-grid_in_boundary
        ;get-lv-data
          (setq GL_TOTAL (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "QTY.LV_VALUE"))))
          (setq GL_i 0)
          (setq GL_offset (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "EDGE TO CENTRIC.LV_VALUE"))))
          (setq GL_dist (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "DISTANCE_LV_VALUE"))))
        ;
        (while ;V_lv_GLine
          (< GL_i GL_total)
          (setq ins_offset_lv (list 
                        (+ (car (nth 3 rec_obj_vertex_pt)) GL_offset)
                        (cadr (nth 3 rec_obj_vertex_pt))
                        0
                      )
          )
          ;offset_V-start_position
            (command "insert" "000-GRID_LINE_DYN" ins_offset_lv 1 0 )
            (setq ins_pt_V-start_obj (vlax-ename->vla-object (entlast)))
            (LM:setdynpropvalue ins_pt_V-start_obj "H" (+ (atoi H_LV_VALUE) (atoi H_SUBF_VALUE)))
            (LM:setdynpropvalue ins_pt_V-start_obj "OFFSET" 25)
            (LM:vl-setattributevalue ins_pt_V-start_obj "LOCATION" "LV_LINE")
          ;
          (setq GL_i (+ GL_i 1))
          (setq GL_offset (+ GL_offset GL_dist))
        )
      ;
      ;preloop_and_while_make_vertical_SUBF-grid_in_boundary
        ; ;get-lv-data
        ;   (setq SUBF_TOTAL (+ (atoi (setq LV_VALUE (LM:vl-getattributevalue REF_block_obj "SUB_FAME_DIST._QTY._VALUE"))) 1))
        ;   (setq SUBF_i 0)
        ;   (setq SUBF_offset (atoi (setq SUBF-E-C_VAL (LM:vl-getattributevalue REF_block_obj "SUB_FAME DIST. E-C_VALUE"))))
        ;   (setq SUBF_dist (atoi (setq SUBF-C-C_VAL (LM:vl-getattributevalue REF_block_obj "SUB_FAME DIST.C-C_VALUE"))))
        ; ;
        ; (while ;V_lv_GLine
        ;   (< SUBF_i SUBF_TOTAL)
        ;   (setq ins_offset_subf (list 
        ;                 (car (nth 0 rec_obj_vertex_pt))
        ;                 (+ (cadr (nth 0 rec_obj_vertex_pt)) SUBF_offset)
        ;                 0
        ;               )
        ;   )
        ;   ;offset_V-start_position
        ;     (command "insert" "000-GRID_LINE_DYN" ins_offset_subf 1 90)
        ;     (setq ins_subf_h-start_obj (vlax-ename->vla-object (entlast)))
        ;     (LM:setdynpropvalue ins_subf_h-start_obj "H" (+ (atoi H_LV_VALUE) (atoi H_SUBF_VALUE)))
        ;     (LM:setdynpropvalue ins_subf_h-start_obj "OFFSET" 25)
        ;     (LM:vl-setattributevalue ins_subf_h-start_obj "LOCATION" "SUBF_LINE")
        ;   ;
        ;   (setq SUBF_i (+ SUBF_i 1))
        ;   (setq SUBF_offset (+ SUBF_offset SUBF_dist))
        ; )
      ;
    ;
    ;selection_set_fillter_grid_line_direction
      (setq rec_obj_bound (vla-getboundingbox rec_obj 'nn 'mm))
      (setq ins_low (vlax-safearray->list nn))
      (setq ins_top (vlax-safearray->list mm))
      (setq ss_GL_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_SUBF
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_GL_set_i 0)
      (setq FILT_GLINE_set_H ())
      (while
        (< ss_GL_set_i (sslength ss_GL_set_))
        (setq ss_GL_set_ename (ssname ss_GL_set_ ss_GL_set_i))
        (setq ss_GL_set_obj (vlax-ename->vla-object ss_GL_set_ename))
        (setq ss_GL_set_efname (LM:Effectivename ss_GL_set_obj))
          (if 
            (and 
              (= ss_GL_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_GL_set_obj))) 90)
            )
            (progn
              ;get_ins_datastart
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_GL_set_obj))))
                (setq mid_ins (list
                                (+ (car base_ins) (/ (LM:getdynpropvalue ss_GL_set_obj "H") 2))  
                                (cadr base_ins)  
                              )
                )
                (setq end_ins (list
                                (+ (car base_ins) (LM:getdynpropvalue ss_GL_set_obj "H"))  
                                (cadr base_ins)  
                              )
                )            
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_GL_set_ename
                        base_ins
                        mid_ins
                        end_ins
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_H (cons ename+ins FILT_GLINE_set_H))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_GL_set_i (/ 100 (float (sslength ss_GL_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_H_total (length FILT_GLINE_set_H))
              ;
            )
            (princ "\n")
          )
        (setq ss_GL_set_i (+ ss_GL_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
      (setq SRT_FILT_GLINE_set_subf_ (reverse (sort_by_list_2nd_cadr FILT_GLINE_set_H)))
    ;
    ;insertion_SUBF_object
      ;preloop_and_while SUBF_object
        (c:LY-001-ASCESSORIES)
        (setq SRT_FILT_GLINE_set_subf_i 0)
        (while ;insert_SUBF
          (< SRT_FILT_GLINE_set_subf_i (- (length SRT_FILT_GLINE_set_subf_) 1))
          (setq SRT_FILT_GLINE_set_subf_list (nth SRT_FILT_GLINE_set_subf_i SRT_FILT_GLINE_set_subf_))
          (command "insert" "001 - DYNAMIC subfame" (nth 2 SRT_FILT_GLINE_set_subf_list) 1 0)
          (setq subf_list_obj (vlax-ename->vla-object (entlast)))
          (setq subf_list_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint subf_list_obj))))
          (setq subf_list_new_ins_ (list
                                     (car subf_list_ins)
                                     (- (cadr subf_list_ins) (/ (atof H_SUBF_VALUE) 2))
                                     0
                                   )
          )
          (setq subf_list_ins (vla-put-insertionpoint subf_list_obj (vlax-3d-point subf_list_new_ins_)))
          (LM:setdynpropvalue subf_list_obj "H" (atoi H_SUBF_VALUE))
          (LM:setdynpropvalue subf_list_obj "W" (atoi WIDTH_VALUE))
          (setq SRT_FILT_GLINE_set_subf_i (+ SRT_FILT_GLINE_set_subf_i 1))
        )
      ;
    ;
    ;preloop_and_while filter_selection_set_LV
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_GL_set_i 0)
      (setq FILT_GLINE_set_V ())
      (while
        (< ss_GL_set_i (sslength ss_GL_set_))
        (setq ss_GL_set_ename (ssname ss_GL_set_ ss_GL_set_i))
        (setq ss_GL_set_obj (vlax-ename->vla-object ss_GL_set_ename))
        (setq ss_GL_set_efname (LM:Effectivename ss_GL_set_obj))
          (if 
            (and 
              (= ss_GL_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_GL_set_obj))) 0)
            )
            (progn
              ;get_ins_datastart
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_GL_set_obj))))
                (setq start_ins (list
                                (car base_ins)
                                (- (cadr base_ins) (atoi H_SUBF_VALUE))  
                              )
                )
                (setq mid_ins (list
                                (car base_ins)
                                (- (cadr base_ins) (/ (LM:getdynpropvalue ss_GL_set_obj "H") 2))  
                              )
                )
                (setq end_ins (list
                                (car base_ins)
                                (- (cadr base_ins) (LM:getdynpropvalue ss_GL_set_obj "H"))  
                              )
                )            
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_GL_set_ename
                        start_ins
                        base_ins
                        mid_ins
                        end_ins
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_V (cons ename+ins FILT_GLINE_set_V))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_GL_set_i (/ 100 (float (sslength ss_GL_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_V_total (length FILT_GLINE_set_V))
              ;
            )
            (princ "\n")
          )
        (setq ss_GL_set_i (+ ss_GL_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
      (setq SRT_FILT_GLINE_set_lv_ (sort_by_list_2nd_car FILT_GLINE_set_V))
    ;
    ;insertion_LV_object
      ;preloop_and_while LV_object
        (c:A04_TUBESERIES)
        (setq SRT_FILT_GLINE_set_lv_i 1)
        (while ;insert_LV
          (< SRT_FILT_GLINE_set_lv_i (- (length SRT_FILT_GLINE_set_lv_) 1))
          (setq SRT_FILT_GLINE_set_lv_list (nth SRT_FILT_GLINE_set_lv_i SRT_FILT_GLINE_set_lv_))
          (command "insert" "001 - DYNAMINC ALU. TUBE V REV01" (nth 1 SRT_FILT_GLINE_set_lv_list) 1 180)  
          (setq ins_pt_V-end_obj (vlax-ename->vla-object (entlast)))
          (LM:setdynpropvalue ins_pt_V-end_obj "H" (atoi H_LV_VALUE))
          (LM:setdynpropvalue ins_pt_V-end_obj "W" (atoi W_LV_VALUE))
          ; (LM:setdynpropvalue ins_pt_V-end_obj "side" (atoi W_LV_VALUE))
          (setq SRT_FILT_GLINE_set_lv_i (+ SRT_FILT_GLINE_set_lv_i 1)) 
        )
      ;
    ;
  ;
  ;mk_dim_mode_sub-H-line
    ;get_previous_var
    (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_sub-V-line
    ;get_previous_var
    (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 2 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_main-H-line
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
  ;mk_dim_mode_main-V-line
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          ; (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
          (setq mode-direction-val 2 )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          ; (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
          (setq mode_location_val 1 )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl mk_scl )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget "C" ins_low ins_top (list (cons 0 "insert"))))
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  ;
) 

(defun c:z637_excel_LV_PART_DATA_to_ATT_CAD ()
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named 'LV_DATA_FROM_EXCEl'.
  ;ชุดคำสั่งนี้ถูกออกแบบมาให้ทำงานคู่กับ LV_DATA_FROM_EXCEl_PART_DATA_2025 เท่านั้น
  ;ชุดคำสั่งจะทำการอ่าน Att/Dyn block ซึ่งมีข้อมูลอยู่เตรียมไว้อยู่แล้ว
  ;ถ้า user ทำงานการเลือกข้อมูล parameter หรือ agument ระบบจะผิดพลาด
  ;
  ;insert_data_from_excel
    ; (setq ref_point1 (getpoint))
    (command "insert" "LV_DATA_FROM_EXCEl_PART_DATA_2025" "0,0" 1 0)
    (setq ref_LV_data_obj (vlax-ename->vla-object (entlast)))
    ;get_ins_data
      (setq ref_lv_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ref_LV_data_obj))))
      (setq ref_lv_re_ins_ (list
                          (+ (car ref_lv_ins_) 2.5)
                          (- (cadr ref_lv_ins_) 2.5)
                          0
                        )
      )
    ; 
    (setq ref_LV_data_obj_att_ (LM:vl-getattributevalues ref_LV_data_obj))
    (setq ref_LV_data_obj_att_total (length ref_LV_data_obj_att_))
  ;
  ;preloop_and_while excel_input_cotent_data
    (setq ref_LV_data_obj_att_i 0)
    (setq ref_LV_data_obj_att_ia 5)
    (setq ACESSORIES_count 0)
    (while ;excel_input_string_cotent_data
      (< ref_LV_data_obj_att_i ref_LV_data_obj_att_total)
      (setq ref_LV_data_obj_att_name (car (nth ref_LV_data_obj_att_i ref_LV_data_obj_att_)))
      (setq context_data (getstring (strcat "\nspecify data into " ref_LV_data_obj_att_name)))
      (LM:vl-setattributevalue ref_LV_data_obj ref_LV_data_obj_att_name context_data)
      (princ (strcat "\n" " "  " " (rtos (* ref_LV_data_obj_att_i (/ 100 (float ref_LV_data_obj_att_total))) 2 0) "%"))
      ; (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      (setq ref_LV_data_obj_att_i (+ ref_LV_data_obj_att_i 1))
    )
    ; (while ;excel_dynamic_lookup_data
    ;   (< ref_LV_data_obj_att_ia 14)
    ;   (setq ref_LV_data_obj_att_name (cdr (nth ref_LV_data_obj_att_ia ref_LV_data_obj_att_)))
    ;   (if (/= ref_LV_data_obj_att_name "")
    ;     (progn
    ;       (setq ACESSORIES_count (+ ACESSORIES_count 1) )
          
    ;     )
    ;     (princ "\n")
    ;   )
    ;   (setq ref_LV_data_obj_att_ia (+ ref_LV_data_obj_att_ia 1) )
    ; )
    ; (LM:setdynpropvalue ref_LV_data_obj "ACESSORIES_LINE_LOOKUP" ACESSORIES_count)
    (setq ucs_world (vlax-3d-point 0 0 0))
    (setq new_ins_pt (getpoint ))
    (setq new_ins_pt1 (vlax-3d-point new_ins_pt))
    (vla-move ref_LV_data_obj ucs_world new_ins_pt1)
    (vla-put-xscalefactor ref_LV_data_obj 5)
  ;
  
)
(defun c:z638A_generate_part_name_LV_DATA_FROM_EXCEl_PART_DATA_2025 () 
    ;user_input_for_get_data_dynamic_block_object
      (if (= 1 1) ;for checking insert is attblock.
        (progn
          ;user_input_get_block_(not loop)
            ;block_data_for insertion
              (if (/= (setq blk_ename_ (car (entsel "specify_block_"))) nil)
                (progn
                  (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
                )
                (alert "Object is blank.")
              )         
              (if (and
                    (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference");For Check objectname
                    ; (= (vla-get-isdynamicblock blk_ename_obj_) :vlax-true );For Check objectname is dynamicblock
                    (= (vla-get-hasattributes blk_ename_obj_) :vlax-true );For Check objectname is attblock
                  ) 
                (progn
                  (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
                  (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
                )
                (alert "Object is not block.")
              )
            ;
          ;
        )
      )
    ;
    ;get_data_process_
      (if (OR 
            (= (LM:effectivename blk_ename_obj_) 
               "LV_DATA_FROM_EXCEl_PART_DATA_2025"
            )
            ; (= (LM:effectivename blk_ename_obj_) 
            ;    "001 - PART DATA CUSTOM 2025"
            ; )
          )
        (progn
          (setq attblock_list_ (LM:vl-getattributevalues blk_ename_obj_ ))
          ;generate_part_name_process
            ;matterial_data_proces
              ;part_1
                (setq matterial_data_length_1 (strlen 
                                              (cdr (nth 2 attblock_list_))
                                              (cdr (nth 3 attblock_list_))
                                              (cdr (nth 4 attblock_list_))
                                              (cdr (nth 5 attblock_list_))
                                              "_"
                                            )
                )
                (setq matterial_data_name_1 (strcat 
                                              (cdr (nth 2 attblock_list_))
                                              (cdr (nth 3 attblock_list_))
                                              (cdr (nth 4 attblock_list_))
                                              (cdr (nth 5 attblock_list_))
                                              "_"
                                            )
                )
              ;
              ;part_2
                (setq matterial_data_length_2 (strlen 
                                              (cdr (nth 6 attblock_list_))
                                            )
                )
                (setq matterial_data_name_2 (strcat 
                                              (cdr (nth 6 attblock_list_))
                                            )
                )
              ;
              ;part_3
                (setq matterial_data_length_3 (strlen 
                                              (cdr (nth 7 attblock_list_))
                                            )
                )
                (setq matterial_data_name_3 (strcat 
                                              (cdr (nth 7 attblock_list_))
                                            )
                )
              ;
              ;concat_text_part
                
              ;
            ;
            ;matterial_sizing_process_
              ;preloop_and_while
                (setq sum_ ())
                (setq sum ())

                (setq attblock_list_i 8)
                (while (< attblock_list_i 13)
                  (setq attblock_list_val_ (cdr (nth attblock_list_i attblock_list_)))
                  (if (/= attblock_list_val_ "")
                    (progn
                      (setq sum_  attblock_list_val_)
                      (setq sum   (cons sum_ sum))
                      ; (setq sum (cons "x" sum))
                    )
                  )
                  (setq attblock_list_i (+ attblock_list_i 1))
                )
                (setq sum   (reverse sum))
                (if (/= sum nil)
                  (progn
                    (setq sum_string (LM:lst->str sum "x" ))
                    (setq sum_string_sum (strcat sum_string "mm._"))
                    (setq sum_string_sum_Len (strlen sum_string_sum))
                  )
                  (setq sum_string nil
                        sum_string_sum nil
                        sum_string_sum_Len nil
                  )
                
                )
              
              ;
            ;
            ;matterial_length_process
              (if (/= (cdr (nth 13 attblock_list_)) "")
                (progn
                  (setq length_string_ nil)
                  (setq length_string_len nil)
                  (setq length_string_ (strcat "(L="(cdr (nth 13 attblock_list_))"mm.)"))
                  (setq length_string_len (strlen length_string_))
                )
                (setq length_string_ ""
                      length_string_len nil
                )
              )

            ;
            ;gentext_part_name_mode
              ;preloop_and_while

                (setq sum_text_ (list matterial_data_length_1 matterial_data_length_2 matterial_data_length_3 sum_string_sum_Len length_string_len ))
                
                (setq sum_text_name (list matterial_data_name_1 matterial_data_name_2 matterial_data_name_3 sum_string_sum length_string_ ))
                (setq sum_text_name (LM:lst->str sum_text_name "" ))
                (setq sum_text_name (LM:str->lst sum_text_name "_" ))

            
                (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
                ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
                
                (setq sum_text_i 0)
                (setq sum_text_len 0)
                (setq new_text_line_1 ())
                (setq new_text_line_2 ())
                (setq new_text_line_3 ())
                
                (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
                  (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                  (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                  (cond
                    ((< sum_text_len 35)
                      (progn
                        (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                      )
                      (princ "x1")
                    )
                    ((and (>= sum_text_len 35) (< sum_text_len (+ 35 35 )) )
                      (progn
                        (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                      )
                      (princ "x2")
                    )
                    ((and (>= sum_text_len (+ 35 35  )) (< sum_text_len (+ 35 35 35 35)) )
                      (progn
                        (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                      )
                      (princ "x3")
                    )
                  )
                  (setq sum_text_i (+ sum_text_i 1))
                )
                (strlen (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_")))
                (strlen (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_")))
                (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

                (if (/= new_text_line_1_sum nil)
                  (progn
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" (strcat new_text_line_1_sum "_"  ) )
                  )
                  (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" "" )
                )
                (if (/= new_text_line_2_sum nil)
                  (progn
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_2" (strcat new_text_line_2_sum "_"  ) )
                  )
                  (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
                )
                (if (/= new_text_line_3_sum nil)
                  (progn
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" (strcat new_text_line_3_sum "_"  ) )
                  )
                  (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
                )
                
                
              ;
            ;
          ;
          ;generate_part_code_process
            ;matterial_data_proces
              ;part_1
                (setq matterial_data_length_1 (strlen 
                                              (cdr (nth 2 attblock_list_))
                                              ; (cdr (nth 3 attblock_list_))
                                              ; (cdr (nth 4 attblock_list_))
                                              ; (cdr (nth 5 attblock_list_))
                                              ; "_"
                                            )
                )
                (setq matterial_data_name_1 (strcat 
                                              (cdr (nth 2 attblock_list_))
                                              ; (cdr (nth 3 attblock_list_))
                                              ; (cdr (nth 4 attblock_list_))
                                              ; (cdr (nth 5 attblock_list_))
                                              ; "_"
                                            )
                )
              ;
              ;part_2
                (setq matterial_data_length_2 (strlen 
                                              (cdr (nth 6 attblock_list_))
                                            )
                )
                (setq matterial_data_name_2 (strcat 
                                              (cdr (nth 6 attblock_list_))
                                            )
                )
              ;
              ;part_3
                (setq matterial_data_length_3 (strlen 
                                              (cdr (nth 7 attblock_list_))
                                            )
                )
                (setq matterial_data_name_3 (strcat 
                                              (cdr (nth 7 attblock_list_))
                                            )
                )
              ;
              ;concat_text_part
                
              ;
            ;
            ;matterial_sizing_process_
              ;preloop_and_while
                (setq sum_ ())
                (setq sum ())

                (setq attblock_list_i 8)
                (while (< attblock_list_i 13)
                  (setq attblock_list_val_ (cdr (nth attblock_list_i attblock_list_)))
                  (if (/= attblock_list_val_ "")
                    (progn
                      (setq sum_  attblock_list_val_)
                      (setq sum   (cons sum_ sum))
                      ; (setq sum (cons "x" sum))
                    )
                  )
                  (setq attblock_list_i (+ attblock_list_i 1))
                )
                (setq sum   (reverse sum))
                (if (/= sum nil)
                  (progn
                    (setq sum_string (LM:lst->str sum "x" ))
                    (setq sum_string_sum (strcat sum_string "mm._"))
                    (setq sum_string_sum_Len (strlen sum_string_sum))
                  )
                  (setq sum_string nil
                        sum_string_sum nil
                        sum_string_sum_Len nil
                  )
                
                )
              
              ;
            ;
            ;matterial_length_process
              (if (/= (cdr (nth 13 attblock_list_)) "")
                (progn
                  (setq length_string_ nil)
                  (setq length_string_len nil)
                  (setq length_string_ (strcat "(L="(cdr (nth 13 attblock_list_))"mm.)"))
                  (setq length_string_len (strlen length_string_))
                )
                (setq length_string_ ""
                      length_string_len nil
                )
              )

            ;
            ;gentext_part_name_mode
              ;preloop_and_while

                (setq sum_text_ (list matterial_data_length_1 matterial_data_length_2 matterial_data_length_3 sum_string_sum_Len length_string_len ))
                
                (setq sum_text_name (list matterial_data_name_1 matterial_data_name_2 matterial_data_name_3 sum_string_sum length_string_ ))
                (setq sum_text_name (LM:lst->str sum_text_name "" ))
                (setq sum_text_name (LM:str->lst sum_text_name "_" ))

            
                (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
                ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
                
                (setq sum_text_i 0)
                (setq sum_text_len 0)
                (setq new_text_line_1 ())
                (setq new_text_line_2 ())
                (setq new_text_line_3 ())
                
                (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
                  (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                  (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                  
                  (if (< sum_text_len 35)
                    (progn
                      (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                    )
                    (princ "x")
                  )
                  (if (and (>= sum_text_len 35) (< sum_text_len (+ 35 35 )) )
                    (progn
                      (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                    )
                    (princ "x")
                  )
                  (if (and (>= sum_text_len (+ 35 35 )) (< sum_text_len (+ 35 35 35 35)) )
                    (progn
                      (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                    )
                    (princ "x")
                  )
                  (setq sum_text_i (+ sum_text_i 1))
                )
                (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
                (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
                (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

                (if (/= new_text_line_1_sum nil)
                  (progn
                    (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" (strcat new_text_line_1_sum "_"  ))
                  )
                  (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" "" )
                )
                (if (/= new_text_line_2_sum nil)
                  (progn
                    (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" (strcat new_text_line_2_sum "_"  ) )
                  )
                  (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" "" )
                )

                
                
              ;
            ;
          ;
        )
      )
    ;

)
(defun c:z638B_generate_part_name_001-PART_DATA_CUSTOM_2025 () 

    ;user_input_for_get_data_dynamic_block_object
      (if (= 1 1) ;for checking insert is attblock.
        (progn
          ;user_input_get_block_(not loop)
            ;block_data_for insertion
              (if (/= (setq blk_ename_ (car (entsel "specify_block_"))) nil)
                (progn
                  (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
                )
                (alert "Object is blank.")
              )         
              (if (and
                    (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference");For Check objectname
                    ; (= (vla-get-isdynamicblock blk_ename_obj_) :vlax-true );For Check objectname is dynamicblock
                    (= (vla-get-hasattributes blk_ename_obj_) :vlax-true );For Check objectname is attblock
                  ) 
                (progn
                  (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
                  (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
                )
                (alert "Object is not block.")
              )
            ;
          ;
        )
      )
    ;
    ;put_data_process_
      (if (= (LM:effectivename blk_ename_obj_ ) "001 - PART DATA CUSTOM 2025" )
        (progn
          (setq attblock_list_ (LM:vl-getattributevalues blk_ename_obj_ ))
            ;generate_part_name_process
              ;matterial_data_proces
                ;part_1
                  (setq matterial_data_length_1 (strlen 
                                                (cdr (nth 3 attblock_list_))
                                                (cdr (nth 4 attblock_list_))
                                                (cdr (nth 5 attblock_list_))
                                                "_"
                                              )
                  )
                  (setq matterial_data_name_1 (strcat 
                                                (cdr (nth 3 attblock_list_))
                                                (cdr (nth 4 attblock_list_))
                                                (cdr (nth 5 attblock_list_))
                                                "_"
                                              )
                  )
                ;
                ;concat_text_part
                  
                ;
              ;
              ;gentext_part_name_mode
                ;preloop_and_while

                  (setq sum_text_ (list matterial_data_length_1 ))
                  
                  (setq sum_text_name (list matterial_data_name_1    ))
                  (setq sum_text_name (LM:lst->str sum_text_name "" ))
                  (setq sum_text_name (LM:str->lst sum_text_name "_" ))
                  (setq sum_text_name (TA:REMOVE_VAL_LIST_ "" sum_text_name))

              
                  (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
                  ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
                  
                  (setq sum_text_i 0)
                  (setq sum_text_len 0)
                  (setq new_text_line_1 ())
                  (setq new_text_line_2 ())
                  (setq new_text_line_3 ())
                  
                  (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
                    (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                    (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                    
                    (if (<= sum_text_len 35)
                      (progn
                        (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                      )
                      (princ "x")
                    )
                    (if (and (> sum_text_len 35) (< sum_text_len (+ 35 35 )) )
                      (progn
                        (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                      )
                      (princ "x")
                    )
                    (if (and (>= sum_text_len (+ 35 35 )) (< sum_text_len (+ 35 35 35 35)) )
                      (progn
                        (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                      )
                      (princ "x")
                    )
                    (setq sum_text_i (+ sum_text_i 1))
                  )
                  (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
                  (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
                  (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

                  (if (/= new_text_line_1_sum nil)
                    (progn
                      (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" (strcat new_text_line_1_sum "_") )
                    )
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" "" )
                  )
                  (if (/= new_text_line_2_sum nil)
                    (progn
                      (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_2" (strcat new_text_line_2_sum "_") )
                    )
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
                  )
                  (if (/= new_text_line_3_sum nil)
                    (progn
                      (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" (strcat new_text_line_3_sum "_") )
                    )
                    (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
                  )
                  
                  
                ;
              ;
            ;
            ;generate_part_code_process
              ;matterial_data_proces
                ;part_1
                  (setq matterial_data_length_1 (strlen 
                                                (cdr (nth 1 attblock_list_))
                                                ; (cdr (nth 3 attblock_list_))
                                                ; (cdr (nth 4 attblock_list_))
                                                ; (cdr (nth 5 attblock_list_))
                                                "_"
                                              )
                  )
                  (setq matterial_data_name_1 (strcat 
                                                (cdr (nth 1 attblock_list_))
                                                ; (cdr (nth 3 attblock_list_))
                                                ; (cdr (nth 4 attblock_list_))
                                                ; (cdr (nth 5 attblock_list_))
                                                "_"
                                              )
                  )
                ;
                ;part_2
                  (setq matterial_data_length_2 (strlen 
                                                (cdr (nth 2 attblock_list_))
                                                "_"
                                              )
                  )
                  (setq matterial_data_name_2 (strcat 
                                                (cdr (nth 2 attblock_list_))
                                                "_"
                                              )
                  )
                ;
                ;concat_text_part
                  
                ;
              ;
              ;gentext_part_name_mode
                ;preloop_and_while

                  (setq sum_text_ (list matterial_data_length_1 matterial_data_length_2    ))
                  
                  (setq sum_text_name (list matterial_data_name_1 matterial_data_name_2    ))
                  (setq sum_text_name (LM:lst->str sum_text_name "" ))
                  (setq sum_text_name (LM:str->lst sum_text_name "_" ))
                  (setq sum_text_name (TA:REMOVE_VAL_LIST_  "" sum_text_name))
          

              
                  (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
                  ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
                  
                  (setq sum_text_i 0)
                  (setq sum_text_len 0)
                  (setq new_text_line_1 ())
                  (setq new_text_line_2 ())
                  (setq new_text_line_3 ())
                  
                  (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
                    (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                    (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                    
                    (if (< sum_text_len 35)
                      (progn
                        (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                      )
                      (princ "x")
                    )
                    (if (and (>= sum_text_len 35) (< sum_text_len (+ 35 35 )) )
                      (progn
                        (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                      )
                      (princ "x")
                    )
                    (if (and (>= sum_text_len (+ 35 35 )) (< sum_text_len (+ 35 35 35 35)) )
                      (progn
                        (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                      )
                      (princ "x")
                    )
                    (setq sum_text_i (+ sum_text_i 1))
                  )
                  (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
                  (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
                  (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

                  (if (/= new_text_line_1_sum nil)
                    (progn
                      (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" (strcat new_text_line_1_sum "_") )
                    )
                    (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" "" )
                  )
                  (if (/= new_text_line_2_sum nil)
                    (progn
                      (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" (strcat new_text_line_2_sum "_") )
                    )
                    (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" "" )
                  )

                  
                  
                ;
              ;
            ;
        )
      )
    ;

)
(defun c:z639A_generate_project_name_ ()
  ;user_input_for_get_data_dynamic_block_object
    (if (= 1 1) ;for checking insert is attblock.
      (progn
        ;user_input_get_block_(not loop)
          ;block_data_for insertion
            (if (/= (setq blk_ename_ (car (entsel "specify_block_"))) nil)
              (progn
                (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
              )
              (alert "Object is blank.")
            )         
            (if (and
                  (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference");For Check objectname
                  ; (= (vla-get-isdynamicblock blk_ename_obj_) :vlax-true );For Check objectname is dynamicblock
                  (= (vla-get-hasattributes blk_ename_obj_) :vlax-true );For Check objectname is attblock
                ) 
              (progn
                (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
                (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
              )
              (alert "Object is not block.")
            )
          ;
        ;
      )
    )
  ;
  ;get_data_process_
    (if (or
          
          (= (LM:effectivename blk_ename_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01" )
          (= (LM:effectivename blk_ename_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01-NMBU2" )
        )
      (progn
        (setq attblock_list_ (LM:vl-getattributevalues blk_ename_obj_ ))
        
        (setq NAME_PROJECT_1 (LM:vl-getattributevalue blk_ename_obj_ "NAME_PROJECT_1"))
        (setq NAME_PROJECT_2 (LM:vl-getattributevalue blk_ename_obj_ "NAME_PROJECT_2"))
        (setq NAME_PROJECT_3 (LM:vl-getattributevalue blk_ename_obj_ "NAME_PROJECT_3"))
        
        (setq NAME_PART_NAME1 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_1"))
        (setq NAME_PART_NAME2 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_2"))
        (setq NAME_PART_NAME3 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_3"))
        
        (setq NAME_PROJECT_OVERALL_ (list NAME_PROJECT_1  NAME_PROJECT_2  NAME_PROJECT_3 ))
        (setq NAME_PROJECT_OVERALL_ (TA:REMOVE_VAL_LIST_ "" NAME_PROJECT_OVERALL_))
        (setq NAME_PROJECT_OVERALL_ (LM:lst->str NAME_PROJECT_OVERALL_ "" ))
        (setq NAME_PROJECT_OVERALL_ (LM:str->lst NAME_PROJECT_OVERALL_ "_" ))
        (setq NAME_PROJECT_OVERALL_ (TA:REMOVE_VAL_LIST_ "" NAME_PROJECT_OVERALL_))
        
        (setq NAME_PART_NAME_OVERALL_(list NAME_PART_NAME1 NAME_PART_NAME2 NAME_PART_NAME3))
        (setq NAME_PART_NAME_OVERALL_(TA:REMOVE_VAL_LIST_ "" NAME_PART_NAME_OVERALL_ ))
        (setq NAME_PART_NAME_OVERALL_(LM:lst->str NAME_PART_NAME_OVERALL_ ""))
        
          (if (and (/= NAME_PART_NAME1 "")(/= NAME_PART_NAME2 "")(/= NAME_PART_NAME3 ""))
            (progn 
              (setq NAME_PART_NAME_OVERALL_(LM:str->lst NAME_PART_NAME_OVERALL_ "_"))
              (setq NAME_PART_NAME_OVERALL_(TA:REMOVE_VAL_LIST_ "" NAME_PART_NAME_OVERALL_ ))
            )
            (princ "x")
          )
      )
    )
  ;
  ;reline_project_name_process
    (setq NAME_PROJECT_OVERALL_i 0)
    (setq NAME_PROJECT_OVERALL_len 0)
    (setq new_text_line_1 ())
    (setq new_text_line_2 ())
    (setq new_text_line_3 ())
    (while (and (< NAME_PROJECT_OVERALL_i (length NAME_PROJECT_OVERALL_)) (< NAME_PROJECT_OVERALL_len (+ 35 35 35 35)))
      
      (setq NAME_PROJECT_OVERALL_name (nth  NAME_PROJECT_OVERALL_i NAME_PROJECT_OVERALL_))
      (setq NAME_PROJECT_OVERALL_len (+ (+ NAME_PROJECT_OVERALL_len (strlen NAME_PROJECT_OVERALL_name) ) 1))
      (if (< NAME_PROJECT_OVERALL_len 35)
        (progn
          (setq new_text_line_1 (cons NAME_PROJECT_OVERALL_name new_text_line_1 ))
        )
        (princ "x")
      )
      (if (and (>= NAME_PROJECT_OVERALL_len 35) (< NAME_PROJECT_OVERALL_len (+ 35 35 )) )
        (progn
          (setq new_text_line_2 (cons NAME_PROJECT_OVERALL_name new_text_line_2 ))
        )
        (princ "x")
      )
      (if (and (>= NAME_PROJECT_OVERALL_len (+ 35 35  )) (< NAME_PROJECT_OVERALL_len (+ 35 35 35 )) )
        (progn
          (setq new_text_line_3 (cons NAME_PROJECT_OVERALL_name new_text_line_3 ))
        )
        (princ "x")
      )
      (setq NAME_PROJECT_OVERALL_i (+ NAME_PROJECT_OVERALL_i 1))
    )
    (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
    (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
    (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

    (if (/= new_text_line_1_sum nil)
      (progn
        (setq new_text_line_1_sum (strcat new_text_line_1_sum "_"))
        (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_1" new_text_line_1_sum )
      )
      (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_1" "" )
    )
    (if (/= new_text_line_2_sum nil)
      (progn
        (setq new_text_line_2_sum (strcat new_text_line_2_sum "_"))
        (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_2" new_text_line_2_sum )
      )
      (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_2" "" )
    )
    (if (/= new_text_line_3_sum nil)
      (progn
        (setq new_text_line_3_sum (strcat new_text_line_3_sum "_"))
        (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_3" new_text_line_3_sum )
      )
      (LM:vl-setattributevalue blk_ename_obj_ "NAME_PROJECT_3" "" )
    )
  ;

)
(defun c:z639B_generate_part_code_project_name_ ()
  ;user_input_for_get_data_dynamic_block_object
    (if (= 1 1) ;for checking insert is attblock.
      (progn
        ;user_input_get_block_(not loop)
          ;block_data_for insertion
            (if (/= (setq blk_ename_ (car (entsel "specify_block_"))) nil)
              (progn
                (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
              )
              (alert "Object is blank.")
            )         
            (if (and
                  (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference");For Check objectname
                  ; (= (vla-get-isdynamicblock blk_ename_obj_) :vlax-true );For Check objectname is dynamicblock
                  (= (vla-get-hasattributes blk_ename_obj_) :vlax-true );For Check objectname is attblock
                ) 
              (progn
                (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
                (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
              )
              (alert "Object is not block.")
            )
          ;
        ;
      )
    )
  ;
  ;get_data_process_
    (if (= (LM:effectivename blk_ename_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01" )
      (progn
        (setq attblock_list_ (LM:vl-getattributevalues blk_ename_obj_ ))
      )
    )
  ;
  ;generate_part_name_process
    ;matterial_data_proces
      ;part_1
        (setq matterial_data_length_1 (strlen 
                                      (cdr (nth 7 attblock_list_))
                                      (cdr (nth 8 attblock_list_))
                                      (cdr (nth 9 attblock_list_))
                                      "_"
                                    )
        )
        (setq matterial_data_name_1 (strcat 
                                      (cdr (nth 7 attblock_list_))
                                      (cdr (nth 8 attblock_list_))
                                      (cdr (nth 9 attblock_list_))
                                      "_"
                                    )
        )
      ;
      ;concat_text_part
        
      ;
    ;
    ;gentext_part_name_mode
      ;preloop_and_while

        (setq sum_text_ (list matterial_data_length_1 ))
        
        (setq sum_text_name (list matterial_data_name_1    ))
        (setq sum_text_name (LM:lst->str sum_text_name "" ))
        (setq sum_text_name (LM:str->lst sum_text_name "_" ))
        (setq sum_text_name (TA:REMOVE_VAL_LIST_ "" sum_text_name))

    
        (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
        ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
        
        (setq sum_text_i 0)
        (setq sum_text_len 0)
        (setq new_text_line_1 ())
        (setq new_text_line_2 ())
        (setq new_text_line_3 ())
        
        (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
          (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
          (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
          
          (if (<= sum_text_len 35)
            (progn
              (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
            )
            (princ "x")
          )
          (if (and (> sum_text_len 35) (< sum_text_len (+ 35 35 )) )
            (progn
              (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
            )
            (princ "x")
          )
          (if (and (>= sum_text_len (+ 35 35 )) (< sum_text_len (+ 35 35 35 35)) )
            (progn
              (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
            )
            (princ "x")
          )
          (setq sum_text_i (+ sum_text_i 1))
        )
        (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
        (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
        (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

        (if (/= new_text_line_1_sum nil)
          (progn
            (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" (strcat new_text_line_1_sum "_") )
          )
          (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_1" "" )
        )
        (if (/= new_text_line_2_sum nil)
          (progn
            (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_2" (strcat new_text_line_2_sum "_") )
          )
          (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
        )
        (if (/= new_text_line_3_sum nil)
          (progn
            (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" (strcat new_text_line_3_sum "_") )
          )
          (LM:vl-setattributevalue blk_ename_obj_ "PART_NAME_VALUE_3" "" )
        )
        
        
      ;
    ;
  ;
  ;generate_part_code_process
    ;matterial_data_proces
      ;part_1
        (setq matterial_data_length_1 (strlen 
                                      (cdr (nth 5 attblock_list_))
                                      (cdr (nth 6 attblock_list_))
                                      
                                      "_"
                                    )
        )
        (setq matterial_data_name_1 (strcat 
                                      (cdr (nth 5 attblock_list_))
                                      (cdr (nth 6 attblock_list_))
                                      
                                      "_"
                                    )
        )
      ;
      ;concat_text_part
        
      ;
    ;
    ;gentext_part_name_mode
      ;preloop_and_while

        (setq sum_text_ (list matterial_data_length_1 matterial_data_length_2    ))
        
        (setq sum_text_name (list matterial_data_name_1     ))
        (setq sum_text_name (LM:lst->str sum_text_name "" ))
        (setq sum_text_name (LM:str->lst sum_text_name "_" ))
        (setq sum_text_name (TA:REMOVE_VAL_LIST_  "" sum_text_name))


    
        (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
        ; (setq sum_text_name (TA:REMOVE_VAL_LIST_ nil sum_text_name))
        
        (setq sum_text_i 0)
        (setq sum_text_len 0)
        (setq new_text_line_1 ())
        (setq new_text_line_2 ())
        (setq new_text_line_3 ())
        
        (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 105))
          (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
          (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
          
          (if (< sum_text_len 35)
            (progn
              (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
            )
            (princ "x")
          )
          (if (and (>= sum_text_len 35) (< sum_text_len (+ 35 35 )) )
            (progn
              (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
            )
            (princ "x")
          )
          (if (and (>= sum_text_len (+ 35 35 )) (< sum_text_len (+ 35 35 35 35)) )
            (progn
              (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
            )
            (princ "x")
          )
          (setq sum_text_i (+ sum_text_i 1))
        )
        (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_"))
        (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_"))
        (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_"))

        (if (/= new_text_line_1_sum nil)
          (progn
            (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" (strcat new_text_line_1_sum "_") )
          )
          (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_1" "" )
        )
        (if (/= new_text_line_2_sum nil)
          (progn
            (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" (strcat new_text_line_2_sum "_") )
          )
          (LM:vl-setattributevalue blk_ename_obj_ "CODE_VALUE_2" "" )
        )

        
        
      ;
    ;
  ;
)
  



(defun TA:add_to_list  (target_SEQ_ target_val_ list_val_)
  ;example using
    ; (setq target_SEQ_ 2)
    ; (setq target_val_ "SS")
    ; (setq list_val_ (list 1  2 3 5 9 ))
    ; (TA:add_to_list target_SEQ_ target_val_ list_val_) 
  ;
  ;preloop_and_while
    (setq newlist_ ())
    (setq list_val_i 0)
    (while (< list_val_i (length list_val_))
      (setq list_val_ename_ (nth  list_val_i list_val_))
      
      (if (= target_SEQ_ list_val_i )
        (progn
          (setq newlist_ (cons target_val_ newlist_))
          (setq newlist_ (cons list_val_ename_ newlist_))
        )
        (setq newlist_ (cons list_val_ename_ newlist_))
      )
      (setq list_val_i (+ list_val_i 1))
    )
    (if (= target_SEQ_ list_val_i )
      (progn
        (setq newlist_ (cons target_val_ newlist_))
      )
      (princ "TARGET_SEQ is over length\n")
    )
    
  ;
  (setq newlist_ (reverse newlist_))
)











































(defun c:CopyTextWithArray (/ startNum numCopies textHeight deltaX deltaY i pt1 newPt newStr textStyle)
  ;; ขอให้ผู้ใช้ป้อนตัวเลขเริ่มต้น และกำหนดค่าดีฟอลต์เป็น 1
  (setq startNum (getstring "\nEnter starting number <1>: "))
  (if (or (null startNum) = startNum "")
    (setq startNum 1)
    (setq startNum (atoi startNum))
  )
  ;; ขอให้ผู้ใช้ป้อนจำนวนครั้งที่ต้องการคัดลอก และกำหนดค่าดีฟอลต์เป็น 10
  (setq numCopies (getstring "\nEnter number of copies <10>: "))
  (if (or (null numCopies) = numCopies "")
    (setq numCopies 10)
    (setq numCopies (atoi numCopies))
  )
  ;; ขอให้ผู้ใช้ป้อนขนาดตัวอักษร และกำหนดค่าดีฟอลต์เป็น 1
  (setq textHeight (getstring "\nEnter text height <1>: "))
  (if (or (null textHeight) = textHeight "")
    (setq textHeight 1)
    (setq textHeight (atof textHeight))
  )
  ;; ขอให้ผู้ใช้กำหนดจุดอ้างอิงที่ 1
  (setq pt1 (getpoint "\nSpecify first reference point: "))
  ;; ขอให้ผู้ใช้กำหนดจุดอ้างอิงที่ 2
  (setq pt2 (getpoint pt1 "\nSpecify second reference point: "))
  ;; คำนวณระยะทางระหว่างสองจุด
  (setq deltaX (- (car pt2) (car pt1)))
  (setq deltaY (- (cadr pt2) (cadr pt1)))
  ;; กำหนดสไตล์ของข้อความ
  (setq textStyle "Standard") ;; สไตล์ของข้อความสามารถปรับได้ตามต้องการ
  ;; วนลูปตามจำนวนครั้งที่ต้องการคัดลอก
  (setq i 0)
  (while (< i numCopies)
    ;; คำนวณตำแหน่งใหม่สำหรับข้อความที่สร้าง
    (setq newPt (list (+ (car pt1) (* i deltaX)) (+ (cadr pt1) (* i deltaY))))
    ;; รันตัวเลขเพิ่มขึ้นเรื่อยๆ
    (setq newStr (itoa (+ startNum i)))
    ;; สร้างข้อความใหม่ที่ตำแหน่งใหม่
    (command "_.TEXT" "_J" "MC" newPt textHeight "0.0" newStr)
    ;; เพิ่มตัวนับ
    (setq i (1+ i))
  )
  (princ)
)
(princ "\nCreate text with array and automatic numbering is ready. Use the 'CopyTextWithArray' command to start.\n")
(princ)


