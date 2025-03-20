;Effective Block Name Funcition Lee Mac
  ;; Effective Block Name  -  Lee Mac
    ;; obj - [vla] VLA Block Reference object
  (defun LM:effectivename (obj) 
    (vlax-get-property obj 
                      (if (vlax-property-available-p obj 'effectivename) 
                        'effectivename
                        'name
                      )
    )
  )
  ;; Effective Block Name  -  Lee Mac
  ;; ent - [ent] Block Reference entity
  (defun LM:al-effectivename (ent / blk rep) 
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**") 
      (if 
        (and 
          (setq rep (cdadr 
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
;Attribute Function Lee Mac
  ;; Get Attribute Value  -  Lee Mac
  ;; Returns the value held by the specified tag within the supplied block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; Returns: [str] Attribute value, else nil if tag is not found.
  (defun LM:vl-getattributevalue (blk tag) 
    (setq tag (strcase tag))
    (vl-some 
      '(lambda (att) 
        (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
      )
      (vlax-invoke blk 'getattributes)
    )
  )
  ;; Set Attribute Value  -  Lee Mac
  ;; Sets the value of the first attribute with the given tag found within the block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; val - [str] Attribute Value
  ;; Returns: [str] Attribute value if successful, else nil.
  (defun LM:vl-setattributevalue (blk tag val) 
    (setq tag (strcase tag))
    (vl-some 
      '(lambda (att) 
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
  (defun LM:vl-getattributevalues (blk) 
    (mapcar '(lambda (att) (cons (vla-get-tagstring att) (vla-get-textstring att))) 
            (vlax-invoke blk 'getattributes)
    )
  )
  (defun LM:vl-getattributevalue-tag-TA-Modifies (blk) 
    (mapcar '(lambda (att) (vla-get-tagstring att)) 
            (vlax-invoke blk 'getattributes)
    )
  )
  (defun LM:vl-getattributevalue-val-TA-Modifies (blk) 
    (mapcar '(lambda (att) (vla-get-textstring att)) 
            (vlax-invoke blk 'getattributes)
    )
  )
  ;; Set Attribute Values  -  Lee Mac
  ;; Sets attributes with tags found in the association list to their associated values.
  ;; blk - [vla] VLA Block Reference Object
  ;; lst - [lst] Association list of ((<tag> . <value>) ... )
  ;; Returns: nil
  (defun LM:vl-setattributevalues (blk lst / itm) 
    (foreach att (vlax-invoke blk 'getattributes) 
      (if (setq itm (assoc (vla-get-tagstring att) lst)) 
        (vla-put-textstring att (cdr itm))
      )
    )
  )
;
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
;Remove&Edit List Function Lee Mac
  (defun LM:RemoveNth (n l / i) 
    ;;  Arguments:                                              ;;
    ;;  n - index of item to remove (zero based)                ;;
    ;;  l - list from which item is to be removed               ;;
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
;
;get_Intersect_func
  (defun LM:intersections (ob1 ob2 mod / lst rtn) 
    (if 
      (and (vlax-method-applicable-p ob1 'intersectwith) 
          (vlax-method-applicable-p ob2 'intersectwith)
          (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
      )
      (repeat (/ (length lst) 3) 
        (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
              lst (cdddr lst)
        )
      )
    )
    (remove_member_list_ 2 (reverse rtn))
    
  )
  (defun LM:intersections+0 (ob1 ob2 mod / lst rtn) 
    (if 
      (and (vlax-method-applicable-p ob1 'intersectwith) 
          (vlax-method-applicable-p ob2 'intersectwith)
          (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
      )
      (repeat (/ (length lst) 3) 
        (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
              lst (cdddr lst)
        )
      )
    )
    ; (remove_member_list_ 2 (reverse rtn))
    (reverse rtn)
    
  )
  (defun LM:intersections_crood_position (ob1 ob2 coord_ mod  / lst rtn) 
    (if 
      (and (vlax-method-applicable-p ob1 'intersectwith) 
          (vlax-method-applicable-p ob2 'intersectwith)
          (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
      )
      (repeat (/ (length lst) 3) 
        (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
              lst (cdddr lst)
        )
      )
    )
    (cond ;soectify corrdinate position
      ((= coord_ "xyz")
      (progn 
        (reverse rtn)
      )
      )
      ((= coord_ "xy")
      (progn 
        (remove_member_list_ 2 (reverse rtn))
      )
      )
      ((or
        (/= coord_ "xy")
        (/= coord_ "xyz")
        (= coord_ nil)
       )
      (progn 
        (princ "\n:::::::::::::::: ERROR ::::::::::::::::::::::")
        (princ "\n:::::Please Specify Coordinate Position::::::\n")
        (princ ":::::::::::::: X Y    = 2 coord :::::::::::::\n")
        (princ ":::::::::::::: X Y Z  = 3 coord :::::::::::::\n")
      )
      )
    )
  )
;
;Filter Selection Set by Effectivename Ta Trai
  (defun TA:EF_Filter_ss_set_ (ss_pre_filter_set_  effectivename) ;กรอง block object ด้วย LM:effectivename
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
    ;example_code_
      ; (setq ss1 (list 222 55))
      ; (setq ss2 (list 222 4 ))
      ; (setq ss3 (list 222  77))
      ; (setq ss (list ss1 ss2 ss3))
      ; (remove_member_list_ 1 ss ) = ((222) (222) (222))
    ;
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
  (defun TA:FIND_DUPLICATE_LIST (target_val_ target_list_)
    ;example_code
      ; (setq target_val_ 69)
      ; (setq target_list_ (list 2 5 69 4 26 9 5 23 6 9 52  6))
    ;
    ;preloop_and_while
      (setq target_list_i 0)
      (setq check_list_val "N")
      (while (and (< target_list_i (length target_list_)) (= check_list_val "N"))
        (setq target_list_ename_ (nth  target_list_i target_list_))
          (if (= target_val_ target_list_ename_ )
            (progn
              (setq check_list_val "Y")
            )
            (setq check_list_val "N")
          )
        (setq target_list_i (+ target_list_i 1))
      )
    ;
    ;summary_checking_list_
      (if (= target_list_i (length target_list_))
        (progn
          (princ (strcat "do not found duplicate targetlist \n" ))
        )
        (princ (strcat "target_val_ has been duplicate in " (rtos target_list_i 2 0) "\n" ))
      )
      (setq check_list_val check_list_val)
    ;
  )
;
;Get_Data Document Ta Trai
  (defun LM:getdocumentobject ( dwg / app dbx dwl err vrs )
      (cond
          (   (not (setq dwg (findfile dwg))) nil)
          (   (cdr
                  (assoc (strcase dwg)
                      (vlax-for doc (vla-get-documents (setq app (vlax-get-acad-object)))
                          (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                      )
                  )
              )
          )
          (   (progn
                  (setq dbx
                      (vl-catch-all-apply 'vla-getinterfaceobject
                          (list app
                              (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                  "objectdbx.axdbdocument"
                                  (strcat "objectdbx.axdbdocument." (itoa vrs))
                              )
                          )
                      )
                  )
                  (or (null dbx) (vl-catch-all-error-p dbx))
              )
              (prompt "\nUnable to interface with ObjectDBX.")
          )
          (   (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-open (list dbx dwg))))
              (prompt (strcat "\n" (vl-catch-all-error-message err)))
          )
          (   dbx   )
      )
  )
  (defun getdrawinglayouts ( dwg / doc idx rtn )
    ;original_method_
      (if (setq doc (LM:getdocumentobject dwg))
          (progn
              (vlax-for lyt (vla-get-layouts doc)
                  (setq rtn (cons (vla-get-name lyt) rtn)
                        idx (cons (vla-get-taborder lyt) idx)
                  )
              )
              (vlax-release-object doc)
              (mapcar '(lambda ( n ) (nth n rtn)) (vl-sort-i idx '<))
          )
      )
    ;
  )
;
;Get_Data Function Ta Trai
  (defun TA:get_name_block_in_drawing (mode)
    ;user_input_
      (setq mode "mode")
    ;
    ;get_obj_file_
      (setq multi_blocks_obj (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq block_list_ ())
    ;
    ;method_for_get_name_block_in_drawing
      ;method_1_get_name_
        (vlax-for block_ multi_blocks_obj 
          (setq block_list_ (append block_list_ (list (vla-get-name block_))))
        )
      ;
      ; ;method_2_get_name_
      ;   (setq block_i 0)
      ;   (while (< block_i (vla-get-count multi_blocks_obj))
      ;     (setq block_ (vla-item multi_blocks_obj block_i))
      ;     (setq block_list_ (append block_list_ (list (vla-get-name block_))))
      ;     (setq block_i (1+ block_i))
      ;   )
      ; ;
    ;
    ;sorting_name_block
      (setq sorted_block_list_ (acad_strlsort block_list_))
    ;
    ;preloop_and_while filtter_
      (setq sorted_block_list_i 0)
      (setq filter_sorted_block_ ())

      (while (< sorted_block_list_i (length sorted_block_list_))
        (setq filter_sorted_block_effectivename_ (nth sorted_block_list_i sorted_block_list_))
        
        (if (/= (substr filter_sorted_block_effectivename_ 1 1) "*") ; ตรวจสอบว่าชื่อเริ่มต้นด้วย *
          (progn
            (setq filter_sorted_block_ (cons filter_sorted_block_effectivename_ filter_sorted_block_))
          )
        )
        (setq sorted_block_list_i (+ sorted_block_list_i 1))
      )
    ;

    
    (setq filter_sorted_block_total (length filter_sorted_block_))
    (princ "\n                  |=====================|")
    (princ "\n                  | TOTAL BLOCK IN FILE |")
    (princ (strcat "\n                  |     = " (itoa filter_sorted_block_total) " block     |"))
    (princ "\n                  |          set        |")
    (princ "\n                  |---------------------|\n")
    (setq filter_sorted_block_ filter_sorted_block_)

  )
  (defun TA:get_name_layer_in_drawing ()
    (setq layers_list_ ())
    (setq layers_obj (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
    ;method_1_get_layers_name
      (vlax-for lay layers_obj
        (setq layers_list (append layers_list (list (vla-get-Name lay))))
      )
    ;
    ;method_2_get_layers_name
      (setq layers_i 0)
      (while (< layers_i (vla-get-count layers_obj))
        (setq layers_ (vla-item layers_obj layers_i))
        (setq layers_list_ (append layers_list_ (list (vla-get-name layers_))))
        (setq layers_i (1+ layers_i))
      )
    ;
    ;summary_layer_data
      (setq layers_list_total (length layers_list_))
      (princ "\n                  |=====================|")
      (princ "\n                  | TOTAL LAYERS IN FILE |")
      (princ (strcat "\n                  |     = " (itoa layers_list_total) " layer     |"))
      (princ "\n                  |          set        |")
      (princ "\n                  |---------------------|")
      (setq layers_list_ layers_list_)
    ;
  )
  (defun TA:ename+vla-get-insertionpoint (obj) ;ประกอบ entity_name กับ insertion_poiint ให้เป็น list (BLOCK ONLY)
    (if (= (vla-get-objectname obj) "AcDbBlockReference")
      (progn
        ;get_data
          (setq ename_ (vlax-vla-object->ename obj)) 
          
          (setq _inspt (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint obj))))
        ;
        ;sum_data_to_list
          (setq sum_list (list 
                          ename_
                          _inspt
                          ;add more
                        )
          )
        ;
      )
      (setq error "OBJECT IS NOT BLOCK")
    )
  )
  (defun TA:ename+vla-getboundingbox (obj) ;ประกอบ entity_name กับ vla-getboundingbox ให้เป็น list (LINE OR POLYLINE ONLY)
    ; (setq obj (vla-get-objectname (vlax-ename->vla-object (car (entsel))))) ;For Check objectname
    (if (or
          (= (vla-get-objectname obj) "AcDbLine")
          (= (vla-get-objectname obj) "AcDbPolyline")
          (= (vla-get-objectname obj) "AcDbBlockReference")
        )
      (progn
        ;get_data
          (setq ename_ (vlax-vla-object->ename obj)) 
          
          (setq _inspt (vla-getboundingbox obj 'min_ 'max_))
          (setq min_ (vlax-safearray->list min_))
          (setq max_ (vlax-safearray->list max_))
        ;
        ;sum_data_to_list
          (setq sum_list (list 
                           ename_
                           min_
                           max_
                           ;add more
                         )
          )
        ;
      )
      (setq error "OBJECT IS NOT LINE/POLYLINE")
    )
  )
  (defun TA:Get_Pline_vertext_angle_case1 (ename_) 
    (setq s_ (cadr (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq d1 (angtos (angle (nth 1 s_) (nth 2 s_))))
    (setq s_i 0)
    (setq s_ii 1)
    (setq end_val_ nil)
    (setq sum_ ())
    (while (< s_i (length s_))
        (setq pt_1 (nth s_i s_))
        (setq pt_3 (nth s_i s_))
        (if (= s_ii (length s_)) 
          (progn 
            (setq end_val_ "end")
            (setq pt_3 (nth (- s_i 1) s_))
          )
          (setq pt_2 (nth s_ii s_))
        )
        (setq angle_pt (atof (angtos (angle pt_1 pt_2))))
        (if (= end_val_ "end") 
          (progn
            (setq angle_pt (atof (angtos (angle pt_3 pt_1))))
            (setq sum (append  (list pt_1) (list angle_pt s_ii) ))
          )
          (setq sum (append  (list pt_1) (list angle_pt s_ii)))
        )
        
        (setq sum_ (cons sum sum_))
        (setq s_i (+ s_i 1))
        (setq s_ii (+ s_ii 1))
    )
    (setq result_ (reverse sum_))
  )
  (defun TA:Get_Pline_vertext_angle_case2 (ename_) 
    (setq s_ (cadr (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq d1 (angtos (angle (nth 1 s_) (nth 2 s_))))
    (setq s_i 0)
    (setq s_ii 1)
    (setq end_val_ nil)
    (setq sum_ ())
    (while (< s_i (length s_))
        (setq pt_1 (nth s_i s_))
        (if (= s_ii (length s_)) 
          (progn 
            (setq end_val_ "end")
          )
          (setq pt_2 (nth s_ii s_))
        )
        (setq angle_pt (atof (angtos (angle pt_1 pt_2))))
        (if (= end_val_ "end") 
          (progn 
            (setq sum (append  (list pt_1) (list end_val_ s_ii) ))
          )
          (setq sum (append  (list pt_1) (list angle_pt s_ii)))
        )
        
        (setq sum_ (cons sum sum_))
        (setq s_i (+ s_i 1))
        (setq s_ii (+ s_ii 1))
    )
    (setq result_ (reverse sum_))
  )
  (defun TA:Get_Pline_vertext_ins_point_ (ename_) ;must be (car (entsel))
    (setq Get_Pline_ (entget ename_))
    (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
    (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
    (setq ename+Get_Pline_vtx_pt_ (list
                                    ename_
                                    Get_Pline_vtx_pt_
                                  )
    
    )
  )
  (defun TA:get_vertex_len_ (ename_)
    ; (setq ref_line (car (entsel)))
    (setq ss (vlax-ename->vla-object ename_))
    (setq vertex_total (length (cadr (TA:Get_Pline_vertext_ins_point_ ename_))))
    (setq sum_ ())

    (setq vertex_i 0)
    (setq vertex_ii 1)

    (while (< vertex_i vertex_total)
      (cond
        (;_case_1
           (and
              (/= vertex_i (- vertex_total 1))
           )
           (progn
             ;get_data_len
              (setq len_i (vlax-curve-getDistatParam ss vertex_i) )
              (setq len_ii (vlax-curve-getDistatParam ss vertex_ii))
              ;
              ;sum
                (setq sum (list (- len_ii len_i)))
                (setq sum_ (append sum sum_ ))
              ;
             (princ "_case_1\n")
           )
        )
        (;_case_2
          (and
            (= vertex_i (- vertex_total 1))    
          )
          (progn
            ;ไม่มี
            (princ "_case_2\n")
          )
        )

      )
      
      
      (setq vertex_i (+ vertex_i 1))
      (setq vertex_ii (+ vertex_ii 1))
    )
    ;summary_reverse
      (setq sum_len (reverse sum_))
    ;
  )
  (defun TA:midpoint (ins_start_ ins_end_) ;คำนวณ midpoint จากระยะหัว-ท้าย ของเส้น
    ; (setq ins_start_ (list ins_start_x1 ins_start_y1))
    ; (setq ins_end_ (list ins_end_x1 ins_end_y1))
    (setq ins_mid_ (list 
                 (/ (+ (car ins_start_) (car ins_end_)) 2.0)
                 (/ (+ (cadr ins_start_) (cadr ins_end_)) 2.0)
               )
    )
  )
  (defun LM:PolyCentroid ( e / l )
    (foreach x (setq e (entget e))
        (if (= 10 (car x)) (setq l (cons (cdr x) l)))
    )
    (
        (lambda ( a )
            (if (not (equal 0.0 a 1e-8))
                (trans
                    (mapcar '/
                        (apply 'mapcar
                            (cons '+
                                (mapcar
                                    (function
                                        (lambda ( a b )
                                            (
                                                (lambda ( m )
                                                    (mapcar
                                                        (function
                                                            (lambda ( c d ) (* (+ c d) m))
                                                        )
                                                        a b
                                                    )
                                                )
                                                (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                                            )
                                        )
                                    )
                                    l (cons (last l) l)
                                )
                            )
                        )
                        (list a a)
                    )
                    (cdr (assoc 210 e)) 0
                )
            )
        )
        (* 3.0
            (apply '+
                (mapcar
                    (function
                        (lambda ( a b )
                            (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                        )
                    )
                    l (cons (last l) l)
                )
            )
        )
    )
  )
  (defun TA:find_center (main_border_) -
    (setq main_border_obj_ (vlax-ename->vla-object main_border_))
    (setq obj (vla-get-objectname main_border_obj_)) ;For Check objectname
    (if (= obj "AcDbPolyline") 
      (progn 
        (setq main_border_obj_boline_ (TA:ename+vla-getboundingbox main_border_obj_))
        (setq min_crood (cadr main_border_obj_boline_))
        (setq max_crood (caddr main_border_obj_boline_))
        (setq Mx (/ (+ (car min_crood) (car max_crood)) 2))
        (setq My (/ (+ (cadr min_crood) (cadr max_crood)) 2))
        (setq Mz (/ (+ (caddr min_crood) (caddr max_crood)) 2))
        (setq center_point_ (list Mx My Mz))
      )
      (alert "TA:find_center : Object is not PolyLine")
    )
  )
  (defun TA:Find_boundary_line_ (main_border_)
    ;for testing code
      ; (setq main_border_ (car (entsel)))
    ;
    ;get_data_
      (setq main_border_obj_ (vlax-ename->vla-object main_border_))
      (setq obj (vla-get-objectname main_border_obj_)) ;For Check objectname
      (if (or (= obj "AcDbPolyline") (= obj "AcDbBlockReference")) 
        (progn 
          (setq main_border_obj_boline_ (TA:ename+vla-getboundingbox main_border_obj_))
          (setq min_crood (cadr main_border_obj_boline_))
          (setq max_crood (caddr main_border_obj_boline_))
          ; (setq Mx (/ (+ (car min_crood) (car max_crood)) 2))
          ; (setq My (/ (+ (cadr min_crood) (cadr max_crood)) 2))
          ; (setq Mz (/ (+ (caddr min_crood) (caddr max_crood)) 2))
          ; (setq center_point_ (list Mx My Mz))
          (setq obj_boline_ (list 
                              (cadr main_border_obj_boline_ )
                              (caddr main_border_obj_boline_ )
                            )
          )
        )
        (alert "TA:find_center : Object is not PolyLine")
      )
    ;
  )
;
;Sorting_Selection_Set_by_Coordinate Ta Trai
  (defun TA:standard_list_croodinate_sorting (ss_post_filter_set_ sequence ) ;เรียง object ตามแนวแกน ใน selection set
    ; ;example_test_
    ;   (setq ss_post_filter_set_ _set_ )
    ; ;
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
      ;   (setq ss_post_filter_set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx  effectivename_val))
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
;convert_value_
  (defun deg-to-rad (angle)
    (* angle (/ pi 180.0))
  )
  (defun rad-to-deg (rad)
    (/ (* rad 180) pi) 
  )
  (defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )
  (defun relocate_ins_ (data_list_ x_sym x_num_ y_sym y_num_ )
    (if (= (length data_list_) 2)
      (progn
        (setq data_list_ (list 
                          (x_sym (car data_list_) x_num_)
                          (y_sym (cadr data_list_) y_num_)
                        )
        )
      )
    )
  )
;
;CLEANER_FUNC Ta Trai
  (defun TA:get_name_block_in_drawing_to_list ()
    ;get_obj_file_
      (setq multi_blocks_obj (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq block_list_ ())
    ;
    ;method_for_get_name_block_in_drawing
      ;method_1_get_name_
        (vlax-for block_ multi_blocks_obj 
          (setq block_list_ (append block_list_ (list (vla-get-name block_))))
        )
      ;
      ; ;method_2_get_name_
      ;   (setq block_i 0)
      ;   (while (< block_i (vla-get-count multi_blocks_obj))
      ;     (setq block_ (vla-item multi_blocks_obj block_i))
      ;     (setq block_list_ (append block_list_ (list (vla-get-name block_))))
      ;     (setq block_i (1+ block_i))
      ;   )
      ; ;
    ;
    ;sorting_name_block
      (setq sorted_block_list_ (acad_strlsort block_list_))
    ;
    ;preloop_and_while filtter_
      (setq sorted_block_list_i 0)
      (setq filter_sorted_block_ ())

      (while (< sorted_block_list_i (length sorted_block_list_))
        (setq filter_sorted_block_effectivename_ (nth sorted_block_list_i sorted_block_list_))
        
        (if (/= (substr filter_sorted_block_effectivename_ 1 1) "*") ; ตรวจสอบว่าชื่อเริ่มต้นด้วย *
          (progn
            (setq filter_sorted_block_ (cons filter_sorted_block_effectivename_ filter_sorted_block_))
          )
        )
        (setq sorted_block_list_i (+ sorted_block_list_i 1))
      )
    ;
    ;summary_total_block
      (setq filter_sorted_block_total (length filter_sorted_block_))
      (princ "\n                  |=====================|")
      (princ "\n                  | TOTAL BLOCK IN FILE |")
      (princ (strcat "\n                  |     = " (itoa filter_sorted_block_total) " block         |"))
      (princ "\n                  |          set        |")
      (princ "\n                  |---------------------|")
      (setq filter_sorted_block_ (acad_strlsort filter_sorted_block_))
    ;
  )
  (defun TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ (c-main-color)
    (setq all_obj_in_block (ssget "x" 
                                  (list 
                                    ; (cons 0 "insert")       ;type of object
                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                    ; (cons 2 "SSSS")       ;kind of nameblock
                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                  )
                            )
    )
    (command "chprop" all_obj_in_block "" "c" c-main-color "")
  )
  (defun TA:RE_COLOR_ALL_DIM+LEADER_OBJ (dim-main-color)
    ;dim_obj_mode
      ; (setq ss (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'objectname)) ;logic for testing
      ; (setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel))) 'objectname))  ;logic for testing
      ;user_input_ selection_dim_object
        (setq all_dim_out_block (ssget "x" 
                                        (list 
                                          (cons 0 "dimension")       ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "SSSS")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                )
        )
      ;
      ; (command "pselect" all_dim_out_block "")
      (if (/= all_dim_out_block nil)
        (progn
          (setq all_dim_out_block_total (sslength all_dim_out_block))
          (setq all_dim_out_block_iii 0)
          (while
            (< all_dim_out_block_iii all_dim_out_block_total)
            (setq all_dim_out_block_ename (ssname all_dim_out_block all_dim_out_block_iii))
            (setq all_dim_out_block_obj (vlax-ename->vla-object all_dim_out_block_ename))
            ; (vlax-dump-object all_dim_out_block_obj)
              (cond 
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbAlignedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                  (princ "\n")
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDb2LineAngularDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbRotatedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbArcDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbDiametricDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbOrdinateDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-ExtensionLineColor  all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbRadialDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
              )
            ; (vla-put-textcolor all_dim_out_block_obj dim-main-color)
            ; (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
            ; ; (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
            (setq all_dim_out_block_iii (+ all_dim_out_block_iii 1))
          )
        )
        (princ "\n")
      )
    ;
    ;leader_obj_mode
      ;user_input_ selection_leader_object    
        (setq all_leader_out_block (ssget "x" 
                                      (list 
                                        (cons 0 "leader")       ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "SSSS")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                              )
        )
      ;
        (if (/= all_leader_out_block nil)
          (progn
            (setq all_leader_out_block_total (sslength all_leader_out_block))
            (setq all_leader_out_block_iii 0)
            (while
              (< all_leader_out_block_iii all_leader_out_block_total)
              (setq all_leader_out_block_ename (ssname all_leader_out_block all_leader_out_block_iii))
              (setq all_leader_out_block_obj (vlax-ename->vla-object all_leader_out_block_ename))
              (vla-put-dimensionlinecolor all_leader_out_block_obj dim-main-color)
              (setq all_leader_out_block_iii (+ all_leader_out_block_iii 1))
            )
          )
          (princ "\n")
        )
    ;
  )
;
;delete_func_
  (defun TA:delete_block_ (selection_set_ efname_val_1 efname_val_2 efname_val_3)
    ;for testing_data_
      ; (setq selection_set_ (ssget ))
      ; (setq efname_val_ "special_block2460499.7018" )
    ;
    ;get_data_
      
    ;
    ;prelooop_and_while
      (setq selection_set_i 0)
      (while  (< selection_set_i (sslength selection_set_))
        (setq selection_set_ename_ (ssname selection_set_ selection_set_i))
        (setq selection_set_ename_obj (vlax-ename->vla-object selection_set_ename_))
        
          ;delete_block_process
            (if
              (and 
                (/= (LM:effectivename selection_set_ename_obj) efname_val_1)
                (/= (LM:effectivename selection_set_ename_obj) efname_val_2)
                (/= (LM:effectivename selection_set_ename_obj) efname_val_3)
              )
              (progn
                (vla-delete selection_set_ename_obj)
              )
              (princ "/nnext")
            )
          ;
        
        (setq selection_set_i (+ selection_set_i 1))
      )
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
;block_editor_FUNC
  (defun TA:set-block-explodable (block-name allow-exploding)
    ;example_using
      ;;allow-exploding = :vlax-true or :vlax-false
      ; (set-block-explodable "A$C5b7816c3" :vlax-true)
      ; (set-block-explodable "A$C5b7816c3" :vlax-false)
    ;
    ;for testing code
      ; (setq block-name "001 - DYNAMIC LV" )
      ; (setq allow-exploding :vlax-false )
      ; (setq allow-exploding :vlax-true )
    ;
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq block-def (vla-Item (vla-get-Blocks doc) block-name))
    (vla-get-Explodable block-def )
    (if (/= block-def nil)  
      (progn
        (vla-put-Explodable block-def allow-exploding)
      )
      (princ (strcat "\nBlock " block-name " not found."))
    )
  )
  (defun TA:set-block-blockscaling (block-name parameter)
  ;example_using
    ;;parameter = acAny or acUniform
    ; (set-block-explodable "A$C5b7816c3" acAny)
    ; (set-block-explodable "A$C5b7816c3" acUniform)
  ;
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq block-def (vla-Item (vla-get-Blocks doc) block-name))
  (if block-def
    (vla-put-blockscaling block-def parameter)
    (princ (strcat "\nBlock " block-name " not found."))
  )
  )
  (defun c:DYN_ADD_VISIBILITY_advs_ ()
    (setq input_ins_ (getpoint ))
    (setq name_vis_ "view")
    (command "BPARAMETER" "v" "L" name_vis_ input_ins_ 1)
    (command "-Bvstate" "s" "VisibilityState0")
    ;preloop_and_while_
      (setq visibility_list_ (list
                              "1"
                              "2"
                              "3"
                              "4"
                              "5"
                            )
      )
      (setq visibility_list_i 0)

      (while (< visibility_list_i (length visibility_list_))
        (setq visibility_list_SEQ (nth visibility_list_i visibility_list_))
        (command "-Bvstate" "n" visibility_list_SEQ "c" )
        (setq visibility_list_i (+ visibility_list_i 1))
      )
    ;
    ;setting first visibility_list
      ;-
    ;
  )
;
;test_command
  (defun C:Dumpit ( / ent) 
    (while (setq ent (entsel)) 
      (vlax-Dump-Object 
        (vlax-Ename->Vla-Object (car ent)) 
      ) 
    ) 
    (princ) 
  )
  (defun c:t1_ins_vla-getboundingbox ()
    (setq ss (vlax-ename->vla-object (car (entsel))) )
    (vla-getboundingbox ss 'ins_min 'ins_max)
                (setq ins_min (vlax-safearray->list ins_min))
                (setq ins_max (vlax-safearray->list ins_max))

    (command "point" ins_min)
    (command "point" ins_max)
  )
  (defun c:t2_find_objname ()
    (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (car (entsel)))))
  )
;
;Common_commmand 
  (defun c:ROUND_INSERTIONPOINT_reins_ ()
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
    ;get_efname_
      (setq blk_name_ (LM:effectivename (vlax-ename->vla-object (car (entsel)))))
    ;
    ;ssget_filtter
      (setq ss_prop_filter_ (TA:Prop_Filter_ss_set_ ss_pre_filter_set_xx "effectivename" blk_name_))
    ;
    ;preloop_and_while_rework_inspoint
      (setq ss_prop_filter_i 0)
      (while  (< ss_prop_filter_i (sslength ss_prop_filter_))
        (setq ss_prop_filter_ename_ (ssname ss_prop_filter_ ss_prop_filter_i))
        (setq ss_prop_filter_obj_ (vlax-ename->vla-object ss_prop_filter_ename_))
        (setq ss_prop_filter_obj_ins_ (TA:ename+vla-get-insertionpoint ss_prop_filter_obj_))
        
        ;re_location_inspt_
          ;get_data
            (setq new_input_ins_ (list
                                      (setq ss_prop_filter_obj_ins_x (LM:round (car (nth 1 ss_prop_filter_obj_ins_))))
                                      (setq ss_prop_filter_obj_ins_y (LM:round (cadr (nth 1 ss_prop_filter_obj_ins_))))
                                      (setq ss_prop_filter_obj_ins_z (LM:round (caddr (nth 1 ss_prop_filter_obj_ins_))))                             
                                    )
            )
          ;
          (vla-put-insertionpoint ss_prop_filter_obj_ (vlax-3d-point new_input_ins_))
        ;
        (setq ss_prop_filter_i (+ ss_prop_filter_i 1))
      )
    ;
  )
  (defun c:BOL ()
    ;get_osmode_process
      (setq getold_osmode_ (getvar "osmode"))
      (setq zero_osmode_ (setvar "osmode" 0))
    ;
    ;user_input
      (setq ref_pline_or_block_ (car (entsel "specify PLINE OR BLOCK OBJECT" )))
    ;
    ;MAIN_IDEA_COMMAND_
      (setq lower_upper_inspoint_ (TA:Find_boundary_line_ ref_pline_or_block_))
      (command "rectangle" (car lower_upper_inspoint_) (cadr lower_upper_inspoint_))
    ;  
    ;Change_Color_process
      (setq new_obj_ (vlax-ename->vla-object (entlast)))
      (setq new_obj_input_color_ (vla-put-color new_obj_ 250))
    ;
    ;return_osmode_process_
      (setq return_osmode_ (setvar "osmode" getold_osmode_))
    ;
  )
  (defun c:Multi_wipeout_mlwe ()
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "lwpolyline") ;type of object
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
                                          (cons 0 "lwpolyline") ;type of object
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
    ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_ename_close_line (vla-put-closed ss_pre_filter_set_xx_obj_ :vlax-true))
      
        (command "wipeout" "p"  ss_pre_filter_set_xx_ename_ "n" )
        (setq wipeput_ename_ (entlast))
        (setq wipeput_ename_obj_ (vlax-ename->vla-object wipeput_ename_))
        (vla-put-layer wipeput_ename_obj_ "000 - WIPEOUT")
        (vla-put-color wipeput_ename_obj_ 256)
        (command "_draworder" wipeput_ename_ "" "b" )
        
    
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
    ;
    
    
    
    
    
    
    
  )
  (defun c:make_detail_box_mkbx_ ()
    ;get_point_data_
      (setq get_pt_ (getpoint ))
      ; (setvar "orthomode" 1)
    ;
    ;get_honrizontal_data_
      (setq dist_hon (getdist get_pt_ ) )
      (setq hon_pt_ (polar get_pt_ 0.0 dist_hon))
      (setq hon_pt_- (polar get_pt_ (deg-to-rad 180.00) dist_hon))
    ;
    ;get_vertical_data_
      (setq dist_ver (getdist get_pt_ ) )
      (setq ver_pt_ (polar get_pt_ (deg-to-rad 90.00) dist_ver))
      (setq ver_pt_- (polar get_pt_ (deg-to-rad 270.00) dist_ver))
    ;
    ;sorting_max_legth_
      (setq length_list_rec (list
                              dist_hon
                              dist_ver
                            )
      )
      (setq min-length (car (vl-sort  length_list_rec '<)))
    ;
    ;get_conner_
      (setq top_conner_ (list
                          (car hon_pt_)
                          (cadr ver_pt_)
                        )
      )
      (setq bot_conner_ (list
                          (car hon_pt_-)
                          (cadr ver_pt_-)
                        )
      )
    ;
  
    ;use_input_for_specify_fllet_rec_
      (setq fllet_rec_val "not pass" )
      (while (= fllet_rec_val "not pass")
        
        (setq fllet_rec_ (cond ( (getint (strcat "\nspecify fllet_rec_<" (rtos (setq fllet_rec_ (cond (fllet_rec_) (1.0) ) ) ) "> : " ) ) ) (fllet_rec_) ) )
        (if
          (< (LM:round (* min-length 0.9)) fllet_rec_)
          (progn
            (setq fllet_rec_val "not pass" )
            (alert "fllet rectangle more than min-length ractangle ")
          )
          (setq fllet_rec_val "pass" )
        )
      )
    ;
    ; ;make_new_object_
    ;   (command "point" get_pt_)
    ;   (command "point" top_conner_)
    ;   (command "point" bot_conner_)
      (command "rectangle" bot_conner_ top_conner_  )
      (command "fillet" "p" "r" fllet_rec_ "L" )
    ;
    ;put_prop_data_
      (setq entlast_ename_ (entlast))
      (vla-get-linetype (setq entlast_obj_ (vlax-ename->vla-object entlast_ename_)))
      (vla-put-linetype (setq entlast_obj_ (vlax-ename->vla-object entlast_ename_)) "byblock")
    ;

    
  )
  (defun c:save_dynamic_scale_scdyn_ ()
    ;user_input_
      (setq get_dynblock_ename_ (car (entsel "specify Object")))
      (setq dynamic_scale_ (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq dynamic_scale_ (cond (dynamic_scale_) (1.0) ) ) ) "> : " ) ) ) (dynamic_scale_) ) )
    ;
  
    (if (and ;condition rule
          ; (/= get_effectivename_ nil)
          (= (vla-get-isdynamicblock (vlax-ename->vla-object get_dynblock_ename_)) :vlax-true)
          ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
        )
      (progn ;add data
        ; (alert "\nplease select block object")
        ; (setq get_effectivename_ nil)
        ; (setq get_effectivename_obj_val_ nil)
        (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_dynblock_ename_)))
        (setq REF_get_att_list_ (LM:getdynprops (vlax-ename->vla-object get_dynblock_ename_)))
        (setq REF_get_att_list_total (length REF_get_att_list_))
        ;add more
      )
      (setq get_effectivename_ nil)
    )
    ;rescale_
      (setq basescale_ (vlax-3d-point dynamic_scale_ dynamic_scale_ dynamic_scale_))
      (vla-put-xscalefactor (vlax-ename->vla-object get_dynblock_ename_) dynamic_scale_ )
    ;
    ;preloop_and_while
      (setq REF_get_att_list_i 0)
      (while (< REF_get_att_list_i (length REF_get_att_list_))
        ;get_dynamic_data
          (setq REF_get_att_list_dynamicset_ (nth  REF_get_att_list_i REF_get_att_list_))
          (setq REF_get_att_list_dynamicset_name_ (nth 0 REF_get_att_list_dynamicset_) )
          (setq REF_get_att_list_dynamicset_val_ (cdr REF_get_att_list_dynamicset_))
        ;
        ;get_vis_data_
          (setq vis_name_ (LM:getvisibilityparametername (vlax-ename->vla-object get_dynblock_ename_) ))
          (setq vis_stage_ (LM:getvisibilitystate (vlax-ename->vla-object get_dynblock_ename_) ))
          (LM:SetVisibilityState (vlax-ename->vla-object get_dynblock_ename_) vis_stage_ )
        ;
        ;get_filp_data_
          (setq Flip_data_ (LM:toggleflipstate (vlax-ename->vla-object get_dynblock_ename_)))
        ;
        
        
        (if (/= REF_get_att_list_dynamicset_name_ "Origin")
          (progn
            (LM:setdynpropvalue (vlax-ename->vla-object get_dynblock_ename_) REF_get_att_list_dynamicset_name_ REF_get_att_list_dynamicset_val_ )
          )
          (princ "next ")
        )
        
        (setq REF_get_att_list_i (+ REF_get_att_list_i 1))
      )
    ;
  )
  (defun c:STAMP_START_POINT_STP_ ()
    
    ;Stamp_ dynamic block "000SYM-START_POINT_" /w RECTANGLE_BLOCK
    (setvar "osmode" 0)
    (setq main_rec_ename_ (car (entsel "specify Object")))
    (setq rec_point_ (TA:find_center main_rec_ename_ ))
    (command "insert" "000SYM-START_POINT_" rec_point_ 0.1 0 )
    (setvar "osmode" 1215)
  )
  (defun c:hatch_obj_HHB ()
    (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
    (cond
      ( ;first process
        (or
          
          (= obj_name_ "AcDbPolyline")
        )
        (progn
          (= (vla-get-closed (setq hatch_ename_obj_ (vlax-ename->vla-object hatch_ename_))) :vlax-true)
          (setq 1st_process "AcDbPolyline")
        )
      )
      (;first process
        (or
          (= obj_name_ "AcDbBlockReference")
        )
        (progn
          (setq 1st_process "AcDbBlockReference")
          
        )
      )
    )
    
    
    (if 
      (and 
        (= hatch_ename_getang_ nil)
        (= hatch_ename_getsc_ nil)
      )
      (progn
        (setq  hatch_ename_getang_ 0)
        (setq  hatch_ename_getsc_ 1)
        
      )
    )
    ;preseting_data_
      (setq hatch_Layer_ "000 - H A T C H")
      (setq hatch_patt_ "ANSI32")
      (setq hatch_color_ 8)
      (setq hatch_background_ 250)
      
    ;
    (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
    (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
    (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))

    
    (command ".-hatch" "s"  hatch_ename_  
              "" 
              "c" hatch_color_ hatch_background_
              "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
              "L" hatch_Layer_ 
              ""  
    )
    
    
  )
  (defun c:hatch_obj_HH1 ()
    (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
    (cond
      ( ;first process
        (or
          
          (= obj_name_ "AcDbPolyline")
        )
        (progn
          (= (vla-get-closed (setq hatch_ename_obj_ (vlax-ename->vla-object hatch_ename_))) :vlax-true)
          (setq 1st_process "AcDbPolyline")
        )
      )
      (;first process
        (or
          (= obj_name_ "AcDbBlockReference")
        )
        (progn
          (setq 1st_process "AcDbBlockReference")
          
        )
      )
    )
    
    
    (if 
      (and 
        (= hatch_ename_getang_ nil)
        (= hatch_ename_getsc_ nil)
      )
      (progn
        (setq  hatch_ename_getang_ 0)
        (setq  hatch_ename_getsc_ 1)
        
      )
    )
    ;preseting_data_
      (setq hatch_Layer_ "000 - H A T C H")
      (setq hatch_patt_ "DOTS")
      (setq hatch_color_ 8)
      (setq hatch_background_ 250)
      
    ;
    (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
    (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
    (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))

    
    (command ".-hatch" "s"  hatch_ename_  
              "" 
              "c" hatch_color_ hatch_background_
              "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
              "L" hatch_Layer_ 
              ""  
    )
    
    
  )
  (defun c:add_breakline_to_rectangle_ADBK ()
    ;user_input_
      (setq rec_ename_ (car (entsel "specify Object")))
      (setq dynamic_scale_ (cond ( (getreal (strcat "\nspecify_dynamic_scale_<" (rtos (setq dynamic_scale_ (cond (dynamic_scale_) (1.0) ) ) ) "> : " ) ) ) (dynamic_scale_) ) )
    ;
    ;get_data_
      (setq rec_obj_ (vlax-ename->vla-object rec_ename_))
      (setq rec_obj_objname_  (vla-get-objectname rec_obj_))
    
      (setq vertex_list (TA:Get_Pline_vertext_angle_case1 rec_ename_))
      (setq vertex_list_ (remove_member_list_ 2 vertex_list))
    ;
    ;preloop_and_while insertion_breakline_dynblock
      (setq vertex_list_i 0)
      (setq vertex_list_ii 1)
      (while (< vertex_list_i (length vertex_list_))
        (cond ;insert_block_case_
          (;_case_1
            (and
                (/= vertex_list_i (- (length vertex_list_ ) 1))
            )
            (progn
                (setq vertex_list_ename_set1 (nth  vertex_list_i vertex_list_))
                (setq vertex_list_ename_set2 (nth  vertex_list_ii vertex_list_))
                
                (setq vertex_list_ename_midpoint_ (TA:midpoint (car vertex_list_ename_set1) (car vertex_list_ename_set2)))
                (setq vertex_list_ename_distance_set1 (distance (car vertex_list_ename_set1)  vertex_list_ename_midpoint_ ))
                (setq vertex_list_ename_distance_set2 (distance (car vertex_list_ename_set2)  vertex_list_ename_midpoint_))
                
                ;main_idea_code
                  (command "insert" "000SYM-BREAK_LINE_DYNBLOCK_" vertex_list_ename_midpoint_ dynamic_scale_ (cadr vertex_list_ename_set1) )
                  (setq new_dynamic_block_ (entlast))
                  (setq new_dynamic_block_obj_ (vlax-ename->vla-object new_dynamic_block_))
                  (LM:setdynpropvalue new_dynamic_block_obj_ "right_width" vertex_list_ename_distance_set1)
                  (LM:setdynpropvalue new_dynamic_block_obj_ "left_width" vertex_list_ename_distance_set2)
                  (cond
                    (;_case_1
                      (and (>= (cadr vertex_list_ename_set1) 90 ) (<= (cadr vertex_list_ename_set1) 135 ))
                      (progn
                        (LM:setdynpropvalue new_dynamic_block_obj_ "flip state1"  1)
                        (princ "_case_1")
                      )
                    )
                    (;_case_2
                      (or
                          (and (>= (cadr vertex_list_ename_set1) 135 ) (<= (cadr vertex_list_ename_set1) 180 ))
                      )
                      (progn
                        (LM:setdynpropvalue new_dynamic_block_obj_ "flip state1"  0)
                        (princ "_case_2")
                      )
                    )
                    (;_case_3
                      (or
                          (and (>= (cadr vertex_list_ename_set1) 180 ) (<= (cadr vertex_list_ename_set1) 360 ))
                      )
                      (progn
                        (LM:setdynpropvalue new_dynamic_block_obj_ "flip state1"  1)
                        (princ "_case_3")
                      )
                    )
                  )
                ;
                (princ "_case_1")
            )
          )
          (;_case_2
            (and
                (= vertex_list_i (- (length vertex_list_ ) 1))
            )
            (progn
                (setq vertex_list_ename_set1 (nth  vertex_list_i vertex_list_))
                (setq vertex_list_ename_set2 (nth  0 vertex_list_))
                
                (setq vertex_list_ename_midpoint_ (TA:midpoint (car vertex_list_ename_set1) (car vertex_list_ename_set2)))
                (setq vertex_list_ename_distance_set1 (distance (car vertex_list_ename_set1)  vertex_list_ename_midpoint_ ))
                (setq vertex_list_ename_distance_set2 (distance (car vertex_list_ename_set2)  vertex_list_ename_midpoint_))
              
                ;main_idea_code
                  (command "insert" "000SYM-BREAK_LINE_DYNBLOCK_" vertex_list_ename_midpoint_ dynamic_scale_ (rad-to-deg (angle (car vertex_list_ename_set1) (car vertex_list_ename_set2) )) )
                  (setq new_dynamic_block_ (entlast))
                  (setq new_dynamic_block_obj_ (vlax-ename->vla-object new_dynamic_block_))
                  (LM:setdynpropvalue new_dynamic_block_obj_ "right_width" vertex_list_ename_distance_set1)
                  (LM:setdynpropvalue new_dynamic_block_obj_ "left_width" vertex_list_ename_distance_set2)
                  (LM:setdynpropvalue new_dynamic_block_obj_ "flip state1"  1)
                  
                ;
              (princ "_case_2")
            )
          )
        )
        
        (setq vertex_list_i (+ vertex_list_i 1))
        (setq vertex_list_ii (+ vertex_list_ii 1))
      )
    ;
    
    
    
  )
;
;old_sub_function ;do not use w/ another command
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
;Sub_FUNC_changing_prop
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
;ATT_Common_command
  (defun c:z660A_add_sequence_att_ ()
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a Attribute Block name.
    
    ;The operation of the command will read the block's attribute values and generate a set of text. n 
    ;

    ;fillter_EF_name_object_
      (setq att_ename_ nil)
      (setq att_obj_ nil)
      (while (= att_ename_ nil)
        ;user_input_data_  
          (setq att_ename_ (car (entsel "specify Object")))
        ;
        ;fillter_object_type_
          (if 
            (and
              (if (/= att_ename_ nil) (progn (setq att_obj_ (vlax-ename->vla-object att_ename_)) ))
              (= (vla-get-objectname att_obj_) "AcDbBlockReference")
              (= (vla-get-hasattributes att_obj_) :vlax-true )
            )
            (progn
              (setq att_tag_list_ (LM:vl-getattributevalues att_obj_))
            )
            (setq att_ename_ nil)
          )
          (if 
            (or
              (= att_ename_ nil)
              (/= (vla-get-objectname att_obj_) "AcDbBlockReference")
              (if (= (vla-get-objectname att_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes att_obj_) :vlax-true ) ))
            )
            (progn
              (alert "Please selection attribute object")
            )
          )
        ;
      )
    ;
    ;preloop_and_while_for_make_sequnce_attribute
      (setq att_tag_list_i 0)
      (while (< att_tag_list_i (length att_tag_list_))
        (setq att_tag_list_tagname_ (car (nth  att_tag_list_i att_tag_list_)))
        
        (LM:vl-setattributevalue att_obj_ att_tag_list_tagname_ (+ att_tag_list_i 1) )
    
        (setq att_tag_list_i (+ att_tag_list_i 1))
      )
    ;
    
  )
;
;match_ATT_Common_command
  (defun c:z661A_matching_attribute_with_same_block-name ()
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can matching attribute name with same block-name only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
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
                (setq REF_get_att_list_ (LM:vl-getattributevalues (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
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
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;filter_and_sorting_selection
      (setq _set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx get_effectivename_obj_val_))
      (setq TAR_fileter_set_ (TA:standard_list_croodinate_sorting _set_ "X"))
    ;
    (if (/= TAR_fileter_set_ nil)
      (progn
        ;preloop_and_while input_att_val_to TAR_fileter_set_
          (setq TAR_fileter_set_i 0)
          (while (< TAR_fileter_set_i (length TAR_fileter_set_))
            ; (setq REF_get_att_list_data_tag (car (nth REF_get_att_list_i REF_get_att_list_ )))
            (setq TAR_fileter_set_obj  (vlax-ename->vla-object (car (nth TAR_fileter_set_i TAR_fileter_set_ ))))
              (setq ref_att_tag_i 0)
              (while (< ref_att_tag_i (length (LM:vl-getattributevalue-tag-TA-Modifies TAR_fileter_set_obj)))
                ;get_att_data_from_ REF_get_att_list_
                  (setq REF_get_att_list_tagname (car (nth ref_att_tag_i REF_get_att_list_)))
                  (setq REF_get_att_list_val (cdr (nth ref_att_tag_i REF_get_att_list_)))
                ;
                ;set_att_data_to_ TAR_fileter_set_
                  (LM:vl-setattributevalue TAR_fileter_set_obj REF_get_att_list_tagname REF_get_att_list_val)
                ;
                (setq ref_att_tag_i (+ ref_att_tag_i 1))
              )
            (setq TAR_fileter_set_i (+ TAR_fileter_set_i 1))
          )
        ;
      )
      (alert "Attribute Block do not match\nPlease try agian")
      
    )
  )
  (defun c:z661B_matching_attribute_with_multi_block_name ()
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can matching attribute name with multi block-name only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;user_input_exspect_att_tag_string selection_set
        (prompt "\n Select exspect_att_ objects: ")
        (setq exspect_mode_ (cond 
                              ((getint 
                                (strcat "\nUser input \nfor clearing \nattribute exspect value \n\n 0 = clear_exspect_value \n 1 = add_exspect_value \n\n<" (rtos (setq exspect_mode_ (cond (exspect_mode_) (1.0) ) ) ) "> : " )
                              )
                              )
                              (exspect_mode_) 
                            )
        )
        (setq exspect_att_ nil)
        (if (= exspect_mode_ 1)
          (progn
            (prompt "\n Select exspect_att_ objects: ")
            (setq exspect_att_ (ssget 
                                (list 
                                  (cons 0 "attdef") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                              )
            )
            (if (/= exspect_att_ nil)
              (progn
                ;preloop_and_while
                  (setq sum_ ())
                  (setq exspect_att_i 0)
                  (while (< exspect_att_i (sslength exspect_att_))
                    (setq exspect_att_ename_ (ssname exspect_att_ exspect_att_i))
                    (setq exspect_att_obj_ (vlax-ename->vla-object exspect_att_ename_))
                    (setq exspect_att_name_tag_ (vla-get-tagstring exspect_att_obj_ ))

                      ;making-list_att_tag_string
                        (setq sum_ (cons exspect_att_name_tag_ sum_))

                      ;
                
                    (setq exspect_att_i (+ exspect_att_i 1))
                  )
                  (setq expect_taglist_ sum_)
                ;
              )
              (setq expect_taglist_ (list))
            )
          )
        )
    ;
    ;selection_set
      (prompt "\n Select attribute block objects: ")
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
          ; (setq ss_pre_filter_set_xx_ (ssget 
          ;                               (list 
          ;                                 (cons 0 "INSERT") ;type of object
          ;                                 ; (cons 8 "000 - GRID")   ;kind of layer
          ;                                 ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
          ;                                 ; (cons 62 1)           ;kind of color call sign with color code index
          ;                               )
          ;                             )
          ; )
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Prototype Attribute Block\n"))))
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
                (setq REF_get_att_list_ (LM:vl-getattributevalues (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
    ;selection_set
      (prompt "\n Select attribute block objects: ")
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
    ;preloop_and_while 
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq ss_pre_filter_set_xx_obj_att_list_ (LM:vl-getattributevalues ss_pre_filter_set_xx_obj_))
        
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_obj_att_list_i 0)
            (while (< ss_pre_filter_set_xx_obj_att_list_i (length ss_pre_filter_set_xx_obj_att_list_))
              (setq ss_pre_filter_set_xx_obj_att_list_attname_ (car (nth ss_pre_filter_set_xx_obj_att_list_i ss_pre_filter_set_xx_obj_att_list_)))
              (setq ss_pre_filter_set_xx_obj_att_list_attdata_ (cdr (nth ss_pre_filter_set_xx_obj_att_list_i ss_pre_filter_set_xx_obj_att_list_)))
              
                ;preloop_and_while
                  (setq ref_get_att_list_i 0)
                  (while (< ref_get_att_list_i (length ref_get_att_list_))
                    (setq ref_get_att_list_attname_ (car (nth  ref_get_att_list_i ref_get_att_list_)))
                    (setq ref_get_att_list_attdata_ (cdr (nth  ref_get_att_list_i ref_get_att_list_)))
                    
                    (if (and (/= get_effectivename_ ss_pre_filter_set_xx_obj_att_list_attname_ )
                            (= ss_pre_filter_set_xx_obj_att_list_attname_ ref_get_att_list_attname_ )
                            ;exspect case
                            ;  (/= ref_get_att_list_attname_ "BLANK_SIZE_VALUE")
                            (/= (TA:FIND_DUPLICATE_LIST ref_get_att_list_attname_ expect_taglist_ ) "Y")
                            ;
                            
                        )
                      (progn
                        (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ ref_get_att_list_attname_ ref_get_att_list_attdata_)
                      )
                    )
                
                    (setq ref_get_att_list_i (+ ref_get_att_list_i 1))
                  )
                ;
                
              
              (setq ss_pre_filter_set_xx_obj_att_list_i (+ ss_pre_filter_set_xx_obj_att_list_i 1))
            )
          ;
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
;
;match_DYN_Common_command
  (defun c:z662A_matching_dynamic_with_same_block-name ()
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can matching attribute name with same block-name only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Attribute Block\n"))))
            (if (and ;condition rule
                  (/= get_effectivename_ nil)
                  (= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                  ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                (setq REF_get_dyn_list_ (LM:getdynprops (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
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
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;filter_and_sorting_selection
      (setq _set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx get_effectivename_obj_val_))
      (setq TAR_fileter_set_ (TA:standard_list_croodinate_sorting _set_ "X"))
    ;
    (if (/= TAR_fileter_set_ nil)
      (progn
        ;preloop_and_while input_dyn_val_to TAR_fileter_set_
          (setq TAR_fileter_set_i 0)
          (while (< TAR_fileter_set_i (length TAR_fileter_set_))
            ; (setq REF_get_dyn_list_data_tag (car (nth REF_get_dyn_list_i REF_get_dyn_list_ )))
            (setq TAR_fileter_set_obj  (vlax-ename->vla-object (car (nth TAR_fileter_set_i TAR_fileter_set_ ))))
              (setq ref_dyn_tag_i 0)
              (while (< ref_dyn_tag_i (length (LM:getdynprops TAR_fileter_set_obj)))
                ;get_dyn_data_from_ REF_get_dyn_list_
                  (setq REF_get_dyn_list_tagname (car (nth ref_dyn_tag_i REF_get_dyn_list_)))
                  (setq REF_get_dyn_list_val (cdr (nth ref_dyn_tag_i REF_get_dyn_list_)))
                ;
                ;set_dyn_data_to_ TAR_fileter_set_
                (if (or
                      (= (substr REF_get_dyn_list_tagname 1 6) "Origin")
                      (= (substr REF_get_dyn_list_tagname 1 8) "Position ")
                      
                    ) 
                  (progn
                    (princ "/n")
                  )
                  (LM:setdynpropvalue TAR_fileter_set_obj REF_get_dyn_list_tagname REF_get_dyn_list_val)
                )
                ;
                (setq ref_dyn_tag_i (+ ref_dyn_tag_i 1))
              )
            (setq TAR_fileter_set_i (+ TAR_fileter_set_i 1))
          )
        ;
      )
      (alert "Attribute Block do not match\nPlease try agian")
      
    )
  )
  (defun c:z662B_matching_dynamic_with_multi_block_name ()
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can matching attribute name with multi block-name only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;user_input_exspect_dyn_tag_string selection_set
        (prompt "\n Select exspect_dyn_ objects: ")
        (setq exspect_mode_ (cond 
                              ((getint 
                                (strcat "\nUser input \nfor clearing \nattribute exspect value \n\n 0 = clear_exspect_value \n 1 = add_exspect_value \n\n<" (rtos (setq exspect_mode_ (cond (exspect_mode_) (1.0) ) ) ) "> : " )
                              )
                              )
                              (exspect_mode_) 
                            )
        )
        (setq exspect_dyn_ nil)
        (if (= exspect_mode_ 1)
          (progn
            (prompt "\n Select exspect_dyn_ objects: ")
            (setq exspect_dyn_ (ssget 
                                (list 
                                  (cons 0 "attdef") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                              )
            )
            (if (/= exspect_dyn_ nil)
              (progn
                ;preloop_and_while
                  (setq sum_ ())
                  (setq exspect_dyn_i 0)
                  (while (< exspect_dyn_i (sslength exspect_dyn_))
                    (setq exspect_dyn_ename_ (ssname exspect_dyn_ exspect_dyn_i))
                    (setq exspect_dyn_obj_ (vlax-ename->vla-object exspect_dyn_ename_))
                    (setq exspect_dyn_name_tag_ (vla-get-tagstring exspect_dyn_obj_ ))

                      ;making-list_dyn_tag_string
                        (setq sum_ (cons exspect_dyn_name_tag_ sum_))

                      ;
                
                    (setq exspect_dyn_i (+ exspect_dyn_i 1))
                  )
                  (setq expect_taglist_ sum_)
                ;
              )
              (setq expect_taglist_ (list))
            )
          )
        )
    ;
    ;selection_set
      (prompt "\n Select attribute block objects: ")
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
          ; (setq ss_pre_filter_set_xx_ (ssget 
          ;                               (list 
          ;                                 (cons 0 "INSERT") ;type of object
          ;                                 ; (cons 8 "000 - GRID")   ;kind of layer
          ;                                 ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
          ;                                 ; (cons 62 1)           ;kind of color call sign with color code index
          ;                               )
          ;                             )
          ; )
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Prototype Attribute Block\n"))))
            (if (and ;condition rule
                  (/= get_effectivename_ nil)
                  (= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                  ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                (setq get_effectivename_obj_xsc_ (vla-get-xeffectivescalefactor (vlax-ename->vla-object get_effectivename_)))
                (setq REF_get_dyn_list_ (LM:getdynprops (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
    ;selection_set
      (prompt "\n Select attribute block objects: ")
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
    ;preloop_and_while 
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq put_uniform_scale_ (vla-put-xeffectivescalefactor ss_pre_filter_set_xx_obj_ get_effectivename_obj_xsc_ ))
        (setq ss_pre_filter_set_xx_obj_dyn_list_ (LM:getdynprops ss_pre_filter_set_xx_obj_))
        
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_obj_dyn_list_i 0)
            (while (< ss_pre_filter_set_xx_obj_dyn_list_i (length ss_pre_filter_set_xx_obj_dyn_list_))
              (setq ss_pre_filter_set_xx_obj_dyn_list_attname_ (car (nth ss_pre_filter_set_xx_obj_dyn_list_i ss_pre_filter_set_xx_obj_dyn_list_)))
              (setq ss_pre_filter_set_xx_obj_dyn_list_attdata_ (cdr (nth ss_pre_filter_set_xx_obj_dyn_list_i ss_pre_filter_set_xx_obj_dyn_list_)))
              
                ;preloop_and_while
                  (setq ref_get_dyn_list_i 0)
                  (while (< ref_get_dyn_list_i (length ref_get_dyn_list_))
                    (setq ref_get_dyn_list_attname_ (car (nth  ref_get_dyn_list_i ref_get_dyn_list_)))
                    (setq ref_get_dyn_list_attdata_ (cdr (nth  ref_get_dyn_list_i ref_get_dyn_list_)))
                    
                    (if (and (/= get_effectivename_ ss_pre_filter_set_xx_obj_dyn_list_attname_ )
                            (= ss_pre_filter_set_xx_obj_dyn_list_attname_ ref_get_dyn_list_attname_ )
                            ;exspect case
                            ;  (/= ref_get_dyn_list_attname_ "BLANK_SIZE_VALUE")
                            (/= (TA:FIND_DUPLICATE_LIST ref_get_dyn_list_attname_ expect_taglist_ ) "Y")  
                             
                            ;
                            
                        )
                      (progn
                        (if (or
                              (= (substr ref_get_dyn_list_attname_ 1 6) "Origin")
                              (= (substr ref_get_dyn_list_attname_ 1 8) "Position")
                              
                            ) 
                          (progn
                            (princ "/n")
                          )
                          (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ ss_pre_filter_set_xx_obj_dyn_list_attname_ ref_get_dyn_list_attdata_)
                        )
                      )
                    )
                
                    (setq ref_get_dyn_list_i (+ ref_get_dyn_list_i 1))
                  )
                ;
                
              
              (setq ss_pre_filter_set_xx_obj_dyn_list_i (+ ss_pre_filter_set_xx_obj_dyn_list_i 1))
            )
          ;
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
;
;ATT_tittle_command

  (defun c:z663A_excel_to_cad_add_floder_path_and_project_name ()
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can export_text_data_from_excel to attribute name with same block-name "LNAD - A4 TITLE BLOCK PART REV01" only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
      (setq name_job_1 (getstring "specify name_job_1"))
      (setq name_job_2 (getstring "specify name_job_2"))
      
      (setq NAME_PROJECT_1 (getstring "specify NAME_PROJECT_1"))
      (setq NAME_PROJECT_2 (getstring "specify NAME_PROJECT_2"))
      (setq NAME_PROJECT_3 (getstring "specify NAME_PROJECT_3"))
    ;
    ;sueper_process
      (setq SUPERLINK (getstring "specify SUPERLINK"))
        (setq SUPERLINK1 (LM:str->lst SUPERLINK "\\")  )
        (setq SUPERLINK2 (LM:lst->str SUPERLINK1 "\\\\")  )
    ;

    ; (setq NAMEFILE (getstring "specify NAMEFILE")) ให้ตัวแปรนี้ใช้คู่กับ superlink
    
      
    
    
    
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
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Attribute Block\n"))))
            (if (and ;condition rule
                  (/= get_effectivename_ nil)
                  (= (vla-get-hasattributes (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                  (= (LM:Effectivename (vlax-ename->vla-object get_effectivename_)) "LNAD - A4 TITLE BLOCK PART REV01")
                  ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                (setq REF_get_att_list_ (LM:vl-getattributevalues (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
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
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;filter_and_sorting_selection
      (setq _set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx get_effectivename_obj_val_))
      (setq TAR_filter_set_ (TA:standard_list_croodinate_sorting _set_ "X"))
    ;
    ;preloop_and_while
      (setq TAR_filter_set_i 0)
      (while (< TAR_filter_set_i (length TAR_filter_set_))
        (setq TAR_filter_set_ename_ (car (nth  TAR_filter_set_i TAR_filter_set_)))
        (setq TAR_filter_set_obj_ (vlax-ename->vla-object TAR_filter_set_ename_))

        ;Edit_ATT_PROCESS
          (setq TAR_filter_set_obj_att_list_ (LM:vl-getattributevalues TAR_filter_set_obj_ ))
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "name_job_1" name_job_1)
            (LM:vl-setattributevalue TAR_filter_set_obj_  "name_job_2" name_job_2)
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_JOB_1" name_job_1)
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_JOB_2" name_job_2)
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_1" NAME_PROJECT_1 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_2" NAME_PROJECT_2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_3" NAME_PROJECT_3 )
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_1" NAME_PROJECT_1 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_2" NAME_PROJECT_2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_3" NAME_PROJECT_3 )
            
            (LM:vl-setattributevalue TAR_filter_set_obj_  "SUPERLINK" SUPERLINK2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAMEFILE" SUPERLINK2 ) ;ให้ตัวแปรนี้ใช้คู่กับ superlink
        ;
        
        (setq TAR_filter_set_i (+ TAR_filter_set_i 1))
      )
    ;
    
  )
  (defun c:z663B_add_atttribute_project_heading_to_tittle_block_ ()
    ;user_input_
      (setq tittle_block_ename_ (car (entsel "specify Object")))
    ;
    ;get_data_tittle_block
      (setq tittle_block_obj_ (vlax-ename->vla-object tittle_block_ename_))
      (setq tittle_block_obj_border_ (TA:ename+vla-getboundingbox tittle_block_obj_))
      (setq ss_selection_set_ (ssget "C" (cadr tittle_block_obj_border_ ) (caddr tittle_block_obj_border_) (list (cons 0 "insert"))))
    ;
    ;get_filter_selecton_via_border_box
      (setq ss_section_set_filter_ (TA:EF_Filter_ss_set_ ss_selection_set_ "000 - HEADING NAME 2025"))
    ;
    ;preloop_and_while_add_scale_to_heading
      (setq sum_text_ ())
      (setq ss_section_set_filter_i 0)
      (while (< ss_section_set_filter_i (sslength ss_section_set_filter_))
        (setq ss_section_set_filter_ename_ (ssname ss_section_set_filter_ ss_section_set_filter_i))
        (setq ss_section_set_filter_obj_ (vlax-ename->vla-object ss_section_set_filter_ename_))
        (setq ss_section_set_filter_obj_id (itoa (vla-get-objectid ss_section_set_filter_obj_)))
        (setq ss_section_set_filter_att_val_list (LM:vl-getattributevalues ss_section_set_filter_obj_  ))
        ;ATT_scale_val
          (setq main_scale_text_ (rtos 1 2 0)) 
          (setq ss_section_set_filter_att_val_auto_scale_ (strcat main_scale_text_ ": " (strcat "%<\\AcObjProp Object(%<\\_ObjId " ss_section_set_filter_obj_id ">%).XEffectiveScaleFactor \\f " "%lu2%pr0"">%")))
        
          (setq ss_section_set_filter_att_val_scale (LM:vl-setattributevalue ss_section_set_filter_obj_ "SC" ss_section_set_filter_att_val_auto_scale_))
          (command "._updatefield" lasted_ "")
          (command "regen")
        ;
        ;DYN_Bubble_code
          (if 
            (and
              (>= (setq ss_section_set_filter_att_val_length_bubble_code_ (strlen (LM:vl-getattributevalue ss_section_set_filter_obj_ "NO.CODE" ))) 5)
              (= (LM:getdynpropvalue ss_section_set_filter_obj_ "Visibility1") "1_BUBBLE_2")
            )
            (progn
              (setq sssssss (- (* (* ss_section_set_filter_att_val_length_bubble_code_ 2.5 ) (vla-get-xeffectivescalefactor ss_section_set_filter_obj_ )) 10))
              (LM:setdynpropvalue ss_section_set_filter_obj_ "BUBBLE_width" sssssss  )
              (LM:setdynpropvalue ss_section_set_filter_obj_ "BUBBLE_length" (/ sssssss 2))
            )
            
          )
          
          
         
        
        ;
    
        (setq ss_section_set_filter_i (+ ss_section_set_filter_i 1))
      )
    ;
    
    
    
    
  )
  (defun c:z663C_generate_project_name_ ()
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
          
          (cond ;visibility_mode_case_
            (;visibility_mode_case_1
               (and 
                 (= (LM:getvisibilitystate blk_ename_obj_) "NORMAL")
               )
               (progn
                  (setq NAME_PROJECT_1 (LM:vl-getattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_1"))
                  (setq NAME_PROJECT_2 (LM:vl-getattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_2"))
                  (setq NAME_PROJECT_3 (LM:vl-getattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_3"))
                  
                  (setq NAME_PART_NAME1 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_1"))
                  (setq NAME_PART_NAME2 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_2"))
                  (setq NAME_PART_NAME3 (LM:vl-getattributevalue blk_ename_obj_ "PART_NAME_VALUE_3"))
                  
                  (setq NAME_PROJECT_OVERALL_ (list NAME_PROJECT_1  NAME_PROJECT_2  NAME_PROJECT_3 ))
                  (setq NAME_PROJECT_OVERALL_ (TA:REMOVE_VAL_LIST_ "" NAME_PROJECT_OVERALL_))
                  (setq NAME_PROJECT_OVERALL_ (LM:lst->str NAME_PROJECT_OVERALL_ "" ))
                  (setq NAME_PROJECT_OVERALL_ (LM:str->lst NAME_PROJECT_OVERALL_ "_" ))
                  (setq NAME_PROJECT_OVERALL_ (TA:REMOVE_VAL_LIST_ "" NAME_PROJECT_OVERALL_))

                  (princ "visibility_mode_case_1")
               )
            )
            (;visibility_mode_case_2
               (and 
                 (= (LM:getvisibilitystate blk_ename_obj_) "PART")
               )
               (progn
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
                  (princ "visibility_mode_case_2")
               )
            )
          )
            
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
    
    
      (cond ;visibility_mode_case_
        (;visibility_mode_case_1
            (and 
              (= (LM:getvisibilitystate blk_ename_obj_) "NORMAL")
            )
            (progn
              (if (/= new_text_line_1_sum nil)
                (progn
                  (setq new_text_line_1_sum (strcat new_text_line_1_sum "_"))
                  (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_1" new_text_line_1_sum )
                )
                (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_1" "" )
              )
              (if (/= new_text_line_2_sum nil)
                (progn
                  (setq new_text_line_2_sum (strcat new_text_line_2_sum "_"))
                  (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_2" new_text_line_2_sum )
                )
                (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_2" "" )
              )
              (if (/= new_text_line_3_sum nil)
                (progn
                  (setq new_text_line_3_sum (strcat new_text_line_3_sum "_"))
                  (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_3" new_text_line_3_sum )
                )
                (LM:vl-setattributevalue blk_ename_obj_ "PRELIM_NAME_PROJECT_3" "" )
              )
              (princ "visibility_mode_case_1")
            )
        )
        (;visibility_mode_case_2
            (and 
              (= (LM:getvisibilitystate blk_ename_obj_) "PART")
            )
            (progn
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
              (princ "visibility_mode_case_2")
            )
        )
      )

    ;

  )
  (defun c:z663D_generate_part_code_project_name_ ()
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
  (defun c:z663E_generate_part_code_project_name_[addmat] ()
    ( c:z666D_add_matterial_data_[addmat])
  )
;

;Part&Mockup_data_
  ;Part&Mockup_data_
    ;LV_DATA_FROM_EXCEl
      (defun c:z664A_ADD_PRODUCT_TO_LV_DATA_FROM_EXCEL_ () ;Database_for_adding_product_name
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
          (C:z664D_excel_LV_MAIN_DATA_refresh_64Crefresh)
        ;
      )
      (defun c:z664C_excel_LV_MAIN_DATA_to_ATT_CAD ()
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
      (defun C:z664D_excel_LV_MAIN_DATA_refresh_64Crefresh ()
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
                          (strcat "000-TYP_LV_" matterial_case_  "@" DISTANCE_LV_VALUE "mm." "_" "V-LINE"  )
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
                          (strcat "000-TYP_LV_" matterial_case_  "@" DISTANCE_LV_VALUE "mm."  "_" "V-LINE"  )
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
      (defun c:z664T_tran_ATT_LV_DATA_FROM_EXCEL_TO_PATTERN_DATA_CUSTOM6 ()
          ;Note By Code_Developer
          ;This command is designed to work exclusively with a block named 'LV_DATA_FROM_EXCEL'.
          ;The Operation code work is designed for keep ATT data BLK name "LV_DATA_FROM_EXCEL" and to some ATT to BLK name "001 - LV_PATTERN DATA CUSTOM6"
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
                  (if
                    (or 
                      (wcmatch MATTERIAL_VALUE "*AT*")
                      (wcmatch MATTERIAL_VALUE "*RT*")
                    )
                    (progn
                      (setq MOCKUP_PART_NAME_1 MATTERIAL_VALUE)
                      (princ "wcmatch_case_1")
                    )
                    (setq MOCKUP_PART_NAME_1 (strcat MATTERIAL_VALUE "_" W_LV_VALUE "x" H_LV_VALUE "@" DISTANCE_LV_VALUE "mm." ))
                  )
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
                  
                  
                  (setq SIZING_VALUE (strcat  WIDTH_VALUE "x" HEIGHT_VALUE "mm. " "(TOTAL_AREA_"  (rtos (* (/ (atof WIDTH_VALUE) 1000)  (/ (atof HEIGHT_VALUE) 1000)) 2 2) "sq.m.)"))
                  (setq THICKNESS_VALUE (strcat  THK_LV_VALUE "mm.(thk.)"))
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
            (LM:vl-setattributevalue REF_block_obj "THICKNESS_VALUE" THICKNESS_VALUE)
            (LM:vl-setattributevalue REF_block_obj "DISTANCE_LOUVERS_VALUE" DISTANCE_LV_VALUE)
            (LM:vl-setattributevalue REF_block_obj "TOTAL_LOUVERS_VALUE1" TOTAL_LOUVERS_VALUE1)
            (LM:vl-setattributevalue REF_block_obj "ACESSORIES_VALUE1" ACESSORIES_VALUE1)
            (LM:vl-setattributevalue REF_block_obj "COATING_VALUE_1" "ALL_SIDE_PAINT")
            (LM:vl-setattributevalue REF_block_obj "TOTAL_SET_VALUE" TOTAL_SET_VALUE)
            (LM:setdynpropvalue REF_block_obj "ACESSORIES_LINE_LOOKUP" 1)

          ; 
      )
    ;
    ;001 - LV_PATTERN DATA CUSTOM6
      (defun c:z665A_add_part_code_to_mockup_data_[addmckcode] ()
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '001 - LV_PATTERN DATA CUSTOM6'.
        ;The operation of the command will read the block's attribute values BLK name "001 - PART DATA CUSTOM 2025" and send Attribute to acessoiries att of 001 - LV_PATTERN DATA CUSTOM6
        ; 
        ;
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
          (setq filter_blk_object_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "001 - PART DATA CUSTOM 2025" ))
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
      (defun c:z665B_excel_LV_PART_DATA_to_ATT_CAD () ;LOUVER_MOCKUP_DATA_
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '001 - LV_PATTERN DATA CUSTOM6'.
        ;The operation of the command will read range cell data in excel  and send the range cell data to attribute block 
        ;if you need to use fully FUNC of command. You have to run and follow this code by sequnce of attribute 
        ;example formula
        ; =concat("z665B_excel_LV_PART_DATA_to_ATT_CAD" " ",CHAR(34),[rangecell],CHAR(34),)
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
    ;
    ;LV_DATA_FROM_EXCEl_PART_DATA_2025
      (defun c:z666A_generate_part_name_LV_DATA_FROM_EXCEl_PART_DATA_2025 () 
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
            (if 
              (OR 
                (= (LM:effectivename blk_ename_obj_) 
                  "LV_DATA_FROM_EXCEl_PART_DATA_2025"
                )
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
                ; ;generate_BLK_name_process *กำลังจะทำ gen ชื่อ blk 
                ;   (setq prefix_blk_name_ "000 - ASSCESSORIES_FAMELINE_")
                ;   (cond
                ;     (;matterial_case_1
                ;        (and
                ;           (wcmatch (LM:vl-getattributevalue blk_ename_obj_ "MATTERIAL_VALUE_1") "*SHEET*")
                ;        )
                ;        (progn
                ;          (setq matterial_type)
                ;          (princ "matterial_case_1")
                ;        )
                ;     )
                ;   )
                  
                  
                
                
                ; ;
              )
            )
          ;

      )
    ;
    ;001-PART_DATA_CUSTOM_2025
      (defun c:z666B_generate_part_name_001-PART_DATA_CUSTOM_2025 () 

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
      (defun c:z666C_input_blanksize_value_ ()
        ;DES
          ;commnad work with 
            ;dynamic block "001 - PART DATA CUSTOM 2023_REV01" 
            ;att_name_ "BLANK_SIZE_VALUE"
            ;only
          ;
        ;
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
        ;user_input get_objectname_1st_ 
          (setq get_objectname_1st_ nil)
          (setq get_objectname_1st_obj_val_ nil)
          ;preloop_and_while_
              (while (not get_objectname_1st_)
                (princ (setq get_objectname_1st_ (car (entsel "\nPlease Specify get_objectname_1st_\n"))))
                (if (and ;condition rule
                      (/= get_objectname_1st_ nil)
                      (= (vla-get-objectname (vlax-ename->vla-object get_objectname_1st_) ) "AcDbRotatedDimension")
                      ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_objectname_1st_)) :vlax-true)
                    )
                  (progn ;add data
                    ; (alert "\nplease select block object")
                    ; (setq get_objectname_1st_ nil)
                    ; (setq get_objectname_1st_obj_val_ nil)
                    (if (= (vla-get-textoverride (vlax-ename->vla-object get_objectname_1st_) ) "")
                      (progn
                        (setq get_objectname_1st_measurement (vla-get-measurement (vlax-ename->vla-object get_objectname_1st_)))
                      )
                      (setq get_objectname_1st_measurement (atof (vla-get-textoverride (vlax-ename->vla-object get_objectname_1st_) ))) 
                    )        
                    ;add more
                  )
                  (setq get_objectname_1st_ nil)
                )
              )
          ;
        ;
        ;user_input get_objectname_2nd_ 
          (setq get_objectname_2nd_ nil)
          (setq get_objectname_2nd_obj_val_ nil)
          ;preloop_and_while_
              (while (not get_objectname_2nd_)
                (princ (setq get_objectname_2nd_ (car (entsel "\nPlease Specifyget_objectname_2nd_\n"))))
                (if (and ;condition rule
                      (/= get_objectname_2nd_ nil)
                      (= (vla-get-objectname (vlax-ename->vla-object get_objectname_2nd_) ) "AcDbRotatedDimension")
                      ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_objectname_2nd_)) :vlax-true)
                    )
                  (progn ;add data
                    ; (alert "\nplease select block object")
                    ; (setq get_objectname_2nd_ nil)
                    ; (setq get_objectname_2nd_obj_val_ nil)
                    (if (= (vla-get-textoverride (vlax-ename->vla-object get_objectname_2nd_) ) "")
                      (progn
                        (setq get_objectname_2nd_measurement (vla-get-measurement (vlax-ename->vla-object get_objectname_2nd_)))
                      )
                      (setq get_objectname_2nd_measurement (atof (vla-get-textoverride (vlax-ename->vla-object get_objectname_2nd_) ))) 
                    )
                            
                    ;add more
                  )
                  (setq get_objectname_2nd_ nil)
                )
              )
          ;
        ;
        ;filter_and_sorting_selection
          (setq _set_1 (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx "001 - PART DATA CUSTOM 2025"))
          (setq _set_2 (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx "LV_DATA_FROM_EXCEl_PART_DATA_2025"))

          (if (= (sslength _set_1) 0)
            (progn
              (setq _set_ _set_2)
            )
            (setq _set_ _set_1)
          )
        
          (setq TAR_fileter_set_ (TA:standard_list_croodinate_sorting _set_ "X"))
        ;
        ;preloop_and_while
          (setq TAR_fileter_set_i 0)
          (while (< TAR_fileter_set_i (length TAR_fileter_set_))
            (setq TAR_fileter_set_ename_ (car (nth  TAR_fileter_set_i TAR_fileter_set_)))
            (setq TAR_fileter_set_obj_ (vlax-ename->vla-object TAR_fileter_set_ename_))
            
            (LM:vl-setattributevalue TAR_fileter_set_obj_ "BLANK_SIZE_VALUE" 
              (strcat (rtos get_objectname_1st_measurement 2 2) 
                      " x " 
                      (rtos get_objectname_2nd_measurement 2 2)
                      "mm."
                      )
            )
            (setq TAR_fileter_set_i (+ TAR_fileter_set_i 1))
          )
        ;
        
      )
      (defun c:z666D_add_matterial_data_[addmat] ()
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '001 - PART DATA CUSTOM 2025'.
        ;This command is designed to work with attributes (att) and a block named "001 - PART DATA CUSTOM 2025" 
        ;functioning alongside the following attributes:
        ;reset_val
          (setq 
            THICKNESS_VALUE nil
          )
        ;
        ; MATTERIAL_VALUE_1
        ; SPEC_VALUE
        ;
        ;fillter_partBLK_ename__name_object_
          (setq partBLK_ename_ nil)
          (while (= partBLK_ename_ nil)
            ;user_input_data
              (setq partBLK_ename_ (car (entsel "specify Object")))
            ;
            ;fillter_object_type_
              (if 
                (and
                  (if (/= partBLK_ename_ nil) (progn (setq partBLK_obj_ (vlax-ename->vla-object partBLK_ename_)) ))
                  (= (vla-get-objectname partBLK_obj_) "AcDbBlockReference")
                  (= (vla-get-hasattributes partBLK_obj_) :vlax-true )
                  (if (or (= (LM:effectivename partBLK_obj_) "001 - PART DATA CUSTOM 2025" ) (= (LM:effectivename partBLK_obj_) "LNAD - A4 TITLE BLOCK PART REV01" )  ) (progn :vlax-true) :vlax-false)
                )
                (progn
                  (setq partBLK_tag_list_ (LM:vl-getattributevalues partBLK_obj_))
                )
                (setq partBLK_ename_ nil)
              )
              (if
                (or
                  (= partBLK_ename_ nil)
                  (/= (vla-get-objectname partBLK_obj_) "AcDbBlockReference")
                  (if (= (vla-get-objectname partBLK_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes partBLK_obj_) :vlax-true ) ))
                )
                (progn
                  (alert "Please selection attribute object")
                )
              )
            ;
          )
        ;
        
        ;condition_product_name_
          (setq product_number_ (cond 
                                  ((getint 
                                      (strcat "\nuser_input_for_product_number
                                                \nAL.EXTRUSION                = 1
                                                \nAL.SHEET                        = 2
                                                \nGALVANIZE_SHEET          = 3
                                                \nSTAINLESS_STEEL            = 4
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
                (setq MATTERIAL_VALUE_1 "AL.EXTRUSION" )
                (setq SPEC_VALUE "6063 T5" )
                (setq COATING_VALUE_1 "ALL_SIDE_PAINT" )
                (setq COATING_VALUE_2 "XXX" )
                (princ "product_name_case_1")
              )
            )
            (;product_name_case_2
              (and
                  (= product_number_ 2)
              )
              (progn
                (setq MATTERIAL_VALUE_1 "AL.SHEET" )
                (setq SPEC_VALUE "1100 H14" )
                (setq COATING_VALUE_1 "ALL_SIDE_PAINT" )
                (setq COATING_VALUE_2 "XXX" )
                (princ "product_name_case_2")
              )
            )
            (;product_name_case_3
              (and
                  (= product_number_ 3)
              )
              (progn
                (setq MATTERIAL_VALUE_1 "GALVANIZE SHEET" )
                (setq SPEC_VALUE "1100 H14" )
                (setq COATING_VALUE_1 "ALL_SIDE_PAINT" )
                (setq COATING_VALUE_2 "RUST PROOF PAINT" )
                (princ "product_name_case_3")
              )
            )
          )
        ;
        
        ;main_IDEA-of_code
          (if (/= partBLK_ename_ nil )
            (progn
              
              (LM:vl-setattributevalue partBLK_obj_ "MATTERIAL_VALUE_1" MATTERIAL_VALUE_1 )
              (LM:vl-setattributevalue partBLK_obj_ "SPEC_VALUE" SPEC_VALUE )
              
              (LM:vl-setattributevalue partBLK_obj_ "THICKNESS_VALUE" 
                
                (strcat 
                  
                  (rtos (setq THICKNESS_VALUE (cond ( (getreal (strcat "\nSpecify THICKNESS_VALUE <" (rtos (setq THICKNESS_VALUE (cond (THICKNESS_VALUE) (1.0) ) ) ) "> : " ) ) ) (THICKNESS_VALUE) ) ) 2 1 )
                  "mm.(thk.)"
                )

              )
              
              (LM:vl-setattributevalue partBLK_obj_ "COATING_VALUE_1" COATING_VALUE_1 )
              (LM:vl-setattributevalue partBLK_obj_ "COATING_VALUE_2" COATING_VALUE_2 )
            )
          )
        ;
      )
    ;
    ;000 - DAYNAMIC_DATA_FAMELINE_PRICE_LIST
      (defun c:z667A_DYNAMIC_DATA_FAMELINE_PRICE_LIST_to_ATT_CAD ()
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '000 - DAYNAMIC_DATA_FAMELINE_PRICE_LIST'.
        ;The operation of the command will read/send range cell data in excel and 
        ;Seuqence of Attribute is sorted by parameter object
        ;if you run this code command in excel this is an example for work 
        ;(=CONCAT( "z670_excel_LV_MAIN_DATA_to_ATT_CAD", " ",CHAR(34),[range cell],CHAR(34), " ",CHAR(34),[range cell],CHAR(34), ))
        ; char (34) = " (single qoute)
        ;
        ;insert_data_from_excel
          (command "insert" "000 - DAYNAMIC_DATA_FAMELINE_PRICE_LIST" "0,0" 1 0)
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

      (defun c:z667T_tran_FAMELINE_PRICE_LIST_to_PART_DATA_CUSTOM_2025 ()
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '000 - DAYNAMIC_DATA_FAMELINE_PRICE_LIST' and "001-PART_DATA_CUSTOM_2025" .
        ;The operation of the command will read the block's attribute values and send attribute data to BLK_NAME "001-PART_DATA_CUSTOM_2025" . 
        ;
        ;fillter_EF_name_object_
          (setq mainBLK_ename_ nil)
          (while (= mainBLK_ename_ nil)
            ;user_input_data
              (setq mainBLK_ename_ (car (entsel "specify Object")))
            ;
            ;fillter_object_type_
              (if 
                (and
                  (if (/= mainBLK_ename_ nil) (progn (setq mainBLK_obj_ (vlax-ename->vla-object mainBLK_ename_)) ))
                  (= (vla-get-objectname mainBLK_obj_) "AcDbBlockReference")
                  (= (vla-get-hasattributes mainBLK_obj_) :vlax-true )
                  ; (= (LM:effectivename mainBLK_obj_) "000 - DAYNAMIC_DATA_FAMELINE_PRICE_LIST" )
                )
                (progn
                  (setq mainBLK_att_list_ (LM:vl-getattributevalues mainBLK_obj_))
                  (setq mainBLK_att_PRODUCT_NAME_VALUE (LM:vl-getattributevalue mainBLK_obj_ "PRODUCT_NAME_VALUE"))
                  (setq mainBLK_att_PRODUCT_CODE_VALUE (LM:vl-getattributevalue mainBLK_obj_ "PRODUCT_CODE_VALUE"))
                )
                (setq mainBLK_ename_ nil)
              )
              (if
                (or
                  (= mainBLK_ename_ nil)
                  (/= (vla-get-objectname mainBLK_obj_) "AcDbBlockReference")
                  (if (= (vla-get-objectname mainBLK_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes mainBLK_obj_) :vlax-true ) ))
                )
                (progn
                  (alert "Please selection attribute object")
                )
              )
            ;
            ;
          )
        ;
        ;fillter_targetBLK_ename_name_object_
          (setq targetBLK_ename_ nil)
          (while (= targetBLK_ename_ nil)
            ;user_input_data
              (setq targetBLK_ename_ (car (entsel "specify Object")))
            ;
            ;fillter_object_type_
              (if 
                (and
                  (if (/= targetBLK_ename_ nil) (progn (setq targetBLK_obj_ (vlax-ename->vla-object targetBLK_ename_)) ))
                  (= (vla-get-objectname targetBLK_obj_) "AcDbBlockReference")
                  (= (vla-get-hasattributes targetBLK_obj_) :vlax-true )
                  (= (LM:effectivename targetBLK_obj_) "001 - PART DATA CUSTOM 2025" )
                )
                (progn
                  (setq targetBLK_tag_list_ (LM:vl-getattributevalues targetBLK_obj_))
                  (LM:vl-setattributevalue targetBLK_obj_ "CODE_VALUE_1" mainBLK_att_PRODUCT_CODE_VALUE)
                  (LM:vl-setattributevalue targetBLK_obj_ "PART_NAME_VALUE_1" mainBLK_att_PRODUCT_NAME_VALUE )
                )       
                (setq targetBLK_ename_ nil)
              )
              (if
                (or
                  (= targetBLK_ename_ nil)
                  (/= (vla-get-objectname targetBLK_obj_) "AcDbBlockReference")
                  (if (= (vla-get-objectname targetBLK_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes targetBLK_obj_) :vlax-true ) ))
                )
                (progn
                  (alert "Please selection attribute object")
                )
              )
            ;
          )
        ;
      )
    ;
    ;make_object_
      ;make_front_view
      (defun c:z668A_MK_MCK_LV_Front_view ()

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
        (C:z664D_excel_LV_MAIN_DATA_refresh_64Crefresh)
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
      ;
      ;make_plan_view
      (defun c:z668B_MK_MCK_LV_PLAN_view ()
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
        (C:z664D_excel_LV_MAIN_DATA_refresh_64Crefresh)
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
      ;
    ;
    ;
    (defun c:z669_excel_LV_MAIN_DATA_to_ATT_CAD ()
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
    ;
  ;
;


(defun c:PLA ()
  (setvar "plinewid" 20)
  (setq pt1 (getpoint "specify point 1"))
  (setq pt2 (getpoint "specify point 2"))
  (command "pline" pt1  "A" "S" pt2 )
  (setvar "plinewid" 0)
)


(defun c:MKMCKHELP_ ()
  (setq pt1_ (getpoint "specify point"))
  
  
  (command "insert" "000 - MOCKUP_HELP" pt1_ 1 0 )
)













