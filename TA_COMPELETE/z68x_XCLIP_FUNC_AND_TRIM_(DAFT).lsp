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
;Remove List Function Lee Mac
  (defun LM:RemoveNth (n l / i) 
    ;;  Arguments:                                              ;;
    ;;  n - index of item to remove (zero based)                ;;
    ;;  l - list from which item is to be removed               ;;
      (setq i -1)
      (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
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
;
;Get_Data Function Ta Trai
  (defun TA:get_name_block_in_drawing ()
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
        (setq sorted_block_list_ (acad_strlsort block_list))
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
      (princ "\n                  |---------------------|")
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
    (setq vertex_total (length (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq sum_ ())

    (setq vertex_i 0)
    (setq vertex_ii 1)

    (while (< vertex_i vertex_total)
      ;get_data_len
      (setq len_i (vlax-curve-getDistatParam ss vertex_i) )
      (setq len_ii (vlax-curve-getDistatParam ss vertex_ii))
      ;
      ;sum
        (setq sum (list (- len_ii len_i)))
        (setq sum_ (append sum sum_ ))
      ;
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
;
;Sorting_Selection_Set_by_Coordinate Ta Trai
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
;convert_value_
  (defun deg-to-rad (angle)
    (* angle (/ pi 180.0))
  )
  (defun rad-to-deg (rad)
    (/ (* rad 180) pi) 
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
    (defun create-pline (coords close_loop_)
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
    (defun TA:add_vertex_point_pline_via_breakpt_ (ename_cut_edge_ ename_intersection_obj_ explode)
      ; ;user_input_for_testing_data
      ;   (setq  ename_cut_edge_ (car (entsel))) ;for_testing_data
      ;   (setq  ename_intersection_obj_ (car (entsel))) ;for_testing_data
      ; ;
      ;user_input_
        (if (= explode "explode")
          (progn
            (setq explode "explode")
          )
          (setq explode "non_explode")
        ) 
      ;
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
      ;for_explode_entlast_object
        ; (command "pselect" ename_intersection_obj_) :for testing
        (if (= explode "explode") 
          (progn 
            (vla-explode (vlax-ename->vla-object ename_intersection_obj_))
            (vla-delete (vlax-ename->vla-object ename_intersection_obj_))
          )
          (setq explode "non_explode")
        )
        
      ;
    )
    (defun TA:add_vertex_point_pline_via_breakpt_1 (ename_cut_edge_ ename_intersection_obj_ explode)
      ;example for test
        ; (setq ename_cut_edge_ (car (entsel )))
        ; (setq ename_intersection_obj_ (car (entsel)))
      ;
      ;user_input_
        (if (= explode "explode")
          (progn
            (setq explode "explode")
          )
          (setq explode "non_explode")
        ) 
      ;
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
        (setq ename_intersection_obj_ (entlast))
        (setq vertex_list_ (TA:Get_Pline_vertext_ins_point_ 
                              ename_intersection_obj_
                            )
        )
        (vla-put-closed (vlax-ename->vla-object ename_intersection_obj_) :vlax-false)
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
          
          
          (command "_.pedit" "_m" ssadd_list_ "" "j" "" "")
          
          
        
          (setq ename_intersection_obj_ (entlast))
          (setq intersec_i (+ intersec_i 1))
          
        )
      ;
      ;for_explode_entlast_object
        ; (command "pselect" ename_intersection_obj_) :for testing
        (if (= explode "explode") 
          (progn 
            (vla-explode (vlax-ename->vla-object ename_intersection_obj_))
            (vla-delete (vlax-ename->vla-object ename_intersection_obj_))
          )
          (setq explode "non_explode")
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
  ;add_arc
    (defun TA:vla-add_arc_half_ (arc_ename_ intersec_point_list_)
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
      ;get_data_
      ; (setq arc_ename_ (car (entsel)))
      (setq arc_ename_obj_ ss_s_non_block_obj)
      (setq arc_ename_obj_head_ang_ (rad-to-deg (vla-get-startangle arc_ename_obj_ )))
      (setq arc_ename_obj_end_ang_ (rad-to-deg (vla-get-endangle arc_ename_obj_ )))
      (setq arc_ename_obj_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center arc_ename_obj_)) ) )
      (setq arc_ename_obj_radius_ (vla-get-radius arc_ename_obj_) )
      (setq arc_ename_obj_startpoint_ (vlax-safearray->list (vlax-variant-value (vla-get-startpoint arc_ename_obj_)) ) )
      (setq arc_ename_obj_endpoint_ (vlax-safearray->list (vlax-variant-value (vla-get-endpoint arc_ename_obj_)) ) )
      (setq sum_start_result_ (list arc_ename_obj_startpoint_ arc_ename_obj_head_ang_ ) )
      (setq sum_end_result_ (list arc_ename_obj_endpoint_ arc_ename_obj_end_ang_ ))
      ;
      ;preloop_and_while sorting point
        (setq intersec_point_list_i 0)
        (setq intersec_point_list_new_ ())
        
        (while (< intersec_point_list_i (length intersec_point_list_))
          (setq intersec_point_list_point_ (append (nth intersec_point_list_i intersec_point_list_) (list 0)))
          (setq intersec_point_list_ang_ (rad-to-deg (angle arc_ename_obj_center_ intersec_point_list_point_ )))
          (setq sum_ (list intersec_point_list_point_ intersec_point_list_ang_))
          
          (setq intersec_point_list_new_ (cons sum_ intersec_point_list_new_))
          (setq intersec_point_list_i (+ intersec_point_list_i 1))
        
        )
        (setq intersec_point_list_new_ (cons sum_start_result_  intersec_point_list_new_))
        (setq intersec_point_list_new_ (cons sum_end_result_  intersec_point_list_new_))
        (setq intersec_point_list_new_sort_ (CO:sort_by_val_ 1 intersec_point_list_new_))
        (if (> arc_ename_obj_head_ang_ 180 )
          (progn
            (setq sorting_and_ (- arc_ename_obj_head_ang_ 360))
            (setq intersec_point_list_new_sort_i 0)
            (setq hotel_California ())
            (while (< intersec_point_list_new_sort_i (length intersec_point_list_new_sort_))
              (setq intersec_point_list_new_sort_val_ (nth intersec_point_list_new_sort_i intersec_point_list_new_sort_))
              (cond
                ((> (setq temp_ang_ (- (cadr intersec_point_list_new_sort_val_) sorting_and_)) 360) 
                  (progn 
                    (setq temp_ang_ (- temp_ang_ 360))
                  ) 
                )
                ((< (setq temp_ang_ (- (cadr intersec_point_list_new_sort_val_) sorting_and_)) 360) 
                  (progn 
                    (setq temp_ang_ (- (cadr intersec_point_list_new_sort_val_) sorting_and_) )
                  ) 
                )
                ((= (setq temp_ang_ (- (cadr intersec_point_list_new_sort_val_) sorting_and_)) 360) 
                  (progn 
                    (setq temp_ang_ 0)
                  ) 
                )
              )
              (setq temp_ang_val_ (append intersec_point_list_new_sort_val_ (list temp_ang_) ))
              (setq hotel_California (cons temp_ang_val_ hotel_California))
              (setq intersec_point_list_new_sort_i (+ intersec_point_list_new_sort_i 1))
            )
            (setq hotel_California_solo_ (CO:sort_by_val_ 2 hotel_California))
            (setq hotel_California_solo (CO:sort_by_val_ 2 hotel_California))
          )
          (setq hotel_California_solo_ (CO:sort_by_val_ 1 intersec_point_list_new_))
        )
      ;
      ;preloop_and_while_add_arc
        (setq hotel_California_solo_i 0)
        (setq hotel_California_solo_ii 1)
        (while (< hotel_California_solo_i (length hotel_California_solo_))
          (setq hotel_California_solo_val1_ (cadr (nth hotel_California_solo_i hotel_California_solo_)))
          (if (>= hotel_California_solo_ii (length hotel_California_solo_))
            (progn 
              (princ "\n")
            )
            (setq hotel_California_solo_val2_ (cadr (nth hotel_California_solo_ii hotel_California_solo_)))
          )
          
          (vla-addarc modelSpace 
                      (vlax-3d-point arc_ename_obj_center_)
                      arc_ename_obj_radius_
                      (deg-to-rad hotel_California_solo_val1_)
                      (deg-to-rad hotel_California_solo_val2_)
          )
          (setq hotel_California_solo_i (+ hotel_California_solo_i 1))
          (setq hotel_California_solo_ii (+ hotel_California_solo_ii 1))
        
        
        )
      ;
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
  ;offset_line
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

(defun TA:cutting_object_cto_ (main_border_ ss_s_non_block_)  
  ;for testing code
    ; (setq main_border_  (car (entsel "specify main border")))
    (setq main_border_obj (vlax-ename->vla-object main_border_))
    (setq main_border_obj_handle_ (vla-get-handle main_border_obj ))
    ; (command "pselect" main_border_ "" )
    ; (command "pselect" ss_s_non_block_ename "" )
  ;
  ; selection_obj_for_triming
    ; (setq ss_s_non_block_ (ssget ;"_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
    ;                             ;(append bo_line_ (list (car bo_line_)))
    ;                             (list 
    ;                               (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
    ;                               ; (cons 8 "000 - GRID")   ;kind of layer
    ;                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
    ;                               ; (cons 62 1)           ;kind of color call sign with color code index
    ;                             )
    ;                       )
    ; )
  ;
  ;preloop_and_while_ for breakline object
    (setq ss_s_non_block_i 0)
    (while (< ss_s_non_block_i (sslength ss_s_non_block_))
      (setq ss_s_non_block_ename (ssname ss_s_non_block_ ss_s_non_block_i))
      (setq ss_s_non_block_obj (vlax-ename->vla-object ss_s_non_block_ename))
      (setq ss_s_non_block_handle_ (vla-get-handle ss_s_non_block_obj))
      (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object ss_s_non_block_ename))) ;For Check objectname
      
      
      
      (setq intersec_point_list_ (LM:intersections main_border_obj ss_s_non_block_obj  acextendnone ))
      
      
      (if (and (/= main_border_obj nil) (/= main_border_obj_handle_ ss_s_non_block_handle_))
        (progn      
          (cond 
            (;ciecle_method
             (and 
                (= type_obj_ "AcDbCircle")
                (> (length intersec_point_list_) 0)
              )
              (progn 
                (setq intersec_point_list_ (append (CO:sort_by_val_ 1 intersec_point_list_) (list (car (CO:sort_by_val_ 1 intersec_point_list_)))))
                (setq intersec_point_list_arc_ intersec_point_list_)
                (setq get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_s_non_block_obj)) ) )
                (setq get_rad_ (vla-get-radius ss_s_non_block_obj))
                (TA:vla-addarc_ get_center_ get_rad_ intersec_point_list_)
                
                ;for delete REF_obj
                  (vla-delete ss_s_non_block_obj)
                ;
              )
            )
            (;arc_method
             (and 
                (= type_obj_ "AcDbArc")
                
              )
              (progn 
                (setq intersec_point_list_ intersec_point_list_)
                ; (setq point_list_ (append (CO:sort_by_val_ 1 intersec_point_list_) (list (car (CO:sort_by_val_ 1 intersec_point_list_)))))
                ; (setq get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_s_non_block_obj)) ) )
                ; (setq center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_s_non_block_obj)) ) )
                ; (setq get_rad_ (vla-get-radius ss_s_non_block_obj))
                ; (setq rad_ (vla-get-radius ss_s_non_block_obj))
                
                
                
              
                
                (TA:vla-add_arc_half_ arc_ename_ intersec_point_list_ )  
                
                ;for delete REF_obj
                  (vla-delete ss_s_non_block_obj)
                ;
              )
            )     
            (;pline_method
             (and 
               (= type_obj_ "AcDbPolyline")
               (/= (length (cadr (TA:Get_Pline_vertext_ins_point_ ss_s_non_block_ename ))) 2)
               (> (length intersec_point_list_) 0)
             )
              (progn    
                (TA:add_vertex_point_pline_via_breakpt_ main_border_ ss_s_non_block_ename "explode")
              )
            )
            (;pline_method_2point
             (and 
               (= type_obj_ "AcDbPolyline")
               (= (length (cadr (TA:Get_Pline_vertext_ins_point_ ss_s_non_block_ename ))) 2)
               (> (length intersec_point_list_) 0)
             )
              (progn 
                (vla-explode ss_s_non_block_obj)
                (vla-delete ss_s_non_block_obj )
                (setq ss_s_non_block_ename (entlast))                
                (TA:add_vertex_point_pline_via_breakpt_1 main_border_ ss_s_non_block_ename "explode")
              )
            )
            (;line_method
             (and 
               (= type_obj_ "AcDbLine")
               (> (length intersec_point_list_) 0)
             )
              (progn    
                (TA:add_vertex_point_pline_via_breakpt_1 main_border_ ss_s_non_block_ename "explode")
              )
            )
          )
        )
        (princ "object error")
      )
      (princ (strcat "object" (rtos ss_s_non_block_i 2 0) "/" (rtos (sslength ss_s_non_block_) 2 0) ))
      (setq ss_s_non_block_i (+ ss_s_non_block_i 1))
        
      
    )
  ;
    
  
)
(defun TA:select_inside (main_border_ )
  (if ;(setq main_border_c_ (car (entsel "specify for cossing select")))
    (and (= 1 1) (/= main_border_ nil)) ;logic here rai ni 5555
    (progn
      (setq main_border_c_ nil)
      (while (= main_border_c_ nil)
        ; (setq main_border_c_ (car (entsel "specify for cossing select")))
        (setq main_border_c_ main_border_ )
        (if 
          (and
            (/= main_border_c_ nil)
          )
          (progn
            (cond
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq main_border_c_get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center (vlax-ename->vla-object main_border_c_) ))))
                  (setq main_border_c_get_radius_ (- (vla-get-radius (vlax-ename->vla-object main_border_c_) )0.005 ) )
                  (command "polygon" "200" main_border_c_get_center_ "i" main_border_c_get_radius_ )
                  
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ (entlast)) )
                  (vla-delete (vlax-ename->vla-object (entlast)))
                )
              )
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbPolyline" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq new_dat_ (TA:Offset_Pline_multi_vertex_ main_border_c_ 0.001 "min"))
                  (setq main_border_c_c_ (entlast) )
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ new_dat_))
                  (vla-delete (vlax-ename->vla-object main_border_c_c_))
                  (setq main_border_c_new_border_ main_border_c_new_border_)
                )
              )
            )
          )
          (setq main_border_c_ nil)
        )
      )
    )
    (princ "sds")
  )
  (setq ss_s_non_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                              (cadr main_border_c_new_border_)
                              
                              (list 
                                (cons 0 "LINE,ARC,CIRCLE,POLYINE,LWPOLYLINE") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                        )
  )
  ; (command "pselect" ss_s_non_block_ "")
  ;entlast_result_data
    (setq ss_s_non_block_ ss_s_non_block_)  
  ;
)
(defun TA:select_inside_block (main_border_ )
  (if ;(setq main_border_c_ (car (entsel "specify for cossing select")))
    (= 1 1) ;logic here rai ni 5555
    (progn
      (setq main_border_c_ nil)
      (while (= main_border_c_ nil)
        ; (setq main_border_c_ (car (entsel "specify for cossing select")))
        (setq main_border_c_ main_border_ )
        (if 
          (and
            (/= main_border_c_ nil)
          )
          (progn
            (cond
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq main_border_c_get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center (vlax-ename->vla-object main_border_c_) ))))
                  (setq main_border_c_get_radius_ (- (vla-get-radius (vlax-ename->vla-object main_border_c_) )0.005 ) )
                  (command "polygon" "200" main_border_c_get_center_ "i" main_border_c_get_radius_ )
                  
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ (entlast)) )
                  (vla-delete (vlax-ename->vla-object (entlast)))
                )
              )
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbPolyline" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq new_dat_ (TA:Offset_Pline_multi_vertex_ main_border_c_ 0.001 "min"))
                  (setq main_border_c_c_ (entlast) )
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ new_dat_))
                  (vla-delete (vlax-ename->vla-object main_border_c_c_))
                  (setq main_border_c_new_border_ main_border_c_new_border_)
                )
              )
            )
          )
          (setq main_border_c_ nil)
        )
      )
    )
    (princ "sds")
  )
  (setq ss_s_non_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                              (cadr main_border_c_new_border_)
                              
                              (list 
                                ; (cons 0 "LINE,ARC,CIRCLE,POLYINE,LWPOLYLINE") ;type of object
                                (cons 0 "insert") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                        )
  )
  ; (command "pselect" ss_s_non_block_ "")
  ;entlast_result_data
    (setq ss_s_non_block_ ss_s_non_block_)  
  ;
)
(defun TA:select_inside_hatch (main_border_ )
  (if ;(setq main_border_c_ (car (entsel "specify for cossing select")))
    (= 1 1) ;logic here rai ni 5555
    (progn
      (setq main_border_c_ nil)
      (while (= main_border_c_ nil)
        ; (setq main_border_c_ (car (entsel "specify for cossing select")))
        (setq main_border_c_ main_border_ )
        (if 
          (and
            (/= main_border_c_ nil)
          )
          (progn
            (cond
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq main_border_c_get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center (vlax-ename->vla-object main_border_c_) ))))
                  (setq main_border_c_get_radius_ (- (vla-get-radius (vlax-ename->vla-object main_border_c_) )0.005 ) )
                  (command "polygon" "200" main_border_c_get_center_ "i" main_border_c_get_radius_ )
                  
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ (entlast)) )
                  (vla-delete (vlax-ename->vla-object (entlast)))
                )
              )
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbPolyline" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq new_dat_ (TA:Offset_Pline_multi_vertex_ main_border_c_ 0.001 "min"))
                  (setq main_border_c_c_ (entlast) )
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ new_dat_))
                  (vla-delete (vlax-ename->vla-object main_border_c_c_))
                  (setq main_border_c_new_border_ main_border_c_new_border_)
                )
              )
            )
          )
          (setq main_border_c_ nil)
        )
      )
    )
    (princ "sds")
  )
  (setq ss_s_non_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                              (cadr main_border_c_new_border_)
                              
                              (list 
                                ; (cons 0 "LINE,ARC,CIRCLE,POLYINE,LWPOLYLINE") ;type of object
                                (cons 0 "hatch") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                        )
  )
  ; (command "pselect" ss_s_non_block_ "")
  ;entlast_result_data
    (setq ss_s_non_block_ ss_s_non_block_)  
  ;
)
(defun TA:select_inside_nom_obj+block (main_border_ )
  (if ;(setq main_border_c_ (car (entsel "specify for cossing select")))
    (= 1 1) ;logic here rai ni 5555
    (progn
      (setq main_border_c_ nil)
      (while (= main_border_c_ nil)
        ; (setq main_border_c_ (car (entsel "specify for cossing select")))
        (setq main_border_c_ main_border_ )
        (if 
          (and
            (/= main_border_c_ nil)
          )
          (progn
            (cond
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq main_border_c_get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center (vlax-ename->vla-object main_border_c_) ))))
                  (setq main_border_c_get_radius_ (- (vla-get-radius (vlax-ename->vla-object main_border_c_) )0.005 ) )
                  (command "polygon" "200" main_border_c_get_center_ "i" main_border_c_get_radius_ )
                  
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ (entlast)) )
                  (vla-delete (vlax-ename->vla-object (entlast)))
                )
              )
              (;pline_mode_
                (and 
                  (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbPolyline" )
                  ; (= (vla-get-objectname (vlax-ename->vla-object main_border_c_)) "AcDbCircle" )
                )
                (progn
                  (setq new_dat_ (TA:Offset_Pline_multi_vertex_ main_border_c_ 0.001 "min"))
                  (setq main_border_c_c_ (entlast) )
                  (setq main_border_c_new_border_ (TA:Get_Pline_vertext_ins_point_ new_dat_))
                  (vla-delete (vlax-ename->vla-object main_border_c_c_))
                  (setq main_border_c_new_border_ main_border_c_new_border_)
                )
              )
            )
          )
          (setq main_border_c_ nil)
        )
      )
    )
    (princ "sds")
  )
  (setq ss_s_non_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                              (cadr main_border_c_new_border_)
                              
                              (list 
                                (cons 0 "insert,LINE,ARC,CIRCLE,POLYINE,LWPOLYLINE") ;type of object
                                ; (cons 0 "insert") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                        )
  )
  ; (command "pselect" ss_s_non_block_ "")
  ;entlast_result_data
    (setq ss_s_non_block_ ss_s_non_block_)  
  ;
)
(defun TA:Blk_x_clip_ (xclip_border_ selection_set_ efname_val_)
  ; ;for testing_data_
  ;   (setq selection_set_ (ssget ))
  ;   (setq efname_val_ "special_block2460499.7324" )
  ; ;
  ;get_data_
    (setq xclip_border_obj (vlax-ename->vla-object xclip_border_ ))
  ;
  ;prelooop_and_while
    (setq selection_set_i 0)
    (while  (< selection_set_i (sslength selection_set_))
      (setq selection_set_ename_ (ssname selection_set_ selection_set_i))
      (setq selection_set_ename_obj_ (vlax-ename->vla-object selection_set_ename_))
      
        (if (/= (LM:effectivename selection_set_ename_obj_ ) efname_val_)
            (progn
              (command "xclip" selection_set_ename_ "" "n" "s" xclip_border_ )
            )
            (princ "/nnext")
          )
      
      (setq selection_set_i (+ selection_set_i 1))
    )
  ;
)
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
(defun C:explode_xcilp_exxc_ ()
  ;nom.object_process
    ;user input name block 
      (setq prefix_name "special_block")
      (setq num_i (rtos (getvar "date" )2 4))
      (setq name_block_ (strcat prefix_name num_i))
      (setq name_block_block_ (strcat prefix_name num_i "_block_"))
      (setq name_block_hatch_ (strcat prefix_name num_i "_hatch_"))
    ;
    ;user_input 
      (if ;(setq ref_block_ (car (entsel "specify for cossing select")))
        (= 1 1) ;logic here rai ni 5555
        (progn
          (setq ref_block_ nil)
          (while (= ref_block_ nil)
            (setq ref_block_ (car (entsel "specify for xcilp block")))
            (if 
              (and
                (/= ref_block_ nil)
              )
              (progn
                (cond
                  (;blk_mode_
                    (and 
                      (= (vla-get-objectname (vlax-ename->vla-object ref_block_)) "AcDbBlockReference" )
                      ; (= (vla-get-objectname (vlax-ename->vla-object ref_block_)) "AcDbCircle" )
                    )
                    (progn
                      (command "xclip" ref_block_ "" "p"  )
                      (setq xclip_border_ (entlast))
                      (setq xclip_border_center_pt_ (TA:find_center xclip_border_ ))
                      (setq xclip_border_center_ename_ (TA:Get_Pline_vertext_ins_point_ xclip_border_ ))
                    )
                  )
                )
              )
              (setq ref_block_ nil)
            )
          )
        )
        (princ "sds")
      )
    ;
    ;generate_border_line
      (setq block_border_ (cdr (TA:ename+vla-getboundingbox (vlax-ename->vla-object ref_block_ ))))
      (command "rectangle" (car block_border_) (cadr block_border_))
      (setq block_border_ename_ (entlast))
    ;  
    ;explode_xclip_
      (command "explode" ref_block_ )
    ;
    ;cutting_object_and_make_block_
      (setq ss_cutting_set_ (TA:select_inside xclip_border_))
      ; (command "pselect" xclip_border_ "")
      (if (/= ss_cutting_set_ nil)
        (progn
          (TA:cutting_object_cto_ xclip_border_ ss_cutting_set_  )
          (setq post_ss_cutting_set_ (TA:select_inside xclip_border_))
          (command "pselect" post_ss_cutting_set_ "")
          (command "_block" name_block_ xclip_border_center_pt_  post_ss_cutting_set_ "" )
        )
        (princ "/n")
      )
    ;
    ;cutting_outside_
      (setq outside_border_ (TA:select_inside block_border_ename_))
      (command "pselect" outside_border_ "" )
      (command "erase" outside_border_ "")
      (if (/= (vlax-ename->vla-object block_border_ename_ ) nil)
        (progn
          (vla-delete (vlax-ename->vla-object block_border_ename_ ))
        )
      )
    ;
    ;insert
      (if (/= ss_cutting_set_ nil)
        (progn
          (command "insert" name_block_ xclip_border_center_pt_ 1 1 0 )
          (create-pline (cadr xclip_border_center_ename_) "close")
          
        )
        (princ "/n")
      )
    ;
  ;
  ;blk_object_process_
    ;user input name block 
      (setq name_block_block_ (strcat prefix_name num_i "_block_"))
    ;
    ;
      (setq xclip_border_ (entlast))
      ; (command "pselect" xclip_border_ "" )
      (if (/= (setq ss_block_set_ (TA:select_inside_block xclip_border_)) nil)
        (progn
          (setq xclip_border_center_pt_ (TA:find_center xclip_border_ ))
          (TA:Blk_x_clip_ xclip_border_ ss_block_set_ name_block_)
          (setq ss_block_set_ (TA:select_inside_block xclip_border_))
          ; (command "pselect" ss_block_set_ "" )
          (command "_block" name_block_block_ xclip_border_center_pt_  ss_block_set_ "" )
          (command "insert" name_block_block_ xclip_border_center_pt_ 1 1 0 )
          ; (command "xclip" (entlast) ""  "n" "s" xclip_border_ )
        )
        (vla-delete (vlax-ename->vla-object xclip_border_ ))
      )
    ;
    ;
      (command "rectangle" (car block_border_) (cadr block_border_))
      (setq xclip_main_border_ (entlast))
      (setq selection_set_ (TA:select_inside_block xclip_main_border_))
      (command "pselect" selection_set_ "")
      (TA:delete_block_ selection_set_ name_block_ name_block_block_ name_block_hatch_)
      (command "pselect" ss "")
    ;
  ;
  ;hatch_object_process
    ;user input name block 
      (setq name_block_hatch_ (strcat prefix_name num_i "_hatch_"))
    ;
    ;crete_boder_pline
      (create-pline (cadr xclip_border_center_ename_) "close")
      (setq xclip_border_ (entlast))
    ;
    ;make_xcilp_hatch_

      (if (/= (setq selection_set_ (TA:select_inside_hatch xclip_border_)) nil)
        (progn
          (setq xclip_border_center_pt_ (TA:find_center xclip_border_ ))
          (command "_block" name_block_hatch_ xclip_border_center_pt_  selection_set_ "" )
          (command "insert" name_block_hatch_ xclip_border_center_pt_ 1 1 0 )
          (command "xclip" (entlast) ""  "n" "s" xclip_border_ )
        )
        (vla-delete (vlax-ename->vla-object xclip_border_ ))
      )
    ;
  ;
)

(defun c:te ()
  (setq main_border_ (car (entsel)))
  (setq main_border_1 main_border_)
  (setq ss_s_non_block_ (ssget))
  (TA:cutting_object_cto_ main_border_ ss_s_non_block_)
  (setq inside_obj_ (TA:select_inside main_border_))
  (command "pselect" inside_obj_ "")
)






;; Polygon Centroid  -  Lee Mac
;; Returns the WCS Centroid of an LWPolyline Polygon Entity

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

(setq ee (car (entsel)))

(setq aa (LM:PolyCentroid ee))

(command "point" aa )


(setq  as_ename_ (car (entsel "specify Object")))
(setq )




(defun c:VV_MK_ELline_ ()
  ;get_main_line_object_
    (setq A_ename_ (car (entsel "specify Object"))) ;main_line_ename_
    (setq A_obj_ (vlax-ename->vla-object A_ename_)) ;main_line_vlax-obj
  ;
  (setq B_ename_ (car (entsel "specify Object")))
  (setq B_obj_ (vlax-ename->vla-object B_ename_))

  (setq get_cross_pt_ (LM:intersections+0 A_obj_ B_obj_ acextendnone))
  
    (command "circle"
            (car get_cross_pt_)
            1
    ) 
)

 





(initget "")

















 
;
  (defun c:tt2 ()
    

  (setq ename_cut_edge_ (car (entsel)))
  (setq ename_intersection_obj_ (car (entsel)))
  (TA:add_vertex_point_pline_via_breakpt_1 ename_cut_edge_ ename_intersection_obj_)
  )



  (defun c:TEMP:copy_from_basepoint_cfb_ () 


    (setq tar_copy_        (car (entsel))
          tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
          tar_copy_obj_ins (vlax-safearray->list 
                            (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                          )
    )
    ; (command "copy" tar_copy_ "" tar_copy_obj_ins)
    (command "copy" tar_copy_ "" "M"  tar_copy_obj_ins)
  )



  (defun c:sdd ()
    (setq ss_s_non_block_ (ssget  ;"_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                                ;(append bo_line_ (list (car bo_line_)))
                                (list 
                                  (cons 0 "dimension") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                          )
    )
    (command "pselect" ss_s_non_block_)
  )
  (defun c:gt4_ () 
    (setq i 0)

    (while (< i 20) 
      (c:gt1_)
      (setq i (+ i 1))
    )
  )
    
  (defun c:gt1_ ()
    ;user_input_mode_val
      ; (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
      (setq reset_dim_  1 )
    ;
    ;get_data
      (setq spl_ename_ (car (entsel)))
      ; (command "zoom" "o" spl_ename_ "")
      (setq spl_obj_ (vlax-ename->vla-object spl_ename_))
      (setq spl_obj_srt_pt_ (vlax-safearray->list 
                              (vlax-variant-value (vla-get-startpoint spl_obj_))
                            )
      )
      (setq spl_obj_end_pt_ (vlax-safearray->list (vlax-variant-value (vla-get-endpoint spl_obj_))))
      (setq spl_obj_length_ (vla-get-length spl_obj_))
      (setq spl_obj_angle_ (atof (angtos (vla-get-angle spl_obj_))))
      (setq spl_obj_ins_mid_ (TA:midpoint spl_obj_srt_pt_ spl_obj_end_pt_))
      (cond 
        ( (= spl_obj_angle_ 270) 
          (progn 
            (setq new_angle_ 0)
          )
        )
        ( (= spl_obj_angle_ 90) 
          (progn 
            (setq new_angle_ 180)
          )
        )
      )
    ;
    ;insert_command_
      (command "insert" "001 - ALU.ANGLE DYNAMIC - VIEW 1" spl_obj_ins_mid_ 1 new_angle_)
    ;
    ;get_data_dyn_block_
    (setq new_dyn_blk_ (entlast))
    (setq new_dyn_blk_obj_ (vlax-ename->vla-object new_dyn_blk_))
    (setq new_dyn_blk_obj_rotation_ (atof (angtos (vla-get-rotation new_dyn_blk_obj_))))
    (setq new_dyn_blk_obj_ins_pt (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint new_dyn_blk_obj_))
                                )
    )
    ;user_input_data
      (setvar "osmode" 2)
      (setvar "ORTHOMODE" 1)
      
      (setq len_inside_ (getdist new_dyn_blk_obj_ins_pt))
      (setvar "osmode" 1215)
      (setvar "ORTHOMODE" 0)

    ;
    ;dyn_data_input_data_
      (LM:setdynpropvalue new_dyn_blk_obj_ "yy" spl_obj_length_)
      (LM:setdynpropvalue new_dyn_blk_obj_ "len_inside" len_inside_)
    ;

      (cond 
        ( (= reset_dim_ 0) 
          (progn 
            (princ "\n")
          )
        )
        ((and 
            (= reset_dim_ 1)
            (= new_dyn_blk_obj_rotation_ 0)
          ) 
          (progn 
            (vla-put-rotation new_dyn_blk_obj_ (deg-to-rad 180))
          )
        )
        ((and 
            (= reset_dim_ 1)
            (= new_dyn_blk_obj_rotation_ 180)
          ) 
          (progn 
            (vla-put-rotation new_dyn_blk_obj_ (deg-to-rad 0))
          )
        )
      )
  )


  (defun c:gt2_ ()
    

    (setq my-selection-set (ssget 
                            (list 
                              (cons 0 "insert") ;type of object
                              ;  (cons 8 "0,1") ;kind of layer
                              ; (cons 2 "SSSS")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
    )
    (setq ss_filter_1_ (TA:Prop_Filter_ss_set_ my-selection-set "effectivename" "001 - ALU.ANGLE DYNAMIC - VIEW 1"))
    (setq ss_filter_1_i 0)

      (while  (< ss_filter_1_i (sslength ss_filter_1_ ))
        (setq ss_filter_1_ename (ssname ss_filter_1_ ss_filter_1_i))
        (setq ss_filter_1_obj_ (vlax-ename->vla-object ss_filter_1_ename))
        (setq ss_filter_1_obj_ins_ (cadr (TA:ename+vla-get-insertionpoint ss_filter_1_obj_)))
        (setq ss_filter_1_obj_rotaion_ (atof (angtos (vla-get-rotation ss_filter_1_obj_))))
        (cond 
          ( (= ss_filter_1_obj_rotaion_ 0) 
            (progn 
              (setq put_ins (list 
                              (- (car ss_filter_1_obj_ins_) 8.05774877)
                              (cadr ss_filter_1_obj_ins_)
                              (caddr ss_filter_1_obj_ins_)
                            )
              )
              (vla-put-insertionpoint ss_filter_1_obj_ (vlax-3d-point put_ins) )
            )
          )
          ( (= ss_filter_1_obj_rotaion_ 180) 
            (progn 
              (setq put_ins (list 
                              (+ (car ss_filter_1_obj_ins_) 6.94225123)
                              (cadr ss_filter_1_obj_ins_)
                              (caddr ss_filter_1_obj_ins_)
                            )
              )
              (vla-put-insertionpoint ss_filter_1_obj_ (vlax-3d-point put_ins) )
            )
          )
        )
        (setq ss_filter_1_i (+ ss_filter_1_i 1))
      )
  )
    

  (defun c:gt5 ()
    (setq eename_ (car (entsel)))
    (setq eename_obj (vlax-ename->vla-object eename_))
    (setq L_pt (cadr (TA:ename+vla-get-insertionpoint eename_obj)))
    (setq pt (getdist L_pt))
    
    (LM:setdynpropvalue eename_obj "YY" (* pt 2))
    
  )

  (defun c:gt7 ()
    (setq ss_pre_filter_set_xx_ (ssget 
                                (list 
                                  (cons 0 "INSERT") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                              )
    )
    (setq ss_pre_filter_set_xx_i 0)
    (setq checking_result_ins ())
    (setq checking_result_YY ())
    (While (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i) )
      (setq ss_pre_filter_set_xx_ename_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_ename_obj_ins_ (cadr (TA:ename+vla-get-insertionpoint ss_pre_filter_set_xx_ename_obj_)))
      (setq ss_pre_filter_set_xx_ename_obj_ins_newlist (list
                                                        (atoi (rtos (car ss_pre_filter_set_xx_ename_obj_ins_) 2 0))
                                                        (atoi (rtos (cadr ss_pre_filter_set_xx_ename_obj_ins_) 2 0))
                                                        (atoi (rtos (caddr ss_pre_filter_set_xx_ename_obj_ins_) 2 0))
                                                      )
      
      )
      ;main_command_
        (vla-put-insertionpoint ss_pre_filter_set_xx_ename_obj_ (vlax-3d-point ss_pre_filter_set_xx_ename_obj_ins_newlist))
        (setq get_dyn_length_ (LM:getdynpropvalue ss_pre_filter_set_xx_ename_obj_ "YY"))
        (setq set_dyn_length (LM:setdynpropvalue ss_pre_filter_set_xx_ename_obj_ "YY" (atoi (rtos get_dyn_length_ 2 0))) )
      ;
      ;recheck_ins_data
        (setq checking_result_YY (cons ss_pre_filter_set_xx_ename_obj_ins_ checking_result_YY))
        (setq checking_result_ins (cons get_dyn_length_ checking_result_ins))
      ;
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
    (CO:sort_by_val_ 0 checking_result_YY)
    
  )

  (defun c:gt8 ()
    (getvar "osmode")
    (setvar "osmode" 32)
    (setq ins_point (getpoint))
    (setq input_dist (getpoint ins_point))
    (setq dist (- (cadr input_dist) (cadr ins_point)))
    (command "insert" "001 - ALU.ANGLE DYNAMIC - VIEW 1_rev02_" ins_point 1 0)
    (setq ename_dyn_ (entlast))
    (setq ename_dyn_obj_ (vlax-ename->vla-object ename_dyn_))
    (LM:setdynpropvalue ename_dyn_obj_ "XX" 75)
    (LM:setdynpropvalue ename_dyn_obj_ "YY" dist)
    
    
    (setvar "osmode" 1215)
  )
  (defun c:gt9 ()

    (setq ename_dyn_ (car (entsel)))
    (setq ename_dyn_obj_ (vlax-ename->vla-object ename_dyn_))
    (setq get_yy_ (LM:getdynpropvalue ename_dyn_obj_ "YY"))
    (LM:setdynpropvalue ename_dyn_obj_ "XX" 75)
    (LM:setdynpropvalue ename_dyn_obj_ "distance1" 80)
    (LM:setdynpropvalue ename_dyn_obj_ "distance2" (/ get_yy_ 2))
    
    
    (setvar "osmode" 1215)
  )
  (defun c:gt10 ()

    (setq ename_dyn_ (car (entsel)))
    (setq ename_dyn_obj_ (vlax-ename->vla-object ename_dyn_))
    (setq get_yy_ (LM:getdynpropvalue ename_dyn_obj_ "YY"))
    ; (LM:setdynpropvalue ename_dyn_obj_ "XX" 75)
    (LM:setdynpropvalue ename_dyn_obj_ "distance1" 80)
    ; (LM:setdynpropvalue ename_dyn_obj_ "distance2" (/ get_yy_ 2))
    
    
    (setvar "osmode" 1215)
  )
;


            