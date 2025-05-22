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
  (defun TA:get_vertex_len_ (ename)
    ; (setq ref_line (car (entsel)))
    (setq ss (vlax-ename->vla-object ename_))
    (setq vertex_total (length (cadr (TA:Get_Pline_vertext_ins_point_ ename_))))
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
  (defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )
  (defun relocate_ins_ (data_list_ x_sym x_num_ y_sym y_num_ )
    (cond
      ( (= (length data_list_) 2)
        (progn
          (setq data_list_ (list 
                            (x_sym (car data_list_) x_num_)
                            (y_sym (cadr data_list_) y_num_)
                          )
          )
        )
      )
      ( (= (length data_list_) 3)
        (progn
          (setq data_list_ (list 
                            (x_sym (car data_list_) x_num_)
                            (y_sym (cadr data_list_) y_num_)
                            0
                          )
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
    (defun TA:vla-addLine (startPoint endPoint)
      ;; This example adds a line in model space
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      
      ; ;; Define the start and end points for the line
      ; (setq startPoint (vlax-3d-point 1 1 0)
      ;       endPoint (vlax-3d-point 5 5 0))
      (if (= (length startPoint) 2)
        (progn
          (setq startPoint (list 
                            (car startPoint)
                            (cadr startPoint)
                            0
                          )
          )
        )
        (princ "\n")
      )
      (if (= (length endPoint) 2)
        (progn
          (setq endPoint (list 
                            (car endPoint)
                            (cadr endPoint)
                            0
                          )
          )
        )
        (princ "\n")
      )
      (setq startPoint (vlax-3d-point startPoint))
      (setq endPoint (vlax-3d-point endPoint))

      
      ;; Create the line in model space
      (setq modelSpace (vla-get-ModelSpace doc))
      (setq lineObj (vla-AddLine modelSpace startPoint endPoint))
      ; (vla-ZoomAll acadObj)
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
      (setq block-name "001 - DYNAMIC LV" )
      (setq allow-exploding :vlax-false )
      (setq allow-exploding :vlax-true )
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
  (defun C:Dumpit ( / ent) 
    (while (setq ent (entsel)) 
      (vlax-Dump-Object 
        (vlax-Ename->Vla-Object (car ent)) 
      ) 
    ) 
    (princ) 
  )
  (defun c:expl-p () 
    (vl-load-com)
    (vlax-for b 
      (vla-get-Blocks 
        (vla-get-ActiveDocument (vlax-get-acad-object))
      )
      (or (wcmatch (vla-get-Name b) "`**_Space*") 
          (vla-put-explodable b :vlax-false)
      )
    )
    (princ)
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
;


(defun TA:Recreate_RECTANGLE_rece_ (test_pline_ename_)
  ;get_data_
    ; (setq test_pline_ename_ (car (entsel)))
    
    (setq vertex_data_ (TA:Get_Pline_vertext_ins_point_ test_pline_ename_))
    (setq vertex_data_ins_ (cadr (TA:Get_Pline_vertext_ins_point_ test_pline_ename_)))
    (setq mid_ins_pt_ (LM:PolyCentroid test_pline_ename_))
    (command "point" mid_ins_pt_ )
  ;
  ;re_Ins_location
    (if (and (/= vertex_data_ nil) (= (length vertex_data_ins_ ) 4))
      (progn
        (setq vertex_data_ins_i 0)
        (while (< vertex_data_ins_i (length vertex_data_ins_)) 
          (setq vertex_data_ins_pt_ (nth vertex_data_ins_i vertex_data_ins_))

          
        (cond
          ( 
            (and 
              (< (car vertex_data_ins_pt_) (car mid_ins_pt_))   ;axis X lessthan X center = 1
              (< (cadr vertex_data_ins_pt_) (cadr mid_ins_pt_)) ;axis Y lessthan Y center = 1
            ) 
            (progn 
              (setq ins_1 vertex_data_ins_pt_)
              (command "circle" vertex_data_ins_pt_ 1)
            )
          )
          ( 
            (and 
              (> (car vertex_data_ins_pt_) (car mid_ins_pt_))   ;axis X morethan X center = 2
              (< (cadr vertex_data_ins_pt_) (cadr mid_ins_pt_)) ;axis Y lessthan Y center = 2
            )
            (progn 
              (setq ins_2 vertex_data_ins_pt_)
              (command "circle" vertex_data_ins_pt_ 2)
            )
          )
          (
            (and 
              (> (car vertex_data_ins_pt_) (car mid_ins_pt_))   ;axis X morethan X center = 3
              (> (cadr vertex_data_ins_pt_) (cadr mid_ins_pt_)) ;axis Y morethan Y center = 3
            )
            (progn 
              (setq ins_3 vertex_data_ins_pt_)
              (command "circle" vertex_data_ins_pt_ 3)
            )
          )
          (
            (and 
              (< (car vertex_data_ins_pt_) (car mid_ins_pt_))   ;axis X lessthan X center = 4
              (> (cadr vertex_data_ins_pt_) (cadr mid_ins_pt_)) ;axis Y morethan Y center = 4
            )
            (progn 
              (setq ins_4 vertex_data_ins_pt_)
              (command "circle" vertex_data_ins_pt_ 4)
            )
          )
        )


          (setq vertex_data_ins_i (+ vertex_data_ins_i 1))
        )
      )
      (princ "ss")
    )
  ;
  ;summary_ins_location
    (setq new_pline_ins_ (list ins_1 ins_2 ins_3 ins_4 ))
  ;
  ;leftest_range_x_axis
    (setq leftest_range_x_axis (car (CO:sort_by_val_ 0 (list ins_1  ins_4 ))))
    (setq leftest_range_x_axis_offset_ (list 
                                         (- (car leftest_range_x_axis) 5)
                                         (cadr leftest_range_x_axis)
                                       )
    )
  ;
  ;rightest_range_x_axis
    (setq rightest_range_x_axis (car (reverse (CO:sort_by_val_ 0 (list ins_2  ins_3 )))))
    (setq rightest_range_x_axis_offset_ (list 
                                          (+ (car rightest_range_x_axis) 5)
                                          (cadr rightest_range_x_axis)
                                        )
    )
  ;
  ;upest_range_x_axis
    (setq upest_range_x_axis (car (reverse (CO:sort_by_val_ 1 (list ins_3  ins_4 )))))
    (setq upest_range_x_axis_offset_ (list 
                                          (car upest_range_x_axis)
                                          (+ (cadr upest_range_x_axis) 5)
                                        )
    )
  ;
  ;lower_range_x_axis
    (setq lower_range_x_axis (car (CO:sort_by_val_ 1 (list ins_1  ins_2 ))))
    (setq lower_range_x_axis_offset_ (list 
                                          (car lower_range_x_axis)
                                          (- (cadr lower_range_x_axis) 5)
                                        )
    )
  ;
  (if ;left_right_logic
    (and 
      (/= vertex_data_ nil) 
      (= (length vertex_data_ins_ ) 4)
    )
    (progn
      (if ;left_side_bot_open_ang_
        (and 
          (= (length new_pline_ins_) 4)
          (> (rad-to-deg (angle ins_1 ins_2)) 270)
        )
        (progn
          (command "DIMLINEAR"  ins_2 ins_1 "v" leftest_range_x_axis_offset_)
          
          (command "DIMCONTINUE" ins_4 "" "" )
          (setq left_side_bot_open_ang_ "T")
        )
        (command "DIMLINEAR"  ins_1 ins_4 "v" leftest_range_x_axis_offset_)
      )
      (if ;left_side_top_open_ang_
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_4 ins_3)) 0)
          (< (rad-to-deg (angle ins_4 ins_3)) 90)
        )
        (progn
          (command "DIMLINEAR"  ins_4 ins_3 "v" leftest_range_x_axis_offset_)
          (setq left_side_top_open_ang_ "T")

        )
        (setq left_side_top_open_ang_ "T")
      )
      (if ;right_side_bot_open_ang_
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_1 ins_2)) 0)
          (< (rad-to-deg (angle ins_1 ins_2)) 90)
        )
        (progn
          
          (command "DIMLINEAR"  ins_1 ins_2  "v" rightest_range_x_axis_offset_)
          
          (command "DIMCONTINUE" ins_3 "" "" )
        )
        (command "DIMLINEAR"  ins_2 ins_3  "v" rightest_range_x_axis_offset_)
      )
      (if ;right_side_top_open_ang_
        (and 
          (= (length new_pline_ins_) 4)
          (> (rad-to-deg (angle ins_3 ins_4)) 90)
          (< (rad-to-deg (angle ins_3 ins_4)) 180)
        )
        (progn 
          (command "DIMLINEAR" ins_3 ins_4 "v" rightest_range_x_axis_offset_)
        )
        (setq right_side_top_open_ang_ "F")
      )
    )
    (princ "\n")
  )
  (if ;upper_side
    (and
      (/= vertex_data_ nil) 
      (= (length vertex_data_ins_ ) 4)
    )
    (progn
      (if ;upper_side_bot_open_ang_left_side
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_1 ins_4)) 90)
          (> (rad-to-deg (angle ins_1 ins_4)) 0)
          (< (rad-to-deg (angle ins_1 ins_4)) 90)
          
          
        )
        (progn
          (command "DIMLINEAR"  ins_1 ins_4 "h" upest_range_x_axis_offset_)
          
          (command "DIMCONTINUE" ins_3 "" "" )
          (setq left_side_bot_open_ang_ "T")
        )
        (command "DIMLINEAR"  ins_4 ins_3 "h" upest_range_x_axis_offset_)
      )
      (if ;upper_side_bot_open_ang_right_side
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_2 ins_3)) 90)
          (> (rad-to-deg (angle ins_2 ins_3)) 90)
          (< (rad-to-deg (angle ins_2 ins_3)) 180)
        )
        (progn
          (command "DIMLINEAR"  ins_3 ins_2 "h" upest_range_x_axis_offset_)
          (setq left_side_bot_open_ang_ "T")
        )
        (setq left_side_bot_open_ang_ "F")
      )
    )
  )
  (if ;lower_side
    (and
      (/= vertex_data_ nil) 
      (= (length vertex_data_ins_ ) 4)
    )
    (progn
      (if ;lower_side_bot_open_ang_left_side
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_1 ins_4)) 90)
          (> (rad-to-deg (angle ins_1 ins_4)) 90)
          (< (rad-to-deg (angle ins_1 ins_4)) 180)
          
          
        )
        (progn
          (command "DIMLINEAR"  ins_4 ins_1 "h" lower_range_x_axis_offset_)
          
          (command "DIMCONTINUE" ins_2 "" "" )
          (setq left_side_bot_open_ang_ "T")
        )
        (command "DIMLINEAR"  ins_1 ins_2 "h" lower_range_x_axis_offset_)
      )
      (if ;upper_side_bot_open_ang_right_side
        (and 
          (= (length new_pline_ins_) 4)
          (/= (rad-to-deg (angle ins_2 ins_3)) 0)
          (> (rad-to-deg (angle ins_2 ins_3)) 0)
          (< (rad-to-deg (angle ins_2 ins_3)) 90)
        )
        (progn
          (command "DIMLINEAR"  ins_3 ins_2 "h" lower_range_x_axis_offset_)
          (setq left_side_bot_open_ang_ "T")
        )
        ; (command "DIMLINEAR"  ins_2 ins_3 "h" lower_range_x_axis_offset_)
      )
    )
  )
  
  
)

(defun c:ctct_ct ()
  (setq test_pline_ename_ (car (entsel)))
(TA:Recreate_RECTANGLE_rece_ test_pline_ename_)
)


(defun c:mlct_ ()
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
        (TA:Recreate_RECTANGLE_rece_ ss_pre_filter_set_xx_ename_)
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)




(defun c:cx1 ()
  ;setvar
    (setvar "osmode" 0)
  ;
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "I" 
                                          (list 
                                            (cons 0 "line") ;type of object
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
                                        (cons 0 "line") ;type of object
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
  ;user_input_
    ; (setq getnum 0.085)
    (setq getnum (cond ( (getreal (strcat "\nAL.ceiling 85F width<" (rtos (setq getnum (cond (getnum) (1.0) ) ) ) "> : " ) ) ) (getnum) ) )
    
    (setq auto_angle (cond ( (getint (strcat "\nauto_angle \n1 = on 0 = off\n<" (rtos (setq auto_angle (cond (auto_angle) (1.0) ) ) ) "> : " ) ) ) (auto_angle) ) )
  
    (setq new_num_width (- (/ getnum 1000) 0.00697500))
  ;
  ;get_data
    ; (setq blk_data_ename_obj_ (vlax-ename->vla-object blk_data_ename_))
    ; ; (setq blk_data_ename_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_data_ename__obj_))))
   
    ; (setq get_point_data_ (relocate_ins_ blk_data_ename_obj_ins_ + W1 + 0 ))
  ;
  ;preloop_and_while
   (setq ss_pre_filter_set_xx_i 0)
   (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      ;get_data_line
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq ss_pre_filter_set_xx_obj_start_end_pt_ (list
                                        (setq ss_pre_filter_set_xx_obj_startpt_ (vlax-safearray->list (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_obj_ ) ) ) )
                                        (setq ss_pre_filter_set_xx_obj_endpt_ (vlax-safearray->list (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_obj_ )) ) )
                                      )
        )
        (setq ss_pre_filter_set_xx_obj_length (vla-get-length ss_pre_filter_set_xx_obj_))
        (setq ss_pre_filter_set_xx_obj_angle (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_obj_)))
        (cond
          (;a180
            (and 
              (= ss_pre_filter_set_xx_obj_angle 180)
            )
            (progn
            (setq get_new_pt_data (relocate_ins_ ss_pre_filter_set_xx_obj_startpt_ + 0 - 0.00697500))
            )
          )
          (;a0
            (and 
              
              (= ss_pre_filter_set_xx_obj_angle 0)
              
            )
            (progn
            (setq get_new_pt_data (relocate_ins_ ss_pre_filter_set_xx_obj_startpt_ + 0 + 0.00697500))
            )
          )
          (;a90
            (and 
              
              (= ss_pre_filter_set_xx_obj_angle 90)
              
            )
            (progn
            (setq get_new_pt_data (relocate_ins_ ss_pre_filter_set_xx_obj_startpt_ + new_num_width + 0))
            )
          )
          (;a270
            (and 
              (= ss_pre_filter_set_xx_obj_angle 270)
            )
            (progn
            (setq get_new_pt_data (relocate_ins_ ss_pre_filter_set_xx_obj_startpt_ + 0.00697500 + 0))
            )
          )
        )
      ;
      
      ;insert_dynamic_block_obj
        ; (TA:set-block-blockscaling "000TYP-AL-LINEAR_CEILINGS_85F-FRONT_VIEW" acUniform)
        (command "insert" "000TYP-AL-LINEAR_CEILINGS_85F-FRONT_VIEW_REV01" get_new_pt_data 0.001 ss_pre_filter_set_xx_obj_angle )
      ;
      ;entlast_new_object_
        (setq entlast_new_object_ename_ (entlast))
        (setq entlast_new_object_obj_ (vlax-ename->vla-object entlast_new_object_ename_))
        (setq L (LM:setdynpropvalue entlast_new_object_obj_ "L" ss_pre_filter_set_xx_obj_length))
        (setq W (LM:setdynpropvalue entlast_new_object_obj_ "W" (/ getnum 1000)))

        (if 
          (or 
            (= ss_pre_filter_set_xx_obj_angle 180)
            (= ss_pre_filter_set_xx_obj_angle 0)
          )
          (progn 
            (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0.17))
            (setq L (LM:setdynpropvalue entlast_new_object_obj_ "L" (- ss_pre_filter_set_xx_obj_length 0.085)))
          )
          (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0))
        )
     
        (cond
          (;(princ "a0180") 
            (and
              (= auto_angle 0)
              (= ss_pre_filter_set_xx_obj_angle 180)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0.085))
              (setq L (LM:setdynpropvalue entlast_new_object_obj_ "L" ss_pre_filter_set_xx_obj_length))
              (princ "a0180")
            )
            
          )
          (;(princ "a1180")
            (and
              (= auto_angle 1)
              (= ss_pre_filter_set_xx_obj_angle 180)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0.17))
              (princ "a1180")
            )
            
          )
          (;(princ "a090")
            (and
              (= auto_angle 0)
              (= ss_pre_filter_set_xx_obj_angle 90)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0.085))
              (princ "a090")
            )
            
          )
          (;(princ "a190")
            (and
              (= auto_angle 1)
              (= ss_pre_filter_set_xx_obj_angle 90)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0))
              (princ "a190")
            )
            
          )
          (;(princ "a1270")
            (and
              (= auto_angle 1)
              (= ss_pre_filter_set_xx_obj_angle 270)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0))
              (setq L (LM:setdynpropvalue entlast_new_object_obj_ "L" (+ ss_pre_filter_set_xx_obj_length 0.085)))
              (princ "a190")
            )
            
          )
          (;(princ "a0270")
            (and
              (= auto_angle 1)
              (= ss_pre_filter_set_xx_obj_angle 270)
            )
            (progn 
              (setq ang (LM:setdynpropvalue entlast_new_object_obj_ "ang" 0.085))
              
              (princ "a190")
            )
            
          )
        )


      ;
    (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
   )
  ;
  ;setvar
    (setvar "osmode" 1215)
  ;
  
)



(defun c:ResetangLine_r11 ()
  ;setvar
    (setvar "osmode" 0)
  ;
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "I" 
                                          (list 
                                            (cons 0 "line") ;type of object
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
                                        (cons 0 "line") ;type of object
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
  ;user_input_
    (setq getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq getang_ (cond (getang_) (1.0) ) ) ) "> : " ) ) ) (getang_) ) )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_)) 
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ) )
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_obj_ ))
                                                             )
                                      )
                                    )
      )
      (setq ss_pre_filter_set_xx_obj_angle_ (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_obj_)))
      (setq ss_pre_filter_set_xx_obj_midpt (TA:midpoint ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_obj_endpt_ ))
      (setq ss_pre_filter_set_xx_obj_midpt_0 (append ss_pre_filter_set_xx_obj_midpt (list 0)))
      (command "rotate" ss_pre_filter_set_xx_ename_ "" ss_pre_filter_set_xx_obj_midpt_0 "R" ss_pre_filter_set_xx_obj_angle_ getang_  )
      
    
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;setvar
    (setvar "osmode" 1215)
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


; (setq ss_ename_ (car (entsel "specify Object")))
; (setq ss_obj_ (vlax-ename->vla-object ss_ename_))
; (rad-to-deg (vla-get-angle ss_obj_ ))

(defun c:DAFT_block_to_line_ ()
  ;this code must work together with dynamic block name "001 - DYNAMIC subfame"
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "I" 
                                          (list 
                                            (cons 0 "line") ;type of object
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
                                        (cons 0 "line") ;type of object
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
                ; (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
              )
              (alert "Object is not block.")
            )
          ;
        ;
      )
    )
  ;
  ;sorting_selection_set_
   (setq ss_pre_filter_set_xx_i 0)
   (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
    (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
    (setq ss_pre_filter_set_xx_ename_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
    (setq ss_pre_filter_set_xx_ename_start_end_pt_ (list 
                                                     (setq ss_pre_filter_set_xx_ename_startpt_ (vlax-safearray->list 
                                                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_ename_obj_ ))
                                                                                               ) 
                                                     )
                                                     (setq ss_pre_filter_set_xx_ename_endpt_ (vlax-safearray->list 
                                                                                               (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_ename_obj_ ))
                                                                                             ) 
                                                     )
                                                   )
    )
    (setq ss_pre_filter_set_xx_ename_obj_get_length_ (vla-get-length ss_pre_filter_set_xx_ename_obj_))
    (setq ss_pre_filter_set_xx_ename_midpt_ (TA:midpoint ss_pre_filter_set_xx_ename_startpt_ ss_pre_filter_set_xx_ename_endpt_ ))
    
    
    (setq ss_pre_filter_set_xx_ename_midpt_up (relocate_ins_ ss_pre_filter_set_xx_ename_midpt_ + 0 + (/ 50 2)))
     
      ;main_idea_code
        (command "insert" "001 - DYNAMIC subfame" ss_pre_filter_set_xx_ename_midpt_up 1 0)
        (setq blk_new_ename_ (entlast))
        (setq blk_new_obj_ (vlax-ename->vla-object blk_new_ename_))
        (setq H (LM:setdynpropvalue blk_new_obj_ "H" 50))
        (setq W (LM:setdynpropvalue blk_new_obj_ "W" ss_pre_filter_set_xx_ename_obj_get_length_))
     
        (vla-delete ss_pre_filter_set_xx_ename_obj_)
      ;
     
     (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
   )
  ;
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


(defun TA:REC-TO-SINGLE_LINE_R2L (rec_ename_ )
  ;user_input_data_
    ; (setq rec_ename_ (car (entsel "specify Object")))
  ;
  
  ;get_data
    (setq rec_obj_ (vlax-ename->vla-object rec_ename_))
    (setq rec_obj_geometric_center_pt_ (TA:find_center rec_ename_))
    (setq rec_obj_vertex_pt (TA:Get_Pline_vertext_ins_point_ rec_ename_))
    (setq rec_obj_max_vertex_len (car (vl-sort (TA:get_vertex_len_ rec_ename_) '>)))
  ;
  ;put_startpoint_for_object
    (setq start_pt_ (list
                      (car rec_obj_geometric_center_pt_)
                      (- (cadr rec_obj_geometric_center_pt_) (/ rec_obj_max_vertex_len 2))
                    )
    )
    (setq ent_pt_ (list
                      (car rec_obj_geometric_center_pt_)
                      (+ (cadr rec_obj_geometric_center_pt_) (/ rec_obj_max_vertex_len 2))
                    )
    )
  ;
  ;
  (TA:vla-addLine start_pt_ ent_pt_)
)


(defun c:TA:MULTI_REC-TO-SINGLE_LINE_MR2L ()
  ;user_input_
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "POLYLINE,LWPOLYLINE") ;type of object
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
                                          (cons 0 "POLYLINE,LWPOLYLINE") ;type of object
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
    ;delete_ref_object
      ; (setq delete_obj_val_ nil)
      (initget 1 "Yes No")
      (if ;ชุดคำสั่งสำหรับ เก็บตัวแปร Getkword 
        (= delete_obj_val_ nil ) 
        (progn
          ;main_idea_command
            (setq delete_obj_val_ "Yes")
            (setq delete_obj_val_ (getkword (strcat "\nChoose an option to delete Object [Yes/No] <" delete_obj_val_ ">: ")))
          ;
        )
        (setq delete_obj_val_ (getkword (strcat "\nChoose an option to delete Object [Yes/No] <" delete_obj_val_ ">: ")))
      )
    ;
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (command "pselect" ss_pre_filter_set_xx_ename_ "" )

        ;main_idea_code
          (TA:REC-TO-SINGLE_LINE_R2L ss_pre_filter_set_xx_ename_)
          (if (= delete_obj_val_ "Yes")
            (progn
              (vla-delete (setq ss_pre_filter_set_xx_ename_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))
            )
          )
        ;

      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  
)


(defun c:multi_3dsweep_MUSW_ ()
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx_ (ssget "I" 
                                          (list 
                                            (cons 0 "LINE") ;type of object
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
                                        (cons 0 "LINE") ;type of object
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
      
      (command "rectangle" "0,0" "100,100")
      (setq eee_ename_ (entlast))
      (command "sweep" eee_ename_ "" "b"  "50,100" ss_pre_filter_set_xx_ename_ )
      
      
  
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  
  
)








