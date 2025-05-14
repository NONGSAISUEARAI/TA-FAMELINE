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
    (defun LM-TA:getdynprops ( blk )
        (mapcar '(lambda ( x ) (vla-get-propertyname x))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
    (defun LM-TA:getdynvals ( blk )
        (mapcar '(lambda ( x ) (vlax-get x 'value))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
    (defun LM-TA:getdynprops+vals ( blk )
        (mapcar '(lambda ( x ) (list (vla-get-propertyname x) (vlax-get x 'value)))
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
  (defun LM:StringSubst ( new old str / inc len )
    (setq len (strlen new)
          inc 0
    )
    (while (setq inc (vl-string-search old str inc))
        (setq str (vl-string-subst new old str inc)
              inc (+ inc len)
        )
    )
    str
  )
  (defun TA:remove-numbers (remove_list_)
    (if (listp remove_list_)
      (progn
        ;preloop_and_while
          (setq remove_list_value_ ())
          (setq remove_list_i 0)
          (while (< remove_list_i (length remove_list_))
            (setq remove_list_ename_ (nth  remove_list_i remove_list_))
            (setq renew_list_ename_ 
              (vl-string-subst "" "0" 
                (vl-string-subst "" "1" 
                  (vl-string-subst "" "2" 
                    (vl-string-subst "" "3" 
                      (vl-string-subst "" "4" 
                        (vl-string-subst "" "5" 
                          (vl-string-subst "" "6" 
                            (vl-string-subst "" "7" 
                              (vl-string-subst "" "8" 
                                (vl-string-subst "" "9" remove_list_ename_))))))))))
            )
            (setq remove_list_value_ (cons renew_list_ename_ remove_list_value_ ))
            (setq remove_list_i (+ remove_list_i 1))
          )
          (setq remove_list_value_ (reverse remove_list_value_) )
        ;
      )
      (setq remove_list_ 
        (vl-string-subst "" "0" 
          (vl-string-subst "" "1" 
            (vl-string-subst "" "2" 
              (vl-string-subst "" "3" 
                (vl-string-subst "" "4" 
                  (vl-string-subst "" "5" 
                    (vl-string-subst "" "6" 
                      (vl-string-subst "" "7" 
                        (vl-string-subst "" "8" 
                          (vl-string-subst "" "9" remove_list_))))))))))
      )
    )
  )
  (defun TA:remove-alphabet (remove_list_)
    (if (listp remove_list_)
      (progn
        ;preloop_and_while
          (setq remove_list_value_ ())
          (setq remove_list_i 0)
          (while (< remove_list_i (length remove_list_))
            (setq remove_list_ename_ (nth  remove_list_i remove_list_))
            (setq remove_list_ename_
              (vl-string-subst "" "A"
                (vl-string-subst "" "B"
                  (vl-string-subst "" "C"
                    (vl-string-subst "" "D"
                      (vl-string-subst "" "E"
                        (vl-string-subst "" "F"
                          (vl-string-subst "" "G"
                            (vl-string-subst "" "H"
                              (vl-string-subst "" "I"
                                (vl-string-subst "" "J"
                                  (vl-string-subst "" "K"
                                    (vl-string-subst "" "L"
                                      (vl-string-subst "" "M"
                                        (vl-string-subst "" "N"
                                          (vl-string-subst "" "O"
                                            (vl-string-subst "" "P"
                                              (vl-string-subst "" "Q"
                                                (vl-string-subst "" "R"
                                                  (vl-string-subst "" "S"
                                                    (vl-string-subst "" "T"
                                                      (vl-string-subst "" "U"
                                                        (vl-string-subst "" "V"
                                                          (vl-string-subst "" "W"
                                                            (vl-string-subst "" "X"
                                                              (vl-string-subst "" "Y"
                                                                (vl-string-subst "" "Z" remove_list_ename_)))))))))))))))))))))))))
              )
            )
            (setq remove_list_value_ (cons renew_list_ename_ remove_list_value_ ))
            (setq remove_list_i (+ remove_list_i 1))
          )
          (setq remove_list_value_ (reverse remove_list_value_) )
      )
      (setq remove_list_
        (vl-string-subst "" "A"
          (vl-string-subst "" "B"
            (vl-string-subst "" "C"
              (vl-string-subst "" "D"
                (vl-string-subst "" "E"
                  (vl-string-subst "" "F"
                    (vl-string-subst "" "G"
                      (vl-string-subst "" "H"
                        (vl-string-subst "" "I"
                          (vl-string-subst "" "J"
                            (vl-string-subst "" "K"
                              (vl-string-subst "" "L"
                                (vl-string-subst "" "M"
                                  (vl-string-subst "" "N"
                                    (vl-string-subst "" "O"
                                      (vl-string-subst "" "P"
                                        (vl-string-subst "" "Q"
                                          (vl-string-subst "" "R"
                                            (vl-string-subst "" "S"
                                              (vl-string-subst "" "T"
                                                (vl-string-subst "" "U"
                                                  (vl-string-subst "" "V"
                                                    (vl-string-subst "" "W"
                                                      (vl-string-subst "" "X"
                                                        (vl-string-subst "" "Y"
                                                          (vl-string-subst "" "Z" remove_list_)))))))))))))))))))))))))
        )
      )
      ;
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
  (defun TA:standard_list_croodinate+ename (ss_post_filter_set_  ) ;เรียง object ตามแนวแกน ใน selection set
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
          (setq sum_data (TA:vla-get-insertionpoint+ename ss_post_filter_set_obj))
          (setq sum_data_ (cons sum_data sum_data_))
        ;
        (setq ss_post_filter_set_i (+ ss_post_filter_set_i 1))
      )
      (setq sum_data_ sum_data_)
      ;
    ;
  )
  (defun TA:corrd_sorting_group (all_vlains_ Toerrance_num_value) ;ก่อนจะใช้คำสั่งชุดนี้ ควรใช้ TA:standard_list_croodinate+ename กับ lambda sort โดยเอาแกน x เรียง ซ้ายไปขาวบนลงล่าง
    ;Note By Code_Developer
    ;principle of codework is designed for finding coordinate in list and create main row 
    ;The main row will be used as fundamental for sorting each column in coordinate list 
    ;Fully command must have sub-functions with names starting with TA: or LM:
    ;
    ;argument list
    ;all_vlains_        =  all coordinate in list 
    ;                       (standard form for all_vlains argument must be 
    ;                        (
    ;                         (x y z)entity_name
    ;                         (x y z)entity_name
    ;                         (x y z)entity_name
    ;                        )
    ;TA:Toerrance_num_  = is argument from Sub FUNC name "TA:Toerrance_num_"
    ;                     that specify and decide coordinate by calculate spacing 
    ;                     coodinate to coodinate is same column or not in row/col
    ;
    ;main idea of codework
    ;first step is always keep lowest insertion coordinate point in variable name "test_vlains_ename_1st"
    ;second step by setting "test_vlains_ename_1st" as main corrdinate that let other coordinates pass
    ;into the loop for checking whether they are in the same row/column,via using SUB_FUNC name TA:Toerrance_num_
    ;
    ;
    ;creating main rows process
      ;preloop_and_while creating main row in list 
        (setq test_vlains_ename_1st (nth 0 all_vlains_))
        (setq all_vlains_i 0)
        (setq main_row ())
        (setq main_row (cons test_vlains_ename_1st main_row))
        (while (< all_vlains_i (length all_vlains_))
          (setq all_vlains_ename_subrow_ (nth  all_vlains_i all_vlains_))
          (if 
            (and 
              (/= test_vlains_ename_1st all_vlains_ename_subrow_)
              (TA:Toerrance_num_ 
                (car (car test_vlains_ename_1st))
                (car (car all_vlains_ename_subrow_))
                Toerrance_num_value
              )
            )
            (progn 
              (setq main_row (cons all_vlains_ename_subrow_ main_row))
              (length main_row)
            )
          )
          (setq all_vlains_i (+ all_vlains_i 1))
        )
      ;
      ;TA:stanndard_lambda_sorting_Y-axis
        (setq main_row_sorted_Y_ (vl-sort main_row ;bigest open indent list
                                                  (function 
                                                    (lambda (a b) 
                                                      (> (nth 1 (car a)) (nth 1 (car b)))
                                                    )
                                                  )
                              ) ;bigest close indent list
        )
      ;
    ;
    ;creating sub rows process
      ;preloop_and_while main row loop first loop
        (setq main+sub_row_ ())
        (setq main_row_sorted_Y_i 0)
        (while (< main_row_sorted_Y_i (length main_row_sorted_Y_))
          (setq main_row_sorted_Y_main_set_ (nth  main_row_sorted_Y_i main_row_sorted_Y_))
            ;preloop_and_while main row loop second loop
              (setq all_vlains_i 0)
              (setq sub_row_ ())
              (while (< all_vlains_i (length all_vlains_))
                (setq all_vlains_subrow_ (nth  all_vlains_i all_vlains_))
                  (if 
                    (and 
                      (/= test_vlains_ename_1st all_vlains_ename_subrow_)
                      (TA:Toerrance_num_ 
                        (cadr (car all_vlains_subrow_))
                        (cadr (car main_row_sorted_Y_main_set_))
                        10 ;do not use argument from TA:corrd_sorting_group
                      )
                    )
                    (progn 
                      (setq sub_row_ (cons all_vlains_subrow_ sub_row_ ))
                      (length sub_row_)
                      ;TA:stanndard_lambda_sorting
                      (setq sub_row_ (vl-sort sub_row_  ;bigest open indent list
                                                                (function 
                                                                  (lambda (a b) 
                                                                    (< (nth 0 (car a)) (nth 0 (car b)))
                                                                  )
                                                                )
                                            ) ;bigest close indent list
                      )
                      
                      ;
                    )
                  )
                (setq all_vlains_i (+ all_vlains_i 1))
              )
            ;
          (setq main+sub_row_ (cons sub_row_ main+sub_row_))
          ; (format-data main+sub_row_)
          (length main+sub_row_)
          (setq main_row_sorted_Y_i (+ main_row_sorted_Y_i 1))
        )
      ;
      ;TA:stanndard_lambda_sorting
        ;88 211 342 405 463 คือกลุ่มตัวเลขที่ต้องการจะใช้เรียงในกลุ่ม ;main
        ;จากที่กล่าวมา ถ้าต้องเรียงกลุ่ม ;main จะทำการดึงตัวแปรกลุ่ม ;main 
        ;เพื่อตั้งต้นก่อน จะได้ผลลัพธ์แบบนี้
        ; (setq main 
        ;   (list ;main
        ;     (list(list 138.044 88.6521 0.0) "<Entity name: 23abbecab20>") 
        ;     (list(list 288.589 90.6521 0.0) "<Entity name: 23abbecabb0>")
        ;     (list(list 392.914 92.6521 0.0) "<Entity name: 23abbecabe0>")
        ;   )
        ; )
        ;ซึ่ง main ในที่นี้ คือตัวแทน a b ใน lambda โดยเป็น main คือ list ย่อยของ main+sub_row_
        ;วิธีตรวจสอบำตอบง่ายๆ คือ ให้ ตั้งการดึงข้อมูลย่อยจาก main
        ;สมมุติว่าใช้ (cadr (nth 0 (nth 0 main))) แล้วได้ตัวเลขในกลุ่มดังกล่าว [88 211 342 405 463]
        ;แสดงว่าถูกต้อง
        ; (setq main+sub_row_ 
        ;   (list
        ;     (list ;main
        ;       (list(list 138.044 88.6521 0.0) "<Entity name: 23abbecab20>") 
        ;       (list(list 288.589 90.6521 0.0) "<Entity name: 23abbecabb0>")
        ;       (list(list 392.914 92.6521 0.0) "<Entity name: 23abbecabe0>")
        ;     ) 
        ;     (list ;main
        ;       (list(list 147.328 211.264 0.0) "<Entity name: 23abbecab50>") 
        ;       (list(list 288.589 210.776 0.0) "<Entity name: 23abbecabc0>")
        ;       (list(list 392.914 212.776 0.0) "<Entity name: 23abbecabf0>")
        ;     )
        ;     (list ;main
        ;       (list(list 100.205 342.1 0.0) "<Entity name: 23abbecab80>") 
        ;       (list(list 288.589 344.1 0.0) "<Entity name: 23abbecabd0>")
        ;       (list(list 392.914 346.1 0.0) "<Entity name: 23abbecac00>")
        ;     )
        ;     (list ;main
        ;       (list(list 119.914 405.524 0.0) "<Entity name: 23abbecaed0>") 
        ;       (list(list 251.971 407.524 0.0) "<Entity name: 23abbecaee0>")
        ;       (list(list 356.296 409.524 0.0) "<Entity name: 23abbecaef0>")
        ;     )
        ;     (list ;main
        ;       (list(list 156.532 463.701 0.0) "<Entity name: 23abbecaf60>") 
        ;       (list(list 288.589 465.701 0.0) "<Entity name: 23abbecaf70>")
        ;       (list(list 392.914 467.701 0.0) "<Entity name: 23abbec7800>")
        ;     )
        ;   )
        ; )
        (setq main+sub_row_sorted_ (vl-sort main+sub_row_  ;bigest open indent list
                                            (function 
                                              (lambda (a b) 
                                                (>
                                                  (cadr (nth 0 (nth 0 a)))
                                                  (cadr (nth 0 (nth 0 b)))
                                                  
                                                )
                                              )
                                            )
                                  ) ;bigest close indent list
        )
        ; (format-data main+sub_row_)
        (setq main+sub_row_sorted_ main+sub_row_sorted_)
      ;
      
    ;
    ;
  )
  (defun TA:custom-sort (user_custom_sort_ target_custom_sort_ )
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a list array 
    ;principle of codework is designed for sorting by user custom, Then user can create custom in array list-form
    ;
    ;example argument
      ; (setq user_custom_sort_ '("Cherry" "Banana" "Apple" "Mango"))
      ; (setq target_custom_sort_  '("Cherry" "Banana" "Cherry" "Banana" "Cherry" "Banana" "Apple" "Mango" "Apple" "Mango" "Apple" "Mango"))
    ;
    ;
    ;preloop_and_while
      (setq new_sort_ ())
      (setq user_custom_sort_i 0)
      (while (< user_custom_sort_i (length user_custom_sort_))
        (setq user_custom_sort_tag_ (nth  user_custom_sort_i user_custom_sort_))
        
        ;preloop_and_while
          (setq target_custom_sort_i 0)
          (while (< target_custom_sort_i (length target_custom_sort_))
            (setq target_custom_sort_val_ (nth  target_custom_sort_i target_custom_sort_))
            
            (if (= target_custom_sort_val_ user_custom_sort_tag_ )
              (progn
                (setq new_sort_ (cons target_custom_sort_val_ new_sort_))
              )
            )
            (setq target_custom_sort_i (+ target_custom_sort_i 1))
          )
        ;
    
        (setq user_custom_sort_i (+ user_custom_sort_i 1))
      )
    ;
    (setq new_sort_ new_sort_)
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
;Filter Selection Set by Effectivename Ta Trai  merge
  ;Note By Code_Developer
  ;
  ;Fully command must have sub-functions with names starting with TA: or LM:
  ;
  ;
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
  (defun TA:remove_val_list_ (val_list_ target_list_)
    ;example
      ; (setq target_list_ (list 1 2 3 4 5 6 7 8 9 nil))
      ; ; (setq target_list_ (list "ปรับปรุงอาตาร_DERMA_HEALTH_คลอ" "ง9_DB_STUDIO" ""))
      ; (setq val_list_ 2) 
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
  (defun TA:remove_val_list_rev02_ (val_list_ target_list_)
    ;example
      ; (setq target_list_ (list 1 2 3 4 5 2 6 7 8 9 nil))
      ; (setq target_list_ (list "ปรับปรุงอาตาร_DERMA_HEALTH_คลอ" "ง9_DB_STUDIO" ""))
      ; (setq val_list_ "Origin") 
      ; (TA:remove_val_list_ 8 target_list_)
    ;
    ;preloop_and_while
      (setq target_list_i 0)
      (while (< target_list_i (length target_list_))
        (setq target_list_ename_ (nth  target_list_i target_list_))
        (if (and (= (TA:member val_list_ target_list_ename_) T)  )
          (progn
            (setq target_list_ (vl-remove (nth target_list_i target_list_) target_list_))
          )
        )
        (setq target_list_i (+ target_list_i 1))
      )
      (setq target_list_ target_list_)
    ; 
  )
  (defun TA:remove_val_list_via_wildcard (val_list_ target_list_)
    ; example
    ;   (setq target_list_ (list 1 2 3 4 5 2 6 7 8 9 nil))
    ;   (setq target_list_ (list "ปรับปรุงอาตาร_DERMA_HEALTH_คลอ" "ง9_DB_STUDIO" ""))
    ;   (setq val_list_ "*DER*") 
    ;   (TA:remove_val_list_ 8 target_list_)
    
    ;preloop_and_while
      (setq target_list_i 0)
      (while (< target_list_i (length target_list_))
        (setq target_list_ename_ (nth  target_list_i target_list_))
        (if (and (= (wcmatch  target_list_ename_ val_list_) T)  )
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
  (defun TA:Count_item_in_list_ (c_list_ )
      ;
        (setq result_ ())
        (while (/= c_list_ nil)
          (length c_list_)
          (setq c_list_val_ (car c_list_))
          (setq count_i (- 
                    (length c_list_)
                    (length (setq c_list_ (vl-remove c_list_val_ c_list_)))
                  )
          )
          (setq result_ (cons (cons c_list_val_ count_i ) result_))
          
        )
      ;
    )
    (defun LM:CountItems (l / c l r x) 
      (while l 
        (setq x (car l))
        (setq c (length l))
        (setq l (vl-remove x (cdr l)))
        (setq r (cons (cons x (- c (length l))) r))
      )
      (reverse r)
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
  (defun TA:vla-get-insertionpoint+ename (obj) ;ประกอบ entity_name กับ insertion_poiint ให้เป็น list (BLOCK ONLY)
    (if (= (vla-get-objectname obj) "AcDbBlockReference")
      (progn
        ;get_data
          (setq ename_ (vlax-vla-object->ename obj)) 
          
          (setq _inspt (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint obj))))
        ;
        ;sum_data_to_list
          (setq sum_list (list 
                           _inspt
                           ename_
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
    ; (setq ename_ new_rec)
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
  (defun LM:PolyCentroid (e / l) 
    (foreach x (setq e (entget e)) (if (= 10 (car x)) (setq l (cons (cdr x) l))))
    ((lambda (a) 
      (if (not (equal 0.0 a 1e-8)) 
        (trans 
          (mapcar '/ 
                  (apply 'mapcar 
                        (cons '+ 
                              (mapcar 
                                (function 
                                  (lambda (a b) 
                                    ((lambda (m) 
                                        (mapcar 
                                          (function (lambda (c d) (* (+ c d) m)))
                                          a
                                          b
                                        )
                                      ) 
                                      (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                                    )
                                  )
                                )
                                l
                                (cons (last l) l)
                              )
                        )
                  )
                  (list a a)
          )
          (cdr (assoc 210 e))
          0
        )
      )
    ) 
    (* 3.0 
        (apply '+ 
              (mapcar 
                (function 
                  (lambda (a b) (- (* (car a) (cadr b)) (* (car b) (cadr a))))
                )
                l
                (cons (last l) l)
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
  (defun TA:Toerrance_num_ (target_num_ main_num_ tor_num_)
    (if
      (and 
        (< target_num_ (+ main_num_ tor_num_))
        (> target_num_ (- main_num_ tor_num_))
      )
      (progn
        T
      )
    )
  )
;
;format_FUNC
  (defun format-all-workbook-list (data)
    (progn
      ;; เริ่มต้นเปิดต้น list
      (princ "(")
      (princ (car data)) ; พิมพ์ตัวเลขหลัก
      (princ "\n") ; เว้นบรรทัดเพื่อจัด layout

      ;; ใช้ foreach เพื่อวนลูป book
      (foreach book (cdr data)
        (progn
          (princ "  (")
          (princ (car book)) ; พิมพ์ index ของ book
          (princ " ")
          (princ (cadr book)) ; พิมพ์ชื่อของ book
          (princ " ")
          (princ (caddr book)) ; พิมพ์ object ของ book
          (princ "\n    (\n") ; เว้นบรรทัดสำหรับ sheet list

          ;; วนลูปภายใน sheet list
          (foreach sheet (cadddr book)
            (progn
              (princ "      (")
              (princ (car sheet)) ; พิมพ์ index ของ sheet
              (princ " ")
              (princ (cadr sheet)) ; พิมพ์ชื่อของ sheet
              (princ " ")
              (princ (caddr sheet)) ; พิมพ์ object ของ sheet
              (princ ")\n"))) ; ปิด sheet list
          (princ "    )\n  )\n"))) ; ปิด book และจัด layout
      (princ ")\n")
    )
  )
  (defun format-all-worksheet-list (lst)
    (progn
      ;; พิมพ์ header ของ list
      (princ "(")
      (princ (car lst)) ; พิมพ์ element ตัวแรก
      (princ " ")
      (princ (cadr lst)) ; พิมพ์ element ตัวที่สอง
      (princ " ")
      (princ (caddr lst)) ; พิมพ์ element ตัวที่สาม
      (princ "\n (\n")

      ;; ใช้ foreach สำหรับวนลูป element ย่อย
      (foreach sublist (cadddr lst) ; ส่วนที่เป็น sublist
        (progn
          (princ "  (")
          (princ (car sublist))  ; พิมพ์ element ย่อยตัวแรก
          (princ " ")
          (princ (cadr sublist)) ; พิมพ์ element ย่อยตัวที่สอง
          (princ " ")
          (princ (caddr sublist)) ; พิมพ์ element ย่อยตัวที่สาม
          (princ ")\n")))
      
      ;; ปิดท้าย list
      (princ " )\n)")
    )
  )
  (defun format-data (data)
    (progn
      ;; เริ่มต้นเปิดต้น list
      (princ "(")
      (princ (car data)) ; พิมพ์ตัวเลขหลัก
      (princ "\n") ; เว้นบรรทัดเพื่อจัด layout

      ;; ใช้ foreach เพื่อวนลูป book
      (foreach book (cdr data)
        (progn
          (princ "  (")
          (princ (car book)) ; พิมพ์ index ของ book
          (princ " ")
          (princ (cadr book)) ; พิมพ์ชื่อของ book
          (princ " ")
          (princ (caddr book)) ; พิมพ์ object ของ book
          (princ "\n    (\n") ; เว้นบรรทัดสำหรับ sheet list

          ;; วนลูปภายใน sheet list
          (foreach sheet (cadddr book)
            (progn
              (princ "      (")
              (princ (car sheet)) ; พิมพ์ index ของ sheet
              (princ " ")
              (princ (cadr sheet)) ; พิมพ์ชื่อของ sheet
              (princ " ")
              (princ (caddr sheet)) ; พิมพ์ object ของ sheet
              (princ ")\n"))) ; ปิด sheet list
          (princ "    )\n  )\n"))) ; ปิด book และจัด layout
      (princ ")\n")
    )
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
    (defun TA:getboundingbox_text (textString blk_scale height )
      ; ;argument text
      ;   (setq textString "SSSSSS")
      ;   (setq blk_scale 1)
      ;   (setq height 1.6)
      ; ;
      ;get-doc
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))
      ;
      ;create_text
      (setq textObj (vla-addtext 
                      modelSpace 
                      textString 
                      (vlax-3d-point 0 0 0) 
                      (* blk_scale height)
                    )
      )
      ;
      ;put_data              
      (vla-put-stylename  textObj "WAROON-EIEI")
      (vla-put-Height textObj (* blk_scale height))
      ;
      (setq textObj_obj_getboundingbox (vla-getboundingbox textObj 'min_ 'max_)) 
      (setq textObj_min_ (vlax-safearray->list min_)) 
      (setq textObj_max_ (vlax-safearray->list max_)) 
      (vla-delete textObj )
      (distance textObj_min_ (list 0 (car textObj_max_) 0) )
      
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
        ; (setq point_list_ (reverse (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_)))))
        (setq point_list_ (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_))))
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
    (defun TA:vla-addarc_half_ (center_ rad_ point_list_ )
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
        ; (setq point_list_ (reverse (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_)))))
        (setq point_list_ (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_))))
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
      (while (< point_list_i (- (length point_list_) 2))
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
    (defun TA:add_full_circle_ (center_pt radius)
      ;default adding VLA
        (setq acadObj (vlax-get-acad-object))
        (setq doc (vla-get-ActiveDocument acadObj))
        (setq modelSpace (vla-get-ModelSpace doc))
      ;
      ;vla-addcircle method
        (vla-addcircle modelSpace  
                      (vlax-3d-point center_pt) 
                      radius
        )
        (entlast)
      ;
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
  (defun TA:vla-insertblock (blockName insertion_point scale rotation)
    (setq acadApp (vlax-get-acad-object))  ; เข้าถึง AutoCAD application
    (setq doc (vla-get-ActiveDocument acadApp))  ; เข้าถึง Active Document
    (setq ms (vla-get-ModelSpace doc))  ; เข้าถึง ModelSpace
    
    (setq block (vla-InsertBlock 
                  ms 
                  (vlax-3d-point insertion_point) 
                  blockName 
                  scale scale scale
                  rotation
                )
    )  ; แทรกบล็อก
  )
  (defun TA:set-block-explodable (block-name allow-exploding) 
    ;example_using
    ;;allow-exploding = :vlax-true or :vlax-false
    ; (set-block-explodable "A$C5b7816c3" :vlax-true)
    ; (set-block-explodable "A$C5b7816c3" :vlax-false)
    ;
    ;for testing code
    ; (setq block-name "001 - DYNAMIC LV" )
    ; (setq block-name "A$C405762cd" )
    ; (setq allow-exploding :vlax-false )
    ; (setq allow-exploding :vlax-true )
    ;
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq block-def (vla-Item (vla-get-Blocks doc) block-name))
    (vla-get-Explodable block-def)
    (if (/= block-def nil) 
      (progn 
        (vla-put-Explodable block-def allow-exploding)
      )
      (princ (strcat "\nBlock " block-name " not found."))
    )
  )
  (defun c:allow_explode_block_ALEX ()
    (setq blk_ename_ (car (entsel "specify Object")))
    (setq blk_obj_ (vlax-ename->vla-object blk_ename_))
    (setq ef_name (LM:effectivename blk_obj_))
    (TA:set-block-explodable ef_name :vlax-true)
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
  (defun c:ADD_VISIBILITY_multi-mode_ADVSM_ ()
    (setq input_ins_ (getpoint ))
    (setq name_vis_ "view")
    (command "BPARAMETER" "v" "L" name_vis_ input_ins_ 1)
    (command "-Bvstate" "s" "VisibilityState0")
    ;preloop_and_while_ 
    (setq visibility_screw_mode_                    "1 = screw_mode") 
    (setq visibility_view_mode_                     "2 = front_top_side_cut_mode") 
    (setq visibility_number_mode_                   "3 = number_mode") 
    (setq visibility_AL.EXTR-U-Profile_mode_        "4 = AL.EXTR-U-Profile_mode")
    (setq visibility_copy_mode_                     "5 = copy visibility from another dynamic block")
    
    (setq visibility_mode_value_ (cond ((getint (strcat visibility_screw_mode_"\n"visibility_view_mode_"\n"visibility_number_mode_"\n"visibility_AL.EXTR-U-Profile_mode_"\n" visibility_copy_mode_"\n" "<" (rtos (setq visibility_mode_value_ (cond (visibility_mode_value_) (1.0) ) ) ) "> : " ) ) ) (visibility_mode_value_) ) )
    ;Data_visibiity_list_normie_mode_
      (cond
        (;visibility_mode_case_1
          (and
              (= visibility_mode_value_ 1)
          )
          (progn
              (setq visibility_list_ (list
                                      "0_iso_view"
                                      "1_top_view"
                                      "2_side_head"
                                      "3_side_view_19mm."
                                      "4_side_view_25mm."
                                      "5_side_view_50mm."
                                      "6_side_view_75mm."
                                      "7_side_view_100mm."
                                    )
              )
              (princ "visibility_mode_case_1")
          )
        )
        (;visibility_mode_case_2
          (and
              (= visibility_mode_value_ 2)
          )
          (progn
              (setq visibility_list_ (list
                                      "0_iso_view"
                                      "1_cut_view"
                                      "2_top_view"
                                      "3_front_view"
                                      "4_side_L_view"
                                      "4_side_R_view"
                                      "5_back_view"
                                      "6_bottom_view"
                                      "7_dynamic_length_view"
                                    )
              )
              (princ "visibility_mode_case_2")
          )
        )
        (;visibility_mode_case_3
          (and
              (= visibility_mode_value_ 3)
          )
          (progn
              (setq visibility_list_ (list
                                      "1_"
                                      "2_"
                                      "3_"
                                      "4_"
                                      "5_"
                                    )
              )
              (princ "visibility_mode_case_3")
          )
        )
        (;visibility_mode_case_4
          (and
              (= visibility_mode_value_ 4)
          )
          (progn
              (setq visibility_list_ (list
                                      "1_cut_view"
                                      "1_cut_view_covering_profile"
                                      "2_top_view"
                                      "3_front_view"
                                      "4_side_L_view"
                                      "4_side_R_view"
                                      "5_back_view"
                                      "6_bottom_view"
                                      "7_dynamic_length_view"
                                    )
              )
              (princ "visibility_mode_case_4")
          )
        )
      )
    ;
    ;visibility_copy_mode_[visibility_copy_mode_
      (if (and (/= visibility_mode_value_ 5) (numberp visibility_mode_value_))
        (progn
          (setq visibility_list_i 0)
          (while (< visibility_list_i (length visibility_list_))
            (setq visibility_list_SEQ (nth visibility_list_i visibility_list_))
            (command "-Bvstate" "n" visibility_list_SEQ "c" )
            (setq visibility_list_i (+ visibility_list_i 1))
          )
        )
        (if (and (= visibility_mode_value_ 5) (numberp visibility_mode_value_))
          (progn          
            (command "_bclose" "s")
            ;while_select_block_on_condition_
              (setq visibility_list_ nil)
              (while (= visibility_list_ nil)
                (setq vis_ename_ (car (entsel "specify Visibility Object")))
                (if 
                  (and ;conditional_rule_for_select_object_
                    (/= vis_ename_ nil)
                    (setq vis_obj_ (vlax-ename->vla-object vis_ename_))
                    (= (vla-get-objectname vis_obj_ ) "AcDbBlockReference")
                    (= (vla-get-isdynamicblock vis_obj_ ) :vlax-true)
                    (/= (LM:getdynpropallowedvalues vis_obj_ "view") nil)
                  )
                  (progn
                    (setq visibility_list_ (LM:removenth 0 (LM:getdynpropallowedvalues vis_obj_ "view")) )
                  )
                  (alert "Object invalid Please try again")
                )
              )
              (setq block_target_ename_ nil)
              (while (= block_target_ename_ nil)
                (setq block_target_ename_ (car (entsel "specify block_target_ename_ Object")))
                (if 
                  (and ;conditional_rule_for_select_object_
                    (/= block_target_ename_ nil)
                    (setq block_target_obj_ (vlax-ename->vla-object block_target_ename_))
                    (= (vla-get-objectname block_target_obj_ ) "AcDbBlockReference")
                  )
                  (progn
                    (setq block_target_obj_efname_ (LM:Effectivename block_target_obj_))
                    (command "_bedit" block_target_obj_efname_)
                    (setq visibility_list_i 0)
                    (while (< visibility_list_i (length visibility_list_))
                      (setq visibility_list_SEQ (nth visibility_list_i visibility_list_))
                      (command "-Bvstate" "n" visibility_list_SEQ "c" )
                      (setq visibility_list_i (+ visibility_list_i 1))
                    )
                  )
                  (alert "Object invalid Please try again")
                )
              )
            ;
          )
        )
      )
    ;    
  )
  (defun c:dyanamic_rename_array_REAR ()
    (setq BPARAMETER_ename_ (car (entsel "specify Object")))
    (if (/= BPARAMETER_ename_ nil)
      (progn
        (setq BPARAMETER_obj_ (vlax-ename->vla-object BPARAMETER_ename_))
        (vlax-get-property BPARAMETER_obj_ "DistName" )
        (vlax-put-property BPARAMETER_obj_ "DistName" "array_width")
        (vlax-put-property BPARAMETER_obj_ "Grips" 1)
      )
      (setq BPARAMETER_ename_ nil)
    )
    
  )
  (defun c:dynamic_rewidth_array_1_rew1 ()
    ;user_input_
      (setq user_num_ (getreal "specify_num_for_array_width"))
    ;
    ;selection_set
      (setq ss_pre_filter_set_xx_ (ssget "x" 
                                        ; (list 
                                        ;   (cons 0 "INSERT") ;type of object
                                        ;   ; (cons 8 "000 - GRID")   ;kind of layer
                                        ;   ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ;   ; (cons 62 1)           ;kind of color call sign with color code index
                                        ; )
                                )
      )
      (sslength ss_pre_filter_set_xx_)
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))
        ; (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        ; (TA:dump_obj_ ss_pre_filter_set_xx_ename_)
        
        ;ColumnOffset process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockArrayActionEntity")
              (= (strcase (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ActionType" )) "ARRAY")
              (= (strcase (setq ss_pre_filter_set_xx_name_ (vla-get-name (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))) "ARRAY_MAIN")
            )
            (progn
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ColumnOffset" user_num_ )
            )
          )
        ;
        ;array_width process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearParameterEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistName" ) "array_width")        
            )
            (progn
              (setq basepoint_edit_ (list 
                                      (* (/ user_num_ 2) -1)
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" )))) 
                                    )
              )
              (setq endpoint_edit_ (list 
                                      (* (/ user_num_ 2) 1)
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "endpoint" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "endpoint" )))) 
                                    )
              )
              
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" (vlax-3d-point basepoint_edit_) )
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "EndPoint" (vlax-3d-point endpoint_edit_) )
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ValueSetIncrement" user_num_ )
              
              (setq Parameter_name_ (vla-get-name (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)  ))
            )
          )
        ;
        ;array_grip_process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearGripEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ParameterName" ) Parameter_name_)        
            )
            (progn
              
              (setq position_edit_ (list 
                                      (* (/ user_num_ 2) 1)
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" )))) 
                                    )
              )
              
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" (vlax-3d-point position_edit_) )
              
            )
          )
        ;
        
        
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:dynamic_rewidth_array_2_rew2_ ()
    ;user_input_
      (setq user_num_ (getreal "specify_num_for_array_width"))
    ;
    ;selection_set
      (setq ss_pre_filter_set_xx_ (ssget "x" 
                                        ; (list 
                                        ;   (cons 0 "INSERT") ;type of object
                                        ;   ; (cons 8 "000 - GRID")   ;kind of layer
                                        ;   ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ;   ; (cons 62 1)           ;kind of color call sign with color code index
                                        ; )
                                )
      )
      (sslength ss_pre_filter_set_xx_)
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))
        ; (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        ; (TA:dump_obj_ ss_pre_filter_set_xx_ename_)
        
        ;ColumnOffset process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockArrayActionEntity")
              (= (strcase (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ActionType" )) "ARRAY")
              (= (strcase (setq ss_pre_filter_set_xx_name_ (vla-get-name (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))) "ARRAY_MAIN")
            )
            (progn
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ColumnOffset" user_num_ )
            )
          )
        ;
        ;array_width process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearParameterEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistName" ) "array_width")        
            )
            (progn
              (setq basepoint_edit_ (list 
                                      0 ;(* (/ user_num_ 2) -1)
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" )))) 
                                    )
              )
              (setq endpoint_edit_ (list 
                                      (* user_num_ 1)
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "endpoint" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "endpoint" )))) 
                                    )
              )
              
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "BasePoint" (vlax-3d-point basepoint_edit_) )
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "EndPoint" (vlax-3d-point endpoint_edit_) )
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ValueSetIncrement" user_num_ )
              
              (setq Parameter_name_ (vla-get-name (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)  ))
            )
          )
        ;
        ;array_grip_process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearGripEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "ParameterName" ) Parameter_name_)        
            )
            (progn
              
              (setq position_edit_ (list 
                                      0 ;(* user_num_ 1) ;incase if i need arrow at the endpoint by TA:Trairat
                                      (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" )))) 
                                      (caddr (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" )))) 
                                    )
              )
              
              
              (vlax-put-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Position" (vlax-3d-point position_edit_) )
              
            )
          )
        ;
        
        
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:dynamic_ARsaveas_array_()
    ;user_input_
      ; (setq user_num_ (getreal "specify_num_for_array_width"))
    ;
    ;selection_set
      (setq ss_pre_filter_set_xx_ (ssget "x"
                                        ; (list 
                                        ;   (cons 0 "INSERT") ;type of object
                                        ;   ; (cons 8 "000 - GRID")   ;kind of layer
                                        ;   ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ;   ; (cons 62 1)           ;kind of color call sign with color code index
                                        ; )
                                )
      )
      (sslength ss_pre_filter_set_xx_)
    ;
    (setq ss_pre_filter_set_xx_ (ssget)) ;test
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))
        ; (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        ; (TA:dump_obj_ ss_pre_filter_set_xx_ename_)
        ;
        ;array_width process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearParameterEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistName" ) "array_width")        
              (/= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistanceDesc" ) "")        
            )
            (progn
              ;saveas new block name process
                (setq name_block_ (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistanceDesc" ))
                (setq check_2 (vl-string-search "@" name_block_  1))
                (setq check_22 (substr name_block_ (+ check_2 1) 8))
                (setq check_222 (vl-string-search "mm." check_22  1))
                (setq check_2222 (substr check_22 1 check_222))

                (setq check_1 nil )
                (setq new_efname_ "ZOO" )
                (setq get_array_distance (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Distance" ))
                (setq i 1)
                (while
                  (and
                    (/= (type check_1) 'INT)
                    
                    
                    ; (< i (strlen name_block_ ))
                  )
                  (if (/= (vl-string-search "@" name_block_  1) "m")
                    (progn
                      (setq new_efname_ (strcat (substr name_block_ 1 i)))
                      (setq check_1 (vl-string-search "@" new_efname_  1))
                      
                      
                      ; (setq new_efname_1 (strcat (substr name_block_ 26 1)))
                    )
                  )
                  (setq i (+ i 1))
                )
                (setq new_efname_val (strcat (substr new_efname_  (strlen new_efname_) 5 ) (rtos get_array_distance 2 0)))
                (setq new_name_block_ (LM:StringSubst 
                                        new_efname_val 
                                        check_2222 
                                        name_block_ 
                                      )
                )
              
                (command "bsaveas" new_name_block_ )
                (command "_bclose" )
              ;
              ;insertion_block_process_
                (setq new_block_name new_name_block_  )
                (TA:set-block-blockscaling new_block_name acUniform)
                (setq getpoint_ (getpoint))
                (command "insert" new_block_name  getpoint_ 1 0 )
              ;
              
            )
          )
        ;
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:dynamic_ARsaveas_array_and_insert_arsins()
    ;user_input_
      ; (setq user_num_ (getreal "specify_num_for_array_width"))
    ;
    ;selection_set
      (setq ss_pre_filter_set_xx_ (ssget "x"
                                        ; (list 
                                        ;   (cons 0 "INSERT") ;type of object
                                        ;   ; (cons 8 "000 - GRID")   ;kind of layer
                                        ;   ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ;   ; (cons 62 1)           ;kind of color call sign with color code index
                                        ; )
                                )
      )
      (sslength ss_pre_filter_set_xx_)
    ;
    (setq ss_pre_filter_set_xx_ (ssget)) ;test
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)))
        ; (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        ; (TA:dump_obj_ ss_pre_filter_set_xx_ename_)
        ;
        ;array_width process
          (if 
            (and
              (= (setq ss_pre_filter_set_xx_hon_objname_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))) "AcDbBlockLinearParameterEntity")  
              (= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistName" ) "array_width")        
              (/= (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistanceDesc" ) "")        
            )
            (progn
              ;saveas new block name process
                (setq name_block_ (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "DistanceDesc" ))
                (setq check_2 (vl-string-search "@" name_block_  1))
                (setq check_22 (substr name_block_ (+ check_2 1) 8))
                (setq check_222 (vl-string-search "mm." check_22  1))
                (setq check_2222 (substr check_22 1 check_222))

                (setq check_1 nil )
                (setq new_efname_ "ZOO" )
                (setq get_array_distance (vlax-get-property (vlax-ename->vla-object ss_pre_filter_set_xx_ename_) "Distance" ))
                (setq i 1)
                (while
                  (and
                    (/= (type check_1) 'INT)
                    
                    
                    ; (< i (strlen name_block_ ))
                  )
                  (if (/= (vl-string-search "@" name_block_  1) "m")
                    (progn
                      (setq new_efname_ (strcat (substr name_block_ 1 i)))
                      (setq check_1 (vl-string-search "@" new_efname_  1))
                      
                      
                      ; (setq new_efname_1 (strcat (substr name_block_ 26 1)))
                    )
                  )
                  (setq i (+ i 1))
                )
                (setq new_efname_val (strcat (substr new_efname_  (strlen new_efname_) 5 ) (rtos get_array_distance 2 0)))
                (setq new_name_block_ (LM:StringSubst 
                                        new_efname_val 
                                        check_2222 
                                        name_block_ 
                                      )
                )
              
                (command "bsaveas" new_name_block_ )
                (command "_bclose" )
              ;
              ;insertion_block_process_
                (setq new_block_name new_name_block_  )
                (TA:set-block-blockscaling new_block_name acUniform)
                (setq replace_ename_ (car (entsel "specify Object")))
                (setq replace_obj_ (vlax-ename->vla-object replace_ename_))
                (if (= (vla-get-objectname replace_obj_) "AcDbBlockReference")
                  (progn
                    (setq replace_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint replace_obj_))))
                    (setq replace_obj_dynlist_ (LM:getdynprops replace_obj_))
                    (setq replace_obj_rotation_ (rad-to-deg (vla-get-rotation replace_obj_)) )
                  )
                )
                (command "insert" new_block_name  replace_obj_ins_ 0.001 replace_obj_rotation_ )
                (setq newblk_dyn_ename_ (entlast))
                (setq  newblk_dyn_obj_ (vlax-ename->vla-object  newblk_dyn_ename_))
                ;preloop_and_while
                  (setq replace_obj_dynlist_i 0)
                  (while (< replace_obj_dynlist_i (length replace_obj_dynlist_))
                    (setq replace_obj_dynlist_val_1 (car (nth  replace_obj_dynlist_i replace_obj_dynlist_)))
                    (setq replace_obj_dynlist_val_2 (cdr (nth  replace_obj_dynlist_i replace_obj_dynlist_)))
                    (if (and
                          (= (wcmatch replace_obj_dynlist_val_1 "Origin") nil)
                        )
                      (progn
                        (LM:setdynpropvalue newblk_dyn_obj_ replace_obj_dynlist_val_1 replace_obj_dynlist_val_2 )
                      )
                    )
                    
                    (setq replace_obj_dynlist_i (+ replace_obj_dynlist_i 1))
                  )
                ;
                (vla-delete replace_obj_)

              ;
              
            )
          )
        ;
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:BasePoint_to_BasePoint_[B2B] ()
    ;selection_set_for_fillter_blk_name
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
    
    ;fillter_main_point_name_object_
    (setq main_point_ename_ERR_CHECK ;Detect the variable's result in case of a function error
      (vl-catch-all-apply 
        (function 
          (lambda () 
            (setq main_point_ename_ "No Selection BLock object")
            (while (= main_point_ename_ "No Selection BLock object")
              ;user_input_data
                (setq main_point_ename_ (car (entsel "specify Base Point Block Object")))
              ;
              ;fillter_object_type_
                (if 
                  (and
                    (if (/= main_point_ename_ nil) (progn (setq main_point_obj_ (vlax-ename->vla-object main_point_ename_)) ))
                      (= (vla-get-objectname main_point_obj_) "AcDbBlockReference")
                    )
                  (progn
                    (setq main_point_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint main_point_obj_))))
                  )
                  (setq main_point_ename_ nil)
                )
                (if
                  (or
                    (= main_point_ename_ nil)
                    (/= (vla-get-objectname main_point_obj_) "AcDbBlockReference")
                    
                  )
                  (progn
                    (= main_point_ename_ nil)
                    (alert "No selection Block object")
                  )
                )
              ;
            )
          )
        )
      )
    )
    (if (vl-catch-all-error-p main_point_ename_ERR_CHECK)
      (progn
        (setq detect_error_main_point_ename_ ;Detect the variable's result in case of a function error
          (wcmatch
            (strcase
              (vl-catch-all-error-message
                main_point_ename_detect_error
              )
            )
            "FUNCTION*,*ERROR*"
          )
        )
      )
      (setq detect_error_main_point_ename_ nil)
    )
    
    ;
    ;user_input_insertionpoint_for_move
      (setq user_input_ins_point_ERR_CHECK nil)
      (setq main_point_ename_ nil)
      (while 
        (and 
          (= main_point_ename_ nil)
          (= user_input_ins_point_ERR_CHECK nil)
        )
        (setq user_input_ins_point_ERR_CHECK ;Detect the variable's result in case of a function error
          (vl-catch-all-apply 
            (function 
              (lambda () 
                (setq user_input_ins_point_ (getpoint "specify getpoint for new insertionpoint"))
              )
            )
          )
        )
        (cond
          (;blank_case_1
            (and
              (= user_input_ins_point_ERR_CHECK nil)
              (= user_input_ins_point_ nil)
            )
            (progn
              (setq main_point_ename_ nil)
              (setq user_input_ins_point_ERR_CHECK nil)
              (alert "Please selection insertionpoint for move")
              (princ "blank_case_1")
            )
          )
          (;blank_case_2
            (and
              (vl-catch-all-error-p user_input_ins_point_ERR_CHECK)
              (= user_input_ins_point_ nil)
            )
            (progn
              (setq main_point_ename_ nil)
              (setq user_input_ins_point_ERR_CHECK nil)
              (alert "Please selection insertionpoint for move")
              (princ "blank_case_2")
            )
          )
          (;have_Inspt
            (and 
              (= (numberp (car user_input_ins_point_)) T)
              (= (numberp (cadr user_input_ins_point_)) T)
            )
            (progn
              (setq main_point_ename_ "no block object")
              (princ "have_Inspt")
            )
          )
        )
        
      )
      (if 
        (and
          (= user_input_ins_point_ nil)
          (vl-catch-all-error-p user_input_ins_point_ERR_CHECK )
        )
        (progn
          (setq detect_error_user_input_ins_point_ ;Detect the variable's result in case of a function error
            (wcmatch
              (strcase
                (vl-catch-all-error-message
                  user_input_ins_point_ERR_CHECK
                )
              )
              "FUNCTION*,*ERROR*,*NIL*"
            )
          )
        )
      )
    ;
    ;main_idea_code
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (setvar "osmode" 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          ;main_idea
            ;getins_data
            (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq ss_pre_filter_set_xx_hon_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_hon_obj_))))
            ;
            ;put_move_data
            (cond
              (;user_input_ins_case_1
                (and
                    (= (numberp (car user_input_ins_point_)) T)
                    (= (numberp (cadr user_input_ins_point_)) T)
                )
                (progn
                  (vla-move ss_pre_filter_set_xx_hon_obj_ 
                            (vlax-3d-point ss_pre_filter_set_xx_hon_obj_ins_)
                            (vlax-3d-point user_input_ins_point_)
                  )
                  (princ "useinput_ins_case_1")
                )
              )
              (;Block_ins_case_2
                (and 
                  (= user_input_ins_point_ nil)
                  (= detect_error_user_input_ins_point_ T)
                )
                (progn  ;move to ins main block
                        (vla-move ss_pre_filter_set_xx_hon_obj_ 
                                  (vlax-3d-point ss_pre_filter_set_xx_hon_obj_ins_)
                                  (vlax-3d-point main_point_obj_ins_)
                        )
                        (setq user_input_ins_point_ nil)
                        (princ "useinput_ins_case_2")
                )
              )
            )
            
              
            ;
          ;
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
        (setvar "osmode" 1215)
      ;
    ;
  )
  (defun c:Replace_block_object_Reblock ()
    ;selection_set_for_fillter_blk_name
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
          (setq ss_pre_filter_set_xx_ (ssget 
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;specify_object_for_filter_by_EFname_
      (setq Filter_target_ename_ (car (entsel "specify Object")))
      (setq Filter_target_obj_ (vlax-ename->vla-object Filter_target_ename_))
      (setq Filter_target_obj_EFname_ (LM:effectivename Filter_target_obj_))
      (setq ss_post_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ Filter_target_obj_EFname_))
    ;
    ;insert_target_object_process_
      (setq insert_target_ename_ (car (entsel "specify Object")))
      (setq insert_target_obj_ (vlax-ename->vla-object insert_target_ename_))
      (setq insert_target_obj_EFname_ (LM:effectivename insert_target_obj_))
    ;
    ;preloop_and_while
      (setq ss_post_filter_set_xx_i 0)
      (setq ss_newset_ (ssadd))
      (sslength ss_newset)
      (while (< ss_post_filter_set_xx_i (sslength ss_post_filter_set_xx_))
        (setq ss_post_filter_set_xx_ename_ (ssname ss_post_filter_set_xx_ ss_post_filter_set_xx_i))
          (setq ss_post_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_post_filter_set_xx_ename_))
            (setq ss_post_filter_set_xx_hon_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_post_filter_set_xx_hon_obj_))))
            (setvar "osmode" 0)
            (command "insert" insert_target_obj_EFname_ ss_post_filter_set_xx_hon_obj_ins_ 1 0)
            (vla-delete ss_post_filter_set_xx_hon_obj_ )
            (setq new_obj_ (entlast ))
            (setq ss_newset_ (ssadd new_obj_ ss_newset_))
        
        (setq ss_post_filter_set_xx_i (+ ss_post_filter_set_xx_i 1))
      )
    ;

    ;preloop_and_while
      ;while_select_block_on_condition_
        ;first_cond_for_while_loop_input
          (setq getRO_ ;Detect the variable's result in case of a function error
            (vl-catch-all-apply 
              (function 
                (lambda () 
                  (setq getRO_RO_ (cond ( (getint (strcat "\nspecify get rotation\n<" (rtos (setq getRO_RO_ (cond (getRO_RO_) (1.0) ) ) ) "> : " ) ) ) (getRO_RO_) ) )
                )
              )
            )
          )
          (while ;start_while-loop_
            (and
              (numberp getRO_)
            )
            (setq ss_newset_i 0)
            (while (< ss_newset_i (sslength ss_newset_))
              (setq ss_newset_ename_ (ssname ss_newset_ ss_newset_i))
              (setq ss_newset_obj_ (vlax-ename->vla-object ss_newset_ename_))
              
              (vla-put-rotation ss_newset_obj_ (deg-to-rad getRO_))
              (setq ss_newset_i (+ ss_newset_i 1))

            )
            ;final_cond_for_while_loop_input_ (ESC for exit while-loop_)
              (setq getRO_ ;Detect the variable's result in case of a function error
                (vl-catch-all-apply 
                  (function 
                    (lambda () 
                      (setq getRO_RO_ (cond ( (getint (strcat "\nspecify get rotation\n<" (rtos (setq getRO_RO_ (cond (getRO_RO_) (1.0) ) ) ) "> : " ) ) ) (getRO_RO_) ) )
                    )
                  )
                )
              )
            ;
          )
        ;
      ;
    ;
    ;return_osmode_mode
      (setvar "osmode" 1279)
    ;
  )
  (defun TA:Assembly_block_ (blk_obj_ )
    ; (setq blk_ename_ (car (entsel "specify Object")))
    ; (setq blk_obj_ (vlax-ename->vla-object blk_ename_))
    (if (= (vla-get-objectname blk_obj_) "AcDbBlockReference")
      (progn
        (setq blkname (cdr (assoc 2 (entget blk_ename_))))
        (setq bdef (tblobjname "BLOCK" blkname))
        (if (= (vl-prin1-to-string (type bdef)) "ENAME")
          (progn
            (setq nested (entnext bdef))
            
            (while 
              (and nested 
                  (/= (cdr (assoc 0 (entget nested))) "ENDBLK")
              )
              (if (= (cdr (assoc 0 (entget nested))) "INSERT")
                (progn 


                  (setq nested_obj_ (vlax-ename->vla-object nested))
                  
                  (if (= (vla-get-visible nested_obj_) :vlax-true)
                    (progn
                      (princ (strcat "\n  └─ Nested block: " (LM:effectivename nested_obj_)) )
                      (setq blk_list_ (cons (LM:effectivename nested_obj_) blk_list_))
                    )
                  )
                  
                )
              )
              (setq nested (entnext nested))
            )
          )
        )
      )
    )
    (TA:Count_item_in_list_ blk_list_ )
    
  
  
  )
  (defun TA:Assembly_des-name-block_ (blk_obj_ )
    ; (setq blk_ename_ (car (entsel "specify Object")))
    ; (setq blk_obj_ (vlax-ename->vla-object blk_ename_))
    ; (setq blk_obj_ target_block_obj_)
    (setq blk_list_ ())
    (if (= (vla-get-objectname blk_obj_) "AcDbBlockReference")
      (progn
        (setq blk_ename_ (vlax-vla-object->ename blk_obj_) )
        (setq blkname (cdr (assoc 2 (entget blk_ename_))))
        (setq bdef (tblobjname "BLOCK" blkname))
        (if (= (vl-prin1-to-string (type bdef)) "ENAME")
          (progn
            (setq nested (entnext bdef))
            
            (while 
              (and nested 
                  (/= (cdr (assoc 0 (entget nested))) "ENDBLK")
              )
              (if (= (cdr (assoc 0 (entget nested))) "INSERT")
                (progn 


                  (setq nested_obj_ (vlax-ename->vla-object nested))
                  (setq block-editor_des-name (vla-get-comments (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) (LM:effectivename nested_obj_))))
                  (if 
                    (and 
                      (= (vla-get-visible nested_obj_) :vlax-true)
                      (/= (LM:getvisibilityparametername nested_obj_) nil)
                      (LM:getdynpropvalue nested_obj_ (LM:getvisibilityparametername nested_obj_))
                    )
                    (progn
                      (princ (strcat "\n  └─ Nested block: " (LM:effectivename nested_obj_)) )
                      (setq blk_list_ (cons
                                        (strcat 
                                          (if (or (= block-editor_des-name nil) (= block-editor_des-name "")) (progn (LM:effectivename nested_obj_)) block-editor_des-name)
                                          (substr (LM:getdynpropvalue nested_obj_ (LM:getvisibilityparametername nested_obj_ ) ) 2 20 )
                                        ) 
                                        blk_list_
                                      )
                      )
                    
                    )
                  )
                  
                )
              )
              (setq nested (entnext nested))
            )
          )
        )
      )
    )
    (TA:Count_item_in_list_ blk_list_ )
  )
  (defun TA:Foreach_Assembly_des-name-block_ (blk_ename_)
    ;preloop_and_while
      (setq sub-block_ ())
      (setq sub-block_ename_ ())
      (setq blkname (cdr (assoc 2 (entget blk_ename_))))
      (setq bdef (tblobjname "BLOCK" blkname))
      (setq nested (entnext bdef))
      (while (/= nested nil )
        (if 
          (and 
            (= (vla-get-objectname (vlax-ename->vla-object nested)) "AcDbBlockReference")
            (= (vla-get-visible (vlax-ename->vla-object nested)) :vlax-true)
            (= (vla-get-isdynamicblock (vlax-ename->vla-object nested)) :vlax-true)
            (/= (LM:getdynpropvalue (vlax-ename->vla-object nested) (LM:getvisibilityparametername (vlax-ename->vla-object nested))) "")
          )
          (progn
            (setq sub-block_ (cons (LM:effectivename (vlax-ename->vla-object nested)) sub-block_))
            (setq sub-block_ename_(cons nested sub-block_ename_))
            ; (length sub-block_)
            ; (length sub-block_ename_)
          )
        )
        (setq nested (entnext nested))
      )
    ;
    (setq blk_list_ ())
    (foreach x sub-block_ename_
      (setq x_obj_ (vlax-ename->vla-object x))
      (princ (strcat "\n  └─ Nested block: " (LM:effectivename x_obj_)) )
      (setq block-editor_des-name (vla-get-comments (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) (LM:effectivename x_obj_))))
      (setq blk_list_ (cons 
                        (strcat 
                          (if (or (= block-editor_des-name nil) (= block-editor_des-name "") ) (progn (LM:effectivename x_obj_)) block-editor_des-name )
                          (substr (LM:getdynpropvalue x_obj_ (LM:getvisibilityparametername x_obj_) ) 2 20 )
                        )
                        blk_list_
                      )
      )
    )
    (TA:Count_item_in_list_ blk_list_ )
  )


;
;Excel_sub_func
  ;get-object-Excel
  (defun TA:New_Excel_File_NEX_ (new_vla-obj_app )
    (if (= new_vla-obj_app nil)
      (progn
        (setq new_vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (if (/= new_vla-obj_app nil)
      (progn
        (setq new_vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        ; (setq new_vla-obj_app (vlax-create-object "Excel.Application"))
        (setq new_Workbooks_ (vlax-get-property new_vla-obj_app 'Workbooks))
        (setq new_workbook_ (vlax-invoke-method new_Workbooks_ 'Add))
        (setq Vla_workbook_list_ (TA:Excel_Assembly_ALL-obj_list_ new_Workbooks_ ))
        (vlax-put-property new_vla-obj_app 'Visible :vlax-true)
        (setq Vla_workbook_list_ Vla_workbook_list_)
      )
    )
    ;princ main object
      (setq new_Workbooks_ new_Workbooks_)
    ;
  )
  (defun TA:Excel_Assembly_ALL-obj_list_ (books_obj_)
    ;Note By Code_Developer
    ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
    ;
    ;books_obj_ = #<VLA-OBJECT Sheets >
    ;preloop_and_while
      (if 
        (or
          (wcmatch (strcase (vl-prin1-to-string books_obj_)) "*<VLA-OBJECT WORKBOOKS*")
          ; (wcmatch (strcase (vl-prin1-to-string books_obj_)) "*SHEETS*")
        )
        (progn
          (setq _workbook_Vla-obj_list_ ())
          (setq _worksheet_vla-obj_name_list_ ())
          
          (setq books_obj_length (vlax-get-property books_obj_ 'count )) ;total file open in 1 app
          (setq books_obj_i 1)
          (while (<= books_obj_i books_obj_length)
            (setq _workbook_obj_ (vlax-get-property books_obj_ 'item books_obj_i))
              (setq _sheets_obj_ (vlax-get-property _workbook_obj_ 'sheets))
                ;preloop_and_while
                  (setq _sheets_obj_length (vlax-get-property _sheets_obj_ 'count))
                  (setq _sheets_obj_i 1 )
                  (setq _worksheet_vla-obj_name_list_ ())
                    (while (<= _sheets_obj_i _sheets_obj_length)
                      (setq _worksheet_obj_ (vlax-get-property _sheets_obj_ 'item _sheets_obj_i))
                        (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
                        (setq _worksheet_vla-obj_name_list_sum (list _sheets_obj_i _worksheet_obj_name _worksheet_obj_))
                      (setq _worksheet_vla-obj_name_list_ (cons _worksheet_vla-obj_name_list_sum _worksheet_vla-obj_name_list_))
                      (setq _sheets_obj_i (+ _sheets_obj_i 1))
                      ; (setq _worksheet_vla-obj_name_list_ (reverse _worksheet_vla-obj_name_list_))
                      (setq _worksheet_vla-obj_name_list_ (vl-sort _worksheet_vla-obj_name_list_  ;bigest open indent list
                                                      (function 
                                                        (lambda (a b) 
                                                          (< (nth 0 a) (nth 0 b))
                                                        )
                                                      )
                                              ) ;bigest close indent list
                      )
                    )
                  ;
            (setq _workbook_obj_name (vlax-get-property _workbook_obj_ 'name))
            (setq _worksheet_Vla-obj_list_sum_ (list books_obj_i _workbook_obj_name _workbook_obj_ _worksheet_vla-obj_name_list_))
            
            (setq _workbook_Vla-obj_list_ (cons _worksheet_Vla-obj_list_sum_ _workbook_Vla-obj_list_))
            (setq books_obj_i (+ books_obj_i 1))
          )
          ; (setq _workbook_Vla-obj_list_ (reverse _workbook_Vla-obj_list_ ))
          (setq _workbook_Vla-obj_list_ (vl-sort _workbook_Vla-obj_list_  ;bigest open indent list
                                          (function 
                                            (lambda (a b) 
                                              (< (nth 0 a) (nth 0 b))
                                            )
                                          )
                                  ) ;bigest close indent list
          )
          (append (list books_obj_) _workbook_Vla-obj_list_)
        )
      )
    ;preloop
  )
  (defun TA:Excel_Assembly_worksheet_obj_list_ (VLA-OBJECT_Workbook)
    ;Note By Code_Developer
    ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
    ;
    ;VLA-OBJECT_Workbook = #<VLA-OBJECT Sheets >
    ;preloop_and_while
      (if 
        (or
          (wcmatch (strcase (vl-prin1-to-string VLA-OBJECT_Workbook)) "*WORKBOOKS*")
          (wcmatch (strcase (vl-prin1-to-string VLA-OBJECT_Workbook)) "*SHEETS*")
        )
        (progn
          (setq _worksheet_Vla-obj_list_ ())
          (setq VLA-OBJECT_Workbooklength (vlax-get-property VLA-OBJECT_Workbook 'count ))
          (setq VLA-OBJECT_Workbooki 1)
          (while (<= VLA-OBJECT_Workbooki VLA-OBJECT_Workbooklength)
            (setq _worksheet_obj_ (vlax-get-property VLA-OBJECT_Workbook 'item VLA-OBJECT_Workbooki))
              (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
              (setq _worksheet_Vla-obj_sum_ (list VLA-OBJECT_Workbooki _worksheet_obj_name _worksheet_obj_ ))
            (setq _worksheet_Vla-obj_list_ (cons _worksheet_Vla-obj_sum_ _worksheet_Vla-obj_list_))
            (setq VLA-OBJECT_Workbooki (+ VLA-OBJECT_Workbooki 1))
          )
          (setq _worksheet_Vla-obj_list_ (reverse _worksheet_Vla-obj_list_ ))
        )
      )
    ;preloop
  )
  (defun Excel:VLA-OBJECT-Workbook->VLA-OBJECT-Sheets  (VLA-OBJECT_Workbook)
    (if 
      (wcmatch 
        (strcase (vl-princ-to-string VLA-OBJECT_Workbook))
        (strcase "*<VLA-OBJECT _Workbook*")
      )
      (progn
        (setq VLA-OBJECT-Sheets (vlax-get-property VLA-OBJECT_Workbook 'sheets))
      )
      (princ "argument invalid")
    )
  )
  (defun Excel:VLA-OBJECT-Sheets->VLA-OBJECT-Worksheet  (VLA-OBJECT-Sheets i)
    (if 
      (and
        (wcmatch 
          (strcase (vl-princ-to-string VLA-OBJECT-Sheets))
          (strcase "*<VLA-OBJECT SHEETS*")
        )
        (numberp i )
        (<= i (vlax-get-property VLA-OBJECT-Sheets 'count))
      )
      (progn
        (vlax-get-property VLA-OBJECT-Sheets 'item i)
      )
      (princ "argument invalid")
    )
  )
  (defun TA:EXCEL_put_all_rowheight (VLA-OBJECT-Worksheet rowheight_value_ )
    (vlax-put-property (vlax-get VLA-OBJECT-Worksheet 'Cells) 'rowheight rowheight_value_)
  )
  (defun TA:EXCEL_put_all_columnwidth (VLA-OBJECT-Worksheet columnwidth_value_ )
    (vlax-put-property (vlax-get VLA-OBJECT-Worksheet 'Cells) 'columnwidth columnwidth_value_)
  )
  (defun TA:EXCEL_put_columnhide (VLA-OBJECT-Worksheet column_number columnhide_value_ )
    (setq vla-object-cols_ (vlax-get-property VLA-OBJECT-Worksheet 'Columns))
      (setq vla-object-col_(vlax-get-property (vlax-variant-value (vlax-get-property vla-object-cols_ 'Item column_number)) 'EntireColumn))
        (vlax-put-property vla-object-col_ 'Hidden  columnhide_value_)
  )
  (defun TA:EXCEL_put_rowhide (VLA-OBJECT-Worksheet row_number rowhide_value_ )
    (setq vla-object-rows_ (vlax-get-property VLA-OBJECT-Worksheet 'rows))
      (setq vla-object-row_(vlax-get-property (vlax-variant-value (vlax-get-property vla-object-rows_ 'Item row_number)) 'Entirerow))
        (vlax-put-property vla-object-row_ 'Hidden rowhide_value_)
  )
  (defun TA:EXCEL_put_columnwidth (VLA-OBJECT-Worksheet column_number columnwidth_value_ )
    (setq vla-object-cols_ (vlax-get-property VLA-OBJECT-Worksheet 'Columns))
      (setq vla-object-col_(vlax-get-property (vlax-variant-value (vlax-get-property vla-object-cols_ 'Item column_number)) 'EntireColumn))
        (vlax-put-property vla-object-col_ 'ColumnWidth columnwidth_value_)
  )
  (defun TA:EXCEL_put_rowheight (VLA-OBJECT-Worksheet row_number rowheight_value_ )
    (setq vla-object-rows_ (vlax-get-property VLA-OBJECT-Worksheet 'rows))
      (setq vla-object-row_(vlax-get-property (vlax-variant-value (vlax-get-property vla-object-rows_ 'Item row_number)) 'Entirerow))
        (vlax-put-property vla-object-row_ 'rowheight rowheight_value_)
  )
  (defun TA:EXCEL_put_columnwidth_viatext  (_worksheet_obj_ Rangecell_row_ Rangecell_column_ Rangecell_target_row_ text_width_val)
    ;Note By Code_Developer
    ;pripicle of code for put columnwidth from string len or number len in rangecell in Excel App by While-loop method 1st-row to target-row (target-row specify by argument)
    ;
    ;Exce_sub_FUNC_list_
      ;TA:EXCEL_put_columnwidth 
    ;
    ;argument
      ;_worksheet_obj_        = <VLA-OBJECT _Worksheet >   
      ;Rangecell_row_         = "1st-Row cell in Excel App"
      ;Rangecell_target_row_  = "lowset-Row cell in Excel App"
      ;Rangecell_column_      = "Column cell in Excel App"
      ;text_width_val         = text_len per width_
    ;
    ; example_argument_
    ;   (setq Rangecell_row_ 1)
    ;   (setq Rangecell_column_ 2)
    ;   (setq _worksheet_obj_ Excel_lasted_worksheet_obj_ )
    ;   (setq Rangecell_target_row_ 2) 
    ;   (setq text_width_val 2) 
    
    ;preloop_and_while
      (setq rangecell_colwidth_list_ ())
      (while
        (< Rangecell_row_ (+ Rangecell_target_row_ 1))
        (setq rangecell_value_ (TA:Excel_get_data_R1C1 _worksheet_obj_ Rangecell_row_ Rangecell_column_ ))
        (if (numberp rangecell_value_)
          (progn 
            (setq col_width_ (* (strlen (rtos rangecell_value_ 2 8)) text_width_val ) )
          )
          (setq col_width_ (* (if (/= rangecell_value_ nil) (progn (strlen rangecell_value_)) 0 ) text_width_val) )
        )
        (setq rangecell_colwidth_list_ (cons col_width_ rangecell_colwidth_list_ ))
        
        
        (setq Rangecell_row_ (+ Rangecell_row_ 1))
      )
    ;
    (TA:EXCEL_put_columnwidth _worksheet_obj_ Rangecell_column_ (setq rangecell_col-max-width_ (car (vl-sort rangecell_colwidth_list_ '>))))
  )
  (defun TA:Excel_Clearcontents (VLA-OBJECT-Worksheet  )
    (setq VLA-OBJECT-Range (vlax-get-property VLA-OBJECT-Worksheet 'Cells  ) )
    (vlax-invoke-method VLA-OBJECT-Range 'ClearContents)
  )
  (defun TA:Excel_get_data_Rangecell (_worksheet_obj_ Rangecell_name_)
    ;Note By Code_Developer
    ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;
    ;argument
      ;_worksheet_obj_ = <VLA-OBJECT _Worksheet >
      ;Rangecell_name_ = "Rangecell name in Excel App"
    ;
    ;Example 
      ;(vlax-get (vlax-get-property _worksheet_obj_ 'Range "AA1") "value2")  ;get_value_from_range_cell
    ;
    (vlax-get (vlax-get-property _worksheet_obj_ 'Range Rangecell_name_) "value2")
  )
  (defun TA:Excel_put_data_Rangecell (_worksheet_obj_ Rangecell_name_ Rangecell_value_)
    ;Note By Code_Developer
    ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_name_  = "Rangecell name in Excel App"
      ;Rangecell_value_ = "Rangecell Value in Excel App"
    ;
    ;Example 
      ;(vlax-put (vlax-get-property _worksheet_obj_ 'Range "AA1") 'Value "XXX") ;put_value_from_range_cell
    ;
    (vlax-put (vlax-get-property _worksheet_obj_ 'Range Rangecell_name_ ) 'Value Rangecell_value_)
  )
  (defun TA:Excel_get_data_R1C1 (_worksheet_obj_  Rangecell_row_ Rangecell_column_)
    ;Note By Code_Developer
    ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
      ;Rangecell_value_ = "get Rangecell Value in Excel App"
    ;
    ;Example 
      ; (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value) ;get_value_from_range_cell
    ;
    (if 
      (and 
        (= (wcmatch (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'NumberFormat ) "*d-mm-yy*") T)
      )
      (progn
        (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'text)
      )
      (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'Value) ;get_value_from_range_cell
    )
    
  )
  (defun TA:Excel_input_data_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ Rangecell_value_ )
    ;Note By Code_Developer
    ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
      ;Rangecell_value_ = "put Rangecell Value in Excel App"
      ;
    ;
    ;Example 
      ; (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value "TESTTESTTESTTEST") ;put_value_from_range_cell
      ;(vlax-dump-object (vlax-get _worksheet_obj_ 'Cells))
      ;(vlax-put-property (vlax-get _worksheet_obj_ 'Cells) 'rowheight 22)
    ;
    (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'Value Rangecell_value_) ;p
  )
  (defun TA:Excel_get_font-size_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_)
    ;Note By Code_Developer
    ;pripicle of code for get font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
    ;
    ;main_idea
      (vlax-get
        (vlax-get 
          (vlax-variant-value 
            (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
          )
          'font
        )
        'size
      )
    ;
  ) 
  (defun TA:Excel_put_font-size_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ font-size_value)
    ;Note By Code_Developer
    ;pripicle of code for put font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
      ;font-size_value = integer for incase or decase size 
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
      ; (vlax-dump-object (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) ) 'font ))
    ;
    ;main_idea
      (vlax-put
        (vlax-get 
          (vlax-variant-value 
            (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
          )
          'font
        )
        'size
        font-size_value
      )
    ;
  ) 
  (defun TA:Excel_get_font-VerticalAlignment_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_)
    ;Note By Code_Developer
    ;pripicle of code for get font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
    ;
    ;main_idea
      (cond
        (;Top Align
          (= 
            (vlax-get 
              (vlax-variant-value 
                (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
              )
              'VerticalAlignment
            )
            -4160
          )
          (progn
            (setq VerticalAlignment "Top Align")
          )
        )
        (;MIddle Align
          (= 
            (vlax-get 
              (vlax-variant-value 
                (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
              )
              'VerticalAlignment
            )
            -4108
          )
          (progn
            (setq VerticalAlignment "Middle Align")
          )
        )
        (;Bottom Align
          (= 
            (vlax-get 
              (vlax-variant-value 
                (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
              )
              'VerticalAlignment
            )
            -4107
          )
          (progn
            (setq VerticalAlignment "Bottom Align")
          )
        )
      )
    ;
  )
  (defun TA:Excel_put_font-VerticalAlignment_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ Alignment_value_)
    ;Note By Code_Developer
    ;pripicle of code for get font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
      ; (setq Alignment_value_ "top")
    ;
    ;main_idea
      (if
        (or
          (= (strcase Alignment_value_) "TOP")
          (= (strcase Alignment_value_) "CENTER")
          (= (strcase Alignment_value_) "BOTTOM")
        )
        (progn
          (cond
            (;Top Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "TOP")
              )
              (progn
                (vlax-put 
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'VerticalAlignment
                  -4160
                )
              )
            )
            (;Middle Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "CENTER")
              )
              (progn
                (vlax-put 
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'VerticalAlignment
                  -4108
                )
              )
            )
            (;Bottom Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "BOTTOM")
              )
              (progn
                (vlax-put 
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'VerticalAlignment
                  -4107
                )
              )
            )
          )
        )
        (princ "object value invalid")
      )
    ;
  )
  (defun TA:Excel_get_font-HonlizontalAlignment_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ )
    ;Note By Code_Developer
    ;pripicle of code for get font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
      ; (setq Alignment_value_ "top")
    ;
    ;main_idea
      (cond
        (;Left Align
          (and 
            (=
              (vlax-get 
                (vlax-variant-value 
                  (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
                )
                'HorizontalAlignment               
              )
              -4131
            )
          )
          (progn
            (setq Alignment_value_ "Align Left")
          )
        )
        (;Middle Align
          (and 
            (=
              (vlax-get 
                (vlax-variant-value 
                  (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
                )
                'HorizontalAlignment               
              )
              -4108
            )
          )
          (progn
            (setq Alignment_value_ "Align Center")
          )
        )
        (;Bottom Align
          (and 
            (=
              (vlax-get 
                (vlax-variant-value 
                  (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ )
                )
                'HorizontalAlignment               
              )
              -4152
            )
          )
          (progn
            (setq Alignment_value_ "Align Right")
          )
        )
      )
    ;
  )
  (defun TA:Excel_put_font-HonlizontalAlignment_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ Alignment_value_)
    ;Note By Code_Developer
    ;pripicle of code for get font-size from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
    ;argument
      ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
      ;Rangecell_row_  = "Row cell in Excel App"
      ;Rangecell_column_  = "Column cell in Excel App"
    ;
    ;example_for_testing_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 1)
      ; (setq Alignment_value_ "top")
    ;
    ;main_idea
      (if
        (or
          (= (strcase Alignment_value_) "LEFT")
          (= (strcase Alignment_value_) "CENTER")
          (= (strcase Alignment_value_) "RIGHT")
        )
        (progn
          (cond
            (;Left Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "LEFT")
              )
              (progn
                (vlax-put
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'HorizontalAlignment 
                  -4131
                )
              )
            )
            (;Middle Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "CENTER")
              )
              (progn
                (vlax-put 
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'HorizontalAlignment
                  -4108
                )
              )
            )
            (;Bottom Align
              (and 
                (= (type  Alignment_value_) 'str)
                (= (strcase Alignment_value_) "RIGHT")
              )
              (progn
                (vlax-put 
                  (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_ ) )
                  'HorizontalAlignment
                  -4152
                )
              )
            )
          )
        )
        (princ "object value invalid")
      )
    ;
  )    
  (defun TA_EXCEL_RangecellName_to_R1C1 (RangeCellName_list_)
    ;preloop_and_while
      (setq RangeCellName_list_i 0)
      (setq r1c1_list_ ())
      (while (< RangeCellName_list_i (length RangeCellName_list_))
        (setq RangeCellName_list_col_ (TA:remove-numbers (nth  RangeCellName_list_i RangeCellName_list_)))
        (setq RangeCellName_list_row_ (TA:remove-alphabet (nth  RangeCellName_list_i RangeCellName_list_)))
        
        
        (setq R1C1_data_ 
                (list 
                  (setq RangeCellName_list_col_ (TA_EXCEL_RangeCOL_to_C1 RangeCellName_list_col_ ) )
                  (atoi RangeCellName_list_row_)
                )
        )
        (setq r1c1_list_ (cons R1C1_data_ r1c1_list_ ))

      
        
        (setq RangeCellName_list_i (+ RangeCellName_list_i 1))
      )
      (setq r1c1_list_ (reverse r1c1_list_ ))
    ;
  )
  (defun TA_EXCEL_RangeCOL_to_C1 (col)
    (setq len (strlen col)) ; หาความยาวของตัวอักษร
    (setq result 0)
    (setq i 0)
    
    ;; loop ผ่านตัวอักษรแต่ละตัวในคอลัมน์
    (repeat len
      (setq char (strcase (substr col (1+ i) 1))) ; เอาตัวอักษรทีละตัว
      (setq result (+ result (* (- (ascii char) 64) (expt 26 (- len (1+ i)))))) ; คำนวณตามฐาน 26
      (setq i (1+ i)) ; เพิ่มตัวนับ
    )
    result
  )
  (defun TA_EXCEL_method_password ()
    (setq _Workbook_obj_ (vlax-invoke-method 
                           (vlax-get-property vla-obj_app 'WorkBooks)
                           'Open
                           filePath ; 1. Filename
                           0 ; 2. UpdateLinks
                           :vlax-false ; 3. ReadOnly
                           nil ; 4. Format
                           "bb" ; 5. for input Password 
                         )
    )
  )
  (defun TA_EXCEL_Merge_cell (_worksheet_obj_ lst_cell_1 lst_cell_2 )
    ;Note By Code_Developer
    ;The principle of codework is desgined for merge cell object in Excel APP
    ;
    ;Example Argument
      ; (setq lst_cell_1 (list 1 5))
      ; (setq lst_cell_2 (list 1 8))
    ;
      (setq cell_1 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_1) (cadr lst_cell_1))))
      (setq cell_2 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_2) (cadr lst_cell_2))))
      (setq cell_range_obj_ (vlax-get-property _worksheet_obj_ 'Range cell_1 cell_2))
      (vlax-put-property cell_range_obj_ 'MergeCells  :vlax-true)
    ;

  )
  (defun TA_EXCEL_UnMerge_cell (_worksheet_obj_ lst_cell_1 lst_cell_2 )
    ;Note By Code_Developer
    ;The principle of codework is desgined for merge cell object in Excel APP
    ;
    ;Example Argument
      ; (setq lst_cell_1 (list 1 5))
      ; (setq lst_cell_2 (list 1 8))
    ;
      (setq cell_1 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_1) (cadr lst_cell_1))))
      (setq cell_2 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_2) (cadr lst_cell_2))))
      (setq cell_range_obj_ (vlax-get-property _worksheet_obj_ 'Range cell_1 cell_2))
      (vlax-put-property cell_range_obj_ 'MergeCells  :vlax-false)
    ;

  )
  (defun TA_EXCEL_get_rangeccell_color (_worksheet_obj_ lst_cell_1 lst_cell_2  )
    ;Note By Code_Developer
    ;The principle of codework is desgined for merge cell object in Excel APP
    ;
    ;Example Argument
      (setq lst_cell_1 (list 1 1))
      (setq lst_cell_2 (list 1 1))
      ; (setq color_rgb_list (list 50 50 50))
    ;
   
      (setq cell_1 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_1) (cadr lst_cell_1))))
      (setq cell_2 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_2) (cadr lst_cell_2))))
      (setq cell_range_obj_ (vlax-get-property _worksheet_obj_ 'Range cell_1 cell_2))
      (vlax-get-property
        (vlax-get-property cell_range_obj_ 'Interior  )
        'color
        
        
      )
      (vlax-dump-object (vlax-get-property cell_range_obj_ 'Interior  ))

    ;

  )
  (defun TA_EXCEL_input_rangeccell_color (_worksheet_obj_ lst_cell_1 lst_cell_2 color_rgb_list )
    ;Note By Code_Developer
    ;The principle of codework is desgined for merge cell object in Excel APP
    ;
    ;Example Argument
      ; (setq lst_cell_1 (list 1 5))
      ; (setq lst_cell_2 (list 1 8))
      ; (setq color_rgb_list (list 50 50 50))
    ;
   
      (setq cell_1 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_1) (cadr lst_cell_1))))
      (setq cell_2 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_2) (cadr lst_cell_2))))
      (setq cell_range_obj_ (vlax-get-property _worksheet_obj_ 'Range cell_1 cell_2))
      (vlax-put-property
        (vlax-get-property cell_range_obj_ 'Interior  )
        'color
        
        (car 
          (list 
            (+ (car color_rgb_list) 
               (* 256 (cadr color_rgb_list))
               (* 256 (* 256 (caddr color_rgb_list)))
            )
          )
        )
      )

    ;

  )
  (defun TA_EXCEL_input_rangeccell_colorindex (_worksheet_obj_ lst_cell_1 lst_cell_2 colorindex_code )
    ;Note By Code_Developer
    ;The principle of codework is desgined for merge cell object in Excel APP
    ;
    ;Example Argument
      ; (setq lst_cell_1 (list 1 5))
      ; (setq lst_cell_2 (list 1 8))
      ; (setq color_rgb_list (list 50 50 50))
    ;
   
      (setq cell_1 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_1) (cadr lst_cell_1))))
      (setq cell_2 (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car lst_cell_2) (cadr lst_cell_2))))
      (setq cell_range_obj_ (vlax-get-property _worksheet_obj_ 'Range cell_1 cell_2))
      (vlax-put-property
        (vlax-get-property cell_range_obj_ 'Interior  )
        'color
        
        colorindex_code
      )

    ;

  )
  (defun TA:Excel_Open-Excel_ (Using_file_mode_ clear_file_mode )
    ;Excel_file_process_ 
      ;Note By Code_Developer
        ;Principle of codework is designed for open Excel file or lasted current excel window 
        ;and keep vla-object workbook in varibale
        ;in normally this sub funcntion was developed from commmand sub code
        ;
      ;
      ;Example incase userinput via command while code running
        ; (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
      ;
      ;Example Argument
      ;   (setq Using_file_mode_ 1) ;integer number for indicate condition  ;open new Excel fiel
      ;   (setq Using_file_mode_ 2) ;integer number for indicate condition  ;Use curent Excel file 
      ;   (setq clear_file_mode 1) ;integer number for indicate condition   ;clear content incase currnet Excel file 
      ; ;
      ;Subfunction 
        ; (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_)
      ;
      (cond
        (;Open new Excel File
          (and
            (= Using_file_mode_ 1)
          )
          (progn 
            (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
            (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
            (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
            (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
            (format-all-workbook-list Excel_File+sheet_data_list_)
            (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
            (format-all-worksheet-list Excel_lasted_file_)
            (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
            (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
            (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
          )
        )
        (;Exiting Excel file
          (and
            (= Using_file_mode_ 2)
            (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
            (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
            (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
          )
          (progn
            (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
            (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
            (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
            (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
            (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
            ;incase userpinput
              ; (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
              ;   (progn
              ;     (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
              ;   )
              ; )
            ;
            (if ( = clear_file_mode 1)
              (progn
                (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
              )
            )                
          )    
        )
      )
      ; (vlax-dump-object get_ExcelApp_object_ )
      ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
      ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
      ; (vlax-dump-object get_ExcelApp_object_ )
      ; (vlax-get get_ExcelApp_object_ 'selection  )
      (setq Excel_lasted_worksheet_obj_ Excel_lasted_worksheet_obj_)
    ;
  )
  (defun TA:EXCEL_autofit_column_ (_worksheet_obj_)
    (vlax-invoke-method
      (vlax-variant-value 
        (vlax-get-property 
          (vlax-get-property 
            _worksheet_obj_ 
            "columns"
          )
          "item"
          1 
        )
      )
      'autofit
    )
  )
  (defun TA:EXCEL_autofit_rangecell_ (_worksheet_obj_ range_cell_1_obj_ range_cell_2_obj_ )
    ;Note By Code_Developer
    ;principle of codework is baseon VBA language 
      ;Worksheets("Sheet1").Range("A1:E1").Columns.AutoFit
    ;
    (setq range_cell_1_obj_ (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car range_cell_1) (cadr range_cell_1))))
    (setq range_cell_2_obj_ (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item (car range_cell_2) (cadr range_cell_2))))
    (vlax-invoke-method 
      (vlax-get-property 
        (setq vla-range_obj_ 
          (vlax-get-property 
            _worksheet_obj_
            'Range
            range_cell_1_obj_
            range_cell_2_obj_
          )
        )
        'columns
      )
      'autofit
    )
  )
  (defun TA:Excel_foreach_input_list_dataset_R1C1 (_worksheet_obj_ row_start_ col_start_ main_list_  )
    ;Note By Code_Developer
      ;principle of coodework is designed for input data list to Excel appication via list/gruop data,
      ;Can specify R1C1 via argument (row_start_ col_start_)

      ;Precautions when using this program 
        ;The argument main_list_ must contain data in the form of a main list that groups multiple sublists.
        ;Each sublist holds detailed data intended for display purposes.
        ;Example
          ; (setq mainlist ;main list_
          ;                (list 
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                )
          ; )
          ;Precautions when using this program 
            ;The argument main_list_ must contain data in the form of a main list that groups multiple sublists.
            ;Each sublist holds detailed data intended for display purposes.
            ;Example 1
              ; mainlist_
              ; └─ sublist
              ;    └─ sublist of sublist (maximun Below lv.3 )
              ; └─ sublist
              ;    └─ sublist of sublist (maximun Below lv.3 )

              ; (setq mainlist ;main list_
              ;                (list 
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 (list "A" "B" "C" "D") 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 (list "A" "B" "C" "D") 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                )
              ; )
            ;
          ;
        ;
      ;
    ;
    ;
    ; ;Example_argument_
    ;   (setq row_start_ 1)
    ;   (setq col_start_ 1)
    ; ;
      (foreach sub_group_ main_list_
        (setq col_ col_start_)
        (if 
          (= (listp sub_group_) T)
          (progn
            (foreach sub_list_ sub_group_  ; ← ลูปชั้นที่ 2
              (if (listp sub_list_)
                (foreach sub_value_ sub_list_  ; ← ลูปชั้นที่ 3
                  (progn
                    (vlax-put
                      (vlax-variant-value
                        (vlax-get-property
                          (vlax-get _worksheet_obj_ 'Cells)
                          'Item
                          row_start_
                          col_
                        )
                      )
                      'Value
                      sub_value_
                    )
                    (setq col_ (+ col_ 1))
                  )
                )
                ;; ถ้า sub_list_ ไม่ใช่ list → แปลว่าเป็นค่าธรรมดา (อาจอยู่แค่ 2 ชั้น)
                (progn
                  (vlax-put
                    (vlax-variant-value
                      (vlax-get-property
                        (vlax-get _worksheet_obj_ 'Cells)
                        'Item
                        row_start_
                        col_
                      )
                    )
                    'Value
                    sub_list_
                  )
                  (setq col_ (+ col_ 1))
                )
              )
            )
          )
        )

        (setq row_start_ (+ row_start_ 1))
      )
  )
  (defun TA:EXCEL_put_columnformat (VLA-OBJECT-Worksheet column_number format_value)
    ;Note By Code_Developer
    ;waiting for input format argument letter
    ;
    (setq vla-object-cols_ (vlax-get-property VLA-OBJECT-Worksheet 'Columns))
      (setq vla-object-col_(vlax-get-property (vlax-variant-value (vlax-get-property vla-object-cols_ 'Item column_number)) 'EntireColumn))
        (vlax-put vla-object-col_ 'NumberFormat format_value)
  )


  (defun c:Open_Excel_File_OPEX_ ()
    (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (setq filePath (getfiled "Select an Excel file"  "C:/TA/TEMP EST FILES/" "" 8)) ;open Ex-file
    (if 
      (and
        (if
          (/= filePath nil)
          (progn
            (or 
              (wcmatch (strcase filePath) "*XLSX")
              (wcmatch (strcase filePath) "*XLSM")
            )
          )
          nil          
        )
      )
      (progn
            ; (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
              (setq _Workbook_obj_ (vlax-invoke-method (vlax-get-property vla-obj_app 'WorkBooks) 'Open filePath)) ;VLA-OBJECT _Workbook
                (setq sheets_obj_ (vlax-get-property _Workbook_obj_ 'Sheets)) ;VLA-OBJECT Sheets
                  (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count )) ;total_worksheet_in_file_
                (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item 1)) ;VLA-OBJECT _Worksheet
                (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))

          
          

           
        
        
        
        
          (vlax-put-property vla-obj_app 'Visible :vlax-true)
      )
      (alert "invalid file please try agaian")
    )
  )




  (defun c:PP2Excel_ ()
    (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (setq Excel_obj_ (TA:New_Excel_File_NEX_ vla-obj_app ))
    ;preloop_and_while
      (setq sum_length_all_i 0)
      (setq row_i 2)
      (while (< sum_length_all_i (length sum_length_all_))
        (setq sum_length_all_main_ (car (nth  sum_length_all_i sum_length_all_)))
        (setq sum_length_all_sub_ (cdr (nth  sum_length_all_i sum_length_all_)))

        (setq Excel_entlast_ (caddr (car (nth 3 (car (reverse (TA:Excel_Assembly_ALL-obj_list_ Excel_obj_ )))))))
        
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 2 sum_length_all_main_ )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 3 sum_length_all_sub_ )
        
        (setq row_i (+ row_i 1))
        (setq sum_length_all_i (+ sum_length_all_i 1))
      )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 1 2 "legnth" )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 1 3 "qty." )
        
    ;
  )








;

;for test command
  (defun c:pt1 () 
    (setq ee_ename_ (car (entsel "specify Object")))
    (setq ee_obj_ (vlax-ename->vla-object ee_ename_))
    (setq ee_obj_ins_ (vlax-safearray->list 
                        (vlax-variant-value (vla-get-insertionpoint ee_obj_))
                      )
    )
    (setq pt (getpoint))

    (command "text" 
            pt
            0
            (strcat 
              (rtos (car ee_obj_ins_) 2 8)
            )
    )
    (command "text" 
            (list (car pt) 
                  (- (cadr pt) 10)
            )
            0
            (strcat 
              (rtos (cadr ee_obj_ins_) 2 8)
            )
    )
  )
;
;MAIN IDEA COMMNAD
  ;Autocad -> EXCEL
      ; (defun c:Caddata_to_Exceldata_C2E ()
      ;     ;while_select_block_on_condition_
      ;         (setq target_EFname_ nil)
      ;         (while (= target_EFname_ nil)
      ;           (setq target_EFname_ (car (entsel "specify target_EFname Object")))
      ;           (if
      ;             (and ;conditional_rule_for_select_object
      ;               (/= target_EFname_ nil)
      ;               (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
      ;               (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
      ;               ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
      ;               ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
      ;             )
      ;             (progn
      ;               (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
      ;             )
      ;             (alert "Object invalid Please try again")
      ;           )
      ;         )
      ;     ;
      ;     ;selection_set_for_fillter_blk_name
      ;       (if  ;pre_select_ssget_or_post_select_ssget
      ;         (=
      ;           (setq ss_pre_filter_set_xx_ (ssget "i"
      ;                                             (list
      ;                                               (cons 0 "INSERT") ;type of object
      ;                                               ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                               ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                             )
      ;                                       )
      ;           )
      ;           nil
      ;         )
      ;         (progn
      ;           (setq ss_pre_filter_set_xx_ (ssget 
      ;                                             (list
      ;                                               (cons 0 "INSERT") ;type of object
      ;                                               ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                               ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                             )
      ;                                       )
      ;           )
      ;         )
      ;         (sslength ss_pre_filter_set_xx_)
      ;       )
      ;     ;
      ;     ;get_data_process_for_fillter_blk_name
      ;       (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
      ;       (sslength target_block_)
      ;     ;

      ;     ;preloop_and_while
      ;       (setq sum_heading_ ())
      ;       (setq sum_att_data_ ())
      ;       (setq target_block_i 0)
      ;       (while 
      ;           (and
      ;             (< target_block_i (sslength target_block_))
      ;           )
      ;           (setq target_block_ename_ (ssname target_block_ target_block_i))
      ;           (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
      ;           ;main_idea_of_code
      ;             (setq standard_heading_
      ;               (list
      ;                 (setq target_block_obj_handle_heading "Handle")     
      ;                 (setq Layout_name_heading "Layout")
      ;                 (setq target_block_obj_EFname_heading "Name_block")
      ;                 (setq target_block_obj_color_heading "Color")
      ;                 (setq target_block_obj_layer_heading "Layer")
      ;                 (setq target_block_obj_linetype_heading "Linetype")
      ;                 (setq target_block_obj_ins_x_heading "position_X")
      ;                 (setq target_block_obj_ins_y_heading "position_Y")
      ;                 (setq target_block_obj_ins_z_heading "position_Z")
      ;                 (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
      ;               )
      ;             )
      ;             (setq standard_data_
      ;               (list
      ;                 (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
      ;                 (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
      ;                 (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
      ;                 (setq target_block_obj_color_data (vla-get-color target_block_obj_))
      ;                 (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
      ;                 (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
      ;                 (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;                 (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;                 (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;                 (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
      ;               )
      ;             )
      ;             (if ;attribute_get_heading+data_process
      ;               (and 
      ;                 (vla-get-hasattributes target_block_obj_)
      ;               )
      ;               (progn
      ;                 (setq att_heading_ 
      ;                   (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
      ;                 )
      ;                 (setq att_data_ 
      ;                   (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
      ;                 )
                      
      ;               )
      ;               (setq att_heading_ nil
      ;                     att_data_    nil
      ;               )
      ;             )
      ;             (if ;dynamic_get_heading+data_process
      ;               (and
      ;                 (vla-get-isdynamicblock target_block_obj_)
      ;               )
      ;               (progn
      ;                 (setq dyn_heading_
      ;                   (LM-TA:getdynprops target_block_obj_ )
      ;                 )
      ;                 (setq dyn_data_
      ;                   (LM-TA:getdynvals target_block_obj_ )
      ;                 )
      ;               )
      ;               (setq dyn_heading_ nil
      ;                     dyn_data_    nil
      ;               )
      ;             )
      ;             ;summary_data/loop_
      ;               (if (= sum_heading_ nil)
      ;                 (progn
      ;                   (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) )
                        
      ;                 )
      ;               )      
      ;             ;
                  
      ;             (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_))
      ;           ;
      ;         (setq target_block_i (+ target_block_i 1))
      ;       )
      ;     ;
      ;     ;Excel_file_process_ 
      ;       (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
      ;       (cond
      ;         (;Open new Excel File
      ;           (and
      ;             (= Using_file_mode_ 1)
      ;           )
      ;           (progn 
      ;             (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
      ;             (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;             (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
      ;             (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
      ;             (format-all-workbook-list Excel_File+sheet_data_list_)
      ;             (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
      ;             (format-all-worksheet-list Excel_lasted_file_)
      ;             (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
      ;             (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;             (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
      ;           )
      ;         )
      ;         (;Exiting Excel file
      ;           (and
      ;             (= Using_file_mode_ 2)
      ;             (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
      ;             (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
      ;             (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
      ;           )
      ;           (progn
      ;             (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;             (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
      ;             (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
      ;             (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;             (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                  
      ;             (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
      ;               (progn
      ;                 (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
      ;               )
      ;             )
      ;           )    
      ;         )
      ;       )
      ;       ; (vlax-dump-object get_ExcelApp_object_ )
      ;       ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
      ;       ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
      ;       ; (vlax-dump-object get_ExcelApp_object_ )
      ;       ; (vlax-get get_ExcelApp_object_ 'selection  )
      ;     ;
      ;     ;preloop_and_while_creating_heading_
      ;       (if 
      ;         (or
      ;           (= Using_file_mode_ 1)
      ;           (= Using_file_mode_ 2)
      ;         )
      ;         (progn
      ;           (setq sum_heading_i 0)
      ;           (setq col_i 1)
      ;           (while (< sum_heading_i (length sum_heading_))
      ;             (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
      ;             (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
      ;               (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
      ;               (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
      ;               (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
      ;               (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
      ;               (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
      ;               (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
      ;             (setq sum_heading_i (+ sum_heading_i 1))
      ;             (setq col_i (+ col_i 1))
      ;           )
      ;         )
      ;       )
            
      ;     ;
      ;     ;preloop_and_while_creating_data_
      ;       (if 
      ;         (or
      ;           (= Using_file_mode_ 1)
      ;           (= Using_file_mode_ 2)
      ;         )
      ;         (progn
      ;           ;preloop_and_while
      ;             (setq sum_att_data_i 0)
                  
      ;             (setq row_i 2)
      ;             (while (< sum_att_data_i (length sum_att_data_))
      ;               (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
      ;                 ;preloop_and_while
      ;                   (setq sum_att_data_list_i 0)
      ;                   (setq col_i 1)
      ;                   (while (< sum_att_data_list_i (length sum_att_data_list_))
      ;                     (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
      ;                       ;special_text
      ;                           (if 
      ;                             (and
      ;                               (= col_i 1)
      ;                             )
      ;                             (progn
      ;                               (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                             )
      ;                             (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                           )
      ;                           (if 
      ;                             (and
      ;                               ( = (numberp sum_att_data_list_value_) nil)
      ;                               (wcmatch sum_att_data_list_value_ "*:*")
      ;                             )
      ;                             (progn
      ;                               (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                             )
      ;                             (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                           )
      ;                         ;

      ;                       (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
      ;                       (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
      ;                       (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                            
      ;                       (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_) 2)

      ;                     (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
      ;                     (setq col_i (+ col_i 1))
      ;                     (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
      ;                     (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
      ;                   )
      ;                 ;
      ;               (setq row_i (+ row_i 1))
      ;               (setq sum_att_data_i (+ sum_att_data_i 1))
                    
                    
      ;             )
      ;           ;
      ;         )
      ;       )
      ;     ;

      ;     ;final_alert
      ;       (alert "Tranfer Data Finish")
      ;     ;
      ; ) 
    ; standard_send
      ; (defun c:Caddata_to_Exceldata_C2EE () 
      ;   ;while_select_block_on_condition_
      ;       (setq target_EFname_ nil)
      ;       (while (= target_EFname_ nil)
      ;         (setq target_EFname_ (car (entsel "specify target_EFname Object")))
      ;         (if
      ;           (and ;conditional_rule_for_select_object
      ;             (/= target_EFname_ nil)
      ;             (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
      ;             (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
      ;             ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
      ;             ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
      ;           )
      ;           (progn
      ;             (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
      ;           )
      ;           (alert "Object invalid Please try again")
      ;         )
      ;       )
      ;   ;
      ;   ;selection_set_for_fillter_blk_name
      ;     (if  ;pre_select_ssget_or_post_select_ssget
      ;       (=
      ;         (setq ss_pre_filter_set_xx_ (ssget "i"
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;         nil
      ;       )
      ;       (progn
      ;         (setq ss_pre_filter_set_xx_ (ssget 
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;       )
      ;       (sslength ss_pre_filter_set_xx_)
      ;     )
      ;   ;
      ;   ;get_data_process_for_fillter_blk_name
      ;     (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
      ;     (sslength target_block_)
      ;   ;

      ;   ;preloop_and_while
      ;     (setq sum_heading_ ())
      ;     (setq sum_att_data_ ())
      ;     (setq target_block_i 0)
      ;     (while 
      ;         (and
      ;           (< target_block_i (sslength target_block_))
      ;         )
      ;         (setq target_block_ename_ (ssname target_block_ target_block_i))
      ;         (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
      ;         ;main_idea_of_code
      ;           (setq standard_heading_
      ;             (list
      ;               (setq target_block_obj_handle_heading "Handle")     
      ;               (setq Layout_name_heading "Layout")
      ;               (setq target_block_obj_EFname_heading "Name_block")
      ;               (setq target_block_obj_color_heading "Color")
      ;               (setq target_block_obj_layer_heading "Layer")
      ;               (setq target_block_obj_linetype_heading "Linetype")
      ;               (setq target_block_obj_ins_x_heading "position_X")
      ;               (setq target_block_obj_ins_y_heading "position_Y")
      ;               (setq target_block_obj_ins_z_heading "position_Z")
      ;               (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
      ;             )
      ;           )
      ;           (setq standard_data_
      ;             (list
      ;               (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
      ;               (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
      ;               (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
      ;               (setq target_block_obj_color_data (vla-get-color target_block_obj_))
      ;               (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
      ;               (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
      ;               (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
      ;             )
      ;           )
      ;           (if ;attribute_get_heading+data_process
      ;             (and 
      ;               (vla-get-hasattributes target_block_obj_)
      ;             )
      ;             (progn
      ;               (setq att_heading_ 
      ;                 (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
      ;               )
      ;               (setq att_data_ 
      ;                 (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
      ;               )
                    
      ;             )
      ;             (setq att_heading_ nil
      ;                   att_data_    nil
      ;             )
      ;           )
      ;           (if ;dynamic_get_heading+data_process
      ;             (and
      ;               (vla-get-isdynamicblock target_block_obj_)
      ;             )
      ;             (progn
      ;               (setq dyn_heading_
      ;                 (LM-TA:getdynprops target_block_obj_ )
      ;               )
      ;               (setq dyn_data_
      ;                 (LM-TA:getdynvals target_block_obj_ )
      ;               )
      ;             )
      ;             (setq dyn_heading_ nil
      ;                   dyn_data_    nil
      ;             )
      ;           )
      ;           ;summary_data/loop_
      ;             (if (= sum_heading_ nil)
      ;               (progn
      ;                 (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) )
                      
      ;               )
      ;             )      
      ;           ;
                
      ;           (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_))
                
      ;         ;
      ;       (setq target_block_i (+ target_block_i 1))
      ;     )
      ;   ;
      ;   ;Excel_file_process_ 
      ;     (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
      ;     (cond
      ;       (;Open new Excel File
      ;         (and
      ;           (= Using_file_mode_ 1)
      ;         )
      ;         (progn 
      ;           (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
      ;           (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
      ;           (format-all-workbook-list Excel_File+sheet_data_list_)
      ;           (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
      ;           (format-all-worksheet-list Excel_lasted_file_)
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
      ;         )
      ;       )
      ;       (;Exiting Excel file
      ;         (and
      ;           (= Using_file_mode_ 2)
      ;           (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
      ;           (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
      ;           (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
      ;         )
      ;         (progn
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
      ;           (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
      ;             (progn
      ;               (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
      ;             )
      ;           )
      ;         )    
      ;       )
      ;     )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
      ;     ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-get get_ExcelApp_object_ 'selection  )
      ;   ;
      ;   ;preloop_and_while_creating_heading_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         (setq sum_heading_i 0)
      ;         (setq col_i 1)
      ;         (while (< sum_heading_i (length sum_heading_))
      ;           (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
      ;           (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
      ;             (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
      ;             (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
      ;             (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
      ;             (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
      ;             (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
      ;             (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
      ;           (setq sum_heading_i (+ sum_heading_i 1))
      ;           (setq col_i (+ col_i 1))
      ;         )
      ;       )
      ;     )
          
      ;   ;
      ;   ;preloop_and_while_creating_data_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         ;preloop_and_while
      ;           (setq sum_att_data_i 0)
                
      ;           (setq row_i 2)
      ;           (while (< sum_att_data_i (length sum_att_data_))
      ;             (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
      ;               ;status_data
      ;                 (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 3) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
      ;               ;
      ;               ;preloop_and_while
      ;                 (setq sum_att_data_list_i 0)
      ;                 (setq col_i 1)
      ;                 (while (< sum_att_data_list_i (length sum_att_data_list_))
      ;                   (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
                              
      ;                     ;special_text
      ;                       (if 
      ;                         (and
      ;                           (= col_i 1)
      ;                         )
      ;                         (progn
      ;                           (vlax-get (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 2 1)) 'NumberFormat ) ;รอเพิ่มเป็น sun func
      ;                           (vlax-put (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 2 1)) 'NumberFormat "@" ) ;รอเพิ่มเป็น sun func
      ;                           (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                         )
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                       )
      ;                       (if 
      ;                         (and
      ;                           (/= (listp sum_att_data_list_value_) T)
      ;                           ( = (numberp sum_att_data_list_value_) nil)
      ;                           (wcmatch sum_att_data_list_value_ "*:*")
      ;                         )
      ;                         (progn
      ;                           (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                         )
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                       )
      ;                       (if 
      ;                         (and
      ;                           (listp sum_att_data_list_value_)
      ;                           (= (numberp (car sum_att_data_list_value_)) T)
                                
      ;                         )
      ;                         (progn
      ;                           (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" (rtos (car sum_att_data_list_value_) 2 8)) )
      ;                         )
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                       )
      ;                     ;

                          
                          
      ;                     (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
      ;                     (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
      ;                     (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                          
      ;                     ; (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_) 2)

      ;                   (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
      ;                   (setq col_i (+ col_i 1))
      ;                   ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
      ;                   ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
      ;                   ;Status_data
                          
      ;                     (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 4) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
      ;                   ;
      ;                 )
      ;               ;
      ;             (setq row_i (+ row_i 1))
      ;             (setq sum_att_data_i (+ sum_att_data_i 1))
      ;           )
      ;         ;
      ;       )
      ;     )
      ;   ;

      ;   ;final_alert
      ;     (alert "Tranfer Data Finish")
      ;   ;
      ; )
      (defun c:Caddata_to_Exceldata_C2EE1 ()
        ;selection_set_for_fillter_blk_name
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
              (setq ss_pre_filter_set_xx_ (ssget
                                                (list
                                                  (cons 0 "INSERT") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
        ;filter_selection_set_process 
          (setq sst_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ (setq EF_name_val (LM:effectivename (vlax-ename->vla-object (car (entsel "specify name block for flter Ef name "))))) ) )
        ;
        ;main_idea_get_properties_data_ process_
          ; (setq sst_ (ssget)) ;uncomment incase don't use fillter Sub FUNC or testing
          ;preloop_and_while
            (setq sst_i 0)
            (setq sst_properties_mainlist_ ())
            (setq sst_properties_sublist_ ())
            (while (< sst_i (sslength sst_))
              (setq sst_ename_ (ssname sst_ sst_i))
                (setq sst_obj_ (vlax-ename->vla-object sst_ename_))
                (setq sst_obj_std_list_ (list ;main standard propertie
                                  (setq sst_obj_handle (vla-get-handle sst_obj_)) ;sub standard propertie
                                  (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
                                  (setq target_block_obj_EFname_data (LM:effectivename sst_obj_))
                                  (setq target_block_obj_color_data (vla-get-color sst_obj_))
                                  (setq target_block_obj_layer_data (vla-get-layer sst_obj_))
                                  (setq target_block_obj_linetype_data (vla-get-linetype sst_obj_))
                                  (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                                  (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                                  (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                                  (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation sst_obj_)))      
                                )
                )
                (if (= (vla-get-isdynamicblock sst_obj_) ) ;dynamic_properties_list_
                  (progn
                    (setq sst_obj_dyn_val_list (mapcar 'cadr 
                                                      (vl-remove-if 
                                                        '(lambda (x) 
                                                            (wcmatch 
                                                              (strcase (car x))
                                                              "*POINT*"
                                                            )
                                                          )
                                                        (TA:remove_val_list_rev02_ 
                                                          "Origin"
                                                          (setq sst_obj_dyn_all_ (LM-TA:getdynprops+vals 
                                                                            sst_obj_
                                                                          )
                                                          )
                                                        )
                                                      )
                                              )
                    )
                  )
                )
                (if (= (vla-get-hasattributes sst_obj_) :vlax-true) ;attribute_properties_list_
                  (progn
                    (setq sst_obj_att_val_list (LM:vl-getattributevalue-val-TA-Modifies sst_obj_ ) )
                  )
                )
                (if (= sst_i 0) ;for create  tag one time 
                  (progn
                    (setq sst_tag_mainlist_ ())
                    (if (= (vla-get-isdynamicblock sst_obj_))  ;dynamic_properties_list_
                      (progn 
                        (setq sst_obj_dyn_tag_list (mapcar 'car
                                                            (vl-remove-if 
                                                              '(lambda (x) 
                                                                (wcmatch 
                                                                  (strcase (car x))
                                                                  "*POINT*"
                                                                )
                                                              )
                                                              (TA:remove_val_list_rev02_ 
                                                                "Origin"
                                                                (setq sst_obj_dyn_all_ (LM-TA:getdynprops+vals 
                                                                                        sst_obj_
                                                                                      )
                                                                )
                                                              )
                                                            )
                                                    )
                        )
                      )
                    )
                    (if (= (vla-get-hasattributes sst_obj_) :vlax-true) ;attribute_properties_list_
                      (progn
                        (setq sst_obj_att_tag_list (LM:vl-getattributevalue-tag-TA-Modifies sst_obj_ ) )
                      )
                    )
                    (setq sst_tag_mainlist_
                      (list
                        (append 
                          (list
                            "Handle"
                            "Layout"
                            "Name_block"
                            "Color"
                            "Layer"
                            "Linetype"
                            "position_X"
                            "position_Y"
                            "position_Z"
                            "Rotation(DEG)"
                          )
                          sst_obj_dyn_tag_list
                          sst_obj_att_tag_list
                        )
                      )
                    )
                  )
                )
              

              
                
              
              
                (setq sst_properties_sublist_ (append sst_obj_std_list_ (list sst_obj_dyn_val_list) (list sst_obj_att_val_list))) 
                (setq sst_properties_mainlist_ (cons sst_properties_sublist_ sst_properties_mainlist_))
              (setq sst_i (+ sst_i 1))
            )
          ;
        ;
        ;export_properties_data_to_Excel
          (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1)) ;Sub-FUNC for oepn Excel Appication_
          (TA:EXCEL_put_columnformat Excel_lasted_worksheet_obj_ 1 "@" )
          (TA:EXCEL_put_columnformat Excel_lasted_worksheet_obj_ 1 "@" )
          (TA:EXCEL_put_columnformat Excel_lasted_worksheet_obj_ 3 "@" )
          (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (tag list_)
            Excel_lasted_worksheet_obj_ 
            1
            1
            sst_tag_mainlist_
          )
          (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (content list_)
            Excel_lasted_worksheet_obj_ 
            2
            1
            sst_properties_mainlist_
          )
        ;
      )
    ;
    ;Pason_FOR BLK NAME "000_ป่าสน-Tittle_block_Drawing_list_and_content"
      ; (defun c:DrawingList_to_Excel_DL2E ()
        
      ;   ; ;while_select_block_on_condition_ ;;;;;;;; do not use in main code
      ;     ;     (setq target_EFname_ nil)
      ;     ;     (while (= target_EFname_ nil)
      ;     ;       (setq target_EFname_ (car (entsel "specify target_EFname Object")))
      ;     ;       (if
      ;     ;         (and ;conditional_rule_for_select_object
      ;     ;           (/= target_EFname_ nil)
      ;     ;           (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
      ;     ;           (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
      ;     ;           ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
      ;     ;           ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
      ;     ;         )
      ;     ;         (progn
      ;     ;           (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
      ;     ;         )
      ;     ;         (alert "Object invalid Please try again")
      ;     ;       )
      ;     ;     )
      ;   ; ;
      ;   ;selection_set_for_fillter_blk_name
      ;     (if  ;pre_select_ssget_or_post_select_ssget
      ;       (=
      ;         (setq ss_pre_filter_set_xx_ (ssget "i"
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;         nil
      ;       )
      ;       (progn
      ;         (setq ss_pre_filter_set_xx_ (ssget 
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;       )
      ;       (sslength ss_pre_filter_set_xx_)
      ;     )
      ;   ;
      ;   ;get_data_process_for_fillter_blk_name
      ;     (setq target_EFname_list_ "000_ป่าสน-Tittle_block_Drawing_list_and_content")
      ;     (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
      ;     (sslength target_block_)
      ;   ;

      ;   ;preloop_and_while
      ;     (setq sum_heading_ ())
      ;     (setq sum_att_data_ ())
      ;     (setq target_block_i 0)
      ;     (while 
      ;         (and
      ;           (< target_block_i (sslength target_block_))
      ;         )
      ;         (setq target_block_ename_ (ssname target_block_ target_block_i))
      ;         (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
      ;         ;main_idea_of_code
      ;           (setq standard_heading_
      ;             (list
      ;               (setq target_block_obj_handle_heading "Handle")     
      ;               (setq Layout_name_heading "Layout")
      ;               (setq target_block_obj_EFname_heading "Name_block")
      ;               (setq target_block_obj_color_heading "Color")
      ;               (setq target_block_obj_layer_heading "Layer")
      ;               (setq target_block_obj_linetype_heading "Linetype")
      ;               (setq target_block_obj_ins_x_heading "position_X")
      ;               (setq target_block_obj_ins_y_heading "position_Y")
      ;               (setq target_block_obj_ins_z_heading "position_Z")
      ;               (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
      ;             )
      ;           )
      ;           (setq standard_data_
      ;             (list
      ;               (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
      ;               (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
      ;               (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
      ;               (setq target_block_obj_color_data (vla-get-color target_block_obj_))
      ;               (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
      ;               (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
      ;               (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
      ;             )
      ;           )
      ;           (if ;attribute_get_heading+data_process
      ;             (and 
      ;               (vla-get-hasattributes target_block_obj_)
      ;             )
      ;             (progn
      ;               (setq att_heading_ 
      ;                 (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
      ;               )
      ;               (setq att_data_ 
      ;                 (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
      ;               )
                    
      ;             )
      ;             (setq att_heading_ nil
      ;                   att_data_    nil
      ;             )
      ;           )
      ;           ; (if ;dynamic_get_heading+data_process ;;;; do not use in main command
      ;             ;   (and
      ;             ;     (vla-get-isdynamicblock target_block_obj_)
      ;             ;   )
      ;             ;   (progn
      ;             ;     (setq dyn_heading_
      ;             ;       (LM-TA:getdynprops target_block_obj_ )
      ;             ;     )
      ;             ;     (setq dyn_data_
      ;             ;       (LM-TA:getdynvals target_block_obj_ )
      ;             ;     )
      ;             ;   )
      ;             ;   (setq dyn_heading_ nil
      ;             ;         dyn_data_    nil
      ;             ;   )
      ;           ; )
      ;           ;summary_data/loop_
      ;             (if (= sum_heading_ nil)
      ;               (progn
      ;                 ; (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) ) ;;;; do not use in main command
      ;                 (setq sum_heading_ (append  standard_heading_ att_heading_ ) ) ;;;; do not use in main command
                      
      ;               )
      ;             )      
      ;           ;
                
      ;           ; (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_)) ;;;; do not use in main command
      ;           (setq sum_att_data_ (cons (append standard_data_ att_data_ ) sum_att_data_))
      ;         ;
      ;       (setq target_block_i (+ target_block_i 1))
      ;     )
      ;   ;
      ;   ;Excel_file_process_ 
      ;     (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
      ;     (cond
      ;       (;Open new Excel File
      ;         (and
      ;           (= Using_file_mode_ 1)
      ;         )
      ;         (progn 
      ;           (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
      ;           (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
      ;           (format-all-workbook-list Excel_File+sheet_data_list_)
      ;           (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
      ;           (format-all-worksheet-list Excel_lasted_file_)
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
      ;         )
      ;       )
      ;       (;Exiting Excel file
      ;         (and
      ;           (= Using_file_mode_ 2)
      ;           (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
      ;           (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
      ;           (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
      ;         )
      ;         (progn
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
      ;           (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
      ;             (progn
      ;               (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
      ;             )
      ;           )
      ;         )    
      ;       )
      ;     )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
      ;     ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-get get_ExcelApp_object_ 'selection  )
      ;   ;
      ;   ;preloop_and_while_creating_heading_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         (setq sum_heading_i 0)
      ;         (setq col_i 1)
      ;         (while (< sum_heading_i (length sum_heading_))
      ;           (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
      ;           (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
      ;             (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
      ;             (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
      ;             (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
      ;             (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
      ;             (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
      ;             (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
      ;           (setq sum_heading_i (+ sum_heading_i 1))
      ;           (setq col_i (+ col_i 1))
      ;         )
      ;       )
      ;     )
          
      ;   ;
      ;   ;preloop_and_while_creating_data_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         ;preloop_and_while
      ;           (setq sum_att_data_i 0)
                
      ;           (setq row_i 2)
      ;           (while (< sum_att_data_i (length sum_att_data_))
      ;             (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
      ;               ;status_data
      ;                 (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 3) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
      ;               ;
      ;               ;preloop_and_while
      ;                 (setq sum_att_data_list_i 0)
      ;                 (setq col_i 1)
      ;                 (while (< sum_att_data_list_i (length sum_att_data_list_))
      ;                   (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
      ;                   ;special text
      ;                     (if 
      ;                       (and
      ;                         (= col_i 1)
      ;                       )
      ;                       (progn
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                       )
      ;                       (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                     )
      ;                     (if 
      ;                       (and
      ;                         ( = (numberp sum_att_data_list_value_) nil)
      ;                         (wcmatch sum_att_data_list_value_ "*:*")
      ;                       )
      ;                       (progn
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                       )
      ;                       (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                     )
      ;                   ;
                          
                          
      ;                     (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
      ;                     (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
      ;                     (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                          
      ;                     (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_) 2)

      ;                   (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
      ;                   (setq col_i (+ col_i 1))
      ;                   ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
      ;                   ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
      ;                   ;Status_data
                          
      ;                     (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 4) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
      ;                   ;
      ;                 )
      ;               ;
      ;             (setq row_i (+ row_i 1))
      ;             (setq sum_att_data_i (+ sum_att_data_i 1))
      ;           )
      ;         ;
      ;       )
      ;     )
      ;   ;
        
      ;   ;hide column object
      ;     ; (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ 2 -1) 
      ;     ; (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ 2 -1) 
      ;       (setq i 1 )
      ;       (setq i_length 16 )
      ;       (while (< i i_length) 
      ;         (if 
      ;           (and 
      ;             (/= i 1)
      ;             (/= i 3)
                  
      ;             (/= i 11)
      ;             (/= i 12)
      ;             (/= i 13)
      ;             (/= i 14)
      ;             (/= i 15)
      ;             (/= i 16)
                
      ;           )
      ;           (progn 
      ;             (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ i -1)
      ;           )
      ;         )
      ;         (setq i (+ i 1))
      ;       )
      ;   ;
      ;   ;final_alert
      ;     (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 (strcat "Summary block object_" target_EFname_list_) )
          
      ;     (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 25)
      ;     (alert "Tranfer Data Finish")
      ;   ;
      ; )
      (defun c:DrawingList_to_Excel_REV01_DL2E1 ()
        
        ; ;while_select_block_on_condition_ ;;;;;;;; do not use in main code
          ;     (setq target_EFname_ nil)
          ;     (while (= target_EFname_ nil)
          ;       (setq target_EFname_ (car (entsel "specify target_EFname Object")))
          ;       (if
          ;         (and ;conditional_rule_for_select_object
          ;           (/= target_EFname_ nil)
          ;           (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
          ;           (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
          ;           ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
          ;           ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
          ;         )
          ;         (progn
          ;           (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
          ;         )
          ;         (alert "Object invalid Please try again")
          ;       )
          ;     )
        ; ;
        ;selection_set_for_fillter_blk_name
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
              (setq ss_pre_filter_set_xx_ (ssget 
                                                (list
                                                  (cons 0 "INSERT") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
        ;get_data_process_for_fillter_blk_name
          (setq target_EFname_list_ "000_ป่าสน-Tittle_block_Drawing_list_and_content")
          (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
          (sslength target_block_)
        ;

        ;preloop_and_while
          (setq sum_heading_ ())
          (setq sum_att_data_ ())
          (setq target_block_i 0)
          (while 
              (and
                (< target_block_i (sslength target_block_))
              )
              (setq target_block_ename_ (ssname target_block_ target_block_i))
              (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
              ;main_idea_of_code
                (setq standard_heading_
                  (list
                    (setq target_block_obj_handle_heading "Handle")     
                    ; (setq Layout_name_heading "Layout")
                    ; (setq target_block_obj_EFname_heading "Name_block") 
                    ; (setq target_block_obj_color_heading "Color")
                    ; (setq target_block_obj_layer_heading "Layer")
                    ; (setq target_block_obj_linetype_heading "Linetype")
                    ; (setq target_block_obj_ins_x_heading "position_X")
                    ; (setq target_block_obj_ins_y_heading "position_Y")
                    ; (setq target_block_obj_ins_z_heading "position_Z")
                    ; (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
                  )
                )
                (setq standard_data_
                  (list
                    (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
                    ; (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
                    ; (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_)) ;ไม่ใช้จะดีกว่า
                    
                    ; (setq target_block_obj_color_data (vla-get-color target_block_obj_))
                    ; (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
                    ; (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
                    ;(setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_))))) ;ไม่ใช้จะดีกว่า

                    ;(setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_))))) ;ไม่ใช้จะดีกว่า

                    ;(setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_))))) ;ไม่ใช้จะดีกว่า

                    ; (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
                  )
                )
                (if ;attribute_get_heading+data_process
                  (and 
                    (vla-get-hasattributes target_block_obj_)
                  )
                  (progn
                    (setq att_heading_ 
                      (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
                    )
                    (setq att_data_ 
                      (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
                    )
                    
                  )
                  (setq att_heading_ nil
                        att_data_    nil
                  )
                )
                ; (if ;dynamic_get_heading+data_process ;;;; do not use in main command
                  ;   (and
                  ;     (vla-get-isdynamicblock target_block_obj_)
                  ;   )
                  ;   (progn
                  ;     (setq dyn_heading_
                  ;       (LM-TA:getdynprops target_block_obj_ )
                  ;     )
                  ;     (setq dyn_data_
                  ;       (LM-TA:getdynvals target_block_obj_ )
                  ;     )
                  ;   )
                  ;   (setq dyn_heading_ nil
                  ;         dyn_data_    nil
                  ;   )
                ; )
                ;summary_data/loop_
                  (if (= sum_heading_ nil)
                    (progn
                      ; (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) ) ;;;; do not use in main command
                      (setq sum_heading_ (append  standard_heading_ att_heading_ ) ) ;;;; do not use in main command
                      
                    )
                  )      
                ;
                
                ; (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_)) ;;;; do not use in main command
                (setq sum_att_data_ (cons (append standard_data_ att_data_ ) sum_att_data_))
              ;
            (setq target_block_i (+ target_block_i 1))
          )
        ;
        ;Excel_file_process_ 
          (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
          (cond
            (;Open new Excel File
              (and
                (= Using_file_mode_ 1)
              )
              (progn 
                (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
                (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
                (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
                (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
                (format-all-workbook-list Excel_File+sheet_data_list_)
                (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
                (format-all-worksheet-list Excel_lasted_file_)
                (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
                (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
                (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
              )
            )
            (;Exiting Excel file
              (and
                (= Using_file_mode_ 2)
                (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
                (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
                (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
              )
              (progn
                (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
                (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
                (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
                (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
                (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
                (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
                  (progn
                    (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
                  )
                )
              )    
            )
          )
          ; (vlax-dump-object get_ExcelApp_object_ )
          ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
          ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
          ; (vlax-dump-object get_ExcelApp_object_ )
          ; (vlax-get get_ExcelApp_object_ 'selection  )
        ;
        ;preloop_and_while_creating_heading_
          (if 
            (or
              (= Using_file_mode_ 1)
              (= Using_file_mode_ 2)
            )
            (progn
              (setq sum_heading_i 0)
              (setq col_i 1)
              (while (< sum_heading_i (length sum_heading_))
                (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
                (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
                  (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
                  (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
                  (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
                  (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
                  (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
                  (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
                (setq sum_heading_i (+ sum_heading_i 1))
                (setq col_i (+ col_i 1))
              )
            )
          )
          
        ;
        ;preloop_and_while_creating_data_
          (if 
            (or
              (= Using_file_mode_ 1)
              (= Using_file_mode_ 2)
            )
            (progn
              ;preloop_and_while
                (setq sum_att_data_i 0)
                
                (setq row_i 2)
                (while (< sum_att_data_i (length sum_att_data_))
                  (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
                    ;status_data
                      (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 3) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
                    ;
                    ;preloop_and_while
                      (setq sum_att_data_list_i 0)
                      (setq col_i 1)
                      (while (< sum_att_data_list_i (length sum_att_data_list_))
                        (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
                        ;special text
                          (if 
                            (and
                              (= col_i 1)
                            )
                            (progn
                              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                            )
                            (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                          )
                          (if 
                            (and
                              ( = (numberp sum_att_data_list_value_) nil)
                              (wcmatch sum_att_data_list_value_ "*:*")
                            )
                            (progn
                              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                            )
                            (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                          )
                        ;
                          
                          
                          (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
                          (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
                          (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                          
                          (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_) 2)

                        (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
                        (setq col_i (+ col_i 1))
                        ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
                        ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
                        ;Status_data
                          
                          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 4) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
                        ;
                      )
                    ;
                  (setq row_i (+ row_i 1))
                  (setq sum_att_data_i (+ sum_att_data_i 1))
                )
              ;
            )
          )
        ;
        
        ; ;hide column object
        ;   ; (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ 2 -1) 
        ;   ; (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ 2 -1) 
        ;     (setq i 1 )
        ;     (setq i_length 16 )
        ;     (while (< i i_length) 
        ;       (if 
        ;         (and 
        ;           (/= i 1)
        ;           (/= i 3)
                  
        ;           (/= i 11)
        ;           (/= i 12)
        ;           (/= i 13)
        ;           (/= i 14)
        ;           (/= i 15)
        ;           (/= i 16)
                
        ;         )
        ;         (progn 
        ;           (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ i -1)
        ;         )
        ;       )
        ;       (setq i (+ i 1))
        ;     )
        ; ;
        ;final_alert
          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 (strcat "Summary block object_" target_EFname_list_) )
          
          (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 25)
          (alert "Tranfer Data Finish")
        ;
      )
    ;
    ;Pason_FOR BLK NAME "000_ป่าสน-Tittle_block_Ax_LAND"
      ; (defun c:Pason_Tittle_block_to_Excel_TT2E ()
      ;   ;Note By Code_Developer
      ;   ;This command is designed to work exclusively with a block named '000_ป่าสน-Tittle_block_Ax_LAND'.
      ;   ;The operation of the command will read the block's attribute values and generate a set of text.
      ;   ;Fully command must have sub-functions with names starting with TA: or LM:
      ;   ;
      ;   ;

      ;   ;while_select_block_on_condition_ ;;;;;;;; do not use in main code
      ;       (setq target_EFname_ nil)
      ;       (while (= target_EFname_ nil)
      ;         (setq target_EFname_ (car (entsel "specify target_EFname Object")))
      ;         (if
      ;           (and ;conditional_rule_for_select_object
      ;             (/= target_EFname_ nil)
      ;             (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
      ;             (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
      ;             ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
      ;             ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
      ;           )
      ;           (progn
      ;             (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
      ;           )
      ;           (alert "Object invalid Please try again")
      ;         )
      ;       )
      ;   ;
      ;   ;selection_set_for_fillter_blk_name
      ;     (if  ;pre_select_ssget_or_post_select_ssget
      ;       (=
      ;         (setq ss_pre_filter_set_xx_ (ssget "i"
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;         nil
      ;       )
      ;       (progn
      ;         (setq ss_pre_filter_set_xx_ (ssget 
      ;                                           (list
      ;                                             (cons 0 "INSERT") ;type of object
      ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                           )
      ;                                     )
      ;         )
      ;       )
      ;       (sslength ss_pre_filter_set_xx_)
      ;     )
      ;   ;
      ;   ;get_data_process_for_fillter_blk_name
      ;     (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
      ;     (sslength target_block_)
      ;   ;

      ;   ;preloop_and_while
      ;     (setq sum_heading_ ())
      ;     (setq sum_att_data_ ())
      ;     (setq target_block_i 0)
      ;     (while 
      ;         (and
      ;           (< target_block_i (sslength target_block_))
      ;         )
      ;         (setq target_block_ename_ (ssname target_block_ target_block_i))
      ;         (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
      ;         ;main_idea_of_code
      ;           (setq standard_heading_
      ;             (list
      ;               (setq target_block_obj_handle_heading "Handle")     
      ;               (setq Layout_name_heading "Layout")
      ;               (setq target_block_obj_EFname_heading "Name_block")
      ;               (setq target_block_obj_color_heading "Color")
      ;               (setq target_block_obj_layer_heading "Layer")
      ;               (setq target_block_obj_linetype_heading "Linetype")
      ;               (setq target_block_obj_ins_x_heading "position_X")
      ;               (setq target_block_obj_ins_y_heading "position_Y")
      ;               (setq target_block_obj_ins_z_heading "position_Z")
      ;               (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
      ;             )
      ;           )
      ;           (setq standard_data_
      ;             (list
      ;               (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
      ;               (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
      ;               (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
      ;               (setq target_block_obj_color_data (vla-get-color target_block_obj_))
      ;               (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
      ;               (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
      ;               (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
      ;               (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
      ;             )
      ;           )
      ;           (if ;attribute_get_heading+data_process
      ;             (and 
      ;               (vla-get-hasattributes target_block_obj_)
      ;             )
      ;             (progn
      ;               (setq att_heading_ 
      ;                 (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
      ;               )
      ;               (setq att_data_ 
      ;                 (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
      ;               )
                    
      ;             )
      ;             (setq att_heading_ nil
      ;                   att_data_    nil
      ;             )
      ;           )
      ;           ; (if ;dynamic_get_heading+data_process ;;;; do not use in main command
      ;             ;   (and
      ;             ;     (vla-get-isdynamicblock target_block_obj_)
      ;             ;   )
      ;             ;   (progn
      ;             ;     (setq dyn_heading_
      ;             ;       (LM-TA:getdynprops target_block_obj_ )
      ;             ;     )
      ;             ;     (setq dyn_data_
      ;             ;       (LM-TA:getdynvals target_block_obj_ )
      ;             ;     )
      ;             ;   )
      ;             ;   (setq dyn_heading_ nil
      ;             ;         dyn_data_    nil
      ;             ;   )
      ;           ; )
      ;           ;summary_data/loop_
      ;             (if (= sum_heading_ nil)
      ;               (progn
      ;                 ; (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) ) ;;;; do not use in main command
      ;                 (setq sum_heading_ (append  standard_heading_ att_heading_ ) ) ;;;; do not use in main command
                      
      ;               )
      ;             )      
      ;           ;
                
      ;           ; (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_)) ;;;; do not use in main command
      ;           (setq sum_att_data_ (cons (append standard_data_ att_data_ ) sum_att_data_))
      ;         ;
      ;       (setq target_block_i (+ target_block_i 1))
      ;     )
      ;   ;
      ;   ;Excel_file_process_ 
      ;     (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
      ;     (cond
      ;       (;Open new Excel File
      ;         (and
      ;           (= Using_file_mode_ 1)
      ;         )
      ;         (progn 
      ;           (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
      ;           (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
      ;           (format-all-workbook-list Excel_File+sheet_data_list_)
      ;           (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
      ;           (format-all-worksheet-list Excel_lasted_file_)
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
      ;         )
      ;       )
      ;       (;Exiting Excel file
      ;         (and
      ;           (= Using_file_mode_ 2)
      ;           (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
      ;           (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
      ;           (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
      ;         )
      ;         (progn
      ;           (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
      ;           (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
      ;           (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
      ;           (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
      ;           (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
      ;           (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
      ;             (progn
      ;               (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
      ;             )
      ;           )
      ;         )    
      ;       )
      ;     )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
      ;     ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
      ;     ; (vlax-dump-object get_ExcelApp_object_ )
      ;     ; (vlax-get get_ExcelApp_object_ 'selection  )
      ;   ;
      ;   ;preloop_and_while_creating_heading_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         (setq sum_heading_i 0)
      ;         (setq col_i 1)
      ;         (while (< sum_heading_i (length sum_heading_))
      ;           (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
      ;           (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
      ;             (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
      ;             (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
      ;             (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
      ;             (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
      ;             (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
      ;             (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
      ;           (setq sum_heading_i (+ sum_heading_i 1))
      ;           (setq col_i (+ col_i 1))
      ;         )
      ;       )
      ;     )
          
      ;   ;
      ;   ;preloop_and_while_creating_data_
      ;     (if 
      ;       (or
      ;         (= Using_file_mode_ 1)
      ;         (= Using_file_mode_ 2)
      ;       )
      ;       (progn
      ;         ;preloop_and_while
      ;           (setq sum_att_data_i 0)
                
      ;           (setq row_i 2)
      ;           (while (< sum_att_data_i (length sum_att_data_))
      ;             (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
      ;               ;status_data
      ;                 (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 3) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
      ;               ;
      ;               ;preloop_and_while
      ;                 (setq sum_att_data_list_i 0)
      ;                 (setq col_i 1)
      ;                 (while (< sum_att_data_list_i (length sum_att_data_list_))
      ;                   (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
      ;                   ;special text
      ;                     (if 
      ;                       (and
      ;                         (= col_i 1)
      ;                       )
      ;                       (progn
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                       )
      ;                       (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                     )
      ;                     (if 
      ;                       (and
      ;                         ( = (numberp sum_att_data_list_value_) nil)
      ;                         (wcmatch sum_att_data_list_value_ "*:*")
      ;                       )
      ;                       (progn
      ;                         (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
      ;                       )
      ;                       (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
      ;                     )
      ;                   ;
                          
                          
      ;                     (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
      ;                     (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
      ;                     (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                          
      ;                     (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_) 2)

      ;                   (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
      ;                   (setq col_i (+ col_i 1))
      ;                   ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
      ;                   ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
      ;                   ;Status_data
                          
      ;                     (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 4) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
      ;                   ;
      ;                 )
      ;               ;
      ;             (setq row_i (+ row_i 1))
      ;             (setq sum_att_data_i (+ sum_att_data_i 1))
      ;           )
      ;         ;
      ;       )
      ;     )
      ;   ;
        
      ;   ;hide column object
      ;     (setq i 1 )
      ;     (setq i_length 41 )
      ;     (while (< i i_length) 
      ;       (if 
      ;         (and 
      ;           (/= i 1)
      ;           (/= i 3)
      ;           (/= i 32)
      ;           (/= i 33)
      ;           (/= i 34)
                
      ;           (/= i 36)
      ;           (/= i 39)
      ;           (/= i 40)
      ;           (/= i 41)
      ;         )
      ;         (progn 
      ;           (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ i -1)
      ;         )
      ;       )
      ;       (setq i (+ i 1))
      ;     )
      ;   ;
      ;   ;final_alert
      ;     (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 (strcat "Summary block object" target_EFname_list_) )
          
      ;     (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 25)
      ;     (alert "Tranfer Data Finish")
      ;   ;
      ; )
      (defun c:Pason_Tittle_block_to_Excel_REV01_TT2E1 ()
        ;Note By Code_Developer
        ;This command is designed to work exclusively with a block named '000_ป่าสน-Tittle_block_Ax_LAND'.
        ;The operation of the command will read the block's attribute values and send all attribute to Excel.
        ;REV02 is Revision number 1 from c:Pason_Tittle_block_to_Excel_TT2E 
        ;Recent Updates
        ;Reduce attributes from all attributes to the following list.
        ; (setq att_heading_ 
        ;   (list
        ;     "แบบแสดง_:_value_1"
        ;     "แบบแสดง_:_value_2"
        ;     "แบบแสดง_:_value_3"
        ;     "แผ่นที่_VALUE"
        ;     "จำนวนแผ่น_VALUE"
        ;     "มาตราส่วน_VALUE"
        ;   )
        ; )
        ;
        ;Fully command must have sub-functions with names starting with TA: or LM:
        ;
        ;

        ;while_select_block_on_condition_ ;;;;;;;; do not use in main code
            (setq target_EFname_ nil)
            (while (= target_EFname_ nil)
              (setq target_EFname_ (car (entsel "specify target_EFname Object")))
              (if
                (and ;conditional_rule_for_select_object
                  (/= target_EFname_ nil)
                  (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
                  (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
                  ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
                  ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
                )
                (progn
                  (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
                )
                (alert "Object invalid Please try again")
              )
            )
        ;
        ;selection_set_for_fillter_blk_name
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
              (setq ss_pre_filter_set_xx_ (ssget 
                                                (list
                                                  (cons 0 "INSERT") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
        ;get_data_process_for_fillter_blk_name
          (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
          (sslength target_block_)
        ;

        ;preloop_and_while [Revision codework]
          (setq sum_heading_ ())
          (setq sum_att_data_ ())
          (setq target_block_i 0)
          (while 
              (and
                (< target_block_i (sslength target_block_))
              )
              (setq target_block_ename_ (ssname target_block_ target_block_i))
              (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
              ;main_idea_of_code
                (setq standard_heading_
                  (list
                    (setq target_block_obj_handle_heading "Handle")     
                    ; (setq Layout_name_heading "Layout")
                    (setq target_block_obj_EFname_heading "Name_block")
                    ; (setq target_block_obj_color_heading "Color")
                    ; (setq target_block_obj_layer_heading "Layer")
                    ; (setq target_block_obj_linetype_heading "Linetype")
                    (setq target_block_obj_ins_x_heading "position_X")
                    (setq target_block_obj_ins_y_heading "position_Y")
                    (setq target_block_obj_ins_z_heading "position_Z")
                    ; (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
                  )
                )
                (setq standard_data_
                  (list
                    (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
                    ; (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
                    (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
                    ; (setq target_block_obj_color_data (vla-get-color target_block_obj_))
                    ; (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
                    ; (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
                    (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
                    (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
                    (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
                    ; (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
                  )
                )
                (if ;attribute_get_heading+data_process
                  (and 
                    (vla-get-hasattributes target_block_obj_)
                  )
                  (progn
                    (setq att_heading_ 
                      (list
                        "แบบแสดง_:_value_1"
                        "แบบแสดง_:_value_2"
                        "แบบแสดง_:_value_3"
                        "แผ่นที่_VALUE"
                        "จำนวนแผ่น_VALUE"
                        "มาตราส่วน_VALUE"
                      )
                    )
                    (setq att_data_ 
                      (list 
                        (LM:vl-getattributevalue target_block_obj_ "แบบแสดง_:_value_1" )
                        (LM:vl-getattributevalue target_block_obj_ "แบบแสดง_:_value_2" )
                        (LM:vl-getattributevalue target_block_obj_ "แบบแสดง_:_value_3" )
                        (LM:vl-getattributevalue target_block_obj_ "แผ่นที่_VALUE")
                        (LM:vl-getattributevalue target_block_obj_ "จำนวนแผ่น_VALUE" )
                        (LM:vl-getattributevalue target_block_obj_ "มาตราส่วน_VALUE" )
                      )
                    )
                  )
                  (setq att_heading_ nil
                        att_data_    nil
                  )
                )
                ; (if ;dynamic_get_heading+data_process ;;;; do not use in main command
                  ;   (and
                  ;     (vla-get-isdynamicblock target_block_obj_)
                  ;   )
                  ;   (progn
                  ;     (setq dyn_heading_
                  ;       (LM-TA:getdynprops target_block_obj_ )
                  ;     )
                  ;     (setq dyn_data_
                  ;       (LM-TA:getdynvals target_block_obj_ )
                  ;     )
                  ;   )
                  ;   (setq dyn_heading_ nil
                  ;         dyn_data_    nil
                  ;   )
                ; )
                ;summary_data/loop_
                  (if (= sum_heading_ nil)
                    (progn
                      ; (setq sum_heading_ (append (append  standard_heading_ att_heading_ )dyn_heading_ ) ) ;;;; do not use in main command
                      (setq sum_heading_ (append  standard_heading_ att_heading_ ) ) ;;;; do not use in main command
                      
                    )
                  )      
                ;
                
                ; (setq sum_att_data_ (cons (append (append standard_data_ att_data_ ) dyn_data_) sum_att_data_)) ;;;; do not use in main command
                (setq sum_att_data_ (cons (append standard_data_ att_data_ ) sum_att_data_))
              ;
            (setq target_block_i (+ target_block_i 1))
          )
        ;
        ;Excel_file_process_ 
          (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
          (cond
            (;Open new Excel File
              (and
                (= Using_file_mode_ 1)
              )
              (progn 
                (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
                (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
                (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
                (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
                (format-all-workbook-list Excel_File+sheet_data_list_)
                (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
                (format-all-worksheet-list Excel_lasted_file_)
                (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
                (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
                (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
              )
            )
            (;Exiting Excel file
              (and
                (= Using_file_mode_ 2)
                (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
                (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
                (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
              )
              (progn
                (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
                (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
                (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
                (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
                (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
                
                (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
                  (progn
                    (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
                  )
                )
              )    
            )
          )
          ; (vlax-dump-object get_ExcelApp_object_ )
          ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
          ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
          ; (vlax-dump-object get_ExcelApp_object_ )
          ; (vlax-get get_ExcelApp_object_ 'selection  )
        ;
        ;preloop_and_while_creating_heading_
          (if 
            (or
              (= Using_file_mode_ 1)
              (= Using_file_mode_ 2)
            )
            (progn
              (setq sum_heading_i 0)
              (setq col_i 1)
              (while (< sum_heading_i (length sum_heading_))
                (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
                (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
                  (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 1 col_i sum_heading_tag_ )
                  (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
                  (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
                  (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i  "center")
                  (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ 1 col_i "center")
                  (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ 1 col_i 15)
                (setq sum_heading_i (+ sum_heading_i 1))
                (setq col_i (+ col_i 1))
              )
            )
          )
          
        ;
        ;preloop_and_while_creating_data_
          (if 
            (or
              (= Using_file_mode_ 1)
              (= Using_file_mode_ 2)
            )
            (progn
              ;preloop_and_while
                (setq sum_att_data_i 0)
                
                (setq row_i 2)
                (while (< sum_att_data_i (length sum_att_data_))
                  (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
                    ;status_data
                      (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 3) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
                    ;
                    ;preloop_and_while
                      (setq sum_att_data_list_i 0)
                      (setq col_i 1)
                      (while (< sum_att_data_list_i (length sum_att_data_list_))
                        (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
                        ;special text
                          (if 
                            (and
                              (= col_i 1)
                            )
                            (progn
                              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                            )
                            (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                          )
                          (if 
                            (and
                              ( = (numberp sum_att_data_list_value_) nil)
                              (wcmatch sum_att_data_list_value_ "*:*")
                            )
                            (progn
                              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                            )
                            (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                          )
                        ;
                          
                          
                          (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
                          (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
                          (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                          
                          (TA:EXCEL_put_columnwidth_viatext 1 col_i (length sum_att_data_list_value_) 2)

                        (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
                        (setq col_i (+ col_i 1))
                        ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
                        ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
                        ;Status_data
                          
                          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 4) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
                        ;
                      )
                    ;
                  (setq row_i (+ row_i 1))
                  (setq sum_att_data_i (+ sum_att_data_i 1))
                )
              ;
            )
          )
        ;
        
        ; ;hide column object
        ;   (setq i 1 )
        ;   (setq i_length 41 )
        ;   (while (< i i_length) 
        ;     (if 
        ;       (and 
        ;         (/= i 1)
        ;         (/= i 3)
        ;         (/= i 32)
        ;         (/= i 33)
        ;         (/= i 34)
                
        ;         (/= i 36)
        ;         (/= i 39)
        ;         (/= i 40)
        ;         (/= i 41)
        ;       )
        ;       (progn 
        ;         (TA:EXCEL_put_columnhide Excel_lasted_worksheet_obj_ i -1)
        ;       )
        ;     )
        ;     (setq i (+ i 1))
        ;   )
        ; ;
        ;final_alert
          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 (strcat "Summary block object" target_EFname_list_) )
          
          (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 5) 1 25)
          (alert "Tranfer Data Finish")
        ;
      )
    ;
  ;
  ;EXCEL -> AUTOCAD
    ;standard_send
      (defun c:Exceldata_to_Caddata_E2C () ;error
        (setq Activesheet_worksheet_obj_  (vlax-get get_ExcelApp_object_ 'activesheet  ))
        (setq Activesheet_worksheet_getRangecell_ (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ))
        (setq RangeCellName_list_ (LM:str->lst (LM:StringSubst  "" "$" Activesheet_worksheet_getRangecell_ ) ":"))
        (setq rangeCell_Exdata_list_ (TA_EXCEL_RangecellName_to_R1C1 RangeCellName_list_))
        
        
        (defun TA:Rangecell_data_ (rangeCell_Exdata_list_ get_att_val_ get_dyn_val_)
          ;Note By Code_Developer
          ;This command is designed to work exclusively with a Excel File data from ATt/DYN block list 
          ;The operation of the command will read an Excel range data in active worksheet and send all rangecell selection back to ATT/DYN block in Autocad by refer from Handle Object ID
          ;Fully command must have sub-functions with names starting with TA: or LM:
          ;
          ;arrgument
          ;rangeCell_Exdata_list_ = rangecelldata via R1C1
          ;get_att_val_           = specify value for allow get attribute data (0 = false 1 = true)
          ;get_dyn_val_           = specify value for allow get dynamic data (0 = false 1 = true)
          ;
          ; ;example_test
          ;   (setq get_att_val_ 1)
          ;   (setq get_dyn_val_ 1)
          ; ;
          ;specify column and row for while loop
            (setq Col_data_
              (list 
                (setq first_col_ (car (nth 0 rangeCell_Exdata_list_)))
                (setq last_col_ (car (nth 1 rangeCell_Exdata_list_)))
              )
            )
            (setq Row_data_
              (list 
                (setq first_row_ (cadr (nth 0 rangeCell_Exdata_list_)))
                (setq last_row_ (cadr (nth 1 rangeCell_Exdata_list_)))
              )
            )
          ;
          ;preloop_and_while get heading of table _ ATT/DYN data list  
            (setq first_row_i first_row_)
            (setq Overall_rangecell_data_ ())
            (while ;generate_ column range  
              (<= first_row_i last_row_ )
              (setq first_col_i first_col_)
              (setq last_col_i last_col_)
              (setq Range_Exdata_ "")
              (setq Range_Exdata_list_ ())
              (while ;generate_ row range  
                (and
                  (<= first_col_i last_col_i)
                  ; (/= Range_Exdata_ nil)
                )
                (setq Range_Exdata_ (TA:Excel_get_data_R1C1 Activesheet_worksheet_obj_ first_row_i first_col_i ))
                (if (/= Range_Exdata_ nil)
                  (progn
                    (setq Range_Exdata_list_  (cons Range_Exdata_ Range_Exdata_list_))
                  )
                  (setq Range_Exdata_list_  (cons "" Range_Exdata_list_))
                )
                (setq first_col_i (+ first_col_i 1))
              )
              (setq Range_Exdata_list_ (reverse Range_Exdata_list_))
              (setq Overall_rangecell_data_ (cons Range_Exdata_list_ Overall_rangecell_data_ ))
              (length Overall_rangecell_data_)
              (setq first_row_i (+ first_row_i 1))
            )
          ;
          ;preloop_and_while process of send data back to Autocad file 
            (setq Overall_rangecell_data_ (reverse Overall_rangecell_data_))
            (setq Overall_rangecell_data_i 1)
            (while (< Overall_rangecell_data_i (length Overall_rangecell_data_))
              (setq Overall_rangecell_data_list_ (nth  Overall_rangecell_data_i Overall_rangecell_data_))
              (setq Overall_rangecell_data_handle (car (nth  Overall_rangecell_data_i Overall_rangecell_data_)))
              
                (if (or (= (type Overall_rangecell_data_handle ) 'REAL) (= (type Overall_rangecell_data_handle ) 'INT)  )
                  (progn
                    (setq Overall_rangecell_data_handle_RTOS_ (RTOS Overall_rangecell_data_handle 2 0))
                  )
                  (setq Overall_rangecell_data_handle_RTOS_ Overall_rangecell_data_handle)
                )
              
                (if (/= (strcase Overall_rangecell_data_handle_RTOS_ ) "HANDLE" )
                  (progn
                    (setq Handle_obj_ (vlax-ename->vla-object (handent Overall_rangecell_data_handle_RTOS_)))
                    (vla-put-color Handle_obj_ (nth 3 Overall_rangecell_data_list_))
                    (vla-put-layer Handle_obj_ (nth 4 Overall_rangecell_data_list_))
                    (vla-put-linetype Handle_obj_ (nth 5 Overall_rangecell_data_list_))
                    
                    (vla-put-insertionpoint Handle_obj_ 
                                            (vlax-3d-point 
                                              (nth 6 Overall_rangecell_data_list_ ) 
                                              (nth 7 Overall_rangecell_data_list_ ) 
                                              (nth 8 Overall_rangecell_data_list_ )
                                            )
                    )
                    (vla-put-rotation Handle_obj_ (deg-to-rad (nth 9 Overall_rangecell_data_list_ )) )
                  )
                )
                (if (and (/= (strcase Overall_rangecell_data_handle_RTOS_ ) "HANDLE") (= (vla-get-hasattributes Handle_obj_) :vlax-true) (= get_att_val_ 1)) 
                  (progn 
                    (setq att_start 10)
                    (setq att_i 10)
                    ; (setq ssslength (length Overall_rangecell_data_list_))
                    (setq Handle_obj_att_taglist_ (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_))
                    (setq Handle_obj_att_taglist_i 0)
                    (setq Handle_obj_Handle_obj_att_taglist_length_ (length (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_)))
                    (setq Handle_obj_Handle_obj_att_taglist_i 0)
                    (setq att_all_length (+ att_start (- Handle_obj_Handle_obj_att_taglist_length_ 1)))
                    (while  
                      (<= att_i att_all_length )
                      (setq Handle_obj_att_taglist_tag (nth Handle_obj_att_taglist_i Handle_obj_att_taglist_)) 
                      (LM:vl-setattributevalue ;SUB-FUNC
                        Handle_obj_ ;Vla-obj
                        Handle_obj_att_taglist_tag ;att_tag_name_
                        (nth  att_i Overall_rangecell_data_list_) ;att_value
                      )
                      
                      
                      (setq att_i (+ att_i 1))
                      (setq Handle_obj_att_taglist_i (+ Handle_obj_att_taglist_i 1))
                      (setq Handle_obj_Handle_obj_att_taglist_i (+ Handle_obj_Handle_obj_att_taglist_i 1))
                    )
                  )
                  (setq att_start 10
                        att_i     10
                  )
                )
                (if (and (/= (strcase Overall_rangecell_data_handle_RTOS_ ) "HANDLE") (= (vla-get-isdynamicblock Handle_obj_) :vlax-true) (= get_dyn_val_ 1) )
                  (progn
                    (setq dyn_start att_i  )
                    (setq Handle_obj_dyn_list_ (LM-TA:getdynprops Handle_obj_))
                    (setq Handle_obj_dyn_list_length_ (length (LM-TA:getdynprops Handle_obj_)))
                    (setq Handle_obj_dyn_list_i 0)
                    (setq final_length (- (+ dyn_start  Handle_obj_dyn_list_length_) 1))
                    ;preloop_and_while
                      (setq Handle_obj_dyn_list_i 0)
                      (while 
                        (<= dyn_start final_length)
                        (setq Handle_obj_dyn_list_tag_ (nth Handle_obj_dyn_list_i Handle_obj_dyn_list_))
                        (if (/= Handle_obj_dyn_list_tag_ "Origin")
                          (progn
                            (LM:setdynpropvalue 
                              Handle_obj_
                              Handle_obj_dyn_list_tag_
                              (nth dyn_start Overall_rangecell_data_list_)
                            )
                          )
                        )
                        
                        
                        (setq dyn_start (+ dyn_start 1))
                        (setq Handle_obj_dyn_list_i (+ Handle_obj_dyn_list_i 1))
                      )
                    ;
                    
                  )
                ) 
                (setq Overall_rangecell_data_i (+ Overall_rangecell_data_i 1))
            )
          ;  
        )
        ; (setq get_dyn_val_ 1)
        (TA:Rangecell_data_ rangeCell_Exdata_list_ 1 1)
      )
    ;
    ;Pason_command
      ; (defun c:Pason_Exceldata_to_Drawinglist_E2DL ()
      ;   (setq Activesheet_worksheet_obj_  (vlax-get get_ExcelApp_object_ 'activesheet  ))
      ;   (setq Activesheet_worksheet_getRangecell_ (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ))
      ;   (setq RangeCellName_list_ (LM:str->lst (LM:StringSubst  "" "$" Activesheet_worksheet_getRangecell_ ) ":"))
      ;   (setq rangeCell_Exdata_list_ (TA_EXCEL_RangecellName_to_R1C1 RangeCellName_list_))
        
        
      ;   (defun TA:Rangecell_data_ (rangeCell_Exdata_list_ get_att_val_ get_dyn_val_)
      ;     ;Note By Code_Developer
      ;     ;This command is designed to work exclusively with a Excel File data from ATt/DYN block list 
      ;     ;The operation of the command will read an Excel range data in active worksheet and send all rangecell selection back to ATT/DYN block in Autocad by refer from Handle Object ID
      ;     ;Fully command must have sub-functions with names starting with TA: or LM:
      ;     ;
      ;     ;arrgument
      ;     ;rangeCell_Exdata_list_ = rangecelldata via R1C1
      ;     ;get_att_val_           = specify value for allow get attribute data (0 = false 1 = true)
      ;     ;get_dyn_val_           = specify value for allow get dynamic data (0 = false 1 = true)
      ;     ;
      ;     ; ;example_test
      ;     ;   (setq get_att_val_ 1)
      ;     ;   (setq get_dyn_val_ 1)
      ;     ; ;
      ;     ;specify column and row for while loop
      ;       (setq Col_data_
      ;         (list 
      ;           (setq first_col_ (car (nth 0 rangeCell_Exdata_list_)))
      ;           (setq last_col_ (car (nth 1 rangeCell_Exdata_list_)))
      ;         )
      ;       )
      ;       (setq Row_data_
      ;         (list 
      ;           (setq first_row_ (cadr (nth 0 rangeCell_Exdata_list_)))
      ;           (setq last_row_ (cadr (nth 1 rangeCell_Exdata_list_)))
      ;         )
      ;       )
      ;     ;
      ;     ;preloop_and_while get heading of table _ ATT/DYN data list  
      ;       (setq first_row_i first_row_)
      ;       (setq Overall_rangecell_data_ ())
      ;       (while ;generate_ column range  
      ;         (<= first_row_i last_row_ )
      ;         (setq first_col_i first_col_)
      ;         (setq last_col_i last_col_)
      ;         (setq Range_Exdata_ "")
      ;         (setq Range_Exdata_list_ ())
      ;         (while ;generate_ row range  
      ;           (and
      ;             (<= first_col_i last_col_i)
      ;             ; (/= Range_Exdata_ nil)
      ;           )
      ;           (setq Range_Exdata_ (TA:Excel_get_data_R1C1 Activesheet_worksheet_obj_ first_row_i first_col_i ))
      ;           (if (/= Range_Exdata_ nil)
      ;             (progn
      ;               (setq Range_Exdata_list_  (cons Range_Exdata_ Range_Exdata_list_))
      ;             )
      ;             (setq Range_Exdata_list_  (cons "" Range_Exdata_list_))
      ;           )
      ;           (setq first_col_i (+ first_col_i 1))
      ;         )
      ;         (setq Range_Exdata_list_ (reverse Range_Exdata_list_))
      ;         (setq Overall_rangecell_data_ (cons Range_Exdata_list_ Overall_rangecell_data_ ))
      ;         (length Overall_rangecell_data_)
      ;         (setq first_row_i (+ first_row_i 1))
      ;       )
      ;     ;
      ;     ;preloop_and_while process of send data back to Autocad file 
      ;       (setq Overall_rangecell_data_ (reverse Overall_rangecell_data_))
      ;       (setq Overall_rangecell_data_i 0)
      ;       (while (< Overall_rangecell_data_i (length Overall_rangecell_data_))
      ;         (setq Overall_rangecell_data_list_ (nth  Overall_rangecell_data_i Overall_rangecell_data_))
              
      ;           (if (/= (strcase (car Overall_rangecell_data_list_ ) ) "HANDLE" )
      ;             (progn
      ;               (setq Handle_obj_ (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))))
      ;               (vla-put-color Handle_obj_ (nth 3 Overall_rangecell_data_list_))
      ;               (vla-put-layer Handle_obj_ (nth 4 Overall_rangecell_data_list_))
      ;               (vla-put-linetype Handle_obj_ (nth 5 Overall_rangecell_data_list_))
                    
      ;               (vla-put-insertionpoint Handle_obj_ 
      ;                                       (vlax-3d-point 
      ;                                         (nth 6 Overall_rangecell_data_list_ ) 
      ;                                         (nth 7 Overall_rangecell_data_list_ ) 
      ;                                         (nth 8 Overall_rangecell_data_list_ )
      ;                                       )
      ;               )
      ;               (vla-put-rotation Handle_obj_ (deg-to-rad (nth 9 Overall_rangecell_data_list_ )) )
      ;             )
      ;           )
      ;           (if (and (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") (= (vla-get-hasattributes Handle_obj_) :vlax-true) (= get_att_val_ 1)) 
      ;             (progn 
      ;               (setq att_start 10)
      ;               (setq att_i 10)
      ;               ; (setq ssslength (length Overall_rangecell_data_list_))
      ;               (setq Handle_obj_att_taglist_ (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_))
      ;               (setq Handle_obj_att_taglist_i 0)
      ;               (setq Handle_obj_Handle_obj_att_taglist_length_ (length (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_)))
      ;               (setq Handle_obj_Handle_obj_att_taglist_i 0)
      ;               (setq att_all_length (+ att_start (- Handle_obj_Handle_obj_att_taglist_length_ 1)))
      ;               (while  
      ;                 (<= att_i att_all_length )
      ;                 (setq Handle_obj_att_taglist_tag (nth Handle_obj_att_taglist_i Handle_obj_att_taglist_)) 
      ;                 (LM:vl-setattributevalue ;SUB-FUNC
      ;                   Handle_obj_ ;Vla-obj
      ;                   Handle_obj_att_taglist_tag ;att_tag_name_
      ;                   (nth  att_i Overall_rangecell_data_list_) ;att_value
      ;                 )
                      
                      
      ;                 (setq att_i (+ att_i 1))
      ;                 (setq Handle_obj_att_taglist_i (+ Handle_obj_att_taglist_i 1))
      ;                 (setq Handle_obj_Handle_obj_att_taglist_i (+ Handle_obj_Handle_obj_att_taglist_i 1))
      ;               )
      ;             )
      ;             (setq att_start 10
      ;                   att_i     10
      ;             )
      ;           )
      ;           (if (and (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") (= (vla-get-isdynamicblock Handle_obj_) :vlax-true) (= get_dyn_val_ 1) )
      ;             (progn
      ;               (setq dyn_start att_i  )
      ;               (setq Handle_obj_dyn_list_ (LM-TA:getdynprops Handle_obj_))
      ;               (setq Handle_obj_dyn_list_length_ (length (LM-TA:getdynprops Handle_obj_)))
      ;               (setq Handle_obj_dyn_list_i 0)
      ;               (setq final_length (- (+ dyn_start  Handle_obj_dyn_list_length_) 1))
      ;               ;preloop_and_while
      ;                 (setq Handle_obj_dyn_list_i 0)
      ;                 (while 
      ;                   (<= dyn_start final_length)
      ;                   (setq Handle_obj_dyn_list_tag_ (nth Handle_obj_dyn_list_i Handle_obj_dyn_list_))
      ;                   (if (/= Handle_obj_dyn_list_tag_ "Origin")
      ;                     (progn
      ;                       (LM:setdynpropvalue 
      ;                         Handle_obj_
      ;                         Handle_obj_dyn_list_tag_
      ;                         (nth dyn_start Overall_rangecell_data_list_)
      ;                       )
      ;                     )
      ;                   )
                        
                        
      ;                   (setq dyn_start (+ dyn_start 1))
      ;                   (setq Handle_obj_dyn_list_i (+ Handle_obj_dyn_list_i 1))
      ;                 )
      ;               ;
                    
      ;             )
      ;           ) 
      ;           (setq Overall_rangecell_data_i (+ Overall_rangecell_data_i 1))
      ;       )
      ;     ;  
      ;   )

      ;   (TA:Rangecell_data_ rangeCell_Exdata_list_ 1 0)
      ; )
      (defun c:Pason_Exceldata_to_Drawinglist_REV01_E2DL1 ()
        (if (= get_ExcelApp_object_ nil) ;Re-open Autocad or Excel case 
          (progn
            (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application"))
          )
        )
        (setq Activesheet_worksheet_obj_  (vlax-get get_ExcelApp_object_ 'activesheet  ))
        (setq Activesheet_worksheet_getRangecell_ (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ))
        (setq RangeCellName_list_ (LM:str->lst (LM:StringSubst  "" "$" Activesheet_worksheet_getRangecell_ ) ":"))
        (setq rangeCell_Exdata_list_ (TA_EXCEL_RangecellName_to_R1C1 RangeCellName_list_))
        
        
        (defun TA:Rangecell_data_for_Drawinglist (rangeCell_Exdata_list_ get_att_val_ get_dyn_val_)
          ;Note By Code_Developer
          ;This command is designed to work exclusively with a Excel File data from ATt/DYN block list 
          ;The operation of the command will read an Excel range data in active worksheet and send all rangecell selection back to ATT/DYN block in Autocad by refer from Handle Object ID
          ;Fully command must have sub-functions with names starting with TA: or LM:
          ;
          ;arrgument
          ;rangeCell_Exdata_list_ = rangecelldata via R1C1
          ;get_att_val_           = specify value for allow get attribute data (0 = false 1 = true)
          ;get_dyn_val_           = specify value for allow get dynamic data (0 = false 1 = true)
          ;
          ; ;example_test
          ;   (setq get_att_val_ 1)
          ;   (setq get_dyn_val_ 1)
          ; ;
          ;
          (setvar "osmode" 0)
          ;
          ;specify column and row for while loop
            (setq Col_data_
              (list 
                (setq first_col_ (car (nth 0 rangeCell_Exdata_list_)))
                (setq last_col_ (car (nth 1 rangeCell_Exdata_list_)))
              )
            )
            (setq Row_data_
              (list 
                (setq first_row_ (cadr (nth 0 rangeCell_Exdata_list_)))
                (setq last_row_ (cadr (nth 1 rangeCell_Exdata_list_)))
              )
            )
          ;
          ;preloop_and_while get heading of table _ ATT/DYN data list  
            (setq first_row_i first_row_)
            (setq Overall_rangecell_data_ ())
            (while ;generate_ column range  
              (<= first_row_i last_row_ )
              (setq first_col_i first_col_)
              (setq last_col_i last_col_)
              (setq Range_Exdata_ "")
              (setq Range_Exdata_list_ ())
              (while ;generate_ row range  
                (and
                  (<= first_col_i last_col_i)
                  ; (/= Range_Exdata_ nil)
                )
                (setq Range_Exdata_ (TA:Excel_get_data_R1C1 Activesheet_worksheet_obj_ first_row_i first_col_i ))
                (if (/= Range_Exdata_ nil)
                  (progn
                    (setq Range_Exdata_list_  (cons Range_Exdata_ Range_Exdata_list_))
                  )
                  (setq Range_Exdata_list_  (cons "" Range_Exdata_list_))
                )
                (setq first_col_i (+ first_col_i 1))
              )
              (setq Range_Exdata_list_ (reverse Range_Exdata_list_))
              (setq Overall_rangecell_data_ (cons Range_Exdata_list_ Overall_rangecell_data_ ))
              (length Overall_rangecell_data_)
              (setq first_row_i (+ first_row_i 1))
            )
          ;
          ;preloop_and_while process of send data back to Autocad file 
            (setq Overall_rangecell_data_ (reverse Overall_rangecell_data_))
            (setq Overall_rangecell_data_i 1)
            (while (< Overall_rangecell_data_i (length Overall_rangecell_data_))
              (setq Overall_rangecell_data_list_ (nth  Overall_rangecell_data_i Overall_rangecell_data_))
                
                (if (and (/= (strcase (car Overall_rangecell_data_list_ ) ) "HANDLE" ) (/= (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))) nil) (/= (car Overall_rangecell_data_list_ ) "") )
                  (progn
                    (setq Handle_obj_ (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))))
                    (setq total_height_ (LM:getdynpropvalue (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))) "total_height_cell" ))
                    ; (vla-put-color Handle_obj_ (nth 3 Overall_rangecell_data_list_))
                    ; (vla-put-layer Handle_obj_ (nth 4 Overall_rangecell_data_list_))
                    ; (vla-put-linetype Handle_obj_ (nth 5 Overall_rangecell_data_list_))
                    (setq drawing_data_list_ 
                                          (TA:remove_val_list_ 
                                            ""
                                            (list 
                                              (nth 2 Overall_rangecell_data_list_)
                                              (nth 3 Overall_rangecell_data_list_)
                                              (nth 4 Overall_rangecell_data_list_)
                                            )
                                          )
                    )
                    (cond
                      (;drawing_data_list_length_case_1
                         (and
                            (= (length drawing_data_list_) 1)
                         )
                         (progn
                            (LM:setdynpropvalue (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))) "lookup1" "content")
                            (princ "drawing_data_list_length_case_1")
                           
                         )
                      )
                      (;drawing_data_list_length_case_2
                         (and
                            (= (length drawing_data_list_) 2)
                         )
                         (progn 
                           (LM:setdynpropvalue (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_))) "lookup1" "content2" )
                           (princ "drawing_data_list_length_case_2")
                         )
                      )
                      (;drawing_data_list_length_case_3
                         (and
                            (= (length drawing_data_list_) 3)
                         )
                         (progn 
                           (LM:setdynpropvalue (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_))) "lookup1" "content3" )
                           (princ "drawing_data_list_length_case_3")
                         )
                      )
                      (;drawing_data_list_length_case_4
                         (or
                            (= (length drawing_data_list_) 0)
                            (= (length drawing_data_list_) nil)
                         )
                         (progn 
                           (vla-delete (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))) )
                           (princ "drawing_data_list_length_case_3")
                         )
                      )
                    )
                    ; (vla-put-rotation Handle_obj_ (deg-to-rad (nth 9 Overall_rangecell_data_list_ )) )
                  )
                )
                (setq get_att_val_ 1)
                (if 
                  (and 
                    (/= (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))) nil) 
                    (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") 
                    (/= (car Overall_rangecell_data_list_ ) "") 
                    (= (vla-get-hasattributes Handle_obj_) :vlax-true) 
                    (= get_att_val_ 1)
                  ) 
                  (progn 
                    (setq att_start 1)
                    (setq att_i 1)
                    ; (setq ssslength (length Overall_rangecell_data_list_))
                    (setq Handle_obj_att_taglist_ (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_))
                    (setq Handle_obj_att_taglist_i 0)
                    (setq Handle_obj_Handle_obj_att_taglist_length_ (length (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_)))
                    (setq Handle_obj_Handle_obj_att_taglist_i 0)
                    (setq att_all_length (+ att_start (- Handle_obj_Handle_obj_att_taglist_length_ 1)))
                    
                    
                    (setq drawing_data_list_length (length drawing_data_list_))
                    (while  
                      (<= att_i att_all_length )
                      (setq Handle_obj_att_taglist_tag (nth Handle_obj_att_taglist_i Handle_obj_att_taglist_)) 
                      (LM:vl-setattributevalue ;SUB-FUNC
                        Handle_obj_ ;Vla-obj
                        Handle_obj_att_taglist_tag ;att_tag_name_
                        (nth  att_i Overall_rangecell_data_list_) ;att_value
                      )
                      
                      
                      (setq att_i (+ att_i 1))
                      (setq Handle_obj_att_taglist_i (+ Handle_obj_att_taglist_i 1))
                      (setq Handle_obj_Handle_obj_att_taglist_i (+ Handle_obj_Handle_obj_att_taglist_i 1))
                    )
                  )
                  (setq att_start 10
                        att_i     10
                  )
                )
                (if (and (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") (= (vla-get-isdynamicblock Handle_obj_) :vlax-true) (= get_dyn_val_ 1) )
                  (progn
                    (setq dyn_start att_i  )
                    (setq Handle_obj_dyn_list_ (LM-TA:getdynprops Handle_obj_))
                    (setq Handle_obj_dyn_list_length_ (length (LM-TA:getdynprops Handle_obj_)))
                    (setq Handle_obj_dyn_list_i 0)
                    (setq final_length (- (+ dyn_start  Handle_obj_dyn_list_length_) 1))
                    ;preloop_and_while
                      (setq Handle_obj_dyn_list_i 0)
                      (while 
                        (<= dyn_start final_length)
                        (setq Handle_obj_dyn_list_tag_ (nth Handle_obj_dyn_list_i Handle_obj_dyn_list_))
                        (if (/= Handle_obj_dyn_list_tag_ "Origin")
                          (progn
                            (LM:setdynpropvalue 
                              Handle_obj_
                              Handle_obj_dyn_list_tag_
                              (nth dyn_start Overall_rangecell_data_list_)
                            )
                          )
                        )
                        
                        
                        (setq dyn_start (+ dyn_start 1))
                        (setq Handle_obj_dyn_list_i (+ Handle_obj_dyn_list_i 1))
                      )
                    ;
                    
                  )
                ) 
                (setq Overall_rangecell_data_i (+ Overall_rangecell_data_i 1))
            )
          ;  
          ;reset_coordinate_
            ;preloop_and_while
              (setq Overall_rangecell_data_i 1)
              (setq new_ssget_ (ssadd))
              (while (< Overall_rangecell_data_i (length Overall_rangecell_data_))
                (setq Overall_rangecell_data_ename_ (handent (car (nth  Overall_rangecell_data_i Overall_rangecell_data_))))
                (if (/= (setq Overall_rangecell_data_obj_ (vlax-ename->vla-object Overall_rangecell_data_ename_)) nil)
                  (progn 
                    (setq new_ssget_ (ssadd Overall_rangecell_data_ename_ new_ssget_ ) )
                    (sslength new_ssget_)
                  )
                )
               
                (setq Overall_rangecell_data_i (+ Overall_rangecell_data_i 1))
              )
              (setq new_sortset_ (reverse (TA:standard_list_croodinate_sorting new_ssget_ "Y")))
            ;
            ;preloop_and_while
              (setq new_sortset_i 0)
              (while (< new_sortset_i (length new_sortset_))
                (setq new_sortset_ename_ (car (nth  new_sortset_i new_sortset_)))
                ;  (command "pselect"  new_sortset_ename_ "") 
                (setq new_sortset_ename_ins (cadr (nth  new_sortset_i new_sortset_)))
                (setq new_sortset_obj_ (vlax-ename->vla-object new_sortset_ename_))
                (setq new_sortset_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint new_sortset_obj_))))
                (setq new_sortset_obj_total_H (LM:getdynpropvalue new_sortset_obj_ "total_height_cell" ))
                (setq get_OBJECT_ID_ (LM:vl-getattributevalue new_sortset_obj_ "OBJECT_ID_" ))
                (setq drawing_num_ (LM:vl-getattributevalue new_sortset_obj_ "แผ่นที่_VALUE" ))

                (setq next_ins_point_ (list 
                                        (car new_sortset_obj_ins_)
                                        (- (cadr new_sortset_obj_ins_) new_sortset_obj_total_H )
                                        0
                                      )
                )
                (if (/= new_sortset_i (- (length new_sortset_) 1))
                  (progn
                    (vla-move 
                      (vlax-ename->vla-object (car (nth (+ 1 new_sortset_i) new_sortset_)) )
                      (vlax-3d-point (cadr (nth (+ 1 new_sortset_i) new_sortset_)))
                      (vlax-3d-point next_ins_point_)
                    )
                  )
                )
                
                ; (setq new_sortset_i (- new_sortset_i 1))
                (setq new_sortset_i (+ new_sortset_i 1))
              )
            ;
          ;
          (setvar "osmode" 1215)
        )
        (TA:Rangecell_data_for_Drawinglist rangeCell_Exdata_list_ 1 0)
        ;user_input_ for recreate Excel data
          (setq update_excel_file_ (cond ( (getint (strcat "\nNot update Excel file = 1\nUpdate Excel file = 2\n<" (rtos (setq update_excel_file_ (cond (update_excel_file_) (1.0) ) ) ) "> : " ) ) ) (update_excel_file_) ) )
          (if 
            (and
              (= update_excel_file_ 2 )
            )
            (progn
              (command "pselect"  new_ssget_ "" )
              (c:DrawingList_to_Excel_REV01_DL2E1)
            )
          )
        ;
        (alert "Drawing list update complete")
      )
      (defun c:Pason_Exceldata_to_Tittleblock_E2TT () ;error
        
        (if (= get_ExcelApp_object_ nil) ;Re-open Autocad or Excel case 
          (progn
            (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application"))
          )
        )
        (setq Activesheet_worksheet_obj_  (vlax-get get_ExcelApp_object_ 'activesheet  ))
        (setq Activesheet_worksheet_getRangecell_ (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ))
        (setq RangeCellName_list_ (LM:str->lst (LM:StringSubst  "" "$" Activesheet_worksheet_getRangecell_ ) ":"))
        (setq rangeCell_Exdata_list_ (TA_EXCEL_RangecellName_to_R1C1 RangeCellName_list_))
        
        
        (defun TA:Rangecell_data_for_Titleblock_ (rangeCell_Exdata_list_ get_att_val_ get_dyn_val_)
          ;Note By Code_Developer
          ;This command is designed to work exclusively with a Excel File data from ATt/DYN block list 
          ;The operation of the command will read an Excel range data in active worksheet and send all rangecell selection back to ATT/DYN block in Autocad by refer from Handle Object ID
          ;Fully command must have sub-functions with names starting with TA: or LM:
          ;
          ;arrgument
          ;rangeCell_Exdata_list_ = rangecelldata via R1C1
          ;get_att_val_           = specify value for allow get attribute data (0 = false 1 = true)
          ;get_dyn_val_           = specify value for allow get dynamic data (0 = false 1 = true)
          ;
          ; ;example_test
          ;   (setq get_att_val_ 1)
          ;   (setq get_dyn_val_ 1)
          ; ;
          ;specify column and row for while loop
            (setq Col_data_
              (list 
                (setq first_col_ (car (nth 0 rangeCell_Exdata_list_)))
                (setq last_col_ (car (nth 1 rangeCell_Exdata_list_)))
              )
            )
            (setq Row_data_
              (list 
                (setq first_row_ (cadr (nth 0 rangeCell_Exdata_list_)))
                (setq last_row_ (cadr (nth 1 rangeCell_Exdata_list_)))
              )
            )
          ;
          ;preloop_and_while get heading of table _ ATT/DYN data list  
            (setq first_row_i first_row_)
            (setq Overall_rangecell_data_ ())
            (while ;generate_ column range  
              (<= first_row_i last_row_ )
              (setq first_col_i first_col_)
              (setq last_col_i last_col_)
              (setq Range_Exdata_ "")
              (setq Range_Exdata_list_ ())
              (while ;generate_ row range  
                (and
                  (<= first_col_i last_col_i)
                  ; (/= Range_Exdata_ nil)
                )
                (setq Range_Exdata_ (TA:Excel_get_data_R1C1 Activesheet_worksheet_obj_ first_row_i first_col_i ))
                (if (/= Range_Exdata_ nil)
                  (progn
                    (setq Range_Exdata_list_  (cons Range_Exdata_ Range_Exdata_list_))
                  )
                  (setq Range_Exdata_list_  (cons "" Range_Exdata_list_))
                )
                (setq first_col_i (+ first_col_i 1))
              )
              (setq Range_Exdata_list_ (reverse Range_Exdata_list_))
              (setq Overall_rangecell_data_ (cons Range_Exdata_list_ Overall_rangecell_data_ ))
              (length Overall_rangecell_data_)
              (setq first_row_i (+ first_row_i 1))
            )
          ;
          ;preloop_and_while process of send data back to Autocad file 
            (setq Overall_rangecell_data_ (reverse Overall_rangecell_data_))
            (setq Overall_rangecell_data_i 0)
            (while (< Overall_rangecell_data_i (length Overall_rangecell_data_))
              (setq Overall_rangecell_data_list_ (nth  Overall_rangecell_data_i Overall_rangecell_data_))
              
                (if (/= (strcase (car Overall_rangecell_data_list_ ) ) "HANDLE" )
                  (progn
                    (setq Handle_obj_ (vlax-ename->vla-object (handent (car Overall_rangecell_data_list_ ))))
                    ; (vla-put-color Handle_obj_ (nth 3 Overall_rangecell_data_list_))
                    ; (vla-put-layer Handle_obj_ (nth 4 Overall_rangecell_data_list_))
                    ; (vla-put-linetype Handle_obj_ (nth 5 Overall_rangecell_data_list_))
                    
                    (vla-put-insertionpoint Handle_obj_ 
                                            (vlax-3d-point 
                                              (nth 2 Overall_rangecell_data_list_ ) 
                                              (nth 3 Overall_rangecell_data_list_ ) 
                                              (nth 4 Overall_rangecell_data_list_ )
                                            )
                    )
                    ; (vla-put-rotation Handle_obj_ (deg-to-rad (nth 9 Overall_rangecell_data_list_ )) )
                  )
                )
                (if (and (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") (= (vla-get-hasattributes Handle_obj_) :vlax-true) ) 
                  (progn 
                    ; (setq att_start 10)
                    ; (setq att_i 10)
                    ; ; (setq ssslength (length Overall_rangecell_data_list_))
                    (setq Handle_obj_att_taglist_ (nth 0 Overall_rangecell_data_ ))
                    ; (setq Handle_obj_att_taglist_i 0)
                    ; (setq Handle_obj_Handle_obj_att_taglist_length_ (length (LM:vl-getattributevalue-tag-TA-Modifies Handle_obj_)))
                    ; (setq Handle_obj_Handle_obj_att_taglist_i 0)
                    ; (setq att_all_length (+ att_start (- Handle_obj_Handle_obj_att_taglist_length_ 1)))
                    (setq Overall_rangecell_data_list_i 5)
                    (setq Handle_obj_att_taglist_i 5)
                    
                    (length Overall_rangecell_data_list_)
                    (length Handle_obj_att_taglist_ )
                    (while  
                      (< Overall_rangecell_data_list_i (length Overall_rangecell_data_list_) )
                      (setq Handle_obj_att_taglist_tag (nth Handle_obj_att_taglist_i Handle_obj_att_taglist_)) 
                       
                      (LM:vl-setattributevalue ;SUB-FUNC
                        Handle_obj_ ;Vla-obj
                        Handle_obj_att_taglist_tag ;att_tag_name_
                        (nth  Overall_rangecell_data_list_i Overall_rangecell_data_list_) ;att_value
                      )
                      
                      
                      (setq Handle_obj_att_taglist_i (+ Handle_obj_att_taglist_i 1))
                      (setq Overall_rangecell_data_list_i (+ Overall_rangecell_data_list_i 1))
                      
                    )
                  )
                  ; (setq att_start 10
                  ;       att_i     10
                  ; )
                )
                ; (if (and (/= (strcase (car Overall_rangecell_data_list_)) "HANDLE") (= (vla-get-isdynamicblock Handle_obj_) :vlax-true) (= get_dyn_val_ 1) )
                ;   (progn
                ;     (setq dyn_start att_i  )
                ;     (setq Handle_obj_dyn_list_ (LM-TA:getdynprops Handle_obj_))
                ;     (setq Handle_obj_dyn_list_length_ (length (LM-TA:getdynprops Handle_obj_)))
                ;     (setq Handle_obj_dyn_list_i 0)
                ;     (setq final_length (- (+ dyn_start  Handle_obj_dyn_list_length_) 1))
                ;     ;preloop_and_while
                ;       (setq Handle_obj_dyn_list_i 0)
                ;       (while 
                ;         (<= dyn_start final_length)
                ;         (setq Handle_obj_dyn_list_tag_ (nth Handle_obj_dyn_list_i Handle_obj_dyn_list_))
                ;         (if (/= Handle_obj_dyn_list_tag_ "Origin")
                ;           (progn
                ;             (LM:setdynpropvalue 
                ;               Handle_obj_
                ;               Handle_obj_dyn_list_tag_
                ;               (nth dyn_start Overall_rangecell_data_list_)
                ;             )
                ;           )
                ;         )
                        
                        
                ;         (setq dyn_start (+ dyn_start 1))
                ;         (setq Handle_obj_dyn_list_i (+ Handle_obj_dyn_list_i 1))
                ;       )
                ;     ;
                    
                ;   )
                ; ) 

                (setq Overall_rangecell_data_i (+ Overall_rangecell_data_i 1))
            )
          ;  
        )

        (TA:Rangecell_data_for_Titleblock_ rangeCell_Exdata_list_ 1 0)
      )     
    ;
  ;
  ;Autocad
    ;Pason_command
    (defun c:Pason_Assembly_Drawing_list_MKDLLIST ()
      ;user_input_
        (setq user_getpoint (getpoint "specify basepoint for insertion"))
        (setq user_input_scale_ (cond ( (getint (strcat "\nSpecify scale for object\n<" (rtos (setq user_input_scale_ (cond (user_input_scale_) (1.0) ) ) ) "> : " ) ) ) (user_input_scale_) ) )
        (setq drawing_data_mode_ (cond ( (getint (strcat "\ndrawing_data_mode_ = กด 0 แบบแสดง 1,2,3 รวมเป็นบรรทัดเดียว\ndrawing_data_mode_ = กด 1 แบบแสดง 1,2,3 แบ่งตามบรรทัดที่ titleblock ปรากฏ\n<" (rtos (setq drawing_data_mode_ (cond (drawing_data_mode_) (1.0) ) ) ) "> : " ) ) ) (drawing_data_mode_) ) )
      ;drawing_data_mode_
      ;new object block get data
        (setvar "osmode" 0)
        (command "insert" "000_ป่าสน-Tittle_block_Drawing_list_and_content" user_getpoint user_input_scale_ 0)
        (setq new_insertion_ename_ (entlast))
        (setq new_insertion_obj_ (vlax-ename->vla-object new_insertion_ename_))
        (setq new_insertion_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint new_insertion_obj_))))
        
        (setq new_insertion_obj_dyn_vis (LM:setdynpropvalue new_insertion_obj_ "Lookup1" "Header" ))
        (setq new_insertion_obj_dyn_total_H_ (LM:getdynpropvalue new_insertion_obj_ "total_height_cell" ))
      ;
      ;while_select_block_on_condition_ 
          (setq target_EFname_ nil)
          (while (= target_EFname_ nil)
            (setq target_EFname_ (car (entsel "specify target_EFname Object")))
            (if
              (and ;conditional_rule_for_select_object
                (/= target_EFname_ nil)
                (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
                (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
                (or 
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A0_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A1_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A2_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A3_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A4_LAND")
                )
                ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
                ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
              )
              (progn
                (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
              )
              (alert "Object invalid Please try again")
            )
          )
      ;
      ;selection_set_for_fillter_blk_name
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
            (setq ss_pre_filter_set_xx_ (ssget 
                                              (list
                                                (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;get_data_process_for_fillter_blk_name
        (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
        (setq target_block_list_ (TA:standard_list_croodinate+ename target_block_))
        (setq target_blk_ (TA:corrd_sorting_group target_block_list_ 100 ))
        
        (sslength target_block_)
        (length target_block_list_)
        (length target_blk_)
      ;
      
      ;preloop_and_while
        (setq target_blk_i 0)
        (while (< target_blk_i (length target_blk_))
          (setq target_blk_rowset_ (nth  target_blk_i target_blk_))
            ;preloop_and_while
              (setq target_blk_rowset_i 0)
              (while (< target_blk_rowset_i (length target_blk_rowset_))
                (setq target_blk_rowset_ename_ (cadr (nth  target_blk_rowset_i target_blk_rowset_)))
                  (setq target_blk_rowset_obj_ (vlax-ename->vla-object target_blk_rowset_ename_))
                  (setq target_blk_rowset_obj_idname_ (vla-get-handle target_blk_rowset_obj_))
                  (setq drawing_date_ (LM:vl-getattributevalue target_blk_rowset_obj_ "วันที่_VALUE" ))
                  (setq drawing_no_ (LM:vl-getattributevalue target_blk_rowset_obj_ "แผ่นที่_VALUE" ))
                  (cond ;drawing_data_mode_
                    (;drawing_data_mode_case_1
                       (and
                          (= drawing_data_mode_ 0)
                       )
                       (progn
                         (setq drawing_data_ 
                                      (strcat 
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_1" )
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_2" )
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_3" )
                                      )
                        )
                        (princ "drawing_data_mode_case_1")
                       )
                    )
                    (;drawing_data_mode_case_2
                       (and
                          (= drawing_data_mode_ 1)
                       )
                       (progn
                         (setq drawing_data_ (list 
                                               (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_1" )
                                               (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_2" )
                                               (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_3" )
                                             )
                         )
                         (princ "drawing_data_mode_case_2")
                       )
                    )
                    (;drawing_data_mode_case_3
                       (and
                          (/= drawing_data_mode_ 0)
                          (/= drawing_data_mode_ 1)
                       )
                       (progn
                         (setq drawing_data_ 
                                      (strcat 
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_1" )
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_2" )
                                        (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_3" )
                                      )
                        )
                         (princ "drawing_data_mode_case_3")
                       )
                    )
                    
                  )
                  (setq drawing_total_ (LM:vl-getattributevalue target_blk_rowset_obj_ "จำนวนแผ่น_VALUE" ))
                  (setq drawing_scale_ (LM:vl-getattributevalue target_blk_rowset_obj_ "มาตราส่วน_VALUE" ))
                  ;basepoint for insertion object
                    ;get data
                      (setq new_user_getpoint (list 
                                                (car user_getpoint)
                                                (- (cadr user_getpoint) new_insertion_obj_dyn_total_H_)
                                                0
                                              )
                      )
                      (command "insert" "000_ป่าสน-Tittle_block_Drawing_list_and_content" new_user_getpoint user_input_scale_ 0)
                      (setq new_insertion_ename_ (entlast))
                      (setq new_insertion_obj_ (vlax-ename->vla-object new_insertion_ename_))
                      (setq new_insertion_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint new_insertion_obj_))))
                      
                      (setq new_insertion_obj_dyn_vis 
                             (LM:setdynpropvalue new_insertion_obj_ 
                               "Lookup1" 
                               (if (= drawing_data_mode_ 1)
                                (progn
                                  (cond
                                    (;content_case_1
                                        (and
                                          (= drawing_data_mode_ 1)
                                          (= (length (TA:remove_val_list_ "" drawing_data_)) 1)
                                        )
                                        (progn
                                          (princ "content_case_1")
                                          "content"
                                        )
                                    )
                                    (;content_case_2
                                        (and
                                          (= drawing_data_mode_ 1)
                                          (= (length (TA:remove_val_list_ "" drawing_data_)) 2)
                                        )
                                        (progn 
                                          (princ "content_case_2")
                                          "content2"
                                        )
                                    )
                                    (;content_case_3
                                        (and
                                          (= drawing_data_mode_ 1)
                                          (= (length (TA:remove_val_list_ "" drawing_data_)) 3)
                                        )
                                        (progn 
                                          (princ "content_case_3")
                                          "content3"
                                        )
                                    )
                                  )
                                )
                                (cond
                                  (;content_case_1
                                      (and
                                        (/= drawing_data_mode_ 1)
                                        
                                      )
                                      (progn
                                        (princ "content_case_1")
                                        (setq drawing_data_ 
                                          (LM:StringSubst "" ",,"
                                            (strcat 
                                              (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_1" )
                                              ","
                                              (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_2" )
                                              ","
                                              (LM:vl-getattributevalue target_blk_rowset_obj_ "แบบแสดง_:_value_3" )
                                            )
                                          )  
                                        )
                                        "content"
                                      )
                                  )
                                )                                 
                               )
                               
                             )
                      )
                      (setq drawing_list_mid_val_ (LM:setdynpropvalue new_insertion_obj_ "drawing_list_mid" (* 4 user_input_scale_)))
                      (setq drawing_list_mid_val_ (LM:setdynpropvalue new_insertion_obj_ "note_list_mid_width" (* 4 user_input_scale_)))
                      (setq new_insertion_obj_dyn_total_H_i (LM:getdynpropvalue new_insertion_obj_ "total_height_cell" ))

                      ;preloop_and_while LM:vl-setattributevalue drawing_data_
                        (if (= drawing_data_mode_ 1 )
                          (progn
                            (setq drawing_data_i 0)
                            (setq drawing_data_tag_list (list "แบบแสดง_:_value_1" "แบบแสดง_:_value_2" "แบบแสดง_:_value_3"))
                            (while (< drawing_data_i (length drawing_data_))
                              (setq drawing_data_value_ (nth  drawing_data_i drawing_data_))
                              (setq drawing_data_tag_list_val_ (nth  drawing_data_i drawing_data_tag_list))
                              
                                (if (/= drawing_data_value_ "")
                                  (progn
                                    (LM:vl-setattributevalue new_insertion_obj_ drawing_data_tag_list_val_ drawing_data_value_)
                                  )
                                )
                              (setq drawing_data_i (+ drawing_data_i 1))
                            )
                          )
                          (LM:vl-setattributevalue new_insertion_obj_ "แบบแสดง_:_value_1" drawing_data_)
                        )
                      ;
                    ;
                    ;put data

                      (LM:vl-setattributevalue new_insertion_obj_ "แผ่นที่_VALUE" drawing_no_)
                      ; (LM:vl-setattributevalue new_insertion_obj_ "แบบแสดง_:_value_1"  drawing_data_ ) HOLD_
                      (LM:vl-setattributevalue new_insertion_obj_ "มาตราส่วน_VALUE"  drawing_scale_ )
                      
                      (LM:vl-setattributevalue new_insertion_obj_ "วันที่_VALUE" drawing_date_)
                      (LM:vl-setattributevalue new_insertion_obj_ "OBJECT_ID_" target_blk_rowset_obj_idname_)

                    ;
                  ;         
                (setq new_insertion_obj_dyn_total_H_ (+ new_insertion_obj_dyn_total_H_ new_insertion_obj_dyn_total_H_i))
                (setq target_blk_rowset_i (+ target_blk_rowset_i 1))
              )
            ;
      
          (setq target_blk_i (+ target_blk_i 1))
        )
      ;
      
     
      ;return osmode
        (setvar "osmode" 1215)
      ;
      
    )
    (defun c:Pason_Drawing_list_Sort_number_SO2 () 
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named "000_ป่าสน-Tittle_block_Drawing_list_and_content".
      ;principle of codework is designed for sotring page of PasonDrawing_list_and_content by up to down
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;selection_set_for_fillter_blk_name
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
            (setq ss_pre_filter_set_xx_ (ssget 
                                              (list
                                                (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;get_data_process_for_fillter_blk_name
        (setq target_EFname_list_ "000_ป่าสน-Tittle_block_Drawing_list_and_content")
        (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
        (setq target_block_list_ (reverse (TA:standard_list_croodinate_sorting target_block_ "Y")))
        (sslength target_block_)
      ;
      ;user_input_
        (if (= (setq drawing_header (getstring "specify drawing header <AR>")) "")
          (progn
            (setq drawing_header "AR")
          )
        )
      ;   
      ;preloop_and_while
        (setq target_block_list_i 0)
        (setq target_block_list_iheader 1)
        (while (< target_block_list_i (length target_block_list_))
          (setq target_block_list_ename_ (car (nth  target_block_list_i target_block_list_)))
          (setq target_block_list_obj_ (vlax-ename->vla-object target_block_list_ename_))
          
            (setq tttt 
              (LM:vl-setattributevalue 
                target_block_list_obj_ 
                "แผ่นที่_VALUE"  
                (strcat drawing_header 
                        ; (rtos target_block_list_i 2 0) 
                        "-" 
                        (if (< target_block_list_iheader 10)
                          (progn
                            (strcat "0" (rtos target_block_list_iheader 2 0))
                          )
                          (strcat (rtos target_block_list_iheader 2 0))
                        )
                )
              )
            )
          
          
          (setq target_block_list_i (+ target_block_list_i 1))
          (setq target_block_list_iheader (+ target_block_list_iheader 1))
        )
      ;
      ;
    )
    (defun c:Pason_Dawing_list_->Title_Block_DL2TB_ ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named "000_ป่าสน-Tittle_block_Drawing_list_and_content".
      ;principle of codework is designed for send attribute data from block name "000_ป่าสน-Tittle_block_Drawing_list_and_content" to Object-ID 
      ;Object-ID can get from using c:Pason_Assembly_Drawing_list_MKDLLIST
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;selection_set_for_fillter_blk_name
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
            (setq ss_pre_filter_set_xx_ (ssget 
                                              (list
                                                (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;get_data_process_for_fillter_blk_name
        (setq target_EFname_list_ "000_ป่าสน-Tittle_block_Drawing_list_and_content")
        (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
        (setq target_block_list_ (reverse (TA:standard_list_croodinate_sorting target_block_ "Y")))
        (sslength target_block_)
      ;
      ;preloop_and_while
        (setq target_block_list_i 0)
        (while (< target_block_list_i (length target_block_list_))
          (setq target_block_list_ename_ (car (nth  target_block_list_i target_block_list_)))
           
          (setq target_block_list_obj_ (vlax-ename->vla-object target_block_list_ename_))
          (setq get-handdent (LM:vl-getattributevalue target_block_list_obj_ "OBJECT_ID_"))
          (setq drawing_data_
                (TA:remove_val_list_
                  ""
                  (LM:str->lst
                    (LM:lst->str
                      (list
                        (LM:vl-getattributevalue target_block_list_obj_ "แบบแสดง_:_value_1")
                        (LM:vl-getattributevalue target_block_list_obj_ "แบบแสดง_:_value_2")
                        (LM:vl-getattributevalue target_block_list_obj_ "แบบแสดง_:_value_3")
                      )
                      ","
                    )
                    ","
                  )
                )
          )
          (setq drawing_date_ (LM:vl-getattributevalue target_block_list_obj_ "วันที่_VALUE" ))
          (setq drawing_no_ (LM:vl-getattributevalue target_block_list_obj_ "แผ่นที่_VALUE" ))
          (setq drawing_scale_ (LM:vl-getattributevalue target_block_list_obj_ "มาตราส่วน_VALUE" ))
          
          (if (/= (handent get-handdent) "")
            (progn
              (setq TB_ename_ (handent get-handdent))
              (setq TB_obj_ (vlax-ename->vla-object TB_ename_))
              ;preloop_and_while LM:vl-setattributevalue drawing_data_
                (LM:vl-setattributevalue TB_obj_ "แบบแสดง_:_value_1" "") ;presetting attribute
                (LM:vl-setattributevalue TB_obj_ "แบบแสดง_:_value_2" "") ;presetting attribute
                (LM:vl-setattributevalue TB_obj_ "แบบแสดง_:_value_3" "") ;presetting attribute
                (setq drawing_data_i 0)
                (setq drawing_data_tag_list (list "แบบแสดง_:_value_1" "แบบแสดง_:_value_2" "แบบแสดง_:_value_3"))
                (while (<= drawing_data_i (length drawing_data_))
                  (setq drawing_data_value_ (nth  drawing_data_i drawing_data_))
                  (setq drawing_data_tag_list_val_ (nth drawing_data_i drawing_data_tag_list))
                    
                    (if 
                      (and
                        (/= drawing_data_value_ nil)
                        (/= drawing_data_tag_list_val_ nil)
                      )
                      (progn
                        (LM:vl-setattributevalue TB_obj_ drawing_data_tag_list_val_ "")
                        (LM:vl-setattributevalue TB_obj_ drawing_data_tag_list_val_ drawing_data_value_)
                      )
                    )
                  (setq drawing_data_i (+ drawing_data_i 1))
                )
              ;
              (LM:vl-setattributevalue TB_obj_ "วันที่_VALUE" drawing_date_)
              (LM:vl-setattributevalue TB_obj_ "แผ่นที่_VALUE" drawing_no_)
              (LM:vl-setattributevalue TB_obj_ "มาตราส่วน_VALUE" drawing_scale_)
            )
            (princ "")
          )
          (setq target_block_list_i (+ target_block_list_i 1))
        )
      ;

    )
    (defun c:Pason_Title_Block_Sorting_list_SOList_ () ;work with one name block only per time
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named "000_ป่าสน-Tittle_block_A2_LAND".
      ;principle of codework is designed for sotring page of Pason title block by left to right and up to down
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;while_select_block_on_condition_ 
          (setq target_EFname_ nil)
          (while (= target_EFname_ nil)
            (setq target_EFname_ (car (entsel "specify target_EFname Object")))
            (if
              (and ;conditional_rule_for_select_object
                (/= target_EFname_ nil)
                (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
                (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
                (or 
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A0_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A1_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A2_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A3_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A4_LAND")
                  (= (LM:effectivename target_EFname_obj_) "A$C3c9191d6")
                )
                ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
                ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
              )
              (progn
                (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
              )
              (alert "Object invalid Please try again")
            )
          )
      ;
      ;selection_set_for_fillter_blk_name
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
            (setq ss_pre_filter_set_xx_ (ssget 
                                              (list
                                                (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;get_data_process_for_fillter_blk_name
        (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
        (setq target_block_list_ (TA:standard_list_croodinate_sorting target_block_ "X"))
        (sslength target_block_)
      ;
      ;test_code
        ; (format-data target_block_list_ )
      ;
      ;preloop_and_while get coordinate x y 
        (setq target_block_i 0)
        (setq  test_vlains_ ())
        (while (< target_block_i (sslength target_block_))
          (setq target_block_ename_ (ssname target_block_ target_block_i))
          (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
          (setq target_block_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_))))
          (setq target_block_obj_ins_+_ename (list
                                              target_block_obj_ins_
                                              target_block_ename_
                                            )
          )
          (setq test_vlains_ (cons target_block_obj_ins_+_ename test_vlains_ ))
          (setq target_block_i (+ target_block_i 1))
        )
        (setq Toerrance_num_value 100 )
        (setq Ins_sorted_ (TA:corrd_sorting_group test_vlains_ 100))
      ;
      ;preloop_and_while
        (setq Ins_sorted_i 0)
        (setq tag_number_i 1)
        (while (< Ins_sorted_i (length Ins_sorted_))
          (setq Ins_sorted_rowset_ (nth  Ins_sorted_i Ins_sorted_))
            ;preloop_and_while
              (setq Ins_sorted_rowset_i 0)
              (while (< Ins_sorted_rowset_i (length Ins_sorted_rowset_))
                (setq Ins_sorted_rowset_ename_ (cadr (nth  Ins_sorted_rowset_i Ins_sorted_rowset_)))
                (setq Ins_sorted_rowset_obj_ (vlax-ename->vla-object Ins_sorted_rowset_ename_))
                (setq num_val_ 
                  (if 
                    (< (+ 1 Ins_sorted_rowset_i ) 10 )
                    (progn 
                      (strcat "0" (rtos (+ 1 Ins_sorted_rowset_i ) 2 0 ) ) 
                    )
                    (+ 1 Ins_sorted_rowset_i )
                  )
                )      
                
                (LM:vl-setattributevalue 
                  Ins_sorted_rowset_obj_ 
                  "แผ่นที่_VALUE" 
                  (strcat "A" (rtos Ins_sorted_i 2 0) "-" num_val_ )
                )
                (LM:vl-setattributevalue 
                  Ins_sorted_rowset_obj_
                  "จำนวนแผ่น_VALUE"
                  (strcat (rtos tag_number_i 2 0) "/" (rtos (sslength target_block_) 2 0))
                )
                (setq tag_number_i (+ tag_number_i 1))
                (setq Ins_sorted_rowset_i (+ Ins_sorted_rowset_i 1))
              )
            ;
          (setq Ins_sorted_i (+ Ins_sorted_i 1))
        )
      ;
    )
    (defun c:Pason_Title_Block_timestamp_ ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named "000_ป่าสน-Tittle_block_Ax_LAND".
      ;principle of codework is designed for stamp current time to Titleblock
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;while_select_block_on_condition_ 
          (setq target_EFname_ nil)
          (while (= target_EFname_ nil)
            (setq target_EFname_ (car (entsel "specify target_EFname Object")))
            (if
              (and ;conditional_rule_for_select_object
                (/= target_EFname_ nil)
                (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
                (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
                (or 
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A0_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A1_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A2_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A3_LAND")
                  (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A4_LAND")
                  ; (= (LM:effectivename target_EFname_obj_) "A$C3c9191d6")
                )
                ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
                ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
              )
              (progn
                (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
              )
              (alert "Object invalid Please try again")
            )
          )
      ;
      ;selection_set_for_fillter_blk_name
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
            (setq ss_pre_filter_set_xx_ (ssget 
                                              (list
                                                (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;get_data_process_for_fillter_blk_name
        (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
        (setq target_block_list_ (TA:standard_list_croodinate_sorting target_block_ "X"))
        (sslength target_block_)
      ;
      ;preloop_and_while
        (setq target_block_list_i 0)
        (while (< target_block_list_i (length target_block_list_))
          (setq target_block_list_ename_ (car (nth  target_block_list_i target_block_list_)))
          (setq target_block_list_obj_ (vlax-ename->vla-object target_block_list_ename_))
          
            (setq YYYYMMDD
                  (strcat 
                    (setq YYYY (substr (rtos (getvar "cdate") 2 8) 1 4))
                    "/"
                    (setq MM (substr (rtos (getvar "cdate") 2 8) 5 2))
                    "/"
                    (setq DD (substr (rtos (getvar "cdate") 2 8) 7 2))
                  )
            )
            (LM:vl-setattributevalue target_block_list_obj_ "วันที่_VALUE" YYYYMMDD )
          (setq target_block_list_i (+ target_block_list_i 1))
        )
      ;
    )
    (defun c:Pason_Title_Block_stamp_Heading_STH ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named "000 - ป่านสน_HEADING NAME 2025".
        ;principle of codework is designed for stamp current attrubute in "000 - ป่านสน_HEADING NAME 2025" to Titleblock
        ;Fully command must have sub-functions with names starting with TA: or LM:
        ;
        (setvar "osmode" 0)
        ;sub-func
          (defun TAS:custom-sort_for_Pason_Title_Block_stamp_Heading_ (user_custom_sort_ target_custom_sort_ )
            ;Note By Code_Developer
            ;This command is designed to work exclusively with a list array 
            ;principle of codework is designed for sorting by user custom, Then user can create custom in array list-form
            ;
            ;example argument
              ; (setq user_custom_sort_ '("Cherry" "Banana" "Apple" "Mango"))
              ; (setq user_custom_sort_ visibility_screw_mode_)
              ; ; (setq target_custom_sort_  '("Cherry" "Banana" "Cherry" "Banana" "Cherry" "Banana" "Apple" "Mango" "Apple" "Mango" "Apple" "Mango"))
              ; ; (setq target_custom_sort_  '("Cherry" "Banana" "Cherry" "Banana" "Cherry" "Banana" "Apple" "Mango" "Apple" "Mango" "Apple" "Mango"))
              ; (setq target_custom_sort_  normie_sort_)
            ;
            ;
            ;preloop_and_while
              (setq new_sort_ ())
              (setq user_custom_sort_i 0)
              (while (< user_custom_sort_i (length user_custom_sort_))
                (setq user_custom_sort_tag_ (nth  user_custom_sort_i user_custom_sort_))
                
                ;preloop_and_while
                  (setq target_custom_sort_i 0)
                  (while (< target_custom_sort_i (length target_custom_sort_))
                    (setq target_custom_sort_val_ (nth  target_custom_sort_i target_custom_sort_))
                    
                    (if 
                      ; (= (car target_custom_sort_val_) user_custom_sort_tag_ ) ;case 1
                      (= (wcmatch (car target_custom_sort_val_) (strcat "*" user_custom_sort_tag_ "*")) T ) ;case 2
                      (progn
                        (setq new_sort_ (cons target_custom_sort_val_ new_sort_))
                      )
                      (princ "no")
                    )
                    (setq target_custom_sort_i (+ target_custom_sort_i 1))
                  )
                ;
            
                (setq user_custom_sort_i (+ user_custom_sort_i 1))
              )
            ;
            (setq new_sort_ (reverse new_sort_))
          )
          (defun TAS:Find_main (new_sum_att_list_main_set_ sum_att_list_sub_set_ )
            ;preloop_and_while
              ; (setq new_sum_att_list_main_set_ '("FRONT VIEW" "SIDE VIEW") )
              ; (setq sum_att_list_sub_set_ '(("FRONT VIEW" ("6")) ("FRONT VIEW" ("1")) ("FRONT VIEW" ("2")) ("FRONT VIEW" ("3")) ("SIDE VIEW" ("5")) ("SIDE VIEW" ("4"))) )
              ; (setq sum_att_list_sub_set_ '(("FRONT VIEW" 6 ) ("FRONT VIEW" 1) ("FRONT VIEW" 2) ("FRONT VIEW" 3) ("SIDE VIEW" 5) ("SIDE VIEW" 4)) )
              (setq new_sum_att_list_main_set_i 0)
              (setq text_data_lv1 ())
              (setq text_data_lv2 ())
              
              (setq temp_val_q13 ())
              (setq temp_val_q14 ())

              (setq new_sum_att_list_main_set_i 0)
              (setq sum_att_list_sub_set_i 0)
              (while (< new_sum_att_list_main_set_i (length new_sum_att_list_main_set_))
              ; (while (< new_sum_att_list_main_set_i 1)
                (setq new_sum_att_list_main_set_val_ (nth  new_sum_att_list_main_set_i new_sum_att_list_main_set_))
                ;preloop_and_while
                  (setq sum_att_list_sub_set_i 0)
                  (setq temp_val_q13 ())
                  (while (< sum_att_list_sub_set_i (length sum_att_list_sub_set_))
                    (setq sum_att_list_sub_set_ename_test (nth  sum_att_list_sub_set_i sum_att_list_sub_set_)) 
                    (cond
                      (
                        (and
                          (eq (type (car sum_att_list_sub_set_ename_test) ) 'STR)
                          (eq (type new_sum_att_list_main_set_val_ ) 'STR)
                          (eq new_sum_att_list_main_set_val_ (car sum_att_list_sub_set_ename_test) )
                        )
                        (progn
                          (setq temp_val_q13 (cons (atoi (car (cadr sum_att_list_sub_set_ename_test))) temp_val_q13))
                          ;TA:stanndard_lambda_sorting
                            (setq temp_val_q13 (vl-sort temp_val_q13  ;bigest open indent list
                                                                      (function 
                                                                        (lambda (a b) 
                                                                          (< a b)
                                                                        )
                                                                      )
                                                  ) ;bigest close indent list
                            )
                          ;
                          

                        )
                      )
                    )
                    (setq sum_att_list_sub_set_i (+ sum_att_list_sub_set_i 1))
                  )
                ;
                (setq temp_val_q13 (append (list new_sum_att_list_main_set_val_) temp_val_q13  ))
                
                (setq temp_val_q14 (cons temp_val_q13 temp_val_q14) )
                (setq new_sum_att_list_main_set_i (+ new_sum_att_list_main_set_i 1))
              )
            ;
            ; (princ temp_val_q14 )
            (setq temp_val_q14 temp_val_q14)
          )
          (defun TA:add_to_list_ ( old_list_ pos val) 
            ;preloop_and_while
              (setq old_list_i 0)
              (setq new_lst_value ())
              (while (< old_list_i (length old_list_))
                (setq old_list_val_ (nth  old_list_i old_list_))
                  (if (= old_list_i pos)
                    (progn
                      (setq new_lst_value (cons val new_lst_value))
                      (setq new_lst_value (cons old_list_val_ new_lst_value))
                    )
                    (setq new_lst_value (cons old_list_val_ new_lst_value))
                  )
                (setq old_list_i (+ old_list_i 1))
              )
            ;
            (setq new_lst_value (reverse new_lst_value))
          )
          (defun TA:int-lst->str-lst ( old_list_)
            ;preloop_and_while
              (setq old_list_i 0)
              (setq new_lst_value ())
              (while (< old_list_i (length old_list_))
                (setq old_list_val_ (nth  old_list_i old_list_))
                  (if (= (type old_list_val_ ) 'INT)
                    (progn
                      (setq new_lst_value (cons (rtos old_list_val_ 2 0) new_lst_value))
                    )
                    (setq new_lst_value (cons old_list_val_ new_lst_value))
                  )
                (setq old_list_i (+ old_list_i 1))
              )
            ;
            (setq new_lst_value (cons " " new_lst_value))
            (setq new_lst_value (reverse new_lst_value))
          )
        ;
        ;while_select_block_on_condition_ 
            (setq target_EFname_ nil)
            (while (= target_EFname_ nil)
              (setq target_EFname_ (car (entsel "specify target_EFname Object")))
              (if
                (and ;conditional_rule_for_select_object
                  (/= target_EFname_ nil)
                  (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
                  (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
                  (or 
                    (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A0_LAND")
                    (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A1_LAND")
                    (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A2_LAND")
                    (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A3_LAND")
                    (= (LM:effectivename target_EFname_obj_) "000_ป่าสน-Tittle_block_A4_LAND")
                    ; (= (LM:effectivename target_EFname_obj_) "A$C3c9191d6")
                  )
                  ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
                  ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
                )
                (progn
                  (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
                )
                (alert "Object invalid Please try again")
              )
            )
        ;
        ;selection_set_for_fillter_blk_name
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
              (setq ss_pre_filter_set_xx_ (ssget 
                                                (list
                                                  (cons 0 "INSERT") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
        ;get_data_process_for_fillter_blk_name
          (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
          (setq target_block_list_ (TA:standard_list_croodinate_sorting target_block_ "X"))
          (sslength target_block_)
        ;
      ;preloop_and_while
        (setq target_block_list_i 0)
        (while (< target_block_list_i (length target_block_list_))
          ;get_standard_data
            (setq target_block_list_ename_ (car (nth  target_block_list_i target_block_list_)))
            (setq target_block_list_obj_ (vlax-ename->vla-object target_block_list_ename_))
          ; 
          ;specialist_data_
            (setq BLK_SC_ (vla-get-xscalefactor target_block_list_obj_ ))
            (setq target_block_list_obj_getboundingbox (vla-getboundingbox target_block_list_obj_ 'min_ 'max_)) 
            (setq target_block_list_min_ (vlax-safearray->list min_)) 
            (setq target_block_list_max_ (vlax-safearray->list max_)) 
            ; (command "rectangle" target_block_list_min_ target_block_list_max_  )
          ;
          ;selection in block filter "000 - ป่านสน_HEADING NAME 2025"
            ; (command "zoom" "o" target_block_list_ename_ "")
            (setq ssset_boundary_ (ssget "C" 
                                         (list (car target_block_list_min_) (cadr target_block_list_min_) )
                                         (list (car target_block_list_max_) (cadr target_block_list_max_) )
                                         (list (cons 0 "insert"))
                                  )
            )  
            ; (alert (strcat "ssset_boundary_"  "=" (rtos (sslength ssset_boundary_) 2 0)))
              ; (command "pselect"  ssset_boundary_ "") 
              ; (command "rectangle"  (list (car target_block_list_min_) (cadr target_block_list_min_) 0) ) 
            (setq ssset_boundary_filter_ (TA:EF_Filter_ss_set_ ssset_boundary_ "000 - ป่านสน_HEADING NAME 2025"))
            ; (alert (strcat "ssset_boundary_filter_"  "=" (rtos (sslength ssset_boundary_filter_) 2 0)))
            

          ;
          ;preloop_and_while make main set sub set
            (sslength ssset_boundary_filter_)
            (setq ssset_boundary_filter_i 0)
            (setq sum_att_list_main_set_ ())
            (setq sum_att_list_sub_set_ ())
            (while (< ssset_boundary_filter_i (sslength ssset_boundary_filter_))
              (setq ssset_boundary_filter_ename_ (ssname ssset_boundary_filter_ ssset_boundary_filter_i))
              (setq ssset_boundary_filter_obj_ (vlax-ename->vla-object ssset_boundary_filter_ename_))
              
              (setq ssset_boundary_filter_obj_ATT-heading (LM:vl-getattributevalue ssset_boundary_filter_obj_ "HEADING"))
              (setq ssset_boundary_filter_obj_ATT-lo (LM:vl-getattributevalue ssset_boundary_filter_obj_ "LO"))
              (setq att_concat_main (list (strcat ssset_boundary_filter_obj_ATT-heading )))
              (setq att_concat_all (list (strcat ssset_boundary_filter_obj_ATT-heading ) (list ssset_boundary_filter_obj_ATT-lo)))
              
              (setq sum_att_list_main_set_ (cons att_concat_main sum_att_list_main_set_ ))
              (setq sum_att_list_sub_set_ (cons att_concat_all sum_att_list_sub_set_ ))
              ;TA:stanndard_lambda_sorting
                (setq sum_att_list_sub_set_ (vl-sort sum_att_list_sub_set_  ;bigest open indent list
                                                          (function 
                                                            (lambda (a b) 
                                                              (< (car a) (car b))
                                                            )
                                                          )
                                      ) ;bigest close indent list
                )
              ;
              
              (setq ssset_boundary_filter_i (+ ssset_boundary_filter_i 1))
            )
          ;
          ;preloop_and_while create_main text
            (setq sum_att_list_main_set_i 0)
            (setq new_sum_att_list_main_set_ ())
            (while (< sum_att_list_main_set_i (length sum_att_list_main_set_))
              (setq sum_att_list_main_set_tag_ (nth  sum_att_list_main_set_i sum_att_list_main_set_))
                  (if 
                    (and 
                      ; (= sum_att_list_main_set_i 0) 
                      (= (TA:FIND_DUPLICATE_LIST (car sum_att_list_main_set_tag_) new_sum_att_list_main_set_) "N")
                    )
                    (progn
                      (setq new_sum_att_list_main_set_ (cons (car sum_att_list_main_set_tag_)  new_sum_att_list_main_set_))
                    )
                  )
              (setq sum_att_list_main_set_i (+ sum_att_list_main_set_i 1))
            )
            (setq new_sum_att_list_main_set_ (vl-sort new_sum_att_list_main_set_ '<))
          ;
          ;sorting_data_
            (setq visibility_screw_mode_  (list
                                            "หน้าปก"
                                            "สารบัญแบบ"
                                            "รายการทั่วไปประกอบแบบก่อสร้าง"
                                            "สัญลักษณ์ประกอบแบบ และคำย่อ (ABBREVIATION)"
                                            "มาตรการป้องกันอันตรายและเหตุเดือดร้อนรำคาญระหว่างก่อสร้างอาคาร"
                                            "รายการพื้น ผนัง ฝ้า"
                                            "แผนที่สังเขป"
                                            "ผังบริเวณ"
                                            "แปลนพื้นชั้นที่" 
                                            "รูปตัด" 
                                            "รูปด้าน"
                                            "แบบขยายประตู"
                                            "แบบขยายหน้าต่าง"
                                            "แบบขยายห้องน้ำ"
                                            "แบบขยายบันได"
                                            "แบบขยายทั่วไป" 
                                          )
            ) 
            (setq normie_sort_ (reverse (TAS:Find_main new_sum_att_list_main_set_ sum_att_list_sub_set_ )))
            (setq normie_sort_ (TAS:custom-sort_for_Pason_Title_Block_stamp_Heading_ visibility_screw_mode_ normie_sort_))
          ;
          ;put_custom_data
            ;preloop_and_while
              (setq normie_sort_i 0)
              (setq new_text_ ())
              (while (< normie_sort_i (length normie_sort_))
                (setq normie_sort_ename_ (nth  normie_sort_i normie_sort_))
                
                (setq new_text_ (cons 
                                  (strcat 
                                    (LM:StringSubst "" "  " (LM:StringSubst "" "/  " (LM:StringSubst "" "0" (LM:StringSubst "" "/ ::: / " (LM:lst->str (TA:add_to_list_ (TA:int-lst->str-lst normie_sort_ename_ ) 1 ":::" ) " / " ) ) ) ) ) "," ) 
                                  new_text_
                                )
                )
                
                
                (setq normie_sort_i (+ normie_sort_i 1))
              )
              (setq new_text_ (reverse new_text_))
            ;
            ;preloop_and_while
              (setq new_text_i 0)
              (setq new_text_length_list_ ())
              (while (< new_text_i (length new_text_))
                
                (setq new_text_val_ (nth  new_text_i new_text_))
                
                (setq new_text_length_ (TA:getboundingbox_text new_text_val_ BLK_SC_ 1.6  ))
                
                (setq new_text_length_list_ (cons new_text_length_ new_text_length_list_))
                (setq new_text_i (+ new_text_i 1))
              )
              (setq new_text_length_list_ (reverse new_text_length_list_))
            ;
            ;preloop_and_while
              (setq new_text_length_list_i 0)
              (setq new_text_length_list_condition_ 0)
              (setq concattext_LV1 "")
              (setq concattext_LV2 "")
              (setq concattext_LV3 "")
              (while (< new_text_length_list_i (length new_text_length_list_))
                (setq new_text_length_list_L_ (nth  new_text_length_list_i new_text_length_list_))
                (setq new_text_val_ (nth  new_text_length_list_i new_text_))
                (setq new_text_length_list_condition_ (+ new_text_length_list_L_ new_text_length_list_condition_ ))
                (setq limit_text_range 40)
                (cond
                  (;new_text_case_1
                     (and
                        (> new_text_length_list_condition_ (* limit_text_range 0 ))
                        (< new_text_length_list_condition_ (* limit_text_range 1 ))
                     )
                     (progn
                       (princ "new_text_case_1\n")
                       (setq concattext_LV1 (strcat concattext_LV1 new_text_val_))
                     )
                  )
                  (;new_text_case_2
                     (and
                        (>  new_text_length_list_condition_ (* limit_text_range 1 ) )
                        (<  new_text_length_list_condition_ (* limit_text_range 2 ) )  
                     )
                     (progn
                       (princ "new_text_case_2")
                       (setq concattext_LV2 (strcat concattext_LV2 new_text_val_))
                     )
                  )
                  (;new_text_case_3
                     (and
                        (> new_text_length_list_condition_ (* limit_text_range 2 ))
                        ; (< new_text_length_list_condition_ (* 36 3 ))
                     )
                     (progn
                       (princ "new_text_case_3\n")
                       (setq concattext_LV3 (strcat concattext_LV3 new_text_val_))
                     )
                  )
                )         
                (setq new_text_length_list_i (+ new_text_length_list_i 1))
              )
            ;
            ;input_attribute-data_to_block
            (if (/= concattext_LV1 "")
              (progn
                (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_1" concattext_LV1 )
              )
              (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_1" concattext_LV2 )
            )
            (if (/= concattext_LV2 "")
              (progn
                (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_2" concattext_LV2 )
              )
              (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_2" concattext_LV3 )
            )
            (if (/= concattext_LV3 "")
              (progn
                (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_3" concattext_LV3 )
              )
              (LM:vl-setattributevalue target_block_list_obj_ "แบบแสดง_:_value_3" "" )
            )
              
            ;
          ;
          (setq target_block_list_i (+ target_block_list_i 1))
          (princ concattext_LV1)
          (princ concattext_LV2)
          (princ concattext_LV3)
          
          
        )
      ;
      (setvar "osmode" 1215)
      
    )
;
;Copy to clipboard FUNC
  (defun TA:copytext->clipboard (clipboard_text)
    ;sub FUNC สำหรับในคัดลอก text ให้อยู่ clipboard สำหรับ ctrl+v ในอนาคต
    ;มีฟังชั่นแบบนี้ด้วยหรอ ถามจริง
    (vlax-invoke
      (vlax-get
        (vlax-get (setq htmlfile (vlax-create-object "htmlfile")) 'ParentWindow)
        'ClipBoardData
      )
      'SetData
      "Text"
      clipboard_text ;input อะไรลงไป
    )
  )
;
;Time_record_FUNC_
  (defun TA:start_time_for_record_ ()
    (setq start_time_ (strcat 
                        (substr (rtos (getvar "cdate") 2 8) 10 2)
                        (substr (rtos (getvar "cdate") 2 8) 12 2)
                        "."
                        (substr (rtos (getvar "cdate") 2 8) 14 4)
                      )
    )
  )
  (defun TA:finish_time_for_record_ ()
    (setq finish_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
    )
  )
;







; (vlax-dump-object (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 2 1)))
; (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 1 2 )
; (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_))
; (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   ;
;     (setq xlapp (vlax-get-or-create-object "Excel.Application")) (vlax-put-property xlapp 'Visible :vlax-true)
;       (setq xlwbks (vlax-get-property xlapp 'WorkBooks)) ;#<VLA-OBJECT Workbooks 000001cc3ea23ff8>
;         (setq xlwbk (vlax-invoke-method xlwbks 'Add)) ;add #<VLA-OBJECT _Workbook 000001cbfd0827f8> ;add file 
;           (setq xlshts (vlax-get-property xlwbk 'Sheets)) ;#<VLA-OBJECT Sheets 000001cc1a83a1e8>
;             (setq xlsht (vlax-get-property xlshts 'Item 1)) ;#<VLA-OBJECT _Worksheet 000001cbd61c8c88> ;get-vla-object sheet in file [sheet1 sheet2 etc]
;               (setq xlcols (vlax-get-property xlsht 'Columns)) ;#<VLA-OBJECT Range 000001cc3ea235f8>
;                 (setq ThirdColumn (vlax-get-property (vlax-variant-value (vlax-get-property xlcols 'Item 3)) 'EntireColumn))
;                   (vlax-put-property ThirdColumn 'ColumnWidth 16)
;   ;
; ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE CLASS OBJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun c:PPline_Expdata () 
 
  ;while_select_block_on_condition_
      (setq target_EFname_ nil)
      (while (= target_EFname_ nil)
        (setq target_EFname_ (car (entsel "specify target_EFname Object")))
        (if
          (and ;conditional_rule_for_select_object
            (/= target_EFname_ nil)
            (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
            (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
            ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
            ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
          )
          (progn
            (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
          )
          (alert "Object invalid Please try again")
        )
      )
  ;
  ;selection_set_for_fillter_blk_name
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
        (setq ss_pre_filter_set_xx_ (ssget 
                                          (list
                                            (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;get_data_process_for_fillter_blk_name
    (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
    (sslength target_block_)
  ;
  ;switch window
    (textscr)
    (vla-put-WindowState (vlax-get-acad-object) acmin)
  ;
  ;preloop_and_while
    (setq sum_heading_ ())
    (setq sum_att_data_ ())
    (setq target_block_i 0)
    (while 
        (and
          (< target_block_i (sslength target_block_))
        )
        (setq target_block_ename_ (ssname target_block_ target_block_i))
        (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
        
        ;main_idea_of_code
          (setq standard_heading_
            (list
              (setq target_block_obj_handle_heading "Handle")     
              ; (setq Layout_name_heading "Layout")
              (setq target_block_obj_EFname_heading "Name_block")
              ; (setq target_block_obj_color_heading "Color")
              ; (setq target_block_obj_layer_heading "Layer")
              ; (setq target_block_obj_linetype_heading "Linetype")
              (setq target_block_obj_ins_x_heading "position_X")
              (setq target_block_obj_ins_y_heading "position_Y")
              (setq target_block_obj_ins_z_heading "position_Z")
              ; (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
            )
          )
          (setq standard_data_
            (list
              (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
              ; (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
              (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
              ; (setq target_block_obj_color_data (vla-get-color target_block_obj_))
              ; (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
              ; (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
              (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              ; (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
            )
          )
          (if ;attribute_get_heading+data_process
            (and 
              (vla-get-hasattributes target_block_obj_)
            )
            (progn
              (setq att_heading_ 
                (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
              )
              (setq att_data_ 
                (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
              )
              
            )
            (setq att_heading_ nil
                  att_data_    nil
            )
          )
          (if ;dynamic_get_heading+data_process
            (and
              (vla-get-isdynamicblock target_block_obj_)
            )
            (progn
              (setq dyn_heading_
                (LM-TA:getdynprops target_block_obj_ )
              )
              (setq dyn_data_
                (LM-TA:getdynvals target_block_obj_ )
              )
              (setq dyn_filter_ (list 
                                  (setq dyn_heading_ (mapcar 'car 
                                                             (vl-remove-if 
                                                               '(lambda (x) 
                                                                  (wcmatch 
                                                                    (strcase (car x))
                                                                    "*POINT*"
                                                                  )
                                                                )
                                                               (TA:remove_val_list_rev02_ 
                                                                 "Origin"
                                                                 (setq dyn_all_ (LM-TA:getdynprops+vals 
                                                                                  target_block_obj_
                                                                                )
                                                                 )
                                                               )
                                                             )
                                                     )
                                  )
                                  (setq dyn_data_ (mapcar 'cadr 
                                                          (vl-remove-if 
                                                            '(lambda (x) 
                                                               (wcmatch 
                                                                 (strcase (car x))
                                                                 "*POINT*"
                                                               )
                                                             )
                                                            (TA:remove_val_list_rev02_ 
                                                              "Origin"
                                                              (setq dyn_all_ (LM-TA:getdynprops+vals 
                                                                               target_block_obj_
                                                                             )
                                                              )
                                                            )
                                                          )
                                                  )
                                  )
                                )
              )
            )
            (setq dyn_heading_ nil
                  dyn_data_    nil
            )
          )
          
          (if ;nested block
            (and
              (= (vla-get-objectname target_block_obj_) "AcDbBlockReference")
            )
            (progn
              (setq target_block_obj_nestblock_name (mapcar 'car (TA:Assembly_des-name-block_ target_block_obj_)))
              (setq target_block_obj_nestblock_qty (mapcar 'cdr (TA:Assembly_des-name-block_ target_block_obj_)))
              
            )
          )
          ;summary_data/loop_
            (if (= sum_heading_ nil)
              (progn
                (setq sum_heading_ (append (append (append  standard_heading_ att_heading_ )dyn_heading_ )target_block_obj_nestblock_name) )
              )
            )      
          ;
          
          (setq sum_att_data_ (cons (append (append (append standard_data_ att_data_ ) dyn_data_)target_block_obj_nestblock_qty ) sum_att_data_))
        ;
      (setq target_block_i (+ target_block_i 1))
    )
  ;
  ;switch window
    (vla-put-WindowState (vlax-get-acad-object) acmax)
    (graphscr)
  ;
  ;Excel_file_process_ 
    (setq Using_file_mode_ (cond ( (getint (strcat "\nOpen new Excel File = 1\nUse Exiting Excel file = 2\n<" (rtos (setq Using_file_mode_ (cond (Using_file_mode_) (1.0) ) ) ) "> : " ) ) ) (Using_file_mode_) ) )
    (cond
      (;Open new Excel File
        (and
          (= Using_file_mode_ 1)
        )
        (progn 
          (setq get_ExcelApp_object_ (vlax-get-or-create-object "Excel.Application"))
          (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
          (setq Workbooks_obj_ (TA:New_Excel_File_NEX_ get_ExcelApp_object_))
          (setq Excel_File+sheet_data_list_ (TA:Excel_Assembly_ALL-obj_list_ Workbooks_obj_))
          (format-all-workbook-list Excel_File+sheet_data_list_)
          (setq Excel_lasted_file_ (nth (- (length Excel_File+sheet_data_list_) 1) Excel_File+sheet_data_list_))
          (format-all-worksheet-list Excel_lasted_file_)
          (setq Excel_lasted_sheet_obj_ (vlax-get-property (nth 2 Excel_lasted_file_ ) 'sheets))
          (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
          (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
        )
      )
      (;Exiting Excel file
        (and
          (= Using_file_mode_ 2)
          (/= (setq get_ExcelApp_object_ (vlax-get-object "Excel.Application")) nil)
          (/= (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) ) nil)
          (/= (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook)) nil)
        )
        (progn
          (setq Workbooks_obj_ (vlax-get-property get_ExcelApp_object_ 'workbooks) )
          (setq workbook_obj_ (vlax-get-property get_ExcelApp_object_ 'ActiveWorkbook))
          (setq Excel_lasted_sheet_obj_ (vlax-get-property workbook_obj_ 'sheets))
          (setq Excel_lasted_worksheet_obj_ (vlax-get-property Excel_lasted_sheet_obj_ 'item 1))
          (vlax-put-property Excel_lasted_worksheet_obj_ 'name  "Summary block object")
          
          (if ( = (setq clear_file_mode (cond ( (getint (strcat "\nClear Contents Excel File = 1\nKeep Existing Contents Excel file = 2\n<" (rtos (setq clear_file_mode (cond (clear_file_mode) (1.0) ) ) ) "> : " ) ) ) (clear_file_mode) ) ) 1)
            (progn
              (TA:Excel_Clearcontents Excel_lasted_worksheet_obj_ )
              (TA_EXCEL_UnMerge_cell Excel_lasted_worksheet_obj_ (list 1 1) (list 200 200) )
              (TA_EXCEL_input_rangeccell_colorindex Excel_lasted_worksheet_obj_ (list 1 1) (list 200 200) -4142 )
            )
          )
        )    
      )
    )
    ; (vlax-dump-object get_ExcelApp_object_ )
    ; (vlax-dump-object (vlax-get (vlax-get get_ExcelApp_object_ 'selection  ) 'address ) )
    ; (vlax-dump-object (vlax-get get_ExcelApp_object_ 'ActiveCell  ) )
    ; (vlax-dump-object get_ExcelApp_object_ )
    ; (vlax-get get_ExcelApp_object_ 'selection  )
  ;
  ;preloop_and_while_creating_heading_
    (if 
      (or
        (= Using_file_mode_ 1)
        (= Using_file_mode_ 2)
      )
      (progn
        (setq sum_heading_i 0)
        (setq col_i 1)
        (setq row_start_ 3)
        
        ;merge cell properties data process 
          (TA_EXCEL_Merge_cell 
            Excel_lasted_worksheet_obj_
            (list row_start_ 1)
            (list row_start_ (setq standard_heading_length_ (length standard_heading_)))
          )
          (TA_EXCEL_input_rangeccell_color Excel_lasted_worksheet_obj_ (list row_start_ 1) (list (+ 5 (sslength ss_pre_filter_set_xx_)) (setq standard_heading_length_ (length standard_heading_))) (list 206 176 234)   )
          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_start_ 1 "block properties" )
          (TA:Excel_put_font-VerticalAlignment_R1C1 
            Excel_lasted_worksheet_obj_ 
            row_start_
            (setq standard_heading_length_ (length standard_heading_))
            "CENTER"
          )
          (TA:Excel_put_font-HonlizontalAlignment_R1C1 
            Excel_lasted_worksheet_obj_ 
            row_start_
            (setq standard_heading_length_ (length standard_heading_))
            "CENTER"
          )
        ;
        ;merge cell att heading process
          (if 
            (and 
              (/= att_heading_ nil)
              (= (listp att_heading_) T)
            )
            (progn
              (TA_EXCEL_input_rangeccell_color Excel_lasted_worksheet_obj_ (list row_start_ (+ 1 standard_heading_length_)) (list (+ 5 (sslength ss_pre_filter_set_xx_)) (+ standard_heading_length_ (length att_heading_))   ) (list 142 169 219)   )
              (setq row_merge_ (+ row_start_ standard_heading_length_))
              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_start_ row_merge_  "att properties" )
              (TA_EXCEL_Merge_cell 
                Excel_lasted_worksheet_obj_ 
                (list row_start_ (setq standard_heading_length_ (+ 1 standard_heading_length_)) ) 
                (list row_start_ (setq standard_heading_length_ (+ standard_heading_length_ (length att_heading_))))
              )
              (TA:Excel_put_font-VerticalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
              (TA:Excel_put_font-HonlizontalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
            )
            (setq standard_heading_length_ (+ 1 standard_heading_length_))
          )
        ;
        ;merge cell dyn heading process
          (if 
            (and 
              (/= dyn_heading_ nil)
              (= (listp dyn_heading_  ) T)
            )
            (progn
              (TA_EXCEL_input_rangeccell_color Excel_lasted_worksheet_obj_ (list row_start_ standard_heading_length_) (list (+ 5 (sslength ss_pre_filter_set_xx_)) (+  standard_heading_length_ (- (length dyn_heading_) 1))   ) (list 132 151 176)   )
              (setq row_merge_ standard_heading_length_)
              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_start_ row_merge_  "dyn properties" )
              (TA_EXCEL_Merge_cell 
                Excel_lasted_worksheet_obj_ 
                (list row_start_ (setq standard_heading_length_ (+ 0 standard_heading_length_))) 
                (list row_start_ (setq standard_heading_length_ (+  standard_heading_length_ (- (length dyn_heading_) 1)))) 
              )
              (TA:Excel_put_font-VerticalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
              (TA:Excel_put_font-HonlizontalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
            )
          )
        ;
        ;merge cell nested block process
          (if 
            (and 
              (/= target_block_obj_nestblock_name nil)
              (= (listp target_block_obj_nestblock_name  ) T)
            )
            (progn
              (TA_EXCEL_input_rangeccell_color Excel_lasted_worksheet_obj_ (list row_start_ (+ 1 standard_heading_length_)) (list (+ 5 (sslength ss_pre_filter_set_xx_)) (+  standard_heading_length_ (+ 1 (length dyn_heading_)))   ) (list 206 176 234)   )
              (setq row_merge_ (+ row_start_ standard_heading_length_))
              (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_start_ row_merge_  "accessories" )
              (TA_EXCEL_Merge_cell 
                Excel_lasted_worksheet_obj_ 
                (list row_start_ (setq standard_heading_length_ (+ 1 standard_heading_length_))) 
                (list row_start_ (setq standard_heading_length_ (+  standard_heading_length_ (- (length target_block_obj_nestblock_name) 1)))) 
              )
              (TA:Excel_put_font-VerticalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
              (TA:Excel_put_font-HonlizontalAlignment_R1C1 
                Excel_lasted_worksheet_obj_ 
                row_start_
                standard_heading_length_
                "CENTER"
              )
              
            )
          )
        ;
        (while (< sum_heading_i (length sum_heading_))
          (setq sum_heading_tag_ (nth  sum_heading_i sum_heading_))
          (setq col_width_ (* (strlen sum_heading_tag_) 1.8))
            (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ 1 row_start_) col_i sum_heading_tag_ )
            (TA:EXCEL_put_columnwidth Excel_lasted_worksheet_obj_ col_i col_width_ )
            (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ col_i 40 )
            (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ (+ 1 row_start_) col_i  "center")
            (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ (+ 1 row_start_) col_i "center")
            (TA:Excel_put_font-size_R1C1 Excel_lasted_worksheet_obj_ (+ 1 row_start_) col_i 15)
          (setq sum_heading_i (+ sum_heading_i 1))
          (setq col_i (+ col_i 1))
        )
        
      )
    )
    
  ;
  ;preloop_and_while_creating_data_
    (if 
      (or
        (= Using_file_mode_ 1)
        (= Using_file_mode_ 2)
      )
      (progn
        ;preloop_and_while
          (setq sum_att_data_i 0)
          
          (setq row_i (+ 2 row_start_))
          (while (< sum_att_data_i (length sum_att_data_))
            (setq sum_att_data_list_ (nth  sum_att_data_i sum_att_data_ ))
              ;status_data
                (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 9) 1 (strcat "main block = " (rtos (+ 1 sum_att_data_i) 2 0) "/" (rtos (length sum_att_data_) 2 0) ) )
              ;
              ;preloop_and_while
                (setq sum_att_data_list_i 0)
                (setq col_i 1)
                (while (< sum_att_data_list_i (length sum_att_data_list_))
                  (setq sum_att_data_list_value_ (nth  sum_att_data_list_i sum_att_data_list_))       
                        
                    ;special_text
                      (if 
                        (and
                          (= col_i 1)
                        )
                        (progn
                          (vlax-get (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 2 1)) 'NumberFormat ) ;รอเพิ่มเป็น sun func
                          (vlax-put (vlax-variant-value (vlax-get-property (vlax-get Excel_lasted_worksheet_obj_ 'Cells) 'Item 2 1)) 'NumberFormat "@" ) ;รอเพิ่มเป็น sun func
                          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                        )
                        (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                      )
                      (if 
                        (and
                          (/= (listp sum_att_data_list_value_) T)
                          ( = (numberp sum_att_data_list_value_) nil)
                          (wcmatch sum_att_data_list_value_ "*:*")
                        )
                        (progn
                          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" sum_att_data_list_value_) )
                        )
                        (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                      )
                      (if 
                        (and
                          (listp sum_att_data_list_value_)
                          (= (numberp (car sum_att_data_list_value_)) T)
                          
                        )
                        (progn
                          (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i (strcat "'" (rtos (car sum_att_data_list_value_) 2 8)) )
                        )
                        (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ row_i col_i sum_att_data_list_value_ )
                      )
                    ;

                    
                    
                    (TA:EXCEL_put_rowheight Excel_lasted_worksheet_obj_ row_i  25 )
                    (TA:Excel_put_font-VerticalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i  "center")
                    (TA:Excel_put_font-HonlizontalAlignment_R1C1 Excel_lasted_worksheet_obj_ row_i col_i "center")
                    
                    ; (TA:EXCEL_put_columnwidth_viatext Excel_lasted_worksheet_obj_ 1 col_i 10 1)

                  (setq sum_att_data_list_i (+ sum_att_data_list_i 1))
                  (setq col_i (+ col_i 1))
                  ; (princ (strcat "main block = " (rtos sum_att_data_i 2 0) "/" (rtos (length sum_att_data_) 2 0) ))
                  ; (princ (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ))
                  ;Status_data
                    
                    (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ (+ (length sum_att_data_) 10) 1 (strcat "sub data = " (rtos sum_att_data_list_i 2 0) "/" (rtos (length sum_att_data_list_) 2 0)  ) )
                  ;
                )
              ;
            (setq row_i (+ row_i 1))
            (setq sum_att_data_i (+ sum_att_data_i 1))
          )
        ;
      )
    )
  ;
  ;final_alert
    (alert "Tranfer Data Finish")
  ;
)
(defun c:PPline_Expdata2_DAFT2_ () 
 
  ;while_select_block_on_condition_
      (setq target_EFname_ nil)
      (while (= target_EFname_ nil)
        (setq target_EFname_ (car (entsel "specify target_EFname Object")))
        (if
          (and ;conditional_rule_for_select_object
            (/= target_EFname_ nil)
            (setq target_EFname_obj_ (vlax-ename->vla-object target_EFname_))
            (= (vla-get-objectname target_EFname_obj_ ) "AcDbBlockReference")
            ; (= (vla-get-isdynamicblock target_EFname_obj_ ) :vlax-true)
            ; (/= (LM:getdynpropallowedvalues target_EFname_obj_ "view") nil)
          )
          (progn
            (setq target_EFname_list_ (LM:effectivename target_EFname_obj_) )
          )
          (alert "Object invalid Please try again")
        )
      )
  ;
  ;selection_set_for_fillter_blk_name
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
        (setq ss_pre_filter_set_xx_ (ssget 
                                          (list
                                            (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;get_data_process_for_fillter_blk_name
    (setq target_block_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ target_EFname_list_)) 
    (sslength target_block_)
  ;
  ;switch window
    (textscr)
    (vla-put-WindowState (vlax-get-acad-object) acmin)
  ;
  ;preloop_and_while
    (setq sum_heading_ ())
    (setq sum_att_data_ ())
    (setq target_block_i 0)
    (while 
        (and
          (< target_block_i (sslength target_block_))
        )
        (setq target_block_ename_ (ssname target_block_ target_block_i))
        (setq target_block_obj_ (vlax-ename->vla-object target_block_ename_))
        
        ;main_idea_of_code
          (setq standard_heading_
            (list
              (setq target_block_obj_handle_heading "Handle")     
              ; (setq Layout_name_heading "Layout")
              (setq target_block_obj_EFname_heading "Name_block")
              ; (setq target_block_obj_color_heading "Color")
              ; (setq target_block_obj_layer_heading "Layer")
              ; (setq target_block_obj_linetype_heading "Linetype")
              (setq target_block_obj_ins_x_heading "position_X")
              (setq target_block_obj_ins_y_heading "position_Y")
              (setq target_block_obj_ins_z_heading "position_Z")
              ; (setq target_block_obj_Rotation_heading "Rotation(DEG)")      
            )
          )
          (setq standard_data_
            (list
              (setq target_block_obj_handle_data (vla-get-handle target_block_obj_))     
              ; (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
              (setq target_block_obj_EFname_data (LM:effectivename target_block_obj_))
              ; (setq target_block_obj_color_data (vla-get-color target_block_obj_))
              ; (setq target_block_obj_layer_data (vla-get-layer target_block_obj_))
              ; (setq target_block_obj_linetype_data (vla-get-linetype target_block_obj_))
              (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint target_block_obj_)))))
              ; (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation target_block_obj_)))      
            )
          )
          (if ;attribute_get_heading+data_process
            (and 
              (vla-get-hasattributes target_block_obj_)
            )
            (progn
              (setq att_heading_ 
                (LM:vl-getattributevalue-tag-TA-Modifies target_block_obj_)
              )
              (setq att_data_ 
                (LM:vl-getattributevalue-val-TA-Modifies target_block_obj_)
              )
              
            )
            (setq att_heading_ nil
                  att_data_    nil
            )
          )
          (if ;dynamic_get_heading+data_process
            (and
              (vla-get-isdynamicblock target_block_obj_)
            )
            (progn
              (setq dyn_heading_
                (LM-TA:getdynprops target_block_obj_ )
              )
              (setq dyn_data_
                (LM-TA:getdynvals target_block_obj_ )
              )
              (setq dyn_filter_ (list 
                                  (setq dyn_heading_ (mapcar 'car 
                                                             (vl-remove-if 
                                                               '(lambda (x) 
                                                                  (wcmatch 
                                                                    (strcase (car x))
                                                                    "*POINT*"
                                                                  )
                                                                )
                                                               (TA:remove_val_list_rev02_ 
                                                                 "Origin"
                                                                 (setq dyn_all_ (LM-TA:getdynprops+vals 
                                                                                  target_block_obj_
                                                                                )
                                                                 )
                                                               )
                                                             )
                                                     )
                                  )
                                  (setq dyn_data_ (mapcar 'cadr 
                                                          (vl-remove-if 
                                                            '(lambda (x) 
                                                               (wcmatch 
                                                                 (strcase (car x))
                                                                 "*POINT*"
                                                               )
                                                             )
                                                            (TA:remove_val_list_rev02_ 
                                                              "Origin"
                                                              (setq dyn_all_ (LM-TA:getdynprops+vals 
                                                                               target_block_obj_
                                                                             )
                                                              )
                                                            )
                                                          )
                                                  )
                                  )
                                )
              )
            )
            (setq dyn_heading_ nil
                  dyn_data_    nil
            )
          )
          
          (if ;nested block
            (and
              (= (vla-get-objectname target_block_obj_) "AcDbBlockReference")
            )
            (progn
              (setq target_block_obj_nestblock_name (mapcar 'car (TA:Assembly_des-name-block_ target_block_obj_)))
              (setq target_block_obj_nestblock_qty (mapcar 'cdr (TA:Assembly_des-name-block_ target_block_obj_)))
              
            )
          )
          ;summary_data/loop_
            (if (= sum_heading_ nil)
              (progn
                (setq sum_heading_ (list (append (append (append  standard_heading_ att_heading_ )dyn_heading_ )target_block_obj_nestblock_name)) )
              )
            )      
          ;
          
          (setq sum_att_data_ (cons (append (append (append standard_data_ att_data_ ) dyn_data_)target_block_obj_nestblock_qty ) sum_att_data_))
        ;
      (setq target_block_i (+ target_block_i 1))
    )
  ;
  ;switch window
    (vla-put-WindowState (vlax-get-acad-object) acmax)
    (graphscr)
  ;
  ;Export_process_
    (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1)) ;open new Excel window (function use follow argument)
    (TA:EXCEL_put_columnformat Excel_lasted_worksheet_obj_ 1 "@" )
    (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (tag list_)
      Excel_lasted_worksheet_obj_ 
      2
      1
      sum_heading_
    )
    (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (tag list_)
      Excel_lasted_worksheet_obj_ 
      3
      1
      sum_att_data_
    )
  ;
  ;final_alert
    (alert "Tranfer Data Finish")
  ;
)



;  (vlax-dump-object (nth 0 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 1 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 2 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 3 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 4 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 5 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 6 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 7 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))
;  (vlax-dump-object (nth 8 (vlax-invoke target_block_obj_ 'getdynamicblockproperties)))



(defun c:Text-to-clipboard_T2C ()
  ;selection_set_for_fillter_blk_name
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
        (setq ss_pre_filter_set_xx_ (ssget
                                          (list
                                            (cons 0 "HATCH") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
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
    (setq sum_area_ 0)
    (setq result_area_ 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq sum_area_ (/ (vla-get-area ss_pre_filter_set_xx_obj_ ) 1000000))
      (setq result_area_ (+ result_area_ sum_area_))
      
      ; (defun real-to-decimal-string (val dec)
      ;   ;; val = ค่า real เช่น 4.06109e-05
      ;   ;; dec = จำนวนทศนิยมที่ต้องการ เช่น 6
      ;   (setq factor (expt 10.0 dec))
      ;   (setq rounded (/ (float (fix (+ (* val factor) 0.5))) factor))
      ;   (vl-princ-to-string rounded)
      ; )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;copy-to-clipboard process
    (TA:copytext->clipboard 
      (strcat
        (rtos result_area_ 2 2)
      )
    )
  ;
  (princ "\n")
  (princ (rtos result_area_ 2 2))
  (command "pselect"  ss_pre_filter_set_xx_ "" )
)





  

  

  (setq row_ 1)
  (setq col_ 0)
  (setq start_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  (foreach sub_list_ main_list_
    (setq col_ 1)
    (if 
      (= (listp sub_list_) T)
      (progn
        (foreach sub_list_value_ sub_list_  ;(setq list_l_ (list 5 65 3 65 32 5 5  5 5 91 4 5 5))
          (vlax-put 
            (vlax-variant-value 
              (vlax-get-property 
                (vlax-get _worksheet_obj_ 'Cells)
                'Item
                row_
                col_
              )
            )
            'Value
            sub_list_value_
          )
          (setq col_ (+ col_ 1))
        )
      )
    )
    (setq row_ (+ row_ 1))
  )
  (setq finish_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  (- (atof finish_time_) (atof start_time_))



  (setq start_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  ;preloop_and_while 
    (setq main_list_i 0)
    (setq row_ 1)
    (while (< main_list_i (length main_list_))
      (setq sub_list_ (nth  main_list_i main_list_))
      (if 
        (= (listp main_list_) T)
        (progn
          ;preloop_and_while
            (setq sub_list_i 0)
            (setq col_ 1)
            (while (< sub_list_i (length sub_list_))
              (setq sub_list_value_ (nth  sub_list_i sub_list_))
                (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item row_ col_)) 'Value sub_list_value_)
              (setq col_ (+ col_ 1))
              (setq sub_list_i (+ sub_list_i 1))
            )
          ;
        )
      )
      (setq row_ (+ row_ 1))
      (setq main_list_i (+ main_list_i 1))
    )
  ;
  (setq finish_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  (- (atof finish_time_) (atof start_time_))


  (defun TA:Excel_foreach_input_list_dataset_R1C1 (_worksheet_obj_ row_start_ col_start_ main_list_  )
    ;Note By Code_Developer
      ;principle of coodework is designed for input data list to Excel appication via list/gruop data,
      ;Can specify R1C1 via argument (row_start_ col_start_)

      ;Precautions when using this program 
        ;The argument main_list_ must contain data in the form of a main list that groups multiple sublists.
        ;Each sublist holds detailed data intended for display purposes.
        ;Example
          ; (setq mainlist ;main list_
          ;                (list 
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                )
          ; )
          ;Precautions when using this program 
            ;The argument main_list_ must contain data in the form of a main list that groups multiple sublists.
            ;Each sublist holds detailed data intended for display purposes.
            ;Example 1
              ; mainlist_
              ; └─ sublist
              ;    └─ sublist of sublist (maximun Below lv.3 )
              ; └─ sublist
              ;    └─ sublist of sublist (maximun Below lv.3 )

              ; (setq mainlist ;main list_
              ;                (list 
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 (list "A" "B" "C" "D") 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                  (list 54 68 6 6 (list "A" "B" "C" "D") 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
              ;                )
              ; )
            ;
          ;
        ;
      ;
    ;
    ;
    ; ;Example_argument_
    ;   (setq row_start_ 1)
    ;   (setq col_start_ 1)
    ; ;
      (foreach sub_group_ main_list_
        (setq col_ col_start_)
        (if 
          (= (listp sub_group_) T)
          (progn
            (foreach sub_list_ sub_group_  ; ← ลูปชั้นที่ 2
              (if (listp sub_list_)
                (foreach sub_value_ sub_list_  ; ← ลูปชั้นที่ 3
                  (progn
                    (vlax-put
                      (vlax-variant-value
                        (vlax-get-property
                          (vlax-get _worksheet_obj_ 'Cells)
                          'Item
                          row_start_
                          col_
                        )
                      )
                      'Value
                      sub_value_
                    )
                    (setq col_ (+ col_ 1))
                  )
                )
                ;; ถ้า sub_list_ ไม่ใช่ list → แปลว่าเป็นค่าธรรมดา (อาจอยู่แค่ 2 ชั้น)
                (progn
                  (vlax-put
                    (vlax-variant-value
                      (vlax-get-property
                        (vlax-get _worksheet_obj_ 'Cells)
                        'Item
                        row_start_
                        col_
                      )
                    )
                    'Value
                    sub_list_
                  )
                  (setq col_ (+ col_ 1))
                )
              )
            )
          )
        )

        (setq row_start_ (+ row_start_ 1))
      )
  )
  (TA:Excel_foreach_input_list_dataset_R1C1 _worksheet_obj_ 3 5 main_list_  )
  (defun TA:Excel_while_input_list_data_R1C1 (_worksheet_obj_ row_start_ col_start_ main_list_  )
    ;Note By Code_Developer
      ;principle of coodework is designed for input data list to Excel appication via list/gruop data,
      ;Can specify R1C1 via argument (row_start_ col_start_)

      ;Precautions when using this program 
        ;The argument main_list_ must contain data in the form of a main list that groups multiple sublists.
        ;Each sublist holds detailed data intended for display purposes.
        ;Example 1
          ; mainlist_
          ; └─ sublist
          ;    └─ sublist of sublist (maximun Below lv.3 )
          ; └─ sublist
          ;    └─ sublist of sublist (maximun Below lv.3 )

          ; (setq mainlist ;main list_
          ;                (list 
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 (list "A" "B" "C" "D") 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                  (list 54 68 6 6 (list "A" "B" "C" "D") 32 68 3 65 6 35 65 3 35 61 64 95 3) ;sub list_
          ;                )
          ; )
        ;
      ;
    ;
    ;
    ;Example_argument_
      ; (setq row_start_ 1)
      ; (setq col_start_ 0)
    ;
    ;preloop_and_while 
      (setq main_list_i 0)
      (setq row_ row_start_)
      (while (< main_list_i (length main_list_))
        (setq sub_list_ (nth  main_list_i main_list_))
        (if 
          (= (listp main_list_) T)
          (progn
            ;preloop_and_while
              (setq sub_list_i 0)
              (setq col_ col_start_)
              (while (< sub_list_i (length sub_list_))
                (setq sub_list_value_ (nth  sub_list_i sub_list_))
                  (if (= (listp sub_list_value_ ) T)
                    (progn
                      ;preloop_and_while
                        (setq sub_list_value_i 0)
                        (while (< sub_list_value_i (length sub_list_value_))
                          (setq sub_list_value_ofsubvalue_ (nth  sub_list_value_i sub_list_value_))
                            (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item row_ col_)) 'Value sub_list_value_ofsubvalue_)
                            (setq col_ (+ col_ 1))
                          (setq sub_list_value_i (+ sub_list_value_i 1))
                        )
                      ;
                    )
                    (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item row_ col_)) 'Value sub_list_value_)
                  )
                (setq col_ (+ col_ 1))
                (setq sub_list_i (+ sub_list_i 1))
              )
            ;
          )
        )
        (setq row_ (+ row_ 1))
        (setq main_list_i (+ main_list_i 1))
      )
    ;
  )
  (TA:Excel_while_input_list_data_R1C1 _worksheet_obj_ 4 1 main_list_ )

  (setq start_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
    (TA:Excel_while_input_list_data_R1C1 _worksheet_obj_ 4 1 main_list_ )

  (setq finish_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  (- (atof finish_time_) (atof start_time_))

  (setq start_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
    (TA:Excel_foreach_input_list_dataset_R1C1 _worksheet_obj_ 1 1 main_list_  )

  (setq finish_time_ (strcat 
                      (substr (rtos (getvar "cdate") 2 8) 10 2)
                      (substr (rtos (getvar "cdate") 2 8) 12 2)
                      "."
                      (substr (rtos (getvar "cdate") 2 8) 14 4)
                    )
  )
  (- (atof finish_time_) (atof start_time_))




  (defun c:Caddata_to_Exceldata_C3EE ()
    
    
    ;selection_set_for_fillter_blk_name
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
          (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;filter_selection_set_process 
      (setq sst_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ (setq EF_name_val (LM:effectivename (vlax-ename->vla-object (car (entsel "specify name block for flter Ef name "))))) ) )
    ;
    ;main_idea_get_properties_data_ process_
      ; (setq sst_ (ssget)) ;uncomment incase don't use fillter Sub FUNC or testing
      ;preloop_and_while
        (setq sst_i 0)
        (setq sst_properties_mainlist_ ())
        (setq sst_properties_sublist_ ())
        (while (< sst_i (sslength sst_))
          (setq sst_ename_ (ssname sst_ sst_i))
            (setq sst_obj_ (vlax-ename->vla-object sst_ename_))
            (setq sst_obj_std_list_ (list ;main standard propertie
                              (setq sst_obj_handle (vla-get-handle sst_obj_)) ;sub standard propertie
                              (setq Layout_name_data (vla-get-Name (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-Acad-Object) ) ) ))
                              (setq target_block_obj_EFname_data (LM:effectivename sst_obj_))
                              (setq target_block_obj_color_data (vla-get-color sst_obj_))
                              (setq target_block_obj_layer_data (vla-get-layer sst_obj_))
                              (setq target_block_obj_linetype_data (vla-get-linetype sst_obj_))
                              (setq target_block_obj_ins_x_data (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                              (setq target_block_obj_ins_y_data (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                              (setq target_block_obj_ins_z_data (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sst_obj_)))))
                              (setq target_block_obj_Rotation_data (rad-to-deg (vla-get-rotation sst_obj_)))      
                            )
            )
            (if (= (vla-get-isdynamicblock sst_obj_) ) ;dynamic_properties_list_
              (progn
                (setq sst_obj_dyn_val_list (mapcar 'cadr 
                                                  (vl-remove-if 
                                                    '(lambda (x) 
                                                        (wcmatch 
                                                          (strcase (car x))
                                                          "*POINT*"
                                                        )
                                                      )
                                                    (TA:remove_val_list_rev02_ 
                                                      "Origin"
                                                      (setq sst_obj_dyn_all_ (LM-TA:getdynprops+vals 
                                                                        sst_obj_
                                                                      )
                                                      )
                                                    )
                                                  )
                                          )
                )
              )
            )
            (if (= (vla-get-hasattributes sst_obj_) :vlax-true) ;attribute_properties_list_
              (progn
                (setq sst_obj_att_val_list (LM:vl-getattributevalue-val-TA-Modifies sst_obj_ ) )
              )
            )
            (if (= sst_i 0) ;for create  tag one time 
              (progn
                (setq sst_tag_mainlist_ ())
                (if (= (vla-get-isdynamicblock sst_obj_))  ;dynamic_properties_list_
                  (progn 
                    (setq sst_obj_dyn_tag_list (mapcar 'car
                                                        (vl-remove-if 
                                                          '(lambda (x) 
                                                            (wcmatch 
                                                              (strcase (car x))
                                                              "*POINT*"
                                                            )
                                                          )
                                                          (TA:remove_val_list_rev02_ 
                                                            "Origin"
                                                            (setq sst_obj_dyn_all_ (LM-TA:getdynprops+vals 
                                                                                    sst_obj_
                                                                                  )
                                                            )
                                                          )
                                                        )
                                                )
                    )
                  )
                )
                (if (= (vla-get-hasattributes sst_obj_) :vlax-true) ;attribute_properties_list_
                  (progn
                    (setq sst_obj_att_tag_list (LM:vl-getattributevalue-tag-TA-Modifies sst_obj_ ) )
                  )
                )
                (setq sst_tag_mainlist_
                  (list
                    (append 
                      (list
                        "Handle"
                        "Layout"
                        "Name_block"
                        "Color"
                        "Layer"
                        "Linetype"
                        "position_X"
                        "position_Y"
                        "position_Z"
                        "Rotation(DEG)"
                      )
                      sst_obj_dyn_tag_list
                      sst_obj_att_tag_list
                    )
                  )
                )
              )
            )
          

          
            
          
          
            (setq sst_properties_sublist_ (append sst_obj_std_list_ (list sst_obj_dyn_val_list) (list sst_obj_att_val_list))) 
            (setq sst_properties_mainlist_ (cons sst_properties_sublist_ sst_properties_mainlist_))
          (setq sst_i (+ sst_i 1))
        )
      ;
    ;
    (setq st_time_ (TA:start_time_for_record_))
    ;export_properties_data_to_Excel
      (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1)) ;Sub-FUNC for oepn Excel Appication_
      (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (tag list_)
        Excel_lasted_worksheet_obj_ 
        1
        1
        sst_tag_mainlist_
      )
      (TA:Excel_foreach_input_list_dataset_R1C1 ;Sub-FUNC create properties data list to Excel (content list_)
        Excel_lasted_worksheet_obj_ 
        2
        1
        sst_properties_mainlist_
      )
    ;
    (setq st_time_1 (TA:start_time_for_record_))
    (alert (strcat 
             "total use time \n"
             (rtos 
               (- 
               (+ 
                 (* 3600 (atof (substr st_time_1 1 2)))
                 (* 60 (atof (substr st_time_1 3 2)))
                 (atof (substr st_time_1 6 2))
               )
               (+ 
                 (* 3600 (atof (substr st_time_ 1 2)))
                 (* 60 (atof (substr st_time_ 3 2)))
                 (atof (substr st_time_ 6 2))
               )
             )
               2 0
             )
             " second"
           )
    )
  )

  (setq VLA-OBJECT-Worksheet Excel_lasted_worksheet_obj_ )

  

  
  
  



