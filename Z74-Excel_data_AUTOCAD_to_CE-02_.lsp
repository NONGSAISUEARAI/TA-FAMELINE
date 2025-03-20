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
;Filter Selection Set by Effectivename Ta Trai  
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
  (defun TA:Excel_Assembly_worksheet_obj_list_ (sheets_obj_)
    ;Note By Code_Developer
    ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
    ;
    ;sheets_obj_ = #<VLA-OBJECT Sheets >
    ;preloop_and_while
      (if 
        (or
          (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*WORKBOOKS*")
          (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*SHEETS*")
        )
        (progn
          (setq _worksheet_Vla-obj_list_ ())
          (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count ))
          (setq sheets_obj_i 1)
          (while (<= sheets_obj_i sheets_obj_length)
            (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item sheets_obj_i))
              (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
              (setq _worksheet_Vla-obj_sum_ (list sheets_obj_i _worksheet_obj_name _worksheet_obj_ ))
            (setq _worksheet_Vla-obj_list_ (cons _worksheet_Vla-obj_sum_ _worksheet_Vla-obj_list_))
            (setq sheets_obj_i (+ sheets_obj_i 1))
          )
          (setq _worksheet_Vla-obj_list_ (reverse _worksheet_Vla-obj_list_ ))
        )
      )
    ;preloop
  )
  (defun TA:Excel:VLA-OBJECT-Workbook->VLA-OBJECT-Sheets  (VLA-OBJECT_Workbook)
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
  (defun TA:Excel:VLA-OBJECT-Sheets->VLA-OBJECT-Worksheet  (VLA-OBJECT-Sheets i)
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
  (defun TA:EXCEL_put_columnwidth_viatext  (Rangecell_row_ Rangecell_column_ Rangecell_target_row_ text_width_val)
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
    ;example_argument_
      ; (setq Rangecell_row_ 1)
      ; (setq Rangecell_column_ 2)
      
      ; (setq Rangecell_target_row_ 2) 
    
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
              (wcmatch (strcase filePath) "*XLT*")
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
  (defun TA:Excel_open_CE-02_ ()
    (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-create-object "Excel.Application"))
        
      )
    )
    ; (setq filePath (getfiled "Select an Excel file"  "C:/TA/TEMP EST FILES/" "" 8)) ;open Ex-file
    ; (setq filePath (getfiled "Select an Excel file"  "C:/Users/Surface Book 3/Documents/Custom Office Templates/" "" 8)) ;open Ex-file
    ; (setq filePath (getfiled "Select an Excel file"  "C:/Users/Surface Book 3/Documents/Custom Office Templates/" "" 8)) ;open Ex-file
    (setq filePath "C:/Users/Surface Book 3/Documents/Custom Office Templates/FM-DE-CE-02-TN-BU2-2025-XXX.xltm") ;open Ex-file
    (if 
      (and
        (if
          (/= filePath nil)
          (progn
            (or 
              (wcmatch (strcase filePath) "*XLSX")
              (wcmatch (strcase filePath) "*XLSM")
              (wcmatch (strcase filePath) "*XLT*")
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
    (setq _Workbook_obj_ _Workbook_obj_)
  )
  (defun c:TA:Excel_activate_worksheet_ (VLA-OBJECT-Worksheet )
    (vlax-invoke-method VLA-OBJECT-Worksheet 'Activate)
  )

;

  (defun format-list (data)
    (print "(") ; พิมพ์เปิดวงเล็บ
    (foreach point data
      (princ " ") ; เพิ่มช่องว่างก่อนแต่ละรายการ
      (print point) ; แสดงแต่ละจุด
    )
    (print ")") ; พิมพ์ปิดวงเล็บ
  )
  (defun format-entity-list (data)
    (print "(") ; พิมพ์เปิดวงเล็บ
    (foreach entry data
      (cond
        ((listp entry) ; ตรวจสอบว่าเป็น list ย่อยหรือไม่
        (princ " ")   ; เพิ่มช่องว่าง
        (print entry) ; แสดง list
        )
        (T ; อื่น ๆ เช่น entity name
        (princ " ")   ; เพิ่มช่องว่าง
        (princ entry) ; แสดง entity
        )
      )
    )
    (print ")") ; พิมพ์ปิดวงเล็บ
  )

  (defun c:Mck2Ecel ()
   
    
    ;autocad_proccess
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
      ;filter and sort efname
        (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "001 - LV_PATTERN DATA CUSTOM6"))
        (setq ss_pre_filter_set_xx_sorted_ (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx_ "X"))
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_sorted_i 0)
        (setq MCK_data_list_ ())
        (while (< ss_pre_filter_set_xx_sorted_i (length ss_pre_filter_set_xx_sorted_))
          (setq ss_pre_filter_set_xx_sorted_ename_ (car (nth  ss_pre_filter_set_xx_sorted_i ss_pre_filter_set_xx_sorted_)))
          
          (setq ss_pre_filter_set_xx_sorted_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_sorted_ename_))
          
          (setq partcode_
            (strcat
              (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "CODE_VALUE_1")
              "_"
              (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "CODE_VALUE_2")
            )
          )
          ;matterial_process
            (setq matterial_
              (strcat
                (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "MATTERIAL_VALUE")           
              )
            )
            (cond
              (;material_process_case_1
                (and
                    (= matterial_ "LITEWOOD_PANEL")
                )
                (progn
                  (setq matterial_val "Aluminium")
                  (princ "material_process_case_1")
                )
              )
              (;material_process_case_2
                (and
                    (= matterial_ "AL.EXTRUSION")
                )
                (progn
                  (setq matterial_val "Alu.Extrusion")
                  (princ "material_process_case_2")
                )
              )
              (;material_process_case_3
                 (and
                    (= matterial_ "")
                )
                 (progn
                   (setq matterial_val "-")
                   (princ "material_process_case_3")
                 )
              )
              ; (;material_process_case_4
              ;    (and
              ;       ()
              ;       ()
              ;    )
              ;    (progn
              ;      ()
              ;      (princ "material_process_case_4")
              ;    )
              ; )
            )
          ;
          ;thickness_process
            (setq thinckness_
              (strcat
                (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "THICKNESS_VALUE")           
              )
            )
            (if (/= thinckness_ "")
              (progn
                (setq thinckness_val (substr thinckness_ 1 (vl-string-search "mm." thinckness_  1 )))
              )
              (setq thinckness_val "-")
            )
          ;
          ;sizing process
            (if (/= (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "SIZING_VALUE") nil)
              (progn
                (setq sizing_ (strcat (LM:StringSubst "" " " (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "SIZING_VALUE" )) ) )
                (setq sizing_val_ (substr sizing_ 1 (+ 3 (vl-string-search "mm." sizing_  1 ))))
                
                (setq sizing_w_val (substr sizing_val_ 1 (vl-string-search "x" sizing_val_  1 )))
                (setq sizing_h_val (LM:StringSubst "" "mm." (substr sizing_val_ (+ 2 (vl-string-search "x" sizing_val_  1 )) (- (vl-string-search "mm." sizing_val_  1 ) 1))))
                
              )
              (setq sizing_w_val "-"
                    sizing_h_val "-"
              )
            )
            
            
          ;
          ;coathing_process
            (setq coathing_
              (strcat
                (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "COATING_VALUE_2")           
              )
            )
            (setq coathing_CC (list 
                                "SD-9101" "SD-9111" "MT-0112" "MT-0113" "FWC-CC-01" "FWC-CC-02" 
                                "FWC-CC-03" "FWC-CC-04" "SD-9201" "MT-0201" "GOLDEN TEAK"
                              )
            )
            (setq coathing_SP (list 
                              "SD-9101" "SD-9103" "SD-9104" "SD-7101" "SD-7102" "SD-8101" "SD-9111" 
                              "MT-0112" "MT-0113" "SD-7201" "SD-1201" "SD-2201" "SD-3201" "SD-5201" 
                              "SD-7201" "SD-1201" "SD-2201" "SD-3201" "SD-5201" "SD-6201" "SD-8201" 
                              "SD-8202" "MT-0203" "MT-0204" "MT-0206" "MT-0207"
                            ) 
            )
            (setq coathing_PC (list "SD-9102" "SD-9201" "SD-9202" "SD-7202" "SD-9211" 
                                  "MT-0201" "MT-0202" "MT-0205" "FWC-WD-01" "FWC-WD-02" 
                                  "FWC-WD-03" "FWC-WD-04"
                            )
            )
            (setq coathing_ACP (list "MA001" "MA002" "MA004" "MA011" "MA012" "MA014" "MA021" "MA022" 
                                  "MA031" "SA001" "SA002" "SA003" "SA004" "SA011" "SA012" "SA013" 
                                  "SA014" "SA021" "SA022" "SA023" "SA024" "VF013" "VF031" "VF041" 
                                  "VF052" "VS001" "VS002" "VS003" "VS004" "VS011" "VS012" "VS021" 
                                  "VS022" "002NC" "003NC" "001NC" "005NC" "004NC" "006NC" "007NC" 
                                  "008NC" "001HG" "002HG" "004HG" "003HG" "042SD" "041SD" "033SD" 
                                  "034SD" "031SD" "032SD" "052SD" "051SD" "081SD" "091SD" "061SD" 
                                  "071SD" "072SD" "031MT" "033MT" "011MT" "014MT" "001MT" "002MT" 
                                  "003MT" "004MT" "021MT" "022MT" "023MT" "024MT" "013MT" "012MT" 
                                  "082MT" "081MT" "041MT" "042MT" "010MT" "051MT" "061MT" "062MT" 
                                  "001SP" "002SP" "003SP" "004SP" "001HL"
                            )
            )
            (cond
              (;Coathing_type_case_1
                (and
                    (= "Y" (TA:FIND_DUPLICATE_LIST coathing_ coathing_CC))
                )
                (progn
                  (setq coathing_ (list "CC" coathing_))
                  (princ "Coathing_type_case_1")
                )
              )
              (;Coathing_type_case_2
                (and
                    (= "Y" (TA:FIND_DUPLICATE_LIST coathing_ coathing_SP))
                )
                (progn
                  (setq coathing_ (list "SP" coathing_))
                  (princ "Coathing_type_case_2")
                )
              )
              (;Coathing_type_case_3
                (and
                    (= "Y" (TA:FIND_DUPLICATE_LIST coathing_ coathing_PC))
                )
                (progn
                  (setq coathing_ (list "PC" coathing_))
                  (princ "Coathing_type_case_3")
                )
              )
              (;Coathing_type_case_4
                (and
                    (= "Y" (TA:FIND_DUPLICATE_LIST coathing_ coathing_ACP))
                )
                (progn
                  (setq coathing_ (list "ACP" coathing_))
                  (princ "Coathing_type_case_4")
                )
              )
              (;Coathing_type_case_5
                (and
                    (= coathing_ "")
                )
                (progn
                  (setq coathing_ "-")
                  (princ "Coathing_type_case_5")
                )
              )
            )
          ;
          ;coathing_side_process
            (setq coathing_side
              (strcat
                (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "COATING_VALUE_1")           
              )
            )
            (cond
              (;coathing_side_case_1
                (and
                  (wcmatch coathing_side "*ALL*")
                )
                (progn
                  (setq coathing_side_val "พ่นรอบตัว")
                  (princ "coathing_side_case_1")
                )
              )
              (;coathing_side_case_2
                (and
                  (wcmatch coathing_side "1 side")
                )
                (progn
                  (setq coathing_side_val "1 ด้าน")
                  (princ "coathing_side_case_2")
                )
              )
              (;coathing_side_case_3
                (and
                  (wcmatch coathing_side "2 side")
                )
                (progn
                  (setq coathing_side_val "2 ด้าน")
                  (princ "coathing_side_case_3")
                )
              )
              (;coathing_side_case_4
                (and
                  (= coathing_ "-")
                )
                (progn
                  (setq coathing_side_val "-")
                  (princ "coathing_side_case_4")
                )
              )
            )
            
          ;
          ;total+process
            (setq total_set_ 
              (atoi (substr (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "TOTAL_SET_VALUE" ) 1 1 ) )
            )
          ;
          ;sumList_
          (setq mck_data_ (list 
                            partcode_
                            matterial_val
                            thinckness_val
                            sizing_w_val
                            sizing_h_val
                            coathing_
                            coathing_side_val
                            total_set_
                          )
          )
          (setq MCK_data_list_ (cons mck_data_ MCK_data_list_ ))
          
          ;
          

          
          (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
        )
      ;
    ;
    ;Excel_process
      (setq set1_ (TA:Excel_open_CE-02_))
      (setq set2_ (TA:Excel:VLA-OBJECT-Workbook->VLA-OBJECT-Sheets set1_ ))
      (setq set3_ (TA:Excel_Assembly_worksheet_obj_list_ set2_ ))
      (setq Sheets_obj_CE-02_ (caddr (nth 1 set3_)))
      (c:TA:Excel_activate_worksheet_ Sheets_obj_CE-02_ )
    ;
    ;Excel_process
      ;preloop_and_while
        (setq MCK_data_list_ (reverse MCK_data_list_))
        (setq MCK_data_list_i 0)
        (setq row_lv_i 9)
        (while (< MCK_data_list_i (length MCK_data_list_))
          (setq MCK_data_list_ename_ (nth  MCK_data_list_i MCK_data_list_))
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 2 (nth 0 MCK_data_list_ename_) ) ;partname
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 5 (nth 1 MCK_data_list_ename_) ) ;material
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 6 (nth 2 MCK_data_list_ename_) ) ;thk
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 7 (nth 3 MCK_data_list_ename_) ) ;width
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 8 (nth 4 MCK_data_list_ename_) ) ;height
          
          (nth 5 MCK_data_list_ename_)
          (cond ;coating
            (;coating_case_1
               (and
                  (= (car (nth 5 MCK_data_list_ename_)) "CC" )
               )
               (progn
                 (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 9 (cadr (nth 5 MCK_data_list_ename_)) )
                 (princ "_case_1")
               )
            )
            (;_case_2
               (and
                  (= (car (nth 5 MCK_data_list_ename_)) "SP" )
               )
               (progn
                 (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 10 (cadr (nth 5 MCK_data_list_ename_)) )
                 (princ "_case_2")
               )
            )
            (;_case_3
               (and
                  (= (car (nth 5 MCK_data_list_ename_)) "PC" )
               )
               (progn
                 (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 11 (cadr (nth 5 MCK_data_list_ename_)) )
                 (princ "_case_3")
               )
            )
            (;_case_4
               (and
                  (= (car (nth 5 MCK_data_list_ename_)) "ACP" )
               )
               (progn
                 (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 12 (cadr (nth 5 MCK_data_list_ename_)) )
                 (princ "_case_4")
               )
            )
          )
          
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 17 (nth 6 MCK_data_list_ename_) ) ;coating_side
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 18 (nth 7 MCK_data_list_ename_) ) ;total_set
          (TA:Excel_input_data_R1C1 Sheets_obj_CE-02_ row_lv_i 19 "ชุด" ) ;total_set
          
          
          (setq MCK_data_list_i (+ MCK_data_list_i 1))
          (setq row_lv_i (+ row_lv_i 1))
        )
      ;
    ;
    

  )
  
  
  
