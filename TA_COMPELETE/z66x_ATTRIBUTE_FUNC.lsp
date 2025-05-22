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
  )
;
;Sorting Coordinate Function Ta Trai
  (defun CO:sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car (cadr a)) (car (cadr b)))))))
  )
  (defun CO:sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))))
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
;
;Get_Data Function Ta Trai
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
  (defun TA:midpoint (ins_start_ ins_end_) ;คำนวณ midpoint จากระยะหัว-ท้าย ของเส้น
    ; (setq ins_start_ (list ins_start_x1 ins_start_y1))
    ; (setq ins_end_ (list ins_end_x1 ins_end_y1))
    (setq ins_mid_ (list 
                 (/ (+ (car ins_start_) (car ins_end_)) 2.0)
                 (/ (+ (cadr ins_start_) (cadr ins_end_)) 2.0)
               )
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


(defun c:Matt_match_attribute_z66x ( )
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
  ;filter_and_sorting_selection
    (setq _set_ (TA:Filter_ss_set_ ss_pre_filter_set_xx get_effectivename_obj_val_))
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

(defun c:Matt_single_match_attribute_z672 ( )
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

  (if (/= ss_pre_filter_set_xx_ nil)
    (progn
      ;preloop_and_while input_att_val_to ss_pre_filter_set_xx_
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          ; (setq REF_get_att_list_data_tag (car (nth REF_get_att_list_i REF_get_att_list_ )))
          (setq ss_pre_filter_set_xx_obj  (vlax-ename->vla-object (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i )))
            (setq ref_att_tag_i 0)
            (while (< ref_att_tag_i (length (LM:vl-getattributevalue-tag-TA-Modifies ss_pre_filter_set_xx_obj)))
              ;get_att_data_from_ REF_get_att_list_
                (setq REF_get_att_list_tagname (car (nth ref_att_tag_i REF_get_att_list_)))
                (setq REF_get_att_list_val (cdr (nth ref_att_tag_i REF_get_att_list_)))
              ;
              ;set_att_data_to_ ss_pre_filter_set_xx_
                (LM:vl-setattributevalue ss_pre_filter_set_xx_obj REF_get_att_list_tagname REF_get_att_list_val)
              ;
              (setq ref_att_tag_i (+ ref_att_tag_i 1))
            )
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    )
    (alert "Attribute Block do not match\nPlease try agian")
    
  )
)


(defun c:Z661_Match_att_multi_block_ ()
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
                           (/= ref_get_att_list_attname_ "BLANK_SIZE_VALUE")
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

(defun c:z662_input_blanksize_value_ ()
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
    (setq _set_ (TA:Filter_ss_set_ ss_pre_filter_set_xx "001 - PART DATA CUSTOM 2025"))
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

(defun c:Z663_Match_att_data_ ()
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
                           (/= ref_get_att_list_attname_ "BLANK_SIZE_VALUE")
                           (/= ref_get_att_list_attname_ "NAME_TITLE_1")
                           (/= ref_get_att_list_attname_ "NAME_TITLE_2")
                           (/= ref_get_att_list_attname_ "NAME_TITLE_3")
                           (/= ref_get_att_list_attname_ "NAME_TITLE_3")
                           (/= ref_get_att_list_attname_ "DRAWING_NO1")
                           (/= ref_get_att_list_attname_ "DRAWING_NO2")
                           (/= ref_get_att_list_attname_ "SHEET_NO.")
                           (/= ref_get_att_list_attname_ "DRAWING_NO2")
                           (/= ref_get_att_list_attname_ "TOTAL_SHEET_NO.")
                           (/= ref_get_att_list_attname_ "DRAWING_NO2_NM")
                           (/= ref_get_att_list_attname_ "TOTAL_SHEET_NO._NM")
                           (/= ref_get_att_list_attname_ "PRELIM_NAME_TITLE_1")
                           (/= ref_get_att_list_attname_ "PRELIM_NAME_TITLE_2")
                           (/= ref_get_att_list_attname_ "PRELIM_NAME_TITLE_3")
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
      
        ;especially_case
          (setq CODE_VALUE (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "CODE_VALUE") )
          (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "NAME_TITLE_1" CODE_VALUE  )
          (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "NAME_TITLE_2" "PLAN VIEW FRONT VIEW SIDE VIEW BACK SIDE"  )
        ;
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)







