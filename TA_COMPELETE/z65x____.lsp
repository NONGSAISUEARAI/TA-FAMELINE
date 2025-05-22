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
  (defun LM:vl-getattributevalue-head-TA-Modifies (blk) 
    (mapcar '(lambda (att) (vla-get-tagstring att)) 
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




(defun LM:RemoveNth (n l / i) 
  ;;  Arguments:                                              ;;
  ;;  n - index of item to remove (zero based)                ;;
  ;;  l - list from which item is to be removed               ;;
    (setq i -1)
    (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
)
(defun CO:sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
  (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car (cadr a)) (car (cadr b)))))))
)
(defun CO:sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
  (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))))
)
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
(defun TA:ename+vla-get-insertionpoint (obj) ;ประกอบ entity_name กับ insertion_poiint ให้เป็น list
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



(defun c:s1ss_select_group_similar ()
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
  ;user_input
    (setq effectivename_val (LM:effectivename (vlax-ename->vla-object (car (entsel "\nSpecify name blk for filter\n" )))))
  ;
  ;get_data_via_sub_func
    (setq new_ss_ename_set_ (TA:Filter_ss_set_ ss_pre_filter_set_xx  effectivename_val))
    (command "pselect" new_ss_ename_set_)
    (princ (strcat "Total Select " "''" effectivename_val "''" " = " (rtos (sslength new_ss_ename_set_) 2 0) " blocks"))
  ;
)

(setq aa (list "a" "b" "s" "ss"))
  
  (LM:RemoveNth 3 aa)
  
  
(defun c:z651_replace_blk_to_ins ()
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
  ;user_input
    (setq effectivename_val (LM:effectivename (vlax-ename->vla-object (car (entsel "\nSpecify name blk for filter\n" )))))
  ;
  ;filter_obj'
    (setq new_filter (TA:Filter_ss_set_ ss_pre_filter_set_xx effectivename_val))
    (sslength new_filter)
  ;
  ;reset_coordiante
    (setq new_filter_sorted_ (TA:standard_list_croodinate_sorting new_filter "X"))
  ;
  ;user_input
    (setq effectivename_replace_val (LM:effectivename (vlax-ename->vla-object (car (entsel "\nSpecify name blk for replace\n" )))))
  ;
  ;preloop_and_while_replace_block_and_delete_ex_blk
    (setq new_filter_sorted_i 0)
    (while (< new_filter_sorted_i (length new_filter_sorted_))
      (setq new_filter_sorted_ename (car (nth new_filter_sorted_i new_filter_sorted_) ))
      (setq new_filter_sorted_obj (vlax-ename->vla-object new_filter_sorted_ename))
      (setq new_filter_sorted_ins (cadr (nth new_filter_sorted_i new_filter_sorted_) ))
      (command "insert" effectivename_replace_val new_filter_sorted_ins 1 180)
      (vla-erase new_filter_sorted_obj)

      (setq new_filter_sorted_i (+ new_filter_sorted_i 1))
    )
  
  ;
)



;test_command
  (defun c:test_ins_vla-getboundingbox ()
    (setq ss (vlax-ename->vla-object (car (entsel))) )


  (vla-getboundingbox ss 'ins_min 'ins_max)
              (setq ins_min (vlax-safearray->list ins_min))
              (setq ins_max (vlax-safearray->list ins_max))

  (command "point" ins_min)
  (command "point" ins_max)
  )
;
