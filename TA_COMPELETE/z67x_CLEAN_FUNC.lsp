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
;CLEANER_FUNC Ta Trai
  (defun TA:GET_NAME_BLOCK_IN_DRAWING_TO_LIST ()
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
                                    (cons 67 0)           ;select in model
                                  )
                            )
    )
    ; (sslength all_obj_in_block)
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
                                          (cons 67 0)           ;select in model
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
;test_command
  (defun c:test_ins_vla-getboundingbox ()
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

(defun c:COLOR_RESET_ ()
  (vl-load-com)
  (setq file_block_list_ (TA:get_name_block_in_drawing_to_list))
  
  ;user_input_to_specify_color
    (setq c-main-color nil) 
    (while (not c-main-color)
      (setq c-main-color (getint "Enter a number \nbetween 1 and 256 : \n256 = bylayer \n0 = byblock")) 
      (if (or (< c-main-color 0) (> c-main-color 256) ) 
          (progn
            (princ "\nPlease enter a number between 0 and 255.") 
            (setq c-main-color nil) 
          )
      )
    )
    (cond ;condition_for_color_to_object
      ((and 
          (= c-main-color 0)
        ) 
        (progn 
          (setq c-main-color "byblock")
          (setq dim-main-color 0)
        )
      )
      ((and 
          (= c-main-color 256)
        ) 
        (progn 
          (setq c-main-color "bylayer")
          (setq dim-main-color 256)
        )
      )
      ((and
          (/= c-main-color 0)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
      ((and
          (/= c-main-color 256)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
    )
  ;
    (setq file_block_list_i 0)
    (textscr)
    (while  (< file_block_list_i (length file_block_list_))
      (setq file_block_list_blkname_ (nth file_block_list_i file_block_list_))
      (command "-bedit" file_block_list_blkname_)
      (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
      (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
      (command "_bclose" "s")
      (command "_attsync" "n" file_block_list_blkname_)
      (setq file_block_list_i (+ file_block_list_i 1))
      ;screen process
        (setq percent (fix (* (/ (float file_block_list_i) (length file_block_list_)) 100)))  
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (princ (strcat "tranfering " (rtos file_block_list_i 2 0) "/" (rtos (length file_block_list_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        
      ; 

      
    )
  ;
  ;sub_func_for_model_page
    ; (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
    ; (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
  ;
)
(defun c:Recolor_ ()
  (vl-load-com)
  
  (setq file_block_list_ (ssget 
                                  (list 
                                     (cons 0 "insert")       ;type of object
                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                    ; (cons 2 "SSSS")       ;kind of nameblock
                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                  )
                            )
    )
  ;user_input_to_specify_color
    (setq c-main-color nil) 
    (while (not c-main-color)
      (setq c-main-color (getint "Enter a number \nbetween 1 and 256 : \n256 = bylayer \n0 = byblock")) 
      (if (or (< c-main-color 0) (> c-main-color 256) ) 
          (progn
            (princ "\nPlease enter a number between 0 and 255.") 
            (setq c-main-color nil) 
          )
      )
    )
    (cond ;condition_for_color_to_object
      ((and 
          (= c-main-color 0)
        ) 
        (progn 
          (setq c-main-color "byblock")
          (setq dim-main-color 0)
        )
      )
      ((and 
          (= c-main-color 256)
        ) 
        (progn 
          (setq c-main-color "bylayer")
          (setq dim-main-color 256)
        )
      )
      ((and
          (/= c-main-color 0)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
      ((and
          (/= c-main-color 256)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
    )
  ;
    (setq file_block_list_i 0)
    (while  (< file_block_list_i (sslength file_block_list_))
      (setq file_block_list_blkname_ (ssname  file_block_list_ file_block_list_i))
      (setq file_block_list_blkname_EFname_ (LM:effectivename (vlax-ename->vla-object file_block_list_blkname_)))
      (command "-bedit" file_block_list_blkname_EFname_)
      (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
      (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
      (command "_bclose" "s")
      (command "_attsync" "n" file_block_list_blkname_EFname_)
      (setq file_block_list_i (+ file_block_list_i 1))
      (princ "\n")
      (princ file_block_list_i)
      
    )
  ;
  ;sub_func_for_model_page
    ; (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
    ; (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
  ;
)



(defun c:stamp_layer_to_model ()
  ;user_inpput for specify point
    (setq mk_point (getpoint "specify point"))
    (setq mk_point_y_ 0)
    (setq rotation_val (angtof "45" ))
    (setq layers_list_ (acad_strlsort (TA:get_name_layer_in_drawing)))
    (setq layers_list_i 0)
    (while (< layers_list_i (length layers_list_))
      (setq layers_list_name (nth layers_list_i layers_list_))
      
      (setq mk_point_xyz (vlax-3d-point 
                           (list 
                             (setq mk_point_x (car mk_point))
                             (setq mk_point_y (+ (cadr mk_point) mk_point_y_))
                             (setq mk_point_z (caddr mk_point))
                           )
                         )
      )
      
      (create-text-vla mk_point_xyz layers_list_name 5 rotation_val)
      (vla-put-layer (vlax-ename->vla-object (entlast)) layers_list_name  )
      (vla-put-color (vlax-ename->vla-object (entlast)) 256)
      (setq mk_point_y_ (+ mk_point_y_ -20))
      (setq layers_list_i (+ layers_list_i 1))
      
    )

)
(defun create-text-vla (pt textString height rotation / acadDoc textObj) 
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq textObj (vla-addtext modelSpace textString pt height))
  (vla-put-Height textObj height)
  (vla-put-Rotation textObj rotation)
  textObj
)
(defun c:stamp_prefix_to_layer ()
  ;sub_func_
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
  ;
  ;expect_layer_name_
    (setq expect_list_ (list 
                          "0"
                          "000 - A_XREF"
                          "000 - BUBBLE"
                          "000 - D I M"
                          "000 - D I M 0.25"
                          "000 - GRID"
                          "000 - GROUND"
                          "000 - H A T C H"
                          "000 - H I D D E N"
                          "000 - L I N E"
                          "000 - L I N E 0.1"
                          "000 - SYMBOL CON"
                          "000 - TEMP LAYER"
                          "000 - T E X T"
                          "000 - TITLEBLOCK"
                          "000 - WIPEOUT"
                          "001 - ASCESSORIES"
                          "001 - DOOR"
                          "001 - WINDOWS"
                          "002 - STEEL LRIP"
                          "002 - STEEL TUBE"
                          "002 - STRUCTURAL STEEL"
                          "004 - CONCRETE"
                          "004 - DOOR WINDOW"
                          "004 - FLOOR"
                          "004 - VIVA BOARD"
                          "004 - WALL"
                          "005 - WATER CLOSET EQUIP"
                          "006 - ROOF"
                          "008 - LIGHTING"
                          "A01_ACP"
                          "A02_LITEWOOD"
                          "A03_SUN LOUVER"
                          "A04_TUBE SERIES"
                          "A05_AHP."
                          "A06_PERFORMANCE LOUVERS"
                          "A07_PRANKCAD"
                          "A08_AEROLITE"
                          "A09_BIFOLDING"
                          "A10_ENTRANCE MATT"
                          "A11_ALU.SOLID SHEET"
                          "A12_PERFORATED"
                          "A13_EXPANDED"
                          "Defpoints"
                      )
    )
  ;
  ;get_data
    (setq layer_name_list_ (acad_strlsort (ta:get_name_layer_in_drawing)))
    (setq backup_layer_name_list_ (acad_strlsort (ta:get_name_layer_in_drawing)))
    (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
    (setq layer_obj (vla-get-Layers doc))
  ;
  ;preloop_and_while
    (setq layer_name_list_i 0)
    (while (< layer_name_list_i (length layer_name_list_))
    (setq layer_name_list_strname_ (nth layer_name_list_i layer_name_list_))
    (setq Layer_name_ (vla-item layer_obj layer_name_list_strname_))
    (setq Layer_newname_ (strcat "ZZZ_REF_" layer_name_list_strname_))
      (if 
        (and 
          (= (TA:member layer_name_list_strname_ expect_list_) nil)
        )
        (progn
          ; (vla-put-color Layer_name_ 250 ) ;change color
          (vl-cmdf "_.RENAME" "_LA" layer_name_list_strname_ Layer_newname_)
        )
        (princ "\n")
      )
    (setq layer_name_ (vla-item layer_obj "0"))
    (setq layer_name_list_i (+ layer_name_list_i 1))
    )
  ;
)
(defun c:styfn (/ Oldtstyle Sttxt Userfont error)
  (defun error (s)
    (setvar "textstyle" oldtstyle)
  )
  (setq oldtstyle (getvar "textstyle"))
  (setq userfont "cordia new") ;"arial.ttf" <<<<change this for your textfont
  (setvar "textstyle" (cdr (assoc 2 (tblnext "style" t))))
  (command "._Style" "" userfont 2 1 0 "N" "N")
  (while
    (setq sttxt (cdr (assoc 2 (tblnext "style"))))
    (setvar "textstyle" sttxt)
    (command "._Style" "" userfont 2 1 0 "N" "N")
  )
  (setvar "textstyle" oldtstyle)
  (princ)
)
