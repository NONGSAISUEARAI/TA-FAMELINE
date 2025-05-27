;ALL  20250527_1115
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
      (defun LM:getdynprops_nonvalue ( blk )
          (mapcar '(lambda ( x ) (vla-get-propertyname x))
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
    (defun TA:corrd_sorting_group (all_vlains_ Toerrance_num_value)
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
    (defun LM:intersectionsinset ( sel / id1 id2 ob1 ob2 rtn )
      (repeat (setq id1 (sslength sel))
          (setq ob1 (vlax-ename->vla-object (ssname sel (setq id1 (1- id1)))))
          (repeat (setq id2 id1)
              (setq ob2 (vlax-ename->vla-object (ssname sel (setq id2 (1- id2))))
                    rtn (cons (LM:intersections ob1 ob2 acextendnone) rtn)
              )
          )
      )
      (apply 'append (reverse rtn))
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
    (defun TA:ss_vla-getboundingbox (sss_)
      (setq sss_ (ssget))
      (cond
        (;type_case_1
            (and
              (= (type sss_) 'PICKSET)
            )
            (progn
              ;preloop_and_while
                (setq sss_i 0)
                (setq sss_obj_getboundingbox_list_ nil)
                (while (< sss_i (sslength sss_))
                  (setq sss_ename_ (ssname sss_ sss_i))
                  (setq sss_obj_ (vlax-ename->vla-object sss_ename_))
                  (setq sss_obj_getboundingbox (vla-getboundingbox sss_obj_ 'min_ 'max_)) 
                  (setq sss_min_ (vlax-safearray->list min_)) 
                  (setq sss_max_ (vlax-safearray->list max_))
                  (setq sss_obj_getboundingbox_list_ (cons (list sss_min_ sss_max_) sss_obj_getboundingbox_list_))      
                  (setq sss_i (+ sss_i 1))
                )
                ;TA:stanndard_lambda_sorting
                  (setq low_xy_ (list 
                                  (setq low_x (car 
                                                (car 
                                                  (car 
                                                    (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                            (function 
                                                              (lambda (a b) 
                                                                (< (nth 0 (car a)) 
                                                                    (nth 0 (car b))
                                                                )
                                                              )
                                                            )
                                                    )
                                                  )
                                                )
                                              ) ;bigest close indent list
                                  )
                                  (setq low_y (cadr 
                                                (car 
                                                  (car 
                                                    (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                            (function 
                                                              (lambda (a b) 
                                                                (< (nth 1 (car a)) 
                                                                    (nth 1 (car b))
                                                                )
                                                              )
                                                            )
                                                    )
                                                  )
                                                )
                                              ) ;bigest close indent list
                                  )
                                )
                  )
                  (setq high_xy_ (list 
                                  (setq high_x (car 
                                                  (cadr 
                                                    (car 
                                                      (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                              (function 
                                                                (lambda (a b) 
                                                                  (> (nth 0 (cadr a)) 
                                                                      (nth 0 (cadr b))
                                                                  )
                                                                )
                                                              )
                                                      )
                                                    )
                                                  )
                                                ) ;bigest close indent list
                                  )
                                  (setq high_y (cadr 
                                                  (cadr 
                                                    (car 
                                                      (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                              (function 
                                                                (lambda (a b) 
                                                                  (> (nth 1 (cadr a)) 
                                                                      (nth 1 (cadr b))
                                                                  )
                                                                )
                                                              )
                                                      )
                                                    )
                                                  )
                                                ) ;bigest close indent list
                                  )
                                )
                  )
                ;
                (list low_xy_ high_xy_  )
                
              ;
              ; (princ "type_case_1")
            )
        )
        (;type_case_2
          (and
            (= (type sss_) 'LIST)
          )
          (progn
            (progn
              ;preloop_and_while
                (setq sss_i 0)
                (setq sss_obj_getboundingbox_list_ nil)
                (while (< sss_i (length sss_))
                  (setq sss_ename_ (nth sss_i sss_ ))
                  (setq sss_obj_ (vlax-ename->vla-object sss_ename_))
                  (setq sss_obj_getboundingbox (vla-getboundingbox sss_obj_ 'min_ 'max_)) 
                  (setq sss_min_ (vlax-safearray->list min_)) 
                  (setq sss_max_ (vlax-safearray->list max_))
                  (setq sss_obj_getboundingbox_list_ (cons (list sss_min_ sss_max_) sss_obj_getboundingbox_list_))      
                  (setq sss_i (+ sss_i 1))
                )
                ;TA:stanndard_lambda_sorting
                  (setq low_xy_ (list 
                                  (setq low_x (car 
                                                (car 
                                                  (car 
                                                    (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                            (function 
                                                              (lambda (a b) 
                                                                (< (nth 0 (car a)) 
                                                                    (nth 0 (car b))
                                                                )
                                                              )
                                                            )
                                                    )
                                                  )
                                                )
                                              ) ;bigest close indent list
                                  )
                                  (setq low_y (cadr 
                                                (car 
                                                  (car 
                                                    (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                            (function 
                                                              (lambda (a b) 
                                                                (< (nth 0 (cadr a)) 
                                                                    (nth 0 (cadr b))
                                                                )
                                                              )
                                                            )
                                                    )
                                                  )
                                                )
                                              ) ;bigest close indent list
                                  )
                                )
                  )
                  (setq high_xy_ (list 
                                  (setq high_x (car 
                                                  (cadr 
                                                    (car 
                                                      (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                              (function 
                                                                (lambda (a b) 
                                                                  (> (nth 0 (car a)) 
                                                                      (nth 0 (car b))
                                                                  )
                                                                )
                                                              )
                                                      )
                                                    )
                                                  )
                                                ) ;bigest close indent list
                                  )
                                  (setq high_y (cadr 
                                                  (cadr 
                                                    (car 
                                                      (vl-sort sss_obj_getboundingbox_list_  ;bigest open indent list
                                                              (function 
                                                                (lambda (a b) 
                                                                  (> (nth 0 (cadr a)) 
                                                                      (nth 0 (cadr b))
                                                                  )
                                                                )
                                                              )
                                                      )
                                                    )
                                                  )
                                                ) ;bigest close indent list
                                  )
                                )
                  )
                ;
                (list low_xy_ high_xy_  )
              ;
              ; (princ "type_case_1")
            ) 
          )
        )
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
    (defun TA:ModifyRotation (Ex_rotation New_rotation)
      ;Note By Code_Developer
      ;The principle of this code is designed to modify the angle value, allowing for both increments and decrements based on a reference number.
      ;The reference number used must be a value representing degrees, ranging from 1 to 360.
      ;The number added or subtracted from the angle is calculated based on the number of degrees rotated, considering 1 full rotation as 360 degrees.
      ;
      (cond 
        ( ;normal_rotation_case_1
          (and 
            (>= New_rotation 0)
            (<= New_rotation 360)
          )
          (progn
            (princ "rotation_case_1\n")
            (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
            (if (<= new_rotaion_val_ 360)
              (progn
                (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
              )
              (setq new_rotaion_val_ (- (+ Ex_rotation New_rotation) 360))
            )
            
            
          )
        )
        (;multi_time_rotation_case_2
          (and 
            (> New_rotation 360)
          )
          (progn 
            (princ "rotation_case_2\n")
            (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
            (setq round_time_val (* round_time_ 360))
            (setq new_rotaion_val_ (- New_rotation round_time_val))
            (if (> (+ Ex_rotation new_rotaion_val_ ) 360)
            (progn
              (setq new_rotaion_val_ans_ (-  (+ Ex_rotation new_rotaion_val_ ) 360 ))
            )
              (setq new_rotaion_val_ans_ (+ Ex_rotation new_rotaion_val_ ))
            )
            
          )
        )
        (;rotation_case_3
          (and 
            (<= New_rotation 0) 
          )
          (progn 
            (princ "rotation_case_3\n")
            (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
            (setq round_time_val (* round_time_ 360))
            (setq new_rotaion_val_ (- New_rotation round_time_val))
            (setq Ex_rotation_val_ (+ Ex_rotation new_rotaion_val_))
            (if (< Ex_rotation_val_ 0)
              (progn
                (setq new_rotaion_val_ans_ (+ 360 Ex_rotation_val_))
              )
              (setq new_rotaion_val_ans_ Ex_rotation_val_ )
            )
            
          )
        )
        
      )
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
  ;text_convert_
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
      (setq doc (vla-get-ActiveDocument acadApp))
      (if (= (getvar "ctab" ) "Model")
        (progn
          (setq ms (vla-get-ModelSpace doc))
        )
        (setq ms (vla-get-paperspace doc))
      )
        ; เข้าถึง Active Document
      
        ; เข้าถึง ModelSpace
      
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
      (setq visibility_copy_mode_                     "6 = Hex bolt ")
      
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
          (;visibility_mode_case_6
            (and
                (= visibility_mode_value_ 6)
            )
            (progn
                (setq visibility_list_ (list
                                        "top_view"
                                        "hex-nut"
                                        "x8mm."
                                        "x10mm."
                                        "x12mm."
                                        "x14mm."
                                        "x15mm."
                                        "x20mm."
                                        "x25mm."
                                        "x30mm."
                                        "x40mm."
                                        "x45mm."
                                        "x50mm."
                                        "x55mm."
                                        "x60mm."
                                        "x65mm."
                                        "x70mm."
                                        "x75mm."
                                        "x80mm."
                                        "x90mm."
                                        "x100mm."
                                      )
                )
                (princ "visibility_mode_case_6")
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
      ; ;reture_visibility_to_defuat
        (command "-Bvstate" "s" "VisibilityState0"  )
        (command "BVHIDE" (ssget "x") "" "A" )
        (setvar "BVMODE" 1)
        (command "BVSHOW" (ssget "x") "" "C" )
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
    (defun c:DynBlk_make_thread-bolt_mktbolt ()
      (setq visibility_list_              
        (list 
          "top_view"
          "hex-nut" 
          "x8mm." 
          "x10mm."
          "x12mm." 
          "x14mm." 
          "x15mm." 
          "x20mm." 
          "x25mm." 
          "x30mm." 
          "x40mm." 
          "x45mm." 
          "x50mm." 
          "x55mm." 
          "x60mm." 
          "x65mm." 
          "x70mm." 
          "x75mm." 
          "x80mm." 
          "x90mm." 
          "x100mm." 
        )
      )
      ;preloop_and_while
        (setq visibility_list_i 0)
        (while (< visibility_list_i (length visibility_list_))
          (setq visibility_list_ename_ (nth  visibility_list_i visibility_list_))
          (if (wcmatch visibility_list_ename_ "*mm.*")
            (progn
              (command "bvstate" "s" visibility_list_ename_ )
              (command "insert" "000 - PROTOTYPE-DYNAMIC_THREAD_BOLT" (list 0 0 0) 1 270 )
              ;get_new_entlast ename
                (setq new_blk_ename_ (entlast))
                (setq new_blk_obj_ (vlax-ename->vla-object new_blk_ename_))
              ;
              ;dyn_object_
                (LM:setdynpropvalue new_blk_obj_ "thread_dia."  5)
                (LM:setdynpropvalue new_blk_obj_ "array_width" (LM:StringSubst "" "mm." (LM:StringSubst "" "x" visibility_list_ename_ )) )
              ;
            )
          )
          (setq visibility_list_i (+ visibility_list_i 1))
        )
      ;
      
      
    )

    (if (= 1 2) ;vvvvvv
      (progn
        (setq ss_ename_ (car (entsel "specify Object")))
        (setq ss_obj_ (vlax-ename->vla-object ss_ename_))
        (TA:Assembly_des-name-block_ ss_obj_)
        
      )
    )
    
  ;
  ;dynamic_block_FUNC
    ;dynamic_Alminium_sheet_FUNC
      (defun c:dynamic_update_U-Carrier_[UCupdate] ()
        
        ;user_input get_effectivename 
          (setq get_effectivename_ nil)
          ; (setq get_effectivename_lasted nil)
          (setq get_effectivename_obj_val_ nil)
        ;
        ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Attribute Block\n"))))   
            (if (/= get_effectivename_ nil)
              (progn
                (setq get_effectivename_lasted get_effectivename_)
              )
            )          
                      
            (if 
              (and ;condition rule
                
                (setq B (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object (if (= get_effectivename_ nil) (progn get_effectivename_lasted ) get_effectivename_ )))) 
                          "AcDbBlockReference"
                        )
                )
                ; (= (substr (LM:Effectivename (vlax-ename->vla-object get_effectivename_)) 1 72) "000 - DYNAMIC_ARRAY_ASSCESSORIES_FAMELINE_AL._U45-CARRIER_BRACKET_FITTING")
                (setq C 
                        (= 
                          (wcmatch
                          (LM:Effectivename (vlax-ename->vla-object (if (= get_effectivename_ nil) (progn get_effectivename_lasted ) get_effectivename_ ))) 
                          "*000 - DYNAMIC_ARRAY_ASSCESSORIES_FAMELINE_AL._U45-CARRIER_BRACKET_FITTING*"
                          )
                        )
                )
                ; (= (vla-get-hasattributes (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                (setq D (= (vla-get-isdynamicblock (vlax-ename->vla-object (if (= get_effectivename_ nil) (progn get_effectivename_lasted ) get_effectivename_ )))
                          :vlax-true
                        )
                )
              )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                ;get_dynamic_data
                  (setq get_effectivename_ get_effectivename_lasted)
                  (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                  (setq array_width (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "array_width" ))
                  (setq total_u-profile_depth (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "total_u-profile_depth" ))
                  (setq u-profile_width_set (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "u-profile_width_set" ))
                  
                  ; (setq louver_angle_ (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_angle" )) ;ปิดชั่วคราวเปลี่ยนเป็น user input 
                  (setq louver_angle_deg (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq louver_angle_deg (cond (louver_angle_deg) (1.2) ) ) ) "> : " ) ) ) (louver_angle_deg) ) ) ;
                  (setq louver_angle_ (deg-to-rad louver_angle_deg) )

                  ; (setq louver_width_(LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_width" )) ;ปิดชั่วคราวเปลี่ยนเป็น user input 
                  (setq louver_width_ (cond ( (getreal (strcat "\nspecify louver_width_<" (rtos (setq louver_width_ (cond (louver_width_) (10) ) ) ) "> : " ) ) ) (louver_width_) ) )
                  (setq u-profile_width_set (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "u-profile_width_set"  ))
                  (setq louver_depth_start_angle_ (deg-to-rad 90))
                  (setq louver_depth_end_angle_ (deg-to-rad (- 180 (+ 90 (rad-to-deg louver_angle_)  ))))
                
                  (setq position_1Y (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "Position1 Y" ))
                ;
                ;reset_block_process_
                  (command "resetblock" get_effectivename_ "" )
                  (setq u-profile_width_set (LM:getdynpropvalue (vlax-ename->vla-object get_effectivename_) "array_width" ))
                  
                ;
                ;trigonmetry_cal
                  ;find_ louver_depth_ formula
                    ;
                      (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_depth" 
                        (if (>= louver_angle_deg 90)
                          (progn
                            louver_width_
                          )
                          (/
                            (* 
                              (sin louver_angle_)
                              louver_width_
                            ) 
                            (sin louver_depth_end_angle_)
                          )
                        )
                      )
                    ;
                  ;
                  ;find_ base_trigon_ formula
                    ;
                      (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "base_trigon" 
                        (if (>= louver_angle_deg 90)
                          (progn
                            louver_width_
                          )
                          (/
                            (* 
                              (sin louver_depth_start_angle_)
                              louver_width_
                            ) 
                            (sin louver_depth_end_angle_)
                          )
                        )
                      )
                    ;
                  ;

                ;
                ;louver_depth_90deg
                  ;
                    (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_depth_90deg" 
                      (if (>= louver_angle_deg 90)
                        (progn
                          (setq louver_depth (cond ( (getreal (strcat "\nspecify louver_depth<" (rtos (setq louver_depth (cond (louver_depth) (1.2) ) ) ) "> : " ) ) ) (louver_depth) ) )
                        )
                        0
                      )
                    )
                  ;
                ;
                ;return_input_data_to_parameter_
                  (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_angle" louver_angle_)
                  
                  ; (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "position1 X" 0)
                  ; (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "position1 Y" position_1Y)
                  
                  (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "louver_width" 
                    (if (>= louver_angle_deg 90)
                      (progn
                        louver_depth
                      )
                      louver_width_
                    )
                  )
                  
                  (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "u-profile_width_set" u-profile_width_set)
                  (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "array_width" array_width)
                  (LM:setdynpropvalue (vlax-ename->vla-object get_effectivename_) "total_u-profile_depth" total_u-profile_depth)
                
                ;
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
        ;
      )
    ;
  ;
  ;hatch_FUNC
    (defun TA:create_hatch_ (hatch_ename_ hatch_ename_getang_ hatch_ename_getsc_)
      ;example_user_input_
        ; (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
        ; (setq hatch_ename_ (car (entsel "specify Object")))
      ;
      ;get_object_name_data
        (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object hatch_ename_)))
      ;
      ;cond for filter objectname_
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
      ;
      
      ;pre_set_vari_
        ; (if 
        ;   (and 
        ;     (= hatch_ename_getang_ nil)
        ;     (= hatch_ename_getsc_ nil)
        ;   )
        ;   (progn
        ;     (setq  hatch_ename_getang_ 0)
        ;     (setq  hatch_ename_getsc_ 1)
            
        ;   )
        ; )
      ;
      ;presetting_hatch_data_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_patt_ "ANSI32")
        (setq hatch_color_ 8)
        (setq hatch_background_ 250)
      ;
      ; (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
      ; (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
      (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))

      
      (command ".-hatch" "s"  hatch_ename_  
                "" 
                "c" hatch_color_ hatch_background_
                "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
                "L" hatch_Layer_ 
                ""  
               ""
      )
      
      
    )
    (defun TA:create_hatch_ANSI37 (hatch_ename_ hatch_ename_getang_ hatch_ename_getsc_)
      ;example_user_input_
        ; (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
        ; (setq hatch_ename_ (car (entsel "specify Object")))
      ;
      ;get_object_name_data
        (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object hatch_ename_)))
      ;
      ;cond for filter objectname_
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
      ;
      
      ;pre_set_vari_
        ; (if 
        ;   (and 
        ;     (= hatch_ename_getang_ nil)
        ;     (= hatch_ename_getsc_ nil)
        ;   )
        ;   (progn
        ;     (setq  hatch_ename_getang_ 0)
        ;     (setq  hatch_ename_getsc_ 1)
            
        ;   )
        ; )
      ;
      ;presetting_hatch_data_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_patt_ "ANSI37")
        (setq hatch_color_ 8)
        (setq hatch_background_ 250)
      ;
      ; (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
      ; (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )

      (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))
      
      
      (command ".-hatch" "s"  hatch_ename_  
                "" 
                "c" hatch_color_ hatch_background_
                "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
                "L" hatch_Layer_ 
                ""  
                ""
      )
      
      
      
    )
    (defun TAS:create_hatch_PPLINE_ (hatch_ename_ hatch_ename_getang_ hatch_ename_getsc_ hatch_mode)
      ;example_user_input_
        ; (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
        ; (setq hatch_ename_ (car (entsel "specify Object")))
      ;
      ;get_object_name_data
        (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object hatch_ename_)))
      ;
      ;cond for filter objectname_
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
      ;
      
      ;pre_set_vari_
        ; (if 
        ;   (and 
        ;     (= hatch_ename_getang_ nil)
        ;     (= hatch_ename_getsc_ nil)
        ;   )
        ;   (progn
        ;     (setq  hatch_ename_getang_ 0)
        ;     (setq  hatch_ename_getsc_ 1)
            
        ;   )
        ; )
      ;
      ;presetting_hatch_data_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_patt_
          (cond 
            ( ;hatctpatt_case_
              (and (= hatch_mode 1))
              (progn 
                (setq hatch_patt_ "000_C01")
                (setq hatch_color_ 8)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 2))
              (progn 
                (setq hatch_patt_ "000_C02")
                (setq hatch_color_ 2)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 3))
              (progn 
                (setq hatch_patt_ "000_C03")
                (setq hatch_color_ 2)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 4))
              (progn 
                (setq hatch_patt_ "000_C04")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 5))
              (progn 
                (setq hatch_patt_ "000_C05")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 51))
              (progn 
                (setq hatch_patt_ "000_C05A")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 6))
              (progn 
                (setq hatch_patt_ "000_C06")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 7))
              (progn 
                (setq hatch_patt_ "000_C07")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 8))
              (progn 
                (setq hatch_patt_ "000_C08")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 9))
              (progn 
                (setq hatch_patt_ "000_C09")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 10))
              (progn 
                (setq hatch_patt_ "000_C10")
                (setq hatch_color_ 4)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 11))
              (progn 
                (setq hatch_patt_ "000_C11")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
          )
        )
        
        (setq hatch_background_ 250)
      ;
      ; (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
      ; (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )

      (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))
      
      
      (command ".-hatch" "s"  hatch_ename_  
                "" 
                "c" hatch_color_ hatch_background_
                "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
                "L" hatch_Layer_ 
                ""  
                ""
      )
      
      
      
    )
    (defun TAS:create_Wallhatch_PPLINE_ (hatch_ename_ hatch_ename_getang_ hatch_ename_getsc_ hatch_mode)
      ;example_user_input_
        ; (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object (setq hatch_ename_ (car (entsel "specify Object"))))))
        ; (setq hatch_ename_ (car (entsel "specify Object")))
      ;
      ;get_object_name_data
        (setq obj_name_ (vla-get-objectname (vlax-ename->vla-object hatch_ename_)))
      ;
      ;cond for filter objectname_
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
      ;
      
      ;pre_set_vari_
        ; (if 
        ;   (and 
        ;     (= hatch_ename_getang_ nil)
        ;     (= hatch_ename_getsc_ nil)
        ;   )
        ;   (progn
        ;     (setq  hatch_ename_getang_ 0)
        ;     (setq  hatch_ename_getsc_ 1)
            
        ;   )
        ; )
      ;
      ;presetting_hatch_data_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_patt_
          (cond 
            ( ;hatctpatt_case_
              (and (= hatch_mode 1))
              (progn 
                (setq hatch_patt_ "000_W01")
                (setq hatch_color_ 8)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 2))
              (progn 
                (setq hatch_patt_ "000_W02")
                (setq hatch_color_ 2)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 3))
              (progn 
                (setq hatch_patt_ "000_W03")
                (setq hatch_color_ 2)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 4))
              (progn 
                (setq hatch_patt_ "000_W04")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 5))
              (progn 
                (setq hatch_patt_ "000_W05")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 6))
              (progn 
                (setq hatch_patt_ "000_W06")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 7))
              (progn 
                (setq hatch_patt_ "000_W07")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 8))
              (progn 
                (setq hatch_patt_ "000_W08")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 9))
              (progn 
                (setq hatch_patt_ "000_W09")
                (setq hatch_color_ 1)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 10))
              (progn 
                (setq hatch_patt_ "000_W10")
                (setq hatch_color_ 4)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 11))
              (progn 
                (setq hatch_patt_ "000_W11")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 12))
              (progn 
                (setq hatch_patt_ "000_W12")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 13))
              (progn 
                (setq hatch_patt_ "000_W13")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 14))
              (progn 
                (setq hatch_patt_ "000_W14")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 15))
              (progn 
                (setq hatch_patt_ "000_W15")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 16))
              (progn 
                (setq hatch_patt_ "000_W16")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 17))
              (progn 
                (setq hatch_patt_ "000_W17")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 18))
              (progn 
                (setq hatch_patt_ "000_W18")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 19))
              (progn 
                (setq hatch_patt_ "000_W19")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 20))
              (progn 
                (setq hatch_patt_ "000_W20")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 21))
              (progn 
                (setq hatch_patt_ "000_W21")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 22))
              (progn 
                (setq hatch_patt_ "000_W22")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 23))
              (progn 
                (setq hatch_patt_ "000_W23")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 24))
              (progn 
                (setq hatch_patt_ "000_W24")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 25))
              (progn 
                (setq hatch_patt_ "000_W25")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 26))
              (progn 
                (setq hatch_patt_ "000_W26")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 27))
              (progn 
                (setq hatch_patt_ "000_W27")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 28))
              (progn 
                (setq hatch_patt_ "000_W28")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 29))
              (progn 
                (setq hatch_patt_ "000_W29")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
            ( ;hatctpatt_case_
              (and (= hatch_mode 30))
              (progn 
                (setq hatch_patt_ "000_W30")
                (setq hatch_color_ 3)
                (princ hatch_patt_)
              )
            )
          )
        )
        
        (setq hatch_background_ 250)
      ;
      ; (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
      ; (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )

      (setq hatch_obj_ (vlax-ename->vla-object hatch_ename_))
      
      
      (command ".-hatch" "s"  hatch_ename_  
                "" 
                "c" hatch_color_ hatch_background_
                "p" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_ 
                "L" hatch_Layer_ 
                ""  
                ""
      )
      
      
      
    )
    (defun c:hatch_obj_Metal_HHM_ ()
      ;user_input_
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
    (defun c:hatch_obj_Solid_HHSolid_ ()
      ;user_input_
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
        (setq hatch_color_ 250)
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
      (vla-setpattern (setq s_obj_ (vlax-ename->vla-object (entlast))) acHatchPatternTypePreDefined "solid") 
    )
    (defun c:hatch_obj_Dot_HHCON_ ()
      ;user_input_
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
        (setq hatch_patt_ "AR-CONC")
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
    (defun c:hatch_obj_Dot_HHD_ ()
      ;user_input_
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
    (defun c:multi_hatch_obj_MHB ()
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
      ;user_input_data_
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
        (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
        (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))

            (TA:create_hatch_ ss_pre_filter_set_xx_ename_ hatch_ename_getang_ hatch_ename_getsc_)  

          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    )
    (defun c:multi_hatch_obj_MHB2 ()
      ;hatch_setting_value_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_patt_ (getstring (strcat "\nSpecify Pattern name <" (if hatch_patt_ hatch_patt_ "") ">: ") ) )
        (setq hatch_patt_ (if (/= hatch_patt_ "") (progn (setq hatch_patt_ hatch_patt_) (setq hatch_patt_pre hatch_patt_) ) (setq hatch_patt_ hatch_patt_pre ) ))
        (setq hatch_color_ 8)
        (setq hatch_background_ 250)
      ;
      ; ;selection_set
      ;   (if  ;pre_select_ssget_or_post_select_ssget
      ;     (= 
      ;       (setq ss_pre_filter_set_xx_ (ssget "I" 
      ;                                         (list 
      ;                                           (cons 0 "lwpolyline") ;type of object
      ;                                           ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                           ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
      ;                                           ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                         )
      ;                                   )
      ;       )
      ;       nil
      ;     )
      ;     (progn 
      ;       (setq ss_pre_filter_set_xx_ (ssget 
      ;                                     (list 
      ;                                       (cons 0 "lwpolyline") ;type of object
      ;                                       ; (cons 8 "000 - GRID")   ;kind of layer
      ;                                       ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
      ;                                       ; (cons 62 1)           ;kind of color call sign with color code index
      ;                                     )
      ;                                   )
      ;       )
      ;     )
      ;     (sslength ss_pre_filter_set_xx_)
      ;   )
      ; ;
      ;user_input_data_
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
        (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
        (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
      ;
      ;preloop_and_while
        ; (setq ss_pre_filter_set_xx_i 0)
        ; (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        ;   (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))

            (command "_-hatch" pause
            )  


            ;new_blk_entlast object
              (setq newhatch_ename (entlast))
              (setq newhatch_obj_ (vlax-ename->vla-object newhatch_ename))
              (command "_-hatchedit" newhatch_ename "P" hatch_patt_ hatch_ename_getsc_ hatch_ename_getang_   )
              (command "_-hatchedit" newhatch_ename "Co" hatch_color_ hatch_background_  )
            ;

              

        ;   (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        ; )
      ;
    )
    (defun c:multi_hatch_obj_MHB3 ()
      ;hatch_setting_value_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_mode (cond ( (getint (strcat "\nspecify hatch_mode <" (rtos (setq hatch_mode (cond (hatch_mode) (1.0) ) ) ) "> : " ) ) ) (hatch_mode) ) )       
        (setq hatch_color_ 8)
        (setq hatch_background_ 250)
      ;
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
      ;user_input_data_
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
         
        (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
        (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))

            (TAS:create_hatch_PPLINE_ ss_pre_filter_set_xx_ename_ hatch_ename_getang_ hatch_ename_getsc_ hatch_mode)    

          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    )
    (defun c:multi_hatch_obj_MHB4 ()
      
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
      ;hatch_setting_value_
        (setq hatch_Layer_ "000 - H A T C H")
        (setq hatch_mode (cond ( (getint (strcat "\nspecify hatch_mode <" (rtos (setq hatch_mode (cond (hatch_mode) (1.0) ) ) ) "> : " ) ) ) (hatch_mode) ) )       
        (setq hatch_color_ 8)
        (setq hatch_background_ 250)
      ;
      ;user_input_data_
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
         
        (setq hatch_ename_getang_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq hatch_ename_getang_ (cond (hatch_ename_getang_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getang_) ) )
        (setq hatch_ename_getsc_ (cond ( (getreal (strcat "\nspecify scale<" (rtos (setq hatch_ename_getsc_ (cond (hatch_ename_getsc_) (hatch_ename_getsc_) ) ) ) "> : " ) ) ) (hatch_ename_getsc_) ) )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))

            (TAS:create_Wallhatch_PPLINE_ ss_pre_filter_set_xx_ename_ hatch_ename_getang_ hatch_ename_getsc_ hatch_mode)    

          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    )
    (defun c:select_hacth_SFH ()
      ;selection_set
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq ss_pre_filter_set_xx_ (ssget "I" 
                                              (list 
                                                (cons 0 "hatch") ;type of object
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
                                            (cons 0 "hatch") ;type of object
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
      (command "pselect" ss_pre_filter_set_xx_ "" ) 
    )
    (defun c:Recrete_hatch_SSRHB_ ()
      ;selection_set_for_fillter_blk_name
        (if  ;pre_select_ssget_or_post_select_ssget
          (=
            (setq ss_pre_filter_set_xx_ (ssget "i"
                                              (list
                                                (cons 0 "HATCH") ;type of object
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
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          
          (command "_-hatchedit" ss_pre_filter_set_xx_ename_  "B" "" "Y")
          
          
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    )
    (defun c:hatch_orgin_move_axis_X_HHX ()
      ;set paremeter for moving
        (setq move_length_ 25)
      ;
      ;user_input_
        (initget "Yes No")
        (setq ss_ ;Detect the variable's result in case of a function error
          (vl-catch-all-apply 
            (function 
              (lambda () 
                (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
              )
            )
          )
        )
      ;
      (cond
        (;delete_case_1
          (and
              (= DELETE_OBJ_ nil)
          )
          (progn
            (setq ssset_ (ssget))
            (princ "delete_case_1")
          )
        )
        (;delete_case_2
          (and
              (= DELETE_OBJ_ nil)
          )
          (progn
            (setq ssset_ ssset_)
            (princ "delete_case_2")
          )
        )
      )
      
      (while
        (= ;condition
          (wcmatch 
            (vl-princ-to-string 
              (setq direction_val_ ;Detect the variable's result in case of a function error
                (vl-catch-all-apply 
                  (function 
                    (lambda () 
                      (setq direction_ (cond ( (getint (strcat "\nspecify get\n1 = +\n0 = -\n<" (rtos (setq direction_ (cond (direction_) (1.0) ) ) ) "> : " ) ) ) (direction_) ) )
                    )
                  )
                )
              )
            ) 
            "*error*"
          )
          nil
        )
        ;properties hacth
          (setq ssset_ename_ (ssname ssset_ 0))
          (setq ssset_obj_ (vlax-ename->vla-object ssset_ename_))
          (setq ssset_obj_origin_ (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-origin ssset_obj_))
                                )
          )
        ;
        (cond
          (
            (and 
              (/= ssset_ nil)
              (= direction_val_ 1)
            )
            (setq sss_obj_putins_ (list 
                                    (+ (car ssset_obj_origin_) move_length_)
                                    (cadr ssset_obj_origin_)
                                    0
                                  )
            )
            (command "HATCHSETORIGIN" ssset_ "" sss_obj_putins_ )
          )
          (
            (and 
              (/= ssset_ nil)
              (= direction_val_ 0)
            )
            (setq sss_obj_putins_ (list 
                                    (- (car ssset_obj_origin_) move_length_)
                                    (cadr ssset_obj_origin_)
                                    0
                                  )
            )
            (command "HATCHSETORIGIN" ssset_ "" sss_obj_putins_ )
          )
        )
        
      )
    )
    (defun c:hatch_orgin_move_axis_Y_HHY ()
      ;set paremeter for moving
        (setq move_length_ 25)
      ;
      ;user_input_
        (initget "Yes No")
        (setq ss_ ;Detect the variable's result in case of a function error
          (vl-catch-all-apply 
            (function 
              (lambda () 
                (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
              )
            )
          )
        )
      ;
      (cond
        (;delete_case_1
          (and
              (= DELETE_OBJ_ nil)
          )
          (progn
            (setq ssset_ (ssget))
            (princ "delete_case_1")
          )
        )
        (;delete_case_2
          (and
              (= DELETE_OBJ_ nil)
          )
          (progn
            (setq ssset_ ssset_)
            (princ "delete_case_2")
          )
        )
      )
      
      (while
        (= ;condition
          (wcmatch 
            (vl-princ-to-string 
              (setq direction_val_ ;Detect the variable's result in case of a function error
                (vl-catch-all-apply 
                  (function 
                    (lambda () 
                      (setq direction_ (cond ( (getint (strcat "\nspecify get\n1 = +\n0 = -\n<" (rtos (setq direction_ (cond (direction_) (1.0) ) ) ) "> : " ) ) ) (direction_) ) )
                    )
                  )
                )
              )
            ) 
            "*error*"
          )
          nil
        )
        ;properties hacth
          (setq ssset_ename_ (ssname ssset_ 0))
          (setq ssset_obj_ (vlax-ename->vla-object ssset_ename_))
          (setq ssset_obj_origin_ (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-origin ssset_obj_))
                                )
          )
        ;
        (cond
          (
            (and 
              (/= ssset_ nil)
              (= direction_val_ 1)
            )
            (setq sss_obj_putins_ (list 
                                    (car ssset_obj_origin_)
                                    (+ (cadr ssset_obj_origin_) move_length_)
                                    0
                                  )
            )
            (command "HATCHSETORIGIN" ssset_ "" sss_obj_putins_ )
          )
          (
            (and 
              (/= ssset_ nil)
              (= direction_val_ 0)
            )
            (setq sss_obj_putins_ (list 
                                    (car ssset_obj_origin_)
                                    (- (cadr ssset_obj_origin_) move_length_)
                                    0
                                  )
            )
            (command "HATCHSETORIGIN" ssset_ "" sss_obj_putins_ )
          )
        )
        
      )
    )

  ;
  ;symbol_FUNC_
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
    (defun c:edit_breakline_to_rectangle_EDBK ()
      ;selection_set_user_input
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq ss_pre_filter_set_xx_ (ssget "I" 
                                              (list 
                                                (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE,INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                
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
      ;filter_object_to_new_ssget_
        (setq ss_set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000SYM-BREAK_LINE_DYNBLOCK_"))
        ;preloop_and_while
          (setq ss_set_i 0)
          (while (< ss_set_i (sslength ss_set_))
            ;get_data_in_ssget_
            (setq ss_set_ename_ (ssname ss_set_ ss_set_i))
            (setq ss_get_obj_ (vlax-ename->vla-object ss_set_ename_))
            (setq ss_get_obj_rotation_ (rad-to-deg (vla-get-rotation ss_get_obj_ )))
            
            (cond
              (;rotation_cond_case_1
                (and
                    (= ss_get_obj_rotation_ 0 )
                )
                (progn
                    (LM:setdynpropvalue ss_get_obj_ "distance1" 40 )
                    (LM:setdynpropvalue ss_get_obj_ "visibility1"  1)
                    (command "draworder" ss_set_ename_ "" "f" )
                  (princ "rotation_cond_case_1")
                )
              )
              (;rotation_cond_case_2
                (and
                    (= ss_get_obj_rotation_ 90)
                    
                )
                  (progn
                      (LM:setdynpropvalue ss_get_obj_ "distance1" 0 )
                      (LM:setdynpropvalue ss_get_obj_ "visibility1"  2)
                    (princ "rotation_cond_case_2")
                  )
              )
              (;rotation_cond_case_3
                (and
                    (= ss_get_obj_rotation_ 180)
                    
                )
                (progn
                    (LM:setdynpropvalue ss_get_obj_ "distance1" 40 )
                    (LM:setdynpropvalue ss_get_obj_ "visibility1"  1)
                    (command "draworder" ss_set_ename_ "" "f" )
                    (princ "rotation_cond_case_3")
                )
              )
              (;rotation_cond_case_4
                (and
                    (= ss_get_obj_rotation_ 270)
                    
                )
                (progn
                  (LM:setdynpropvalue ss_get_obj_ "distance1" 0 )
                  (LM:setdynpropvalue ss_get_obj_ "visibility1"  2)
                  (princ "rotation_cond_case_4")
                )
              )
            )
            
            
            
            
          (setq ss_set_i (+ ss_set_i 1))
        )
      ;
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
    (defun c:move_to_zero_xyz_M20 ()
      ;example_user_input_main_object
        (setq main_ename_ (car (entsel "specify Object")))
      ;
      ;get_data_form_user_input_
        (setq main_obj_ (vlax-ename->vla-object main_ename_))
        (setq main_obj_get_object_name_ (vla-get-objectname main_obj_))
      ;
      ;condoitional_for_diferent_acad_object_
        (cond  ;condoitional_for_diferent_acad_object_
          ( ;conditional_case_1
          (and 
            (= main_obj_get_object_name_ "AcDbPolyline")
          )
          (progn 
              (setq center_object_ (TA:find_center main_ename_))
              (princ "conditional_case_1")
          )
          )
          ( ;conditional_case_2
          (and 
            (= main_obj_get_object_name_ "AcDbCircle")
          )
          (progn 
              (setq center_object_ (setq _obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-center main_obj_ )))))
              (princ "conditional_case_2")
          )
          )
          ( ;conditional_case_3
          (and 
            (= main_obj_get_object_name_ "AcDbBlockReference")
          )
          (progn 
              (setq center_object_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint main_obj_))))
              (princ "conditional_case_3")
          )
          )
        )
      ;
      ;move_command_process_
        (vla-move main_obj_  (vlax-3d-point center_object_) (vlax-3d-point 0 0 0)  )
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
        (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq ss_pre_filter_set_xx_ename_close_line (vla-put-closed ss_pre_filter_set_xx_hon_obj_ :vlax-true))
        
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
  ;
  ;xclip_FUNC
    (defun c:MULTI_XCILP_OBJECT_MXCV ()
      ;note_from_DEV
      ;READMORE PROCESS IN ALERT MESSAGE
        (alert
          "
          1 - selection_set_for_blk_object_
          2 - selection_circle_object_
          3 - whileloop ssname for XCILP process
          4 - complete ? not sure
          "
        )
      ;
      ;user_input_blk_object_
        ;selection_set
          (princ "selection_set_BLKOBJECT_process")
          (if  ;pre_select_ssget_or_post_select_ssget
            (= 
              (setq ss_pre_selection_blkobject_set_ (ssget "I" 
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
              (setq ss_pre_selection_blkobject_set_ (ssget 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                          )
              )
            )
            (sslength ss_pre_selection_blkobject_set_)
          )
        ;
      ;

      ;user_input_circle
        ;selection_set
          (princ "ss_for_circle_")
          (if  ;pre_select_ssget_or_post_select_ssget
            (= 
              (setq ss_for_circle_ (ssget "I" 
                                                (list 
                                                  (cons 0 "CIRCLE,lwpolyline") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                                )
                                          )
              )
              nil
            )
            (progn 
              (setq ss_for_circle_ (ssget 
                                            (list 
                                              (cons 0 "CIRCLE,lwpolyline") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                          )
              )
            )
            (sslength ss_for_circle_)
          )
        ;
      ;
      (cond 
        (;UCS_Detect_variable_
          (and
              (= (getvar "WORLDUCS") 0)
          )
          (progn
            (alert "Please reset Worlducs variable")
          )
        )
        (;object_case_1_polygon_case
          (and
              (= (vla-get-objectname (vlax-ename->vla-object (ssname ss_for_circle_ 0))) "AcDbPolyline")
              (= (getvar "WORLDUCS") 1)
          )
          (progn
            (setq ucs_detect_error_ 1)
            (setvar "osmode" 0)
            (setq new_polygon_ename_ (ssname ss_for_circle_ 0))
            (setvar "osmode" 1279)
            (princ "object_case_1")
          )
        )
        (;object_case_2_circle_obejct_
          (and
              (= (vla-get-objectname (vlax-ename->vla-object (ssname ss_for_circle_ 0))) "AcDbCircle")
              (= (getvar "WORLDUCS") 1)
          )
          (progn
              ;tranfer_circle_to_polyogn_process
                (setvar "osmode" 0)
                (setq segment_curve_ (cond ( (getint (strcat "\nspecify fllet_rec_<" (rtos (setq segment_curve_ (cond (segment_curve_) (1.0) ) ) ) "> : " ) ) ) (segment_curve_) ) )
                (setq center_circle_ (vlax-safearray->list (vlax-variant-value (vla-get-center (vlax-ename->vla-object (ssname ss_for_circle_ 0))))))
                (setq rad_circle_ (vla-get-radius (vlax-ename->vla-object (ssname ss_for_circle_ 0))))
                (command "polygon" segment_curve_ center_circle_ "i"  rad_circle_ )
                (setq new_polygon_ename_ (entlast))
                (setq new_polygon_obj_ (vlax-ename->vla-object new_polygon_ename_))
                (setvar "osmode" 1279)
              ;
            (princ "object_case_2")
          )
        )
      )
      
      

      ;multi_xcilp_process_
        ;preloop_and_while
          (setq ss_pre_selection_blkobject_set_i 0)
          (while 
            (and
              (= (getvar "WORLDUCS") 1)
              (< ss_pre_selection_blkobject_set_i (sslength ss_pre_selection_blkobject_set_))
            )
            (setq ss_pre_selection_blkobject_set_ename_ (ssname ss_pre_selection_blkobject_set_ ss_pre_selection_blkobject_set_i))
              (command "xclip"  ss_pre_selection_blkobject_set_ename_  "" "n" "s" new_polygon_ename_)
            (setq ss_pre_selection_blkobject_set_i (+ ss_pre_selection_blkobject_set_i 1))
          )
        ;
        ;delete_object_
          (vla-delete  new_polygon_obj_)
        ;
      ;
    )
  ;
  ;scale_FUNC
    (defun c:save_dynamic_scale_scdyn_ ()
      ;user_input_
        (setq get_dynblock_ename_ (car (entsel "specify Object")))
        (setq dynamic_scale_ (cond ( (getreal (strcat "\nspecify_scale<" (rtos (setq dynamic_scale_ (cond (dynamic_scale_) (1.0) ) ) ) "> : " ) ) ) (dynamic_scale_) ) )
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
  ;
  ;EF_NAME_TO_ATT_TEXT_BLOCK_FUNC
    (defun TA:fillter_block_name_ (blk_ename_ )
      ;LEE-MAC_SUB_FUNC
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
      ;
      ;Example_for_testing_
        ; (setq blk_ename_ (car (entsel "specify Object")))
      ;
      ;
      (setq blk_obj_ (vlax-ename->vla-object blk_ename_))

      (if (= (vla-get-objectname blk_obj_ ) "AcDbBlockReference")
        (progn
          (setq blk_raw_efname_ (LM:effectivename blk_obj_ ))
          ;user_setting_for_filter_ename_
            ; (setq blk_raw_efname_ "000 - DYNAMIC_VIEW_ASSCESSORIES_PHILLIPS_HEAD_SELF_DRILLING_SCREW_No.8_COMBINE_VIEW") ;FOR EXAMPLE CODING 
            (setq blk_raw_efname_ (LM:StringSubst "" "000 - " blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "DYNAMIC" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "VIEW" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "ASSCESSORIES" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "COMBINE+" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "FRONT" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "TOP" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "SIDE" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "BOT" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "BACK" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "CUT" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "ARRAY+" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "ARRAY" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "" "SET" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "_" "__" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "_" "___" blk_raw_efname_))
            (setq blk_raw_efname_ (LM:StringSubst "_" "____" blk_raw_efname_))
            
          ;
          ;front_name_setting_
            ;preloop_and_while_for_delete "_"
              (setq i 0)
              (setq ii 1)
              (setq test_text_ (substr blk_raw_efname_ ii 1))
              (while (= test_text_ "_")
                (setq test_text_ (substr blk_raw_efname_ ii 1))
                (if (/= i 0)
                  (progn
                    (setq ii (+ ii 1))
                    (setq i (+ i 1))
                  )
                  (setq i (+ i 1))
                )
              )
              (setq blk_raw_efname__strlen (strlen blk_raw_efname_))
              (if (= ii 1)
                (progn
                  (setq final_text_ blk_raw_efname_)
                )
                (setq final_text_ (substr blk_raw_efname_ (setq new_i (- ii 1)) blk_raw_efname__strlen))
              )
              

          ;
          ;bot_name_setting_
            (setq l 0)
            (setq ll 1)
            (setq final_text_l (strlen final_text_))
            (setq test_text_ (substr final_text_ final_text_l 1))
            (while (= test_text_ "_")
              (setq test_text_ (substr final_text_ final_text_l 1))
              (setq final_text_l (- final_text_l 1))
            )
            (setq final_text_last (+ final_text_l 1))
            (setq final_text_final (substr final_text_ 1 final_text_last))
          ;
        )
      )
    )
    (defun TA:remove_att_value_ (att_obj_ except_att_list_ )
      ;Note By Code_Developer
      ;This command is designed to work exclusively with attribute blocks only.
      ;It will remove all attributes in the block, but it allows certain attributes to be excluded by adding their names to an exception list.
      ;att_obj_ = vla-ename > object
      ;except_att_list_ = (list "att1" "att2" etc.)
      ;example code (TA:remove_att_value_ att_obj_ (list))
      ;example_code (setq except_att_list_ (list "NO.CODE")) 
      ;The operation of the command will read the block's attribute values and generate a set of text. n 
      ;

      ;fillter_EF_name_object_
        ; (setq att_ename_ nil)
        ; (setq att_obj_ nil)
        ; (while (= att_ename_ nil)
          ;user_input_data_  
            ; (setq att_ename_ (car (entsel "specify Object")))
          ;
          ;fillter_object_type_
            (if 
              (and
                ; (if (/= att_ename_ nil) (progn (setq att_obj_ (vlax-ename->vla-object att_ename_)) ))
                (= (vla-get-objectname att_obj_) "AcDbBlockReference")
                (= (vla-get-hasattributes att_obj_) :vlax-true )
              )
              (progn
                (setq att_tag_list_ (LM:vl-getattributevalue-tag-TA-Modifies att_obj_))
              )
              ; (setq att_ename_ nil)
            )
            (if 
              (or
                ; (= att_ename_ nil)
                (/= (vla-get-objectname att_obj_) "AcDbBlockReference")
                (if (= (vla-get-objectname att_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes att_obj_) :vlax-true ) ))
              )
              (progn
                (alert "Please selection attribute object")
              )
            )
          ;
        ; )
      ;
      ;preloop_and_while_for_make_sequnce_attribute
        (setq att_tag_list_i 0)
        (while (< att_tag_list_i (length att_tag_list_))
          (setq att_tag_list_tagname_ (nth  att_tag_list_i att_tag_list_))
          (if (= (TA:FIND_DUPLICATE_LIST att_tag_list_tagname_ except_att_list_) "N" )
            (progn
              (LM:vl-setattributevalue att_obj_ att_tag_list_tagname_ "" )
            )
          )
          (setq att_tag_list_i (+ att_tag_list_i 1))
        )
      ;
      
    )
    (defun c:fillter_blockname_to_text_b2text ()
      ;NOTE-FROM_DEV
        ;this is code for filler_efectivename_block_name
      ;
      ;preloop_and_while
        (setq return_code_ nil)
        (while (and 
                (= return_code_ nil)
                
              )
          ;user_input_
            (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
            (setq fillter_block_name_ (TA:fillter_block_name_ eee_ename_))
            (setq dynamic_ename_ (car (entsel "specify Object for input fillter_block_name")))
            (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
          ;
          (if (and 
                (= (setq alert_val_1 (vla-get-objectname dynamic_obj_)) "AcDbBlockReference")
                (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "CONTENT NAME3 LEFT - BUBBLE v6")
                (/= eee_ename_ dynamic_ename_)
              )
            (progn
              (LM:vl-setattributevalue dynamic_obj_ "name1" fillter_block_name_)
              (setq return_code_"full")
            )
            (setq return_code_ nil)
          )
          (if (and 
                (= return_code_ nil)
                (/= alert_val_1 "AcDbBlockReference")
              )
            (progn
              (alert (strcat "this object is " alert_val_1 "\n" "block name = nil\n" "something is incorrect"  ))
              (setq return_code_ nil)
            )

          )

        )
      ;
    )
    (defun c:effectivename_to_att_EF2text ()
      ;NOTE-FROM_DEV
        ;this is code for filler_efectivename_block_name
      ;
      ;preloop_and_while
        (setq return_code_ nil)
        (while (and 
                (= return_code_ nil)
                
              )
          ;user_input_
            (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
            (setq fillter_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
            (setq dynamic_ename_ (car (entsel "specify Object for input fillter_block_name")))
            (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
          ;
          (if (and 
                (= (setq alert_val_1 (vla-get-objectname dynamic_obj_)) "AcDbBlockReference")
                (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "000 - BOX_EF_NAME")
                (/= eee_ename_ dynamic_ename_)
              )
            (progn
              (LM:vl-setattributevalue dynamic_obj_ "nameblk" fillter_block_name_)
              (setq return_code_"full")
            )
            (setq return_code_ nil)
          )
          (if (and 
                (= return_code_ nil)
                (/= alert_val_1 "AcDbBlockReference")
              )
            (progn
              (alert (strcat "this object is " alert_val_1 "\n" "block name = nil\n" "something is incorrect"  ))
              (setq return_code_ nil)
            )

          )

        )
      ;
    )
    (defun c:rename_block_RENBLK ()
      ;NOTE-FROM_DEV
        ;this is code for filler_efectivename_block_name
      ;
      ;user_input
        (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
        (setq old_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
        (setq new_ename_ (car (entsel "specify Object for input fillter_block_name")))
        (setq new_ename_obj_ (vlax-ename->vla-object new_ename_))
      ;
      ;checking_process
        (if 
          (and 
            (= (setq alert_val_1 (vla-get-objectname eee_ename_obj_)) "AcDbBlockReference")
            (= (setq alert_val_2 (LM:effectivename new_ename_obj_)) "000 - BOX_EF_NAME")
            (/= eee_ename_ dynamic_ename_)
          )
          (progn
            ;get_data_old_EF_NAME_
              (setq old_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
            ;
            ;get_data
              (setq new_ef_name_ (LM:vl-getattributevalue new_ename_obj_ "nameblk"  ))
            ;
          )
          (alert "SOMETHING BAD")
        )
      ;
      ;Rename_process
        (command "rename" "B" old_block_name_ new_ef_name_)
      ;
          


    )
    (defun c:limit_content_name_[limittext_] ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named 'CONTENT NAME3 LEFT - BUBBLE v6'.
      ;The operation of the command will read the block's attribute values and regenerate By this command can limit number of chhracters per line.
      ;Due to this code is designed to work exclusively with a block named "CONTENT NAME3 LEFT - BUBBLE v6"
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;
      ;
      ;fillter_content_name_object_
        (setq content_ename_ nil)
        (while (= content_ename_ nil)
          ;user_input_data
            (setq content_ename_ (car (entsel "specify Object")))
            (setq limit_text_length (cond ( (getint (strcat "\nSpecify limit_text_count   <" (rtos (setq limit_text_length (cond (limit_text_length) (1.0) ) ) ) "> : " ) ) ) (limit_text_length) ) )
          ;
          ;fillter_object_type_
            (if 
              (and
                (if (/= content_ename_ nil) (progn (setq content_obj_ (vlax-ename->vla-object content_ename_)) ))
                (= (vla-get-objectname content_obj_) "AcDbBlockReference")
                (= (vla-get-hasattributes content_obj_) :vlax-true )
                (= (LM:effectivename content_obj_) "CONTENT NAME3 LEFT - BUBBLE v6" )
              )
              (progn ;MAIN_IDEA
                ;getdata_limit_text_
                  (setq content_tag_list_ (LM:removenth 9 (LM:vl-getattributevalues content_obj_)))
                  (setq content_tag_list_length_ (strlen 
                                                  (cdr (nth 0 content_tag_list_))
                                                  (cdr (nth 1 content_tag_list_))
                                                  (cdr (nth 2 content_tag_list_))
                                                  (cdr (nth 3 content_tag_list_))
                                                  (cdr (nth 4 content_tag_list_))
                                                  (cdr (nth 5 content_tag_list_))
                                                  (cdr (nth 6 content_tag_list_))
                                                  (cdr (nth 7 content_tag_list_))
                                                  (cdr (nth 8 content_tag_list_))
                                                  
                                                  "_"
                                                )
                  )
                  (setq content_tag_list_value_ (strcat 
                                                  (cdr (nth 0 content_tag_list_))
                                                  (cdr (nth 1 content_tag_list_))
                                                  (cdr (nth 2 content_tag_list_))
                                                  (cdr (nth 3 content_tag_list_))
                                                  (cdr (nth 4 content_tag_list_))
                                                  (cdr (nth 5 content_tag_list_))
                                                  (cdr (nth 6 content_tag_list_))
                                                  (cdr (nth 7 content_tag_list_))
                                                  (cdr (nth 8 content_tag_list_))
                                                  
                                                  "_"
                                                )
                  )
                  
                  (setq sum_text_ (list content_tag_list_length ))
                  (setq sum_text_name (list content_tag_list_value_))
                  (setq sum_text_name (LM:lst->str sum_text_name ""))
                  (setq sum_text_name (LM:str->lst sum_text_name "_"))
                  (setq sum_text_name (TA:REMOVE_VAL_LIST_ "" sum_text_name ))
                  
                  (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
                ;
                ;preparing_summary_data_
                  (setq sum_text_i 0)
                  (setq sum_text_len 0)
                  ; (setq new_text_line_0 ())
                  (setq new_text_line_1 ())
                  (setq new_text_line_2 ())
                  (setq new_text_line_3 ())
                  (setq new_text_line_4 ())
                  (setq new_text_line_5 ())
                  (setq new_text_line_6 ())
                  (setq new_text_line_7 ())
                  (setq new_text_line_8 ())
                  (setq new_text_line_9 ())
                  (setq new_text_line_10 ())
                ;
                ;summary_limit_text_to_line
                  (setq limit_text_length limit_text_length)
                  (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 500))
                    (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                    (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                    ;main_IDEA_limit_text_process
                      (cond
                        ( ;line_1
                          (<= sum_text_len limit_text_length)
                          (progn
                            (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                          )
                          (princ "line_1")
                        )
                        ( ;line_2
                          (and (> sum_text_len (* limit_text_length 1)) (< sum_text_len (* limit_text_length 2)) )
                          (progn
                            (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                          )
                          (princ "line_2")
                        )
                        ( ;line_3
                          (and (>= sum_text_len (* limit_text_length 2)) (< sum_text_len (* limit_text_length 3)) )
                          (progn
                            (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                          )
                          (princ "line_3")
                        )
                        ( ;line_4
                          (and (>= sum_text_len (* limit_text_length 3)) (< sum_text_len (* limit_text_length 4)) )
                          (progn
                            (setq new_text_line_4 (cons sum_text_ename_name new_text_line_4 ))
                          )
                          (princ "line_4")
                        )
                        ( ;line_5
                          (and (>= sum_text_len (* limit_text_length 4)) (< sum_text_len (* limit_text_length 5)) )
                          (progn
                            (setq new_text_line_5 (cons sum_text_ename_name new_text_line_5 ))
                          )
                          (princ "line_5")
                        )
                        ( ;line_6
                          (and (>= sum_text_len (* limit_text_length 6)) (< sum_text_len (* limit_text_length 7)) )
                          (progn
                            (setq new_text_line_6 (cons sum_text_ename_name new_text_line_6 ))
                          )
                          (princ "line_6")
                        )
                        ( ;line_7
                          (and (>= sum_text_len (* limit_text_length 7)) (< sum_text_len (* limit_text_length 8)) )
                          (progn
                            (setq new_text_line_7 (cons sum_text_ename_name new_text_line_7 ))
                          )
                          (princ "line_7")
                        )
                        ( ;line_8
                          (and (>= sum_text_len (* limit_text_length 8)) (< sum_text_len (* limit_text_length 9)) )
                          (progn
                            (setq new_text_line_8 (cons sum_text_ename_name new_text_line_8 ))
                          )
                          (princ "line_8")
                        )
                        ( ;line_9
                          (and (>= sum_text_len (* limit_text_length 9)) (< sum_text_len (* limit_text_length 10)) )
                          (progn
                            (setq new_text_line_9 (cons sum_text_ename_name new_text_line_9 ))
                          )
                          (princ "line_9")
                        )
                      )
                    ;
                    (setq sum_text_i (+ sum_text_i 1))
                  )
                  (setq limit_text_value_set_ 
                                              (TA:REMOVE_VAL_LIST_ 
                                                ""
                                                (list 
                                                  (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_" ) )
                                                  (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_" ) )
                                                  (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_" ) )
                                                  (setq new_text_line_4_sum (LM:lst->str (reverse new_text_line_4) "_" ) )
                                                  (setq new_text_line_5_sum (LM:lst->str (reverse new_text_line_5) "_" ) )
                                                  (setq new_text_line_6_sum (LM:lst->str (reverse new_text_line_6) "_" ) )
                                                  (setq new_text_line_7_sum (LM:lst->str (reverse new_text_line_7) "_" ) )
                                                  (setq new_text_line_8_sum (LM:lst->str (reverse new_text_line_8) "_" ) )
                                                  (setq new_text_line_9_sum (LM:lst->str (reverse new_text_line_9) "_" ) )
                                                )
                                              )
                  )
                ;
                ;Clear_att_process_
                  (setq except_att_list_ (list "NO.CODE") )
                  (TA:remove_att_value_ content_obj_ except_att_list_)
                ;
                ;preloop_and_while_limit_text_to_line
                    (setq limit_text_i 0)
                    (while (< limit_text_i (length limit_text_value_set_))
                      (setq content_tag_list_att (car (nth limit_text_i content_tag_list_))) ;att_tag
                      (setq limit_text_list_ (nth limit_text_i limit_text_value_set_)) ;att_value
                      
                      (if (/= limit_text_list_ nil)
                        (progn
                          (LM:vl-setattributevalue content_obj_ content_tag_list_att (strcat limit_text_list_ "_") )
                        )
                        (LM:vl-setattributevalue content_obj_ content_tag_list_att "" )
                      )
                  
                      (setq limit_text_i (+ limit_text_i 1))
                    )
                ;
                ;indicate_visibility_BLK_atttribute
                  (setq limit_text_vis_ (TA:REMOVE_VAL_LIST_ "" limit_text_value_set_))
                  (LM:setdynpropvalue content_obj_ "content_line" (rtos(length limit_text_vis_)2 0))
                ;
              
              )
              (setq content_ename_ nil)
            )
              
            
            
            (if
              (or
                (= content_ename_ nil)
                (/= (vla-get-objectname content_obj_) "AcDbBlockReference")
                (if (= (vla-get-objectname content_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes content_obj_) :vlax-true ) ))
              )
              (progn
                (alert "Please selection attribute object")
              )
            )
          ;
        )
      ;
    )
    (defun c:replace_text_attribute_retatt_ ()
      
      (setq replace_old_text_ (getstring (strcat "\nSpecify old_text <" (if replace_old_text_ replace_old_text_ "") ">: ") ) )
      (setq replace_new_text_ (getstring (strcat "\nSpecify new_text <" (if replace_new_text_ replace_new_text_ "") ">: ") ) )
      
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
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ))
          (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          
          (setq att_list_ (LM:vl-getattributevalues ss_pre_filter_set_xx_obj_))
          
          ;preloop_and_while
            (setq att_list_i 0)
            (while (< att_list_i (length att_list_))
              (setq att_list_head_ (car (nth  att_list_i att_list_)))
              (setq att_list_val_ (cdr (nth  att_list_i att_list_)))
              
              (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ att_list_head_ (LM:StringSubst replace_new_text_ replace_old_text_ att_list_val_))
          
              (setq att_list_i (+ att_list_i 1))
            )
          ;
          

          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;

      
      
      
      
    )

  ;
  ;Joint_FUNC
    (defun c:joint_line_JOL ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a LWPOLYLINE espectespecially object from offset line
      ;The principle of this code is to read the insertion points (vertices) of the polyline at the head and tail,then create polylines at both insertion points and join them together.
      ;
      ;joint_pline
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
        ;condition_rule_for_run
          (if (= (sslength ss_pre_filter_set_xx_) 2)
            (progn
              ;preloop_and_while
                (setq ss_pre_filter_set_xx_i 0)
                (setq ss_pre_filter_set_xx_ii 1)
                (while (< ss_pre_filter_set_xx_i 1)
                  ;first_pline_process
                    (setq ss_pre_filter_set_xx_i_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                      (setq ss_pre_filter_set_xx_i_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_i_ename_))
                        (setq ss_pre_filter_set_xx_i_obj_vertex_len_ (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_i_ename_)))
                        ;reverse insertion point 
                          (if (>
                                (car (car (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_i_ename_))))
                                (car (car (reverse (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_i_ename_)))))
                              )
                            (progn
                              (command "reverse" ss_pre_filter_set_xx_i_ename_ "")
                              (setq ss_pre_filter_set_xx_i_obj_vertex_len_ (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_i_ename_)))
                            )
                          )
                        ;
                          (setq ss_pre_filter_set_xx_i_obj_vertex_len_headline_ (nth 0 ss_pre_filter_set_xx_i_obj_vertex_len_))
                          (setq ss_pre_filter_set_xx_i_obj_vertex_len_botline_ (nth (- (length ss_pre_filter_set_xx_i_obj_vertex_len_) 1) ss_pre_filter_set_xx_i_obj_vertex_len_))
                    ;copy_pline_object
                      (vla-copy ss_pre_filter_set_xx_i_obj_)
                    ;
                  ;
                  ;second_pline_process
                    (setq ss_pre_filter_set_xx_ii_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_ii))
                      (setq ss_pre_filter_set_xx_ii_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ii_ename_))

                        (setq ss_pre_filter_set_xx_ii_obj_vertex_len_ (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_ii_ename_)))
                        ;reverse insertion point 
                          (if (>
                                (car (car (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_ii_ename_))))
                                (car (car (reverse (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_ii_ename_)))))
                              )
                            (progn
                              (command "reverse" ss_pre_filter_set_xx_ii_ename_ "")
                              (setq ss_pre_filter_set_xx_ii_obj_vertex_len_ (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_ii_ename_)))
                            )
                          )
                        ;
                          (setq ss_pre_filter_set_xx_ii_obj_vertex_len_headline_ (nth 0 ss_pre_filter_set_xx_ii_obj_vertex_len_))
                          (setq ss_pre_filter_set_xx_ii_obj_vertex_len_botline_ (nth (- (length ss_pre_filter_set_xx_ii_obj_vertex_len_) 1) ss_pre_filter_set_xx_ii_obj_vertex_len_))
                    ;copy_pline_object
                      (vla-copy ss_pre_filter_set_xx_ii_obj_)
                    ;
                  ;
                  ;making_pline_process_
                    (setvar "osmode" 0)
                    (create-pline (list ss_pre_filter_set_xx_i_obj_vertex_len_headline_ ss_pre_filter_set_xx_ii_obj_vertex_len_headline_)) ;head_of_line
                      (setq new_joint_headline_ (entlast))
                    (create-pline (list ss_pre_filter_set_xx_i_obj_vertex_len_botline_ ss_pre_filter_set_xx_ii_obj_vertex_len_botline_)) ;bot_of_line
                      (setq new_joint_botline_ (entlast))
                  ;
                  ;Jointline_process
                    (command "join" new_joint_headline_ new_joint_botline_ ss_pre_filter_set_xx_i_ename_ ss_pre_filter_set_xx_ii_ename_ "")
                  ;
                  (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
                )
              ;
              (setvar "osmode" 1215)
              (command "draworder"  "L" "" "B") 

            )
          )
        ;
      ;
    )
  ;
  ;Selection_FUNC
    (defun c:filter_object_and_selection_[sscircle] () ;ssget["Depend_On_User"]
      ;Note By Code_Developer
      ;The principle of codework is to selection object by ssget method via variable name [ss_pre_filter_set_xx_]
      ;Example code 
      ;
      
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "circle") ;type of object
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
                                          (cons 0 "circle") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      (command "pselect" ss_pre_filter_set_xx_ "")
    )
    (defun c:filter_object_and_selection_[ssspline] ()
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "spline") ;type of object
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
                                          (cons 0 "spline") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )

      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (command "pedit" ss_pre_filter_set_xx_ename_ "Y" "2"  "")
      
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
      
    )
    (defun c:make_selection_set_boudary_line_SSBOL ()
      (setq sss_ (ssget ))
      (setvar "osmode" 0)
      (setq ssssasas (TA:ss_vla-getboundingbox sss_))
      (command "rectangle" (car ssssasas) (cadr ssssasas) )
      (setvar "osmode" 1215)
    )
    (defun c:copybase_selection_set_boudary_line_CCBOL ()
      (setq sss_ (ssget ))
      (setvar "osmode" 0)
      (setq ssssasas (TA:ss_vla-getboundingbox sss_))
      (command "rectangle" (car ssssasas) (cadr ssssasas) )
      (setq new_ename_ (entlast))
      (setq new_obj_ (vlax-ename->vla-object new_ename_))
      (vla-put-color new_obj_ 0)
      (ssadd new_ename_ sss_)
      (sslength sss_)
      (command "_copybase" (LM:PolyCentroid new_ename_) sss_ "" )
      (setvar "osmode" 1215)
      (command "_pasteblock" )
      
    )
    (defun c:selection_by_handent_hand_ ()
      (setq handent_string_ (getstring "specify handent string"))
      (command "pselect"  (handent handent_string_) "" )
      
      
    )
    (defun c:lastselect_entity_lse ()
      (command "pselect"  (entlast) "" )
    )


  ;
  ;test_command
    (defun TA:dump_obj_ (dump_ename_ )
      ; (setq dump_ename_ (car (entsel "specify Object")))
      (setq dump_obj_ (vlax-ename->vla-object dump_ename_))
      (vlax-dump-object (vlax-ename->vla-object dump_ename_))
    )
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
  ;ALuminium Tube FUNC
    (defun c:insert_ALTUBE_insAL ()
      ;Note By Code_Developer
      ;This command is designed to work exclusively with a block named '001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint'.
      ;principle of codework is design to work woth line object only by referce start-end point of the line is basepoint for first insertion Block object
      ;After insertion block complete, main code will keep Ename data by "enlast" method and use Lee Mac dynamic block for strech object 
      ;Fully command must have sub-functions with names starting with TA: or LM:
      ;
      ;
      ;user_input_dynammic_
        (setq user_input_width_ (cond ( (getint (strcat "\nspecify fllet_rec_<" (rtos (setq user_input_width_ (cond (user_input_width_) (1.0) ) ) ) "> : " ) ) ) (user_input_width_) ) )
      ;
      ;selection_set_for_fillter_blk_name
        (if  ;pre_select_ssget_or_post_select_ssget
          (=
            (setq ss_pre_filter_set_xx_ (ssget "i"
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
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
      ;while_select_block_on_condition_
          (setq blk_ename_ nil)
          (while (= blk_ename_ nil)
            (setq blk_ename_ (car (entsel "specify blk_ename Object")))
            (if
              (and ;conditional_rule_for_select_object
                (/= blk_ename_ nil)
                (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
                (= (vla-get-objectname blk_ename_obj_ ) "AcDbBlockReference")
                (= (vla-get-isdynamicblock blk_ename_obj_ ) :vlax-true)
                
              )
              (progn
                (setq blk_ename_efname_ (LM:effectivename blk_ename_obj_ ) )
              )
              (alert "Object invalid Please try again")
            )
          )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          ;get_line_data
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
            (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                            (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                      (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                                    )
                                            )
                                            (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                      (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                                  )
                                            )
                                          )
            )
            (setq ss_pre_filter_set_xx_hon_obj_length (atoi (rtos (vla-get-length ss_pre_filter_set_xx_hon_obj_) 2 0)))
          
          ;
          ;insertion_new_object
            (command "insert" blk_ename_efname_ ss_pre_filter_set_xx_hon_obj_startpt_ 1 0)
            (LM:setdynpropvalue (vlax-ename->vla-object (entlast)) "H" ss_pre_filter_set_xx_hon_obj_length )
            (LM:setdynpropvalue (vlax-ename->vla-object (entlast)) "W" user_input_width_ )

          ;
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
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







  ;Common_commmand 
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
    (defun c:round_up_down_insertion_point_REINS ()
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
    (defun c:make_boudary_line_MKBOL ()
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
    (defun c:STAMP_START_POINT_STP_ ()
      
      ;Stamp_ dynamic block "000SYM-START_POINT_" /w RECTANGLE_BLOCK
      (setvar "osmode" 0)
      (setq main_rec_ename_ (car (entsel "specify Object")))
      (setq rec_point_ (TA:find_center main_rec_ename_ ))
      (command "insert" "000SYM-START_POINT_" rec_point_ 0.1 0 )
      (setvar "osmode" 1215)
    )
    (defun c:relocation_rectangle_vertex_REVX ()
        (setq rec_ename_ (car (entsel "specify Object")))
        (setq rec_obj_ (vlax-ename->vla-object rec_ename_))
      
        ; (if (and
        ;       (= (vla-get-objectname rec_obj_) "AcDbPolyline")
        ;     )
        ;   (progn
        ;     (thenexpr)
        ;   )
        ; )
        (setq rec_obj_getboundingbox (vla-getboundingbox rec_obj_ 'min_ 'max_)) 
        (setq rec_min_ (vlax-safearray->list min_)) 
        (setq rec_max_ (vlax-safearray->list max_)) 
        
      (command "rectangle" rec_min_ rec_max_ )
      (vla-delete   rec_obj_ )
      
      
    )
    (defun c:make_thickness_sheet_THS ()
      ;user_input_
        (setq 2dblank_ename_ (car (entsel "specify Object")))
        
        (setq user_input_thickness_ (cond ( (getreal (strcat "\nspecify_height blank size<" (rtos (setq user_input_thickness_ (cond (user_input_thickness_) (1.0) ) ) ) "> : " ) ) ) (user_input_thickness_) ) )
        (setq user_input_offset_ (cond ( (getreal (strcat "\n\n1 = Max 0 = Min [1/0] <" (rtos (setq user_input_offset_ (cond (user_input_offset_) (1.0) ) ) ) "> : " ) ) ) (user_input_offset_) ) )
        (cond
          (;user_input_offset_case_1
            (and
                (= user_input_offset_ 1)
            )
            (progn
              (setq user_input_offset_val "max")
              (princ "user_input_offset_case_1")
            )
          )
          (;user_input_offset_case_2
            (and
                (= user_input_offset_ 0)
            )
            (progn
              (setq user_input_offset_val "min")
              (princ "user_input_offset_case_2")
            )
          )
        )
      ;
      ;new_ssget_process_
        (setq new_ssget_ ())
        (setq new_ssget_ (ssadd))
        (setq new_ssget_ (ssadd  2dblank_ename_ new_ssget_))
        (sslength new_ssget_)
      ;
      ;get_data_
        (setq r2dblank_obj_ (vlax-ename->vla-object 2dblank_ename_))
        (setq 2dblank_objname_  (vla-get-objectname r2dblank_obj_))
      
        (setq vertex_list_  (TA:Get_Pline_vertext_ins_point_ 2dblank_ename_))
        (setq 2dblank_len_val_ (TA:get_vertex_len_ 2dblank_ename_))
      ;
      ;offset_process

        (TA:Offset_Pline_multi_vertex_ 2dblank_ename_ user_input_thickness_ user_input_offset_val)
        (setq new_line_ (entlast))
        (setq new_line_vertex_list_  (TA:Get_Pline_vertext_ins_point_ new_line_))
        (setq new_ssget_ (ssadd  new_line_ new_ssget_))
        (sslength new_ssget_)
      ;
      ;create_start_line_process
        (setq startline_
              (list
                  (nth 0 (cadr vertex_list_ ))
                  (nth 0 (cadr new_line_vertex_list_ ))
              )
        )
        (create-pline startline_ )
        (setq new_line_ (entlast))
        (setq new_ssget_ (ssadd  new_line_ new_ssget_))
        (sslength new_ssget_)
      
        (setq vertex_list_i (- (length (cadr vertex_list_ ) ) 1))
        (setq endline_
              (list
                  (nth vertex_list_i (cadr vertex_list_ ))
                  (nth vertex_list_i (cadr new_line_vertex_list_ ))
              )
        )

        (create-pline endline_ )
        (setq new_line_ (entlast))
        (setq new_ssget_ (ssadd  new_line_ new_ssget_))
        (sslength new_ssget_)
      ;
      ;joint_all_selection_set_process
        (command "pedit"  "m" new_ssget_ "" "j" "" "" )
      ;
    )
    (defun c:rotate_counter_clockwise_rccw ()
      ;Note By Code_Developer
      ;The principle of Codework is designed for block objects, where the commands
      ;will rotale block object only
      ;
      ;sub-FUNC
        (defun TA:ModifyRotation (Ex_rotation New_rotation)
          ;Note By Code_Developer
          ;The principle of this code is designed to modify the angle value, allowing for both increments and decrements based on a reference number.
          ;The reference number used must be a value representing degrees, ranging from 1 to 360.
          ;The number added or subtracted from the angle is calculated based on the number of degrees rotated, considering 1 full rotation as 360 degrees.
          ;
          (cond 
            ( ;normal_rotation_case_1
              (and 
                (>= New_rotation 0)
                (<= New_rotation 360)
              )
              (progn
                (princ "rotation_case_1\n")
                (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
                (if (<= new_rotaion_val_ 360)
                  (progn
                    (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
                  )
                  (setq new_rotaion_val_ (- (+ Ex_rotation New_rotation) 360))
                )
                
                
              )
            )
            (;multi_time_rotation_case_2
              (and 
                (> New_rotation 360)
              )
              (progn 
                (princ "rotation_case_2\n")
                (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
                (setq round_time_val (* round_time_ 360))
                (setq new_rotaion_val_ (- New_rotation round_time_val))
                (if (> (+ Ex_rotation new_rotaion_val_ ) 360)
                (progn
                  (setq new_rotaion_val_ans_ (-  (+ Ex_rotation new_rotaion_val_ ) 360 ))
                )
                  (setq new_rotaion_val_ans_ (+ Ex_rotation new_rotaion_val_ ))
                )
                
              )
            )
            (;rotation_case_3
              (and 
                (<= New_rotation 0) 
              )
              (progn 
                (princ "rotation_case_3\n")
                (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
                (setq round_time_val (* round_time_ 360))
                (setq new_rotaion_val_ (- New_rotation round_time_val))
                (setq Ex_rotation_val_ (+ Ex_rotation new_rotaion_val_))
                (if (< Ex_rotation_val_ 0)
                  (progn
                    (setq new_rotaion_val_ans_ (+ 360 Ex_rotation_val_))
                  )
                  (setq new_rotaion_val_ans_ Ex_rotation_val_ )
                )
                
              )
            )
            
          )
        )
      ;Exaample use
      ;no Example just use it !!!
      ;
      ;user_input_
        (initget "Yes No")
        (setq ss_ ;Detect the variable's result in case of a function error
          (vl-catch-all-apply 
            (function 
              (lambda () 
                (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
              )
            )
          )
        )
        (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
          (progn
            ;selection_set_for_fillter_blk_name
              (if  ;pre_select_ssget_or_post_select_ssget
                (=
                  (setq ss_pre_filter_set_xx_ (ssget "i"
                                                    (list
                                                      (cons 0 "insert") ;type of object
                                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                      ; (cons 62 0)           ;kind of color call sign with color code index
                                                    )
                                              )
                  )
                  nil
                )
                (progn
                  (setq ss_pre_filter_set_xx_ (ssget 
                                                    (list
                                                      (cons 0 "insert") ;type of object
                                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                      ; (cons 62 0)           ;kind of color call sign with color code index
                                                    )
                                              )
                  )
                )
                (sslength ss_pre_filter_set_xx_)
              )
            ;
          )
          (setq ss_pre_filter_set_xx_ ss_pre_filter_set_xx_)
        )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          (setq ss_pre_filter_set_xx_hon_obj_ro_ (rad-to-deg (vla-get-rotation ss_pre_filter_set_xx_hon_obj_)))
          
          (vla-put-rotation ss_pre_filter_set_xx_hon_obj_ (deg-to-rad (TA:ModifyRotation ss_pre_filter_set_xx_hon_obj_ro_ -90)))
          
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;)
    )
    (defun c:rotate_clockwise_rcw ()
      ;Note By Code_Developer
      ;The principle of Codework is designed for block objects, where the commands
      ;will rotale block object only
      ;
      ;
      ;sub-FUNC
        (defun TA:ModifyRotation (Ex_rotation New_rotation)
          ;Note By Code_Developer
          ;The principle of this code is designed to modify the angle value, allowing for both increments and decrements based on a reference number.
          ;The reference number used must be a value representing degrees, ranging from 1 to 360.
          ;The number added or subtracted from the angle is calculated based on the number of degrees rotated, considering 1 full rotation as 360 degrees.
          ;
          (cond 
            ( ;normal_rotation_case_1
              (and 
                (>= New_rotation 0)
                (<= New_rotation 360)
              )
              (progn
                (princ "rotation_case_1\n")
                (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
                (if (<= new_rotaion_val_ 360)
                  (progn
                    (setq new_rotaion_val_ (+ Ex_rotation New_rotation))
                  )
                  (setq new_rotaion_val_ (- (+ Ex_rotation New_rotation) 360))
                )
                
                
              )
            )
            (;multi_time_rotation_case_2
              (and 
                (> New_rotation 360)
              )
              (progn 
                (princ "rotation_case_2\n")
                (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
                (setq round_time_val (* round_time_ 360))
                (setq new_rotaion_val_ (- New_rotation round_time_val))
                (if (> (+ Ex_rotation new_rotaion_val_ ) 360)
                (progn
                  (setq new_rotaion_val_ans_ (-  (+ Ex_rotation new_rotaion_val_ ) 360 ))
                )
                  (setq new_rotaion_val_ans_ (+ Ex_rotation new_rotaion_val_ ))
                )
                
              )
            )
            (;rotation_case_3
              (and 
                (<= New_rotation 0) 
              )
              (progn 
                (princ "rotation_case_3\n")
                (setq round_time_ (atoi (rtos (/ New_rotation 360) 2 0)))
                (setq round_time_val (* round_time_ 360))
                (setq new_rotaion_val_ (- New_rotation round_time_val))
                (setq Ex_rotation_val_ (+ Ex_rotation new_rotaion_val_))
                (if (< Ex_rotation_val_ 0)
                  (progn
                    (setq new_rotaion_val_ans_ (+ 360 Ex_rotation_val_))
                  )
                  (setq new_rotaion_val_ans_ Ex_rotation_val_ )
                )
                
              )
            )
            
          )
        )
      ;
      ;Exaample use
      ;no Example just use it !!!
      ;
      ;user_input_
        (initget "Yes No")
        (setq ss_ ;Detect the variable's result in case of a function error
          (vl-catch-all-apply 
            (function 
              (lambda () 
                (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
              )
            )
          )
        )
        (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
          (progn
            ;selection_set_for_fillter_blk_name
              (if  ;pre_select_ssget_or_post_select_ssget
                (=
                  (setq ss_pre_filter_set_xx_ (ssget "i"
                                                    (list
                                                      (cons 0 "insert") ;type of object
                                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                      ; (cons 62 0)           ;kind of color call sign with color code index
                                                    )
                                              )
                  )
                  nil
                )
                (progn
                  (setq ss_pre_filter_set_xx_ (ssget 
                                                    (list
                                                      (cons 0 "insert") ;type of object
                                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                      ; (cons 62 0)           ;kind of color call sign with color code index
                                                    )
                                              )
                  )
                )
                (sslength ss_pre_filter_set_xx_)
              )
            ;
          )
          (setq ss_pre_filter_set_xx_ ss_pre_filter_set_xx_)
        )
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          (setq ss_pre_filter_set_xx_hon_obj_ro_ (rad-to-deg (vla-get-rotation ss_pre_filter_set_xx_hon_obj_)))
          
          (vla-put-rotation ss_pre_filter_set_xx_hon_obj_ (deg-to-rad (TA:ModifyRotation ss_pre_filter_set_xx_hon_obj_ro_ 90)))
          
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;)
      
    )
  ;


  ;                                   
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
    (defun c:2d_blank_size ()
      ;user_input_
        (setq 2dblank_ename_ (car (entsel "specify Object")))
        (setq inserttion_point_ (getpoint "specify Point"))
        (setq user_input_height_ (cond ( (getreal (strcat "\nspecify_height blank size<" (rtos (setq user_input_height (cond (user_input_height) (1.0) ) ) ) "> : " ) ) ) (user_input_height) ) )
      ;
      ;get_data_
        (setq r2dblank_obj_ (vlax-ename->vla-object 2dblank_ename_))
        (setq 2dblank_objname_  (vla-get-objectname r2dblank_obj_))
      
        (setq vertex_list_  (TA:Get_Pline_vertext_ins_point_ 2dblank_ename_))
        (setq 2dblank_len_val_ (TA:get_vertex_len_ 2dblank_ename_))
      
      ;
      ;preloop_and_while for main_idea for insertion_dynamicblock_data_
        (setq 2dblank_len_val_i 0)
        (while (< 2dblank_len_val_i (length 2dblank_len_val_))
          (setq 2dblank_len_val_width_ (nth  2dblank_len_val_i 2dblank_len_val_))
          
          ;main_idea_
            (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_startpoint" inserttion_point_ 1 0 )
            (setq new_dynamic_block_ (entlast))
            (setq new_dynamic_block_obj_ (vlax-ename->vla-object new_dynamic_block_))
            (LM:setdynpropvalue new_dynamic_block_obj_ "H" user_input_height_)
            (LM:setdynpropvalue new_dynamic_block_obj_ "W" 2dblank_len_val_width_)
          ;
          ;incaseloop_coordinate_x_axis
            (setq inserttion_point_ (list
                                        (+ (car inserttion_point_ ) 2dblank_len_val_width_ )
                                        (cadr inserttion_point_ )
                                      )
            )
          ;
          (setq 2dblank_len_val_i (+ 2dblank_len_val_i 1))
        )
      ;
        
      ;
      
    )
    (defun c:temp1_tte1 ()
      (setq p1_ename_ (car (entsel "specify Object")))
      (setq p2_ename_ (car (entsel "specify Object")))
      (setq p1_obj_ (vlax-ename->vla-object p1_ename_))
      (setq p2_obj_ (vlax-ename->vla-object p2_ename_))
      (setq p1_obj_start_end_pt_ (list
                                      (setq p1_obj_startpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-startpoint p1_obj_ ))
                                                              )
                                      )
                                      (setq p1_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint p1_obj_ ))
                                                            )
                                      )
                                    )
      )
      (setq p2_obj_start_end_pt_ (list
                                      (setq p2_obj_startpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-startpoint p2_obj_ ))
                                                              )
                                      )
                                      (setq p2_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint p2_obj_ ))
                                                            )
                                      )
                                    )
      )
      ; (setq ptpt_ (remove_member_list_ 2 (list p1_obj_startpt_ p2_obj_startpt_ p2_obj_startpt_ p2_obj_endpt_)))
      (setq ptpt_ (list p1_obj_startpt_ p1_obj_endpt_ p2_obj_endpt_ p2_obj_startpt_ ))
      
      (create-plines ptpt_ "close")
      
    )
    (defun c:DAFT_insert_to_mid_pline_ins1_ ()
      (setq pline_ename_ (car (entsel "specify Object")))
      (setq pline_obj_ (vlax-ename->vla-object pline_ename_))
      (setq vt_point_ (cadr (TA:Get_Pline_vertext_ins_point_ pline_ename_)))
      
      ; (setq H_VAL (cond ( (getreal (strcat "\nSpecify Dim Scale Overall <" (rtos (setq H_VAL (cond (H_VAL) (1.0) ) ) ) "> : " ) ) ) (H_VAL) ) )
      (setq H_VAL_MAX 112.5 )
      (setq H_VAL_MIN 50 )
      
      ;preloop_and_while
        (setq vt_point_i 0)
        (setq vt_point_ii 1)
        (while (< vt_point_i (length vt_point_))
          (setq vt_point_ename_pt1 (nth  vt_point_i vt_point_))
          (if (= vt_point_ii (length vt_point_))
            (progn
              (setq vt_point_ename_pt2 (nth  0 vt_point_))
            )
            (setq vt_point_ename_pt2 (nth  vt_point_ii vt_point_))
          )
          
      
          (setq vt_point_ename_mid_ (TA:midpoint vt_point_ename_pt1 vt_point_ename_pt2))
          (setq vt_distance_ (distance vt_point_ename_pt1 vt_point_ename_pt2))
          
          (setq vt_angle_ (rad-to-deg (angle vt_point_ename_pt1 vt_point_ename_pt2)))
          
          (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" vt_point_ename_mid_ 1 vt_angle_ )
          (setq new_insert_blk_ (entlast))
          (setq new_insert_blk_obj_ (vlax-ename->vla-object new_insert_blk_))
          ; (LM:setdynpropvalue new_insert_blk_obj_ "H" H_VAL )
          
          (if (or (= vt_point_i 0) (= vt_point_i 2) )
            (progn
              (LM:setdynpropvalue new_insert_blk_obj_ "W" (- (- vt_distance_ (* 50 2) ) 50))
              (LM:setdynpropvalue new_insert_blk_obj_ "H" 50 )
            )
            
            (LM:setdynpropvalue new_insert_blk_obj_ "H" 75 )
          )
          (if (or (= vt_point_i 0) (= vt_point_i 2) )
            (progn
              (LM:setdynpropvalue new_insert_blk_obj_ "W" (- (- vt_distance_ (* 50 2) ) 50))
              (LM:setdynpropvalue new_insert_blk_obj_ "H" 50 )
            )
            (LM:setdynpropvalue new_insert_blk_obj_ "W" vt_distance_)
            
          )
          
          
          (setq vt_point_i (+ vt_point_i 1))
          (setq vt_point_ii (+ vt_point_ii 1))
        )
      ;
    )
    (defun c:ztest_ztt ()
      ;this_code_only_work_with_"A$Cc9a77f15"
      (setq pline_ename_ (car (entsel "specify Object")))
      (setq insertion_point_ (getpoint ))
      (command "insert" "A$Cc9a77f15" insertion_point_ 1 0)
      (TA:Get_Pline_vertext_angle_case1 pline_ename_)
      (setq tt_len_ (TA:get_vertex_len_ pline_ename_))
      (setq tt_len_total_ (length tt_len_))
      (setq dynamic_ename_ (entlast))
      (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
      (LM:setdynpropvalue dynamic_obj_ "Visibility1" tt_len_total_)

      ;preloop_and_while
        (setq tt_len_i 0)
        (setq tt_len_ii 1) 
        (while (< tt_len_i (length tt_len_))
          (setq tt_len_num_ (nth  tt_len_i tt_len_))
          (setq tt_len_parametername_ (strcat "LINE" (rtos tt_len_ii 2 0)))
          (LM:setdynpropvalue dynamic_obj_ tt_len_parametername_ tt_len_num_)
          (setq tt_len_i (+ tt_len_i 1))
          (setq tt_len_ii (+ tt_len_ii 1))
        )
      ;
      
    )
    (defun c:increment_rotation_to_block_inc1_ ()
      ;DES from dev
      ;this is code work with Block only by process of code was design by selection set and filter name , sorting corrdinate axis block from SUB-FUNC TA:EF_Filter_ss_set_ 
      ;TA:standard_list_croodinate_sorting 
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
      ;filter and sorting axis selection set
        (setq ssget_efname_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "001 - AF100 VIEW 1"))
        (setq ssget_efname_filter_sorted_X_ (TA:standard_list_croodinate_sorting ssget_efname_filter_ "X"))
      ;
      ;user_input for increment rotation angle of block 
        (setq user_num_i (getreal "increment rotation angle of block "))
        (setq time_i 1)  
      ;
      ;preloop_and_while
        (setq ssget_efname_filter_sorted_X_i 0)
        (while (< ssget_efname_filter_sorted_X_i (length ssget_efname_filter_sorted_X_))
          (setq ssget_efname_filter_sorted_X_ename_ (car (nth  ssget_efname_filter_sorted_X_i ssget_efname_filter_sorted_X_)))
          (setq ssget_efname_filter_sorted_X_obj_ (vlax-ename->vla-object ssget_efname_filter_sorted_X_ename_))
          

            (setq get-rotation_angle (rad-to-deg (vla-get-rotation ssget_efname_filter_sorted_X_obj_ )))
            (if (< user_num_i 0)
              (progn
                (setq put-rotation (- get-rotation_angle  (* (* user_num_i time_i) -1)))
              )
                (setq put-rotation (+ get-rotation_angle  (* (* user_num_i time_i) -1)))
            )
            
            (vla-put-rotation ssget_efname_filter_sorted_X_obj_ (deg-to-rad put-rotation))

          
          
          (setq time_i (+ time_i 1))
          (setq ssget_efname_filter_sorted_X_i (+ ssget_efname_filter_sorted_X_i 1))
        )
      ;
      
      
      
    )
    (defun c:ccc1_ ()
      (setq cc_ename_ (car (entsel "specify Object")))
      (setq cc_obj_ (vlax-ename->vla-object cc_ename_))
      (setq cc_obj_get_rotation_ (vla-get-rotation cc_obj_))
      (setq name_block_ (strcat "000CUS - AF-100_END_CAP_" (rtos (rad-to-deg cc_obj_get_rotation_) 2 2) "_DEG"))

      (setq ss_set_ (ssget))
      (setq blk_get_pt_ (getpoint ))
      (command "_block"  name_block_ blk_get_pt_ ss_set_ "" )
      (command "insert" name_block_ blk_get_pt_ 1 1 0)
    )
  ;





  (defun c:Dynamic_make_welding_line_[mkWL] ()
    (setq pline_ename_ (car (entsel "specify Object")))
    (setq pline_obj_ (vlax-ename->vla-object pline_ename_))
    (if 
      (= 
        (setq errro_checking_ (length (cadr (TA:Get_Pline_vertext_ins_point_ pline_ename_))))
        2
      )
      (progn
        (setq pline_obj_vertex_list_ (cadr (TA:Get_Pline_vertext_ins_point_ pline_ename_))) 
        (setq pline_obj_vertex_len_list_ (TA:get_vertex_len_ pline_ename_))
        (setq pline_obj_vertex_len_list_endset_ (- (length pline_obj_vertex_list_) 1) )
      )
    )
    (if 
      (>
        (setq errro_checking_ (length (cadr (TA:Get_Pline_vertext_ins_point_ pline_ename_))))
        2
      )
      (progn
        (setq pline_obj_vertex_list_ (TA:Get_Pline_vertext_angle_case1 pline_ename_))
        (setq pline_obj_vertex_len_list_ (TA:get_vertex_len_ pline_ename_))
        (setq pline_obj_vertex_len_list_endset_ (- (length pline_obj_vertex_list_) 1))
      )
      
    )
    ;preloop_and_while
      (setvar "osmode" 0)
      (setq pline_obj_vertex_list_i 0)
      (while (< pline_obj_vertex_list_i pline_obj_vertex_len_list_endset_)
        (setq pline_obj_vertex_lists_ (nth pline_obj_vertex_list_i pline_obj_vertex_list_))
        
          ;get_data_
            (if (> pline_obj_vertex_len_list_endset_ 2)
              (progn
                (setq pline_obj_vertex_len_lists_length_ (nth  pline_obj_vertex_list_i pline_obj_vertex_len_list_))
                (setq pline_obj_vertex_lists_ins_point (car pline_obj_vertex_lists_))
                (setq pline_obj_vertex_lists_rotation (cadr pline_obj_vertex_lists_))
              )
              (setq pline_obj_vertex_lists_ins_point pline_obj_vertex_lists_ 
                    pline_obj_vertex_lists_rotation (rad-to-deg (angle (car pline_obj_vertex_list_ ) (cadr pline_obj_vertex_list_)))
                    pline_obj_vertex_len_lists_length_ (distance (car pline_obj_vertex_list_ ) (cadr pline_obj_vertex_list_) )
              
              )
              
            )
          ;

        ;main_idea_code_
          ;insertion_process
          (command "insert" "000 - DYNAMIC SYMBOL WELDS LINE" 
                  pline_obj_vertex_lists_ins_point
                  0.5
                  pline_obj_vertex_lists_rotation
                  
          ) 
          ;
          ;put-data_process_
            (LM:setdynpropvalue (vlax-ename->vla-object (entlast) ) "total_length" pline_obj_vertex_len_lists_length_  )
            (setq entlast_obj_put_color (vla-put-color (vlax-ename->vla-object (entlast) ) 1)) ;0 = bylock, 256 = bylayer
            (setq entlast_obj_put_layer (vla-put-layer (vlax-ename->vla-object (entlast) ) "001 - ASCESSORIES")) ;by_string_name_layer
          ;
        ;
        (setq pline_obj_vertex_list_i (+ pline_obj_vertex_list_i 1))
      )
      (setvar "osmode" 1215)
    ;
  )

  (defun c:sumArea_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
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
      (setq sum_area_ 0)
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq ss_pre_filter_set_xx_ojb_get_area_ (vla-get-area ss_pre_filter_set_xx_hon_obj_))
        (setq sum_area_ (+ sum_area_ ss_pre_filter_set_xx_ojb_get_area_) )
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )

  (defun TA:delete_duplucate_coord_list_ (coord_list_ user_input_case less-than_symbol )
    ;Note By Code_Developer
    ;This command is designed to work exclusively with coord_list
    ;The principle of the code will read coordinate x or y that specify from argument of code,
    ;then filter single coordinates to create a sequence,
    ;
    ;examle_argument_
      ; (setq less-than_sym '>)
      ; (TA:delete_duplucate_coord_list_ coord_list_ "X" '<)
      ; (TA:delete_duplucate_coord_list_ coord_list_ "X" '>)
      ; (TA:delete_duplucate_coord_list_ coord_list_ "Y" '>)
      ; (TA:delete_duplucate_coord_list_ coord_list_ "Y" '>)
    ;
    ;ususe_code_
      ; (cond ;less-than_sym
      ;   (;lass-than_case_1
      ;      (and
      ;         (= less-than_sym "less")

      ;      )
      ;      (progn
      ;        (setq less-than_symbol '<)
      ;        (princ "lass-than_case_1")
      ;      )
      ;   )
      ;   (;lass-than_case_2
      ;      (and
      ;         (= less-than_sym "than")

      ;      )
      ;      (progn
      ;        (setq less-than_symbol '>)
      ;        (princ "lass-than_case_2")
      ;      )
      ;   )
      ; )
    ;
    ;preloop_and_while
      (setq duplicate_coord_ ())
      (setq coord_list_i 0)
      (while (< coord_list_i (length coord_list_))
        (setq coord_list_xyz_ (nth coord_list_i coord_list_))
        (cond
          (;user_input_case_1
            (and
                (= user_input_case "X")
            )
            (progn
              (setq coord_list_x_ (car coord_list_xyz_))
              (setq duplicate_coord_ (cons coord_list_x_ duplicate_coord_))
              ; (setq duplicate_coord_ (LM:Unique duplicate_coord_))
              (setq duplicate_coord_ (vl-sort duplicate_coord_ less-than_symbol ))
              (princ "user_input_case_1")
            )
          )
          (;user_input_case_2
            (and
                (= user_input_case "Y")
            )
            (progn
              (setq coord_list_y_ (cadr coord_list_xyz_))
              (setq duplicate_coord_ (cons coord_list_y_ duplicate_coord_))
              ; (setq duplicate_coord_ (LM:Unique duplicate_coord_))
              (setq duplicate_coord_ (vl-sort duplicate_coord_ less-than_symbol ))
              (princ "user_input_case_2")
            )
          )
        )
        (setq duplicate_coord_ duplicate_coord_)
        (setq coord_list_i (+ coord_list_i 1))
      )
    ;
    ;
    ;summay_value_
      (setq duplicate_coord_ duplicate_coord_)
    ;
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
  

  (defun LM:Unique ( l )
      (if l (cons (car l) (LM:Unique (vl-remove (car l) (cdr l)))))
  )
  (defun LM:Unique ( l / x r )
      (while l
          (setq x (car l)
                l (vl-remove x (cdr l))
                r (cons x r)
          )
      )
      (reverse r)
  )
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




   




  


  (defun c:qmsave ()
    (setq FILEPATH (getvar "DWGPREFIX"))
    (setq FILENAME (getvar "DWGNAME"))
    (setq sum (strcat FILEPATH FILENAME))
    (COMMAND "_SAVEAS" 
            "2010"
            sum
            "Y"
            
            
            
    )
    (command "close")

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
    (setq Filter_target_ename_ (car (entsel "specify for filter selection set Object")))
    (setq Filter_target_obj_ (vlax-ename->vla-object Filter_target_ename_))
    (setq Filter_target_obj_EFname_ (LM:effectivename Filter_target_obj_))
    (setq ss_post_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ Filter_target_obj_EFname_))
  ;
  ;insert_target_object_process_
    (setq insert_target_ename_ (car (entsel "specify for replace Object ")))
    (setq insert_target_obj_ (vlax-ename->vla-object insert_target_ename_))
    (setq insert_target_obj_EFname_ (LM:effectivename insert_target_obj_))
  ;
  ;preloop_and_while
    (setq ss_post_filter_set_xx_i 0)
    (setq ss_newset_ (ssadd))
    ; (sslength ss_newset)
    (while (< ss_post_filter_set_xx_i (sslength ss_post_filter_set_xx_))
      (setq ss_post_filter_set_xx_ename_ (ssname ss_post_filter_set_xx_ ss_post_filter_set_xx_i))
        (setq ss_post_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_post_filter_set_xx_ename_))
          (setq ss_post_filter_set_xx_hon_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_post_filter_set_xx_hon_obj_))))
          (setvar "osmode" 0)
          (TA:set-block-blockscaling insert_target_obj_EFname_ acUniform)
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





(defun TA:make_line_for_void (border_main_)
  
  (setq line_list_ (ssadd))
  (sslength line_list_)
  (setq cneter_base_ (LM:PolyCentroid border_main_))
      (command "line" (LM:PolyCentroid border_main_)  (polar (LM:PolyCentroid border_main_) (deg-to-rad 45) 50) "" )
      (setq line_list_ (ssadd (entlast) line_list_))
      (command "line" (LM:PolyCentroid border_main_)  (polar (LM:PolyCentroid border_main_) (deg-to-rad 135) 50) "" )
      (setq line_list_ (ssadd (entlast) line_list_))
      (command "line" (LM:PolyCentroid border_main_)  (polar (LM:PolyCentroid border_main_) (deg-to-rad 225) 50) "" )
      (setq line_list_ (ssadd (entlast) line_list_))
      (command "line" (LM:PolyCentroid border_main_)  (polar (LM:PolyCentroid border_main_) (deg-to-rad 315) 50) "" )
      (setq line_list_ (ssadd (entlast) line_list_))
      
  
  (setq line_list_ line_list_)
)

(defun c:Make_void_polygon_MKVP ()
  ;Note By Code_Developer
  ;The principle of codework is making a void in polygon objects, which is suitable for objects with more than 4 sides.
  ;The process of code is finding centroid of polygon and create intersect line to the centroid polygon
  ;
  ;
  ;
  ; (setq main_ename_ (car (entsel "specify Object")))
  ;user_input_object_
    (setq border_main_ (car (entsel "specify Object border_main_")))
  ;
  ;user_input_value 
  (setq segment_pol (cond ( (getint (strcat "\nspecify segment_pol <" (rtos (setq segment_pol (cond (segment_pol) (1.0) ) ) ) "> : " ) ) ) (segment_pol) ) )
  (setq rad_dia_ (cond ( (getint (strcat "\nspecify fllet_rec_ <" (rtos (setq rad_dia_ (cond (rad_dia_) (1.0) ) ) ) "> : " ) ) ) (rad_dia_) ) )
  ;
  ;setvar osmode
    (setvar "osmode" 0)
  ;
  ;
  (setq ss_pre_filter_set_xx_ (TA:make_line_for_void border_main_ ))
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq main_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq main_obj_ (vlax-ename->vla-object main_ename_))
        (cond
          (;_case_1
            (and
                (= (vla-get-objectname main_obj_) "AcDbLine")
                
            )
            (progn
              (setq main_obj_start_end_pt_ (list
                                              (setq main_obj_startpt_ (vlax-safearray->list
                                                                          (vlax-variant-value ( vla-get-startpoint main_obj_ ))
                                                                        )
                                              )
                                              (setq main_obj_endpt_ (vlax-safearray->list
                                                                        (vlax-variant-value ( vla-get-endpoint main_obj_ ))
                                                                      )
                                              )
                                            )
              )
              
              (princ "_case_1")
            )
          )
          ; (;_case_2
          ;    (and
          ;       ()
          ;       ()
          ;    )
          ;    (progn
          ;      ()
          ;      (princ "_case_2")
          ;    )
          ; )
          ; (;_case_3
          ;    (and
          ;       ()
          ;       ()
          ;    )
          ;    (progn
          ;      ()
          ;      (princ "_case_3")
          ;    )
          ; )
          ; (;_case_4
          ;    (and
          ;       ()
          ;       ()
          ;    )
          ;    (progn
          ;      ()
          ;      (princ "_case_4")
          ;    )
          ; )
        ) 
        ; ;head-set
        ;   (command "polygon" segment_pol (car main_obj_start_end_pt_ ) "C" rad_dia_ )
        ;   (setq new_head_pt_ (entlast))
        ; ;
        ;end-set
          (command "polygon" segment_pol (cadr main_obj_start_end_pt_ ) "C" rad_dia_ )
          (setq new_end_pt_ (entlast))
        ;
        ; ;head_line
        ;   (setq new_head_pt_list_ (cadr (TA:Get_Pline_vertext_ins_point_ new_head_pt_ )))
        ;   (command "_extend" main_ename_ "" "b" border_main_ "" "f" (car new_head_pt_list_) (cadr new_head_pt_list_) (caddr new_head_pt_list_) (cadddr new_head_pt_list_) (car new_head_pt_list_) "" "")
        ;   (vla-delete (setq  1_obj_ (vlax-ename->vla-object  new_head_pt_)))
        ; ;
        ;end_line
          (setq new_end_pt_list_ (cadr (TA:Get_Pline_vertext_ins_point_ new_end_pt_ )))
          (command "_extend" main_ename_ "" "b" border_main_ "" "f" (car new_end_pt_list_) (cadr new_end_pt_list_) (caddr new_end_pt_list_) (cadddr new_end_pt_list_) (car new_end_pt_list_) "" "")
          (vla-delete (setq  2_obj_ (vlax-ename->vla-object  new_end_pt_)))
        ;
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ; 
  ;return osmode
    (setvar "osmode" 1215)
  ;
)
(defun c:Make_void_rectangle_MKVR ()
  ;Note By Code_Developer
  ;The principle of codework is making a void in polygon objects, which is suitable for objects with 4 sides.
  ;The process of code is finding centroid of polygon and create intersect line to the centroid polygon
  ;
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq newcrossline_ssset_ (ssadd))
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq border_main_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      
      ;main_idea
        (setq center_pt_ (TA:find_center border_main_) )
        (setq new_pt_ (cadr (TA:Get_Pline_vertext_ins_point_ border_main_)))
        ;preloop_and_while
          (setq new_pt_i 0)
          (while (< new_pt_i (length new_pt_))
            (setq new_pt_value_ (nth  new_pt_i new_pt_))
            (command "line" center_pt_  new_pt_value_ "" )
            ;get_newcrossline_object_
              (setq newcrossline_ename_ (entlast))
              (setq newcrossline_obj_ (vlax-ename->vla-object newcrossline_ename_))
              (vla-put-color newcrossline_obj_ 8)
              (vla-put-linetype newcrossline_obj_ "Hidden")
              (vla-put-linetypescale newcrossline_obj_ 10)
            ;
            ;add_new_selection_set_
              (setq newcrossline_ssset_ (ssadd newcrossline_ename_ newcrossline_ssset_ ))
              (sslength newcrossline_ssset_)
            ;
            
            (setq new_pt_i (+ new_pt_i 1))
          )
        ;
        ;add selection set for new object
          (command "pedit" "m" newcrossline_ssset_ "" "Y" "j" "" "" )
        ;
      ;
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;return OSMODE
    (setvar "osmode" 1215)
  ;
  ;
    (initget "Yes No")
    (setq ss_ ;Detect the variable's result in case of a function error
      (vl-catch-all-apply 
        (function 
          (lambda () 
            (setq DELETE_OBJ_ (not (eq "No" (getkword "\nDelete block with object? [Yes/No] <Y>: "))))
          )
        )
      )
    )
    (if (= ss_ T )
      (progn
        (command "erase" ss_pre_filter_set_xx_ ""  )
      )
    )

  ;
)


(defun c:PASONMK_TITLE_BLOCK_ ()
  

  (setq type_liscene_ename_ (car (entsel "specify Object")))
  (setq type_liscene_obj_ (vlax-ename->vla-object type_liscene_ename_))
  (setq type_liscene_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint type_liscene_obj_))))
  (setq type_liscene_obj_dyn_width (- (/ (LM:getdynpropvalue type_liscene_obj_ "total_width") 2) 2.5))
  (setq type_liscene_obj_dyn_line_height (- (LM:getdynpropvalue type_liscene_obj_ "line_height") 4))
  (setq type_liscene_obj_att_ (LM:vl-getattributevalue type_liscene_obj_ "TYPE_LICENSE"))
  
  
    (if (wcmatch type_liscene_obj_att_ "* *")
      (progn
        (setq type_liscene_obj_att_ (LM:StringSubst "_" " " type_liscene_obj_att_))
      )
    )


      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))  
      (setq att-ins_
                  (list
                    (setq type_liscene_obj_ins_stpt_att_x (- (car type_liscene_obj_ins_ ) type_liscene_obj_dyn_width))
                    (setq type_liscene_obj_ins_stpt_att_y (- (cadr type_liscene_obj_ins_ ) 9))
                    0
                  )
      )
      (setq loop_length (+ (/ type_liscene_obj_dyn_line_height 4) 1 ))
      (setq loop_ii 0)
    (while
      (< loop_ii loop_length)
      
      (vla-addattribute modelSpace 
                        1.5
                        acAttributeModeLockPosition
                        "NEW_TAG"
                        (vlax-3d-point att-ins_)
                        (strcat type_liscene_obj_att_ "_value_" (rtos (+ loop_ii 1) 2 0) )
                        "NEW_MAN"
      )
      (setq att-ins_
                  (list
                    (setq type_liscene_obj_ins_stpt_att_x (- (car type_liscene_obj_ins_ ) type_liscene_obj_dyn_width))
                    (setq type_liscene_obj_ins_stpt_att_y (- type_liscene_obj_ins_stpt_att_y 4))
                    0
                  )
      )
      (setq loop_ii ( + loop_ii 1 ))
      
    )

)
(defun c:pamk1_TITLE_BLOCK_ ()
  

  (setq type_liscene_ename_ (car (entsel "specify Object")))
  (setq type_liscene_obj_ (vlax-ename->vla-object type_liscene_ename_))
  (setq type_liscene_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint type_liscene_obj_))))
  (setq type_liscene_obj_dyn_width (- (/ (LM:getdynpropvalue type_liscene_obj_ "total_width") 2) 2.5))
  (setq type_liscene_obj_dyn_line_height (+ (LM:getdynpropvalue type_liscene_obj_ "line_height") 5))
  (setq type_liscene_obj_att_ (LM:vl-getattributevalue type_liscene_obj_ "TYPE_LICENSE"))
  
  
    (if (wcmatch type_liscene_obj_att_ "* *")
      (progn
        (setq type_liscene_obj_att_ (LM:StringSubst "_" " " type_liscene_obj_att_))
      )
    )


      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))  
      (setq att-ins_
                  (list
                    (setq type_liscene_obj_ins_stpt_att_x (- (car type_liscene_obj_ins_ ) type_liscene_obj_dyn_width))
                    (setq type_liscene_obj_ins_stpt_att_y (- (cadr type_liscene_obj_ins_ ) type_liscene_obj_dyn_line_height))
                    0
                  )
      )
      (setq loop_length (+ (/ (- type_liscene_obj_dyn_line_height 5) 4) 1 ))
      (setq loop_ii 1)
    (while
      (> loop_length loop_ii )
      
      (vla-addattribute modelSpace 
                        1.5
                        acAttributeModeLockPosition
                        "NEW_TAG"
                        (vlax-3d-point att-ins_)
                        (strcat type_liscene_obj_att_ "_value_" (rtos (- loop_length 1) 2 0) )
                        "NEW_MAN"
      )
      (setq att-ins_
                  (list
                    (setq type_liscene_obj_ins_stpt_att_x (- (car type_liscene_obj_ins_ ) type_liscene_obj_dyn_width))
                    (setq type_liscene_obj_ins_stpt_att_y (+ type_liscene_obj_ins_stpt_att_y 4))
                    0
                  )
      )
      (setq loop_length ( - loop_length 1 ))
      
    )

)





(defun c:textlength_TL ()
    (setq ss_ename_ (car (entsel "specify Object")))
    (setq ss_obj_ (vlax-ename->vla-object ss_ename_))
    (if (= (LM:Effectivename ss_obj_) "000_TEXT_length") ;on work w/ block name "000_TEXT_length"
      (progn
        (LM:setdynpropvalue ss_obj_ "distance1" 0)
        (setq ss_obj_boundary_ (cdr(TA:ename+vla-getboundingbox ss_obj_)))
        (setq tolal_text_lenth_ (distance 
                                  (nth 0 ss_obj_boundary_)
                                  (list 
                                    (car (nth 1 ss_obj_boundary_))
                                    (cadr (nth 0 ss_obj_boundary_))
                                  )
                                )
        )
        (setq subtotal_A (strlen (LM:vl-getattributevalue ss_obj_ "Heading_tag")))
        (LM:setdynpropvalue ss_obj_ "distance1" tolal_text_lenth_)
      )
    )
  
  
)









;RASTER MACHNE FUNC
(defun c:Foldline_ ()
  ;Foldline_process_
  (setq input_PT (Getpoint "specify point"))
  (setvar "osmode" 0)
  (setq PL_ename_ (car (entsel "specify Object")))
  (setq PL_L_ (TA:get_vertex_len_ PL_ename_ )) 
  ;preloop_and_while
    (setq PL_L_i 0)
    (while (< PL_L_i (length PL_L_))
      (setq PL_x (nth  PL_L_i PL_L_))
      (setq PL_xyz (list 
                     (+ PL_x (car input_PT)) 
                     (cadr input_PT) 
                     0
                   )
      )
      (command "line"
               input_PT
               PL_xyz
               ""
      )
      (if 
        (or
          (= PL_L_i 0)
          (= PL_L_i 2)
          (= PL_L_i 4)
          (= PL_L_i 6)
          (= PL_L_i 8)
          (= PL_L_i 10)
          (= PL_L_i 12)
          (= PL_L_i 14)
          (= PL_L_i 16)
          )
        (progn
          (vla-put-color (vlax-ename->vla-object (entlast)) 3)
        )
        (vla-put-color (vlax-ename->vla-object (entlast)) 1)
      )
      
      (setq input_PT PL_xyz )
      (setq PL_L_i (+ PL_L_i 1))
    )
  ;
  (setvar "osmode" 1215)
)
(defun c:F0 () ;คำสั่งหมุนแบบง่าย
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

  (initget "Yes No")
  (setq ss_ ;Detect the variable's result in case of a function error
    (vl-catch-all-apply 
      (function 
        (lambda () 
          (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
        )
      )
    )
  )
  (if (and (= ss_ nil)  )
    (progn
      
      (setq input_RO_PT_ (car (entsel "Specify block "))
          input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ ))))
      )
    )
    (setq input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ )))))
    
  )
  
  
  (command "rotate" ss_pre_filter_set_xx_ "" input_RO_PT_ins 90)
)
(defun c:F90 () ;คำสั่งหมุนแบบง่าย
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

  (initget "Yes No")
  (setq ss_ ;Detect the variable's result in case of a function error
    (vl-catch-all-apply 
      (function 
        (lambda () 
          (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
        )
      )
    )
  )
  (if (and (= ss_ nil)  )
    (progn
      
      (setq input_RO_PT_ (car (entsel "Specify block "))
          input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ ))))
      )
    )
    (setq input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ )))))
    
  )
  
  
  (command "rotate" ss_pre_filter_set_xx_ "" input_RO_PT_ins 270)
)
(defun c:QMM () ;คำสั่งหมุนแบบง่าย
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

  (initget "Yes No")
  (setq ss_ ;Detect the variable's result in case of a function error
    (vl-catch-all-apply 
      (function 
        (lambda () 
          (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
        )
      )
    )
  )
  (if (and (= ss_ nil)  )
    (progn
      
      (setq input_RO_PT_ (car (entsel "Specify block "))
          input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ ))))
      )
    )
    (setq input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ )))))
    
  )
  (setq input_RO_PT_ins_end (list (+ 250 (car input_RO_PT_ins)) (cadr input_RO_PT_ins) 0) )
  
  (command "mirror" ss_pre_filter_set_xx_ "" input_RO_PT_ins input_RO_PT_ins_end "Y"  )
  
  
  
)
(defun c:STAMP_START_CIRCLE_AT_LINE_CCL ()
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
      (setq new_pt_line_ (TA:Get_line_ins_point_ ss_pre_filter_set_xx_ename_ ))
      (command "circle" (car new_pt_line_ ) 3  )
      
   
      (if (= ss_pre_filter_set_xx_i 0)
        (progn
          (command "circle" (cadr new_pt_line_ ) 3  )
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      
    )
  ;  
  
  
)



(defun c:Selection_make_boudary_line_SSBOL ()
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
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq lower_upper_inspoint_ (TA:Find_boundary_line_ ss_pre_filter_set_xx_ename_))
          (command "rectangle" (car lower_upper_inspoint_) (cadr lower_upper_inspoint_))
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ; 
)
(defun c:find_area_ ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq area_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_hon_obj_area_ (vla-get-area ss_pre_filter_set_xx_hon_obj_ ))
      (setq area_i (+ area_i ss_pre_filter_set_xx_hon_obj_area_))
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;  
)






(defun c:add_sorted_point->list_ ()
  (setq MAIN_BLK_ename_ (car (entsel "specify Object")))
  (setq Main_BLK_obj_ (vlax-ename->vla-object Main_BLK_ename_))
  (setq k1_ (rad-to-deg (vla-get-angle Main_BLK_obj_ )))
  (setq k2_ (vla-get-length Main_BLK_obj_ ))
  (setq k3_ (list k1_ k2_))
  (setq sum (cons k3_ sum ))
)
(defun TAS:MAKE_AREA_SELECTION (test_point_)
  ;example_user_input_
    ; (setq test_point_ (getpoint))
    (setvar "osmode" 0)
  ;
  ;existing_area_list_
    (setq selection_area_list_ (list (list 40.5877 50.9799) 
                                    (list 77.3102 106.0)
                                    (list 167.31 12.0)
                                    (list 128.925 37.0)
                                    (list 90.0 13.7238)
                                    (list 180.0 215.348)
                                    (list 270.0 312.079)
                                    (list 3.42803e-14 380.03)
                                    (list 90.0 127.355)
                                    (list 180.0 191.728)
                                    (list 90.0 3.0)
                              )
    )
  ;
  ;preloop_and_while
    (setq sumpoint_ ())
    (setq selection_area_list_i 0)
    (while (< selection_area_list_i (length selection_area_list_))
      (setq selection_area_list_ang_ (car (nth  selection_area_list_i selection_area_list_)))
      (setq selection_area_list_length_ (cadr (nth  selection_area_list_i selection_area_list_)))
      (setq test_point_in_ test_point_)
      (setq test_point_out_ (polar 
                              test_point_in_
                              (deg-to-rad selection_area_list_ang_)
                              selection_area_list_length_
                            )
      )
      (setq test_point_ test_point_out_)
      ; (command "point" test_point_ )
      (setq sumpoint_ (cons test_point_ sumpoint_))
      (setq selection_area_list_i (+ selection_area_list_i 1))
    )
    (setvar "osmode" 1215)
    (princ (setq sumpoint_ (reverse sumpoint_)))
  ;
)
(defun c:E1TEST ()
  
  ;preset_user_input_
    (setq method_1                                  "1 = fold up 90") 
    (setq method_2                                  "2 = fold up 0") 
    (setq method_3                                  "3 = move ") 
    (setq method_4                                  "4 = mirror ") 
    (setq method_5                                  "5 = copy step ") 
  ;
  ;user_input_
    (initget "Yes No")
    (setq ss_ ;Detect the variable's result in case of a function error
      (vl-catch-all-apply 
        (function 
          (lambda () 
            (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
          )
        )
      )
    )
    (if (and (= ss_ nil)  )
      (progn
        (setq input_RO_PT_    (car (entsel "Specify block "))
              input_RO_PT_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_)) ) )
              input_RO_BOL_ (TA:ename+vla-getboundingbox (vlax-ename->vla-object input_RO_PT_))
              
        )
        (setq distance1_ (LM:getdynpropvalue (vlax-ename->vla-object input_RO_PT_ ) "distance1" ))
        (setq distance1x_ distance1_)
        (setq offset_pt_ 100)
        (setq ii-i 1)
      )
      (setq input_RO_PT_ins (vlax-safearray->list (vlax-variant-value(vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_ ))))
            input_RO_BOL_ (TA:ename+vla-getboundingbox (vlax-ename->vla-object input_RO_PT_))
            
      )
    )
  ;
  ;ss area
    (setq ssset_boundary_ (ssget "WP" 
                                (setq input_RO_PT_ins_PT_LIST (TAS:MAKE_AREA_SELECTION (list 
                                                                                         (+ 0.01 (car input_RO_PT_ins))
                                                                                         (cadr input_RO_PT_ins)
                                                                                         (caddr input_RO_PT_ins)
                                                                                       ) 
                                                              ) 
                                )
                                (list 
                                  (cons 0 "line")
                                )
                          )
    )
  
  ;
  ;preloop_and_while_
    (setq LV_1_ans_ nil)
    (while (/= (vl-catch-all-error-p LV_1_ans_ ) T)
      ;error_detect_
      (setq LV_1_ans_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq LV_1_ans_int_ (cond ( (getint (strcat method_1 "\n" method_2 "\n" method_3 "\n" method_4 "\n" method_5 "\n<" (rtos (setq LV_1_ans_int_ (cond (LV_1_ans_int_) (1.0) ) ) ) "> : " ) ) ) (LV_1_ans_int_) ) )
              
            )
          )
        )
      )
      (if (= (vl-catch-all-error-p LV_1_ans_ ) T)
        (progn
          (vl-catch-all-error-message LV_1_ans_ )
        )
      )
      ;
      
      ;condition_for_folding_control
        (cond
          (;raster_case_1
            (and
              (= LV_1_ans_int_ 1)
              (/= (vl-catch-all-error-p LV_1_ans_ ) T)
            )
            (progn
              
              (command "rotate" ssset_boundary_ "" input_RO_PT_ins 270)
              (princ "raster_case_1")
            )
          )
          (;raster_case_2
            (and
              (= LV_1_ans_int_ 2)
              (/= (vl-catch-all-error-p LV_1_ans_ ) T)
            )
            (progn
              
              (command "rotate" ssset_boundary_ "" input_RO_PT_ins 90)
              (princ "raster_case_2")
            )
          )
          (;raster_case_3
            (and
              (= LV_1_ans_int_ 3)
              (/= (vl-catch-all-error-p LV_1_ans_ ) T)
            )
            (progn
              (setq ss_L_ (ssget "C" 
                              (list (+ (car input_RO_PT_ins) -500) 
                                    (+ (cadr input_RO_PT_ins) -5)
                                    0
                              )
                              (list (+ (car input_RO_PT_ins) 500) 
                                    (+ (cadr input_RO_PT_ins) 5)
                                    0
                              )
                              (list (cons 0 "line") )
                      )
              )
              ; (command "rectangle" (list (+ (car input_RO_PT_ins) -500) (+ (cadr input_RO_PT_ins) -5) 0 ) (list (+ (car input_RO_PT_ins) 500) (+ (cadr input_RO_PT_ins) 5) 0 ) )
              ;preloop_and_while
                (setq ss_L_i 0)
                (setq startpt_sum_list_ ())
                (while (< ss_L_i (sslength ss_L_))
                  (setq ss_L_ename_ (ssname ss_L_ ss_L_i))
                  (setq ss_L_obj_ (vlax-ename->vla-object ss_L_ename_))
                  (if 
                    (or 
                      (= (rad-to-deg (vla-get-angle ss_L_obj_)) 0)
                      (= (rad-to-deg (vla-get-angle ss_L_obj_)) 180)
                    )
                    (progn
                      (setq ss_L_ename_obj_start_end_pt_ (list 
                                                          (setq ss_L_ename_obj_startpt_ ( vlax-safearray->list ( vlax-variant-value ( vla-get-startpoint ss_L_obj_ ) ) ) )
                                                          (setq ss_L_ename_obj_endpt_ ( vlax-safearray->list ( vlax-variant-value ( vla-get-endpoint ss_L_obj_ ) ) ) )
                                                        )
                      )
                      (setq startpt_sum_list_ (cons ss_L_ename_obj_startpt_ startpt_sum_list_))
                    )
                  )
                  (setq ss_L_i (+ ss_L_i 1))
                )
                ;TA:stanndard_lambda_sorting
                  (setq startpt_sum_list_sorted_ (vl-sort startpt_sum_list_  ;bigest open indent list
                                                          (function 
                                                            (lambda (a b) 
                                                              (< (car a) (car b))
                                                            )
                                                          )
                                                ) ;bigest close indent listLV_2_
                  )
                ;
              ;
              ;preloop_and_while
                (setq LV_3_ans_ nil)
                (setq move_i 0)
                (while (/= (vl-catch-all-error-p LV_3_ans_ ) T)
                  ;error_detect_
                  (setq LV_3_ans_ ;Detect the variable's result in case of a function error
                    (vl-catch-all-apply 
                      (function 
                        (lambda () 
                          (setq LV_3_ans_int_ (cond ( (getint (strcat "\nspecify get move to point\n<" (rtos (setq LV_3_ans_int_ (cond (LV_3_ans_int_) (1.0) ) ) ) "> : " ) ) ) (LV_3_ans_int_) ) )
                        )
                      )
                    )
                  )
                  (if (= (vl-catch-all-error-p LV_3_ans_ ) T)
                    (progn
                      (vl-catch-all-error-message LV_3_ans_ )
                    )
                  )
                  ;
                  ;
                  (if 
                    (and 
                      (= LV_3_ans_ 1)
                      (< move_i (- (length startpt_sum_list_sorted_ ) 1) )
                    )
                    (progn
                      (setq move_i (+ move_i LV_3_ans_))
                        (vla-move 
                          (vlax-ename->vla-object input_RO_PT_)
                          (vlax-3d-point (setq aa_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_))))))
                          (vlax-3d-point (nth (atoi (rtos move_i 2 0)) startpt_sum_list_sorted_ ))
                        )
                    )
                  )
                  (if (or 
                        (= move_i (- (length startpt_sum_list_sorted_ ) 1) )
                      )
                    (progn
                      (setq move_i -1)
                    )
                  )
                  ;
                )
                (setq input_RO_PT_ins (vlax-safearray->list 
                                        (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_) ) )
                                      )
                      input_RO_BOL_   (TA:ename+vla-getboundingbox (vlax-ename->vla-object input_RO_PT_) )
                )
                (setq ssset_boundary_ (ssget "WP" 
                                            (setq input_RO_PT_ins_PT_LIST (TAS:MAKE_AREA_SELECTION 
                                                                            (list 
                                                                              (+ 0.01 (car input_RO_PT_ins ) ) 
                                                                              (cadr input_RO_PT_ins) 
                                                                              (caddr input_RO_PT_ins) 
                                                                            )
                                                                          )
                                            )
                                            (list 
                                              (cons 0 "line")
                                            )
                                      )
                )
                
              ;
              (princ "raster_case_3")
            )
          )
          (;raster_case_4
            (and
              (= LV_1_ans_int_ 4)
              (/= (vl-catch-all-error-p LV_1_ans_ ) T)
            )
            (progn
              (setq input_RO_PT_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object input_RO_PT_)))))
              (setq input_RO_BOL_ (TA:ename+vla-getboundingbox (vlax-ename->vla-object input_RO_PT_)))   
              (setq ss_L_minror_ (ssget "C" 
                              (cadr input_RO_BOL_)
                              (caddr input_RO_BOL_)
                              
                              (list (cons 0 "line") )
                      )
              )
              ;preloop_and_while
                (setq LV_4_ans_ nil)
                (setq mirror_i 0)
                (while (/= (vl-catch-all-error-p LV_4_ans_ ) T)
                  ;error_detect_
                  (setq LV_4_ans_ ;Detect the variable's result in case of a function error
                    (vl-catch-all-apply 
                      (function 
                        (lambda () 
                          (setq LV_4_ans_int_ (cond ( (getint (strcat "\nspecify get miror to point \n0 = Ver 1 = Hon\n<" (rtos (setq LV_4_ans_int_ (cond (LV_4_ans_int_) (1.0) ) ) ) "> : " ) ) ) (LV_4_ans_int_) ) )
                        )
                      )
                    )
                  )
                  (if (= (vl-catch-all-error-p LV_4_ans_ ) T)
                    (progn
                      (vl-catch-all-error-message LV_4_ans_ )
                    )
                  )
                  ;
                  (cond
                    (;mirror_case_1
                      (and
                          (= LV_4_ans_ 0)
                          (/= (vl-catch-all-error-p LV_4_ans_ ) T)
                      )
                      (progn
                        (setq input_RO_PT_ins_end (list (+ 0 (car input_RO_PT_ins)) (+ 500 (cadr input_RO_PT_ins)) 0) )
                        (command "mirror" ss_L_minror_ "" input_RO_PT_ins input_RO_PT_ins_end "Y"  )
                        (princ "mirror_case_1")
                      )
                    )
                    (;mirror_case_2
                      (and
                          (= LV_4_ans_ 1)
                          (/= (vl-catch-all-error-p LV_4_ans_ ) T)
                      )
                      (progn
                        (setq input_RO_PT_ins_end (list (+ 500 (car input_RO_PT_ins)) (+ 0 (cadr input_RO_PT_ins)) 0) )
                        (command "mirror" ss_L_minror_ "" input_RO_PT_ins input_RO_PT_ins_end "Y"  )
                        (princ "mirror_case_2")
                      )
                    )
                    
                    
                  )
                  
                  
                  ;
                )
              ;
              (setq ssset_boundary_ (ssget "WP" 
                                (setq input_RO_PT_ins_PT_LIST (TAS:MAKE_AREA_SELECTION (list 
                                                                                         (+ 0.01 (car input_RO_PT_ins))
                                                                                         (cadr input_RO_PT_ins)
                                                                                         (caddr input_RO_PT_ins)
                                                                                       ) 
                                                              ) 
                                )
                                (list 
                                  (cons 0 "line")
                                )
                          )
              )
              (princ "raster_case_4")
            )
          )
          (;raster_case_5
            (and
              (= LV_1_ans_int_ 5)
              (/= (vl-catch-all-error-p LV_1_ans_ ) T)
            )
            (progn
              (setq ss_copy_ (ssget "C" 
                              (cadr input_RO_BOL_)
                              (caddr input_RO_BOL_)
                              
                              (list (cons 0 "line,insert") )
                      )
              )
              (sslength ss_copy_)
              
              ;copy_process
                (setq new_copy_insertion_point_ (list 
                                                  (nth 0 (cadr input_RO_BOL_))
                                                  (- (- (nth 1 (cadr input_RO_BOL_)) distance1x_  ) offset_pt_)
                                                  0
                                                )
                )
                (setq sssssssss (LM:vl-setattributevalue (vlax-ename->vla-object input_RO_PT_ ) "step_no." ii-i )  )
                (command "copy" ss_copy_ "" ( cadr input_RO_BOL_) new_copy_insertion_point_ )
                
                (setq distance1x_ (+ distance1x_ distance1_))  
                (setq offset_pt_ (+ offset_pt_ 100))  
                (setq ii-i (+ ii-i 1))  
              ;
              (princ "raster_case_5")
            )
          )
          (;raster_case_5
            (and
              (vl-catch-all-error-p LV_1_ans_ )
                
            )
            (progn
              
              (princ "raster_case_6")
            )
          )
        )
      ;
    )
  ;
)




;dynamic temp for create ACP line and cut
  (defun c:was1 ()
    (setvar "osmode" 0)

    ;user_input_and_get-data_
      (setq acp_main_1_ename_ (car (entsel)))
      (setq acp_main_1_obj_ (vlax-ename->vla-object acp_main_1_ename_))
      (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))

    ;
    ;offset_2-2_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 0.50 "min")
      (setq 2-2_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-2_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-2_groove_angle_tri-C__ename_))
      (setq 2-2_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-2_groove_angle_tri-C__ename_ )) 
    ;
    ;preloop_and_while
      (setq acp_main_1_as10_i 1)
      (while (< acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 1))
        (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
        (setq 2-2_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-2_groove_angle_tri-C__as10_list_))
        (setq groove_angle_90V (rad-to-deg (angle (car 2-2_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
        (setq groove_angle_90H (- groove_angle_90V  90))
        (setq groove_angle_tri-A_ (- groove_angle_90H (cadr 2-2_groove_angle_tri-C__as10_list_val)))
        (setq groove_angle_tri-B_ 90)
        (setq groove_angle_tri-C_ (- groove_angle_tri-B_ groove_angle_tri-A_ ))
        ;TRIGONMETRY-PROCESS
          (setq point_1 
                  (polar 
                    (polar (car 2-2_groove_angle_tri-C__as10_list_val) (deg-to-rad groove_angle_90H) 0.1 )
                    (deg-to-rad (+ groove_angle_90V 180))
                    (/ (* 0.10000 (sin (deg-to-rad groove_angle_tri-A_))) (sin (deg-to-rad groove_angle_tri-C_)) )
                  )
          )
          ; (command "point" point_1 )
          
          (setq point_2
                  (polar 
                    point_1
                      (deg-to-rad groove_angle_90V)
                      0.2
                  )
          )
          ; (command "point" point_2 )
          (setq point_3
                  (polar 
                    point_2
                    (deg-to-rad (+ 90 groove_angle_90V))
                    0.2
                  )
          )
          ; (command "point" point_3 )
          (setq point_4
                  (polar 
                    point_3
                    (deg-to-rad (+ 180 groove_angle_90V))
                    0.2
                  )
          )
          ; (command "point" point_4 )
          ; (command "_line" point_1 point_2 ""   )
          ; (command "_line" point_2    "")
          ; (command "_line" point_3    "")
          ; (command "_line" point_4    "")
          (setq new_ssset_(ssadd))
          (setq new_ssset_ (ssadd 2-2_groove_angle_tri-C__ename_ new_ssset_  )  )
          (command "pline" point_1 point_2 point_3 point_4 "")    
          (setq new_groove_ename (entlast))
          (setq new_ssset_ (ssadd new_groove_ename new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_1)
          (setq new_groove_ex1 (entlast))
          (setq new_ssset_ (ssadd new_groove_ex1 new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_4)
          (setq new_groove_ex2 (entlast))
          (vla-delete (vlax-ename->vla-object new_groove_ex2))
          
          
          (command "pedit" "m" new_ssset_ "" "J" "" "")
        ;
      

        (setq acp_main_1_as10_i (+ acp_main_1_as10_i 1))
      )
    ;

    (setvar "osmode" 1215)
  )
  (defun c:was3 ()
    (setvar "osmode" 0)

    ;user_input_and_get-data_
      (setq acp_main_1_ename_ (car (entsel)))
      (setq acp_main_1_obj_ (vlax-ename->vla-object acp_main_1_ename_))
      (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))

    ;
    ;offset_2-2_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 3.5 "min")
      (setq 2-2_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-2_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-2_groove_angle_tri-C__ename_))
      (setq 2-2_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-2_groove_angle_tri-C__ename_ )) 
    ;
    ;preloop_and_while
      (setq acp_main_1_as10_i 1)
      (while (< acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 1))
        (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
        (setq 2-2_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-2_groove_angle_tri-C__as10_list_))
        (setq groove_angle_90V (rad-to-deg (angle (car 2-2_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
        (setq groove_angle_90H (- groove_angle_90V  90))
        (setq groove_angle_tri-A_ (- groove_angle_90H (cadr 2-2_groove_angle_tri-C__as10_list_val)))
        (setq groove_angle_tri-B_ 90)
        (setq groove_angle_tri-C_ (- groove_angle_tri-B_ groove_angle_tri-A_ ))
        ;TRIGONMETRY-PROCESS
          (setq point_1 
                  (polar 
                    (polar (car 2-2_groove_angle_tri-C__as10_list_val) (deg-to-rad groove_angle_90H) 0.1 )
                    (deg-to-rad (+ groove_angle_90V 180))
                    (/ (* 0.10000 (sin (deg-to-rad groove_angle_tri-A_))) (sin (deg-to-rad groove_angle_tri-C_)) )
                  )
          )
          ; (command "point" point_1 )
          
          (setq point_2
                  (polar 
                    point_1
                      (deg-to-rad groove_angle_90V)
                      0.2
                  )
          )
          ; (command "point" point_2 )
          (setq point_3
                  (polar 
                    point_2
                    (deg-to-rad (+ 90 groove_angle_90V))
                    0.2
                  )
          )
          ; (command "point" point_3 )
          (setq point_4
                  (polar 
                    point_3
                    (deg-to-rad (+ 180 groove_angle_90V))
                    0.2
                  )
          )
          ; (command "point" point_4 )
          ; (command "_line" point_1 point_2 ""   )
          ; (command "_line" point_2    "")
          ; (command "_line" point_3    "")
          ; (command "_line" point_4    "")
          (setq new_ssset_(ssadd))
          (setq new_ssset_ (ssadd 2-2_groove_angle_tri-C__ename_ new_ssset_  )  )
          (command "pline" point_1 point_2 point_3 point_4 "")    
          (setq new_groove_ename (entlast))
          (setq new_ssset_ (ssadd new_groove_ename new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_1)
          (setq new_groove_ex1 (entlast))
          (setq new_ssset_ (ssadd new_groove_ex1 new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_4)
          (setq new_groove_ex2 (entlast))
          (vla-delete (vlax-ename->vla-object new_groove_ex2))
          
          
          (command "pedit" "m" new_ssset_ "" "J" "" "")
        ;
      

        (setq acp_main_1_as10_i (+ acp_main_1_as10_i 1))
      )
    ;

    (setvar "osmode" 1215)
  )
  (defun c:was4 ()
    (setvar "osmode" 0)

    ;user_input_and_get-data_
      (setq acp_main_1_ename_ (car (entsel)))
      (setq acp_main_1_obj_ (vlax-ename->vla-object acp_main_1_ename_))
      (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))

    ;
    ;offset_2-2_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 4 "min")
      (setq 2-2_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-2_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-2_groove_angle_tri-C__ename_))
      (setq 2-2_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-2_groove_angle_tri-C__ename_ )) 
    ;
    ;preloop_and_while
      (setq acp_main_1_as10_i 1)
      (while (< acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 1))
        (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
        (setq 2-2_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-2_groove_angle_tri-C__as10_list_))
        (setq groove_angle_90V (rad-to-deg (angle (car 2-2_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
        (setq groove_angle_90H (- groove_angle_90V  90))
        (setq groove_angle_tri-A_ (- groove_angle_90H (cadr 2-2_groove_angle_tri-C__as10_list_val)))
        (setq groove_angle_tri-B_ 90)
        (setq groove_angle_tri-C_ (- groove_angle_tri-B_ groove_angle_tri-A_ ))
        ;TRIGONMETRY-PROCESS
          (command "point" (car 2-2_groove_angle_tri-C__as10_list_val))
          (setq point_1 
                  (polar 
                    (polar (car 2-2_groove_angle_tri-C__as10_list_val) (deg-to-rad groove_angle_90H) 0.1 )
                    (deg-to-rad (+ groove_angle_90V 180))
                    (/ (* 0.10000 (sin (deg-to-rad groove_angle_tri-A_))) (sin (deg-to-rad groove_angle_tri-C_)) )
                  )
          )
          (command "point" point_1 )       
          (setq point_2
                  (polar 
                    point_1
                      (deg-to-rad groove_angle_90V)
                      0.2
                  )
          )
          (command "point" point_2 )
          (setq point_3
                  (polar 
                    point_2
                    (deg-to-rad (+ 90 groove_angle_90V))
                    0.2
                  )
          )
          (command "point" point_3 )
          (setq point_4
                  (polar 
                    point_3
                    (deg-to-rad (+ 180 groove_angle_90V))
                    0.2
                  )
          )
          (command "point" point_4 )
          ; (command "_line" point_1 point_2 ""   )
          ; (command "_line" point_2    "")
          ; (command "_line" point_3    "")
          ; (command "_line" point_4    "")
          (setq new_ssset_(ssadd))
          (setq new_ssset_ (ssadd 2-2_groove_angle_tri-C__ename_ new_ssset_  )  )
          (command "pline" point_1 point_2 point_3 point_4 "")    
          (setq new_groove_ename (entlast))
          (setq new_ssset_ (ssadd new_groove_ename new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_1)
          (setq new_groove_ex1 (entlast))
          (setq new_ssset_ (ssadd new_groove_ex1 new_ssset_  )  )
          (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ point_4)
          (setq new_groove_ex2 (entlast))
          (vla-delete (vlax-ename->vla-object new_groove_ex2))
          
          
          (command "pedit" "m" new_ssset_ "" "J" "" "")
        ;
      

        (setq acp_main_1_as10_i (+ acp_main_1_as10_i 1))
      )
    ;

    (setvar "osmode" 1215)
  )
  (defun c:was41 ()
    (setvar "osmode" 0)

    ;user_input_and_get-data_
      (setq acp_main_1_ename_ (car (entsel)))
      (setq acp_main_1_obj_ (vlax-ename->vla-object acp_main_1_ename_))
      (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))
      ; (if 
      ;   (and
      ;     (> (car (car (nth 0 (reverse acp_main_1_as10_list_)))) (car (car (nth 0 acp_main_1_as10_list_))) )
          
      ;   )
      ;   (progn
      ;     (command "reverse" acp_main_1_ename_ "" )
      ;     (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))
      ;   )
      ; )
    ;
    ;offset_2-2_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 0.50 "min")
      (setq 2-2_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-2_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-2_groove_angle_tri-C__ename_))
      (setq 2-2_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-2_groove_angle_tri-C__ename_ )) 
    ;
    ;offset_2-3_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 3.50 "min")
      (setq 2-3_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-3_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-3_groove_angle_tri-C__ename_))
      (setq 2-3_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-3_groove_angle_tri-C__ename_ )) 
    ;
    ;offset_2-4_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 4 "min")
      (setq 2-4_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-4_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-4_groove_angle_tri-C__ename_))
      (setq 2-4_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-4_groove_angle_tri-C__ename_ )) 
    ;

    ;preloop_and_while
      (setq acp_main_1_as10_i 1)
      (while (< acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 1))
        
          ;2-2process
            (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
            (setq 2-2_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-2_groove_angle_tri-C__as10_list_))
            (setq 2-2_groove_angle_90V (rad-to-deg (angle (car 2-2_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
            (setq 2-2_groove_angle_90H (- 2-2_groove_angle_90V  90))
            (setq 2-2_groove_angle_tri-A_ (- 2-2_groove_angle_90H (cadr 2-2_groove_angle_tri-C__as10_list_val)))
            (setq 2-2_groove_angle_tri-B_ 90)
            (setq 2-2_groove_angle_tri-C_ (- 2-2_groove_angle_tri-B_ 2-2_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-2point_1 
                      (polar 
                        (polar (car 2-2_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-2_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-2_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-2_groove_angle_tri-A_))) (sin (deg-to-rad 2-2_groove_angle_tri-C_)) )
                      )
              )
              ;(command "point" 2-2point_1 )
              (setq 2-2point_2
                      (polar 
                        2-2point_1
                          (deg-to-rad 2-2_groove_angle_90V)
                          0.2
                      )
              )
              ;(command "point" 2-2point_2 )
              (setq 2-2point_3
                      (polar 
                        2-2point_2
                        (deg-to-rad (+ 90 2-2_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-2point_3 )
              (setq 2-2point_4
                      (polar 
                        2-2point_3
                        (deg-to-rad (+ 180 2-2_groove_angle_90V))
                        0.2
                      )
              )
              ; (command "point" 2-2point_4 )
        
              ; (command "point" 2-2point_1 )
              ; (command "point" 2-2point_2 )
              ; (command "point" 2-2point_3 )
              ; (command "point" 2-2point_4 )
              
    
              (setq new_ssset_(ssadd))
              (sslength new_ssset_)
              (setq new_ssset_ (ssadd 2-2_groove_angle_tri-C__ename_ new_ssset_  )  )
              (command "pline" 2-2point_1 2-2point_2 2-2point_3 2-2point_4 "")    
              (setq new_groove_ename (entlast)) 
              (setq new_ssset_ (ssadd new_groove_ename new_ssset_  )  )
              (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ 2-2point_1)
              
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_ (ssadd new_groove_ex1 new_ssset_  )  )
              (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ 2-2point_4)
              (setq new_groove_ex2 (entlast))
              (if (/= (vl-prin1-to-string new_groove_ename) (vl-prin1-to-string new_groove_ex2))
                (progn
                  (vla-delete (vlax-ename->vla-object new_groove_ex2))
                )
              )
              
              
              
              (command "pedit" "m" new_ssset_ "" "J" "" "")
            ;
          ;
          ;2-3process
              (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
              (setq 2-3_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-3_groove_angle_tri-C__as10_list_))
              (setq 2-3_groove_angle_90V (rad-to-deg (angle (car 2-3_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
              (setq 2-3_groove_angle_90H (- 2-3_groove_angle_90V  90))
              (setq 2-3_groove_angle_tri-A_ (- 2-3_groove_angle_90H (cadr 2-3_groove_angle_tri-C__as10_list_val)))
              (setq 2-3_groove_angle_tri-B_ 90)
              (setq 2-3_groove_angle_tri-C_ (- 2-3_groove_angle_tri-B_ 2-3_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-3point_1 
                      (polar 
                        (polar (car 2-3_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-3_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-3_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-3_groove_angle_tri-A_))) (sin (deg-to-rad 2-3_groove_angle_tri-C_)) )
                      )
              )
              ;(command "point" 2-3point_1 )
              
              (setq 2-3point_2
                      (polar 
                        2-3point_1
                          (deg-to-rad 2-3_groove_angle_90V)
                          0.2
                      )
              )
              ;(command "point" 2-3point_2 )
              (setq 2-3point_3
                      (polar 
                        2-3point_2
                        (deg-to-rad (+ 90 2-3_groove_angle_90V))
                        0.2
                      )
              )
              ; (command "point" 2-3point_3 )
              (setq 2-3point_4
                      (polar 
                        2-3point_3
                        (deg-to-rad (+ 180 2-3_groove_angle_90V))
                        0.2
                      )
              )
              ; (command "point" 2-3point_4 )
              (setq new_ssset_2(ssadd))
              (setq new_ssset_2 (ssadd 2-3_groove_angle_tri-C__ename_ new_ssset_2  )  )
              ; (command "pline" 2-3point_1 2-3point_2 2-3point_3 2-3point_4 "")    
              (command "pline" 2-3point_1 2-2point_2 2-2point_3 2-3point_4 "")
              (setq new_groove_ename (entlast))
              (setq new_ssset_2 (ssadd new_groove_ename new_ssset_2  )  )
              (command "breakatpoint" 2-3_groove_angle_tri-C__ename_ 2-3point_1)
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_2 (ssadd new_groove_ex1 new_ssset_2  )  )
              (command "breakatpoint" 2-3_groove_angle_tri-C__ename_ 2-3point_4)
              (setq new_groove_ex2 (entlast))
              (if (/= (vl-prin1-to-string new_groove_ename) (vl-prin1-to-string new_groove_ex2))
                (progn
                  (vla-delete (vlax-ename->vla-object new_groove_ex2))
                )
              )
              
              
              (command "pedit" "m" new_ssset_2 "" "J" "" "")
            ;
          ;
          ;2-4process
              (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
              (setq 2-4_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-4_groove_angle_tri-C__as10_list_))
              (setq 2-4_groove_angle_90V (rad-to-deg (angle (car 2-4_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
              (setq 2-4_groove_angle_90H (- 2-4_groove_angle_90V  90))
              (setq 2-4_groove_angle_tri-A_ (- 2-4_groove_angle_90H (cadr 2-4_groove_angle_tri-C__as10_list_val)))
              (setq 2-4_groove_angle_tri-B_ 90)
              (setq 2-4_groove_angle_tri-C_ (- 2-4_groove_angle_tri-B_ 2-4_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-4point_1 
                      (polar 
                        (polar (car 2-4_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-4_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-4_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-4_groove_angle_tri-A_))) (sin (deg-to-rad 2-4_groove_angle_tri-C_)) )
                      )
              )
              ; (command "point" 2-4point_1 )    
              (setq 2-4point_2
                      (polar 
                        2-4point_1
                          (deg-to-rad 2-4_groove_angle_90V)
                          0.2
                      )
              )
              ; (command "point" 2-4point_2 )
              (setq 2-4point_3
                      (polar 
                        2-4point_2
                        (deg-to-rad (+ 90 2-4_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-4point_3 )
              (setq 2-4point_4
                      (polar 
                        2-4point_3
                        (deg-to-rad (+ 180 2-4_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-4point_4 )
              (setq new_ssset_3(ssadd))
              (setq new_ssset_3 (ssadd 2-4_groove_angle_tri-C__ename_ new_ssset_3  )  )
              ; (command "pline" 2-4point_1 2-4point_2 2-4point_3 2-4point_4 "")    
              (command "pline" 2-4point_1 2-2point_2 2-2point_3 2-4point_4 "")
              (setq new_groove_ename (entlast))
              (setq new_ssset_3 (ssadd new_groove_ename new_ssset_3  )  )
              (command "breakatpoint" 2-4_groove_angle_tri-C__ename_ 2-4point_1)
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_3 (ssadd new_groove_ex1 new_ssset_3  )  )
              (command "breakatpoint" 2-4_groove_angle_tri-C__ename_ 2-4point_4)
              (setq new_groove_ex2 (entlast))
              (if (/= (vl-prin1-to-string new_groove_ename) (vl-prin1-to-string new_groove_ex2))
                (progn
                  (vla-delete (vlax-ename->vla-object new_groove_ex2))
                )
              )
              
              
              (command "pedit" "m" new_ssset_3 "" "J" "" "")
            ;
            
            

          ;


          (setq acp_main_1_as10_i (+ acp_main_1_as10_i 1))
        
      )
    ;

    (setvar "osmode" 1215)
  )
  (defun c:was5 ()
    (setvar "osmode" 0)

    ;user_input_and_get-data_
      (setq acp_main_1_ename_ (car (entsel)))
      (setq acp_main_1_obj_ (vlax-ename->vla-object acp_main_1_ename_))
      (setq acp_main_1_as10_list_ (TA:Get_Pline_vertext_angle_case1 acp_main_1_ename_ ))

    ;
    ;offset_2-2_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 0.50 "min")
      (setq 2-2_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-2_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-2_groove_angle_tri-C__ename_))
      (setq 2-2_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-2_groove_angle_tri-C__ename_ )) 
    ;
    ;offset_2-3_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 3.50 "min")
      (setq 2-3_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-3_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-3_groove_angle_tri-C__ename_))
      (setq 2-3_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-3_groove_angle_tri-C__ename_ )) 
    ;
    ;offset_2-4_groove_angle_tri-C__
      (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 4 "min")
      (setq 2-4_groove_angle_tri-C__ename_ (entlast)) 
      (setq 2-4_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-4_groove_angle_tri-C__ename_))
      (setq 2-4_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-4_groove_angle_tri-C__ename_ )) 
    ;
    ; ;offset_2-5_groove_angle_tri-C__
    ;   (TA:Offset_Pline_multi_vertex_ acp_main_1_ename_ 20 "min")
    ;   (setq 2-5_groove_angle_tri-C__ename_ (entlast)) 
    ;   (setq 2-5_groove_angle_tri-C__obj_ (vlax-ename->vla-object 2-5_groove_angle_tri-C__ename_))
    ;   (setq 2-5_groove_angle_tri-C__as10_list_ (TA:Get_Pline_vertext_angle_case1 2-5_groove_angle_tri-C__ename_ )) 
    ; ;
    ;preloop_and_while
      (setq acp_main_1_as10_i 1)
      (while (< acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 1))
        
          ;2-2process
            (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
            (setq 2-2_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-2_groove_angle_tri-C__as10_list_))
            (setq 2-2_groove_angle_90V (rad-to-deg (angle (car 2-2_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
            (setq 2-2_groove_angle_90H (- 2-2_groove_angle_90V  90))
            (setq 2-2_groove_angle_tri-A_ (- 2-2_groove_angle_90H (cadr 2-2_groove_angle_tri-C__as10_list_val)))
            (setq 2-2_groove_angle_tri-B_ 90)
            (setq 2-2_groove_angle_tri-C_ (- 2-2_groove_angle_tri-B_ 2-2_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-2point_1 
                      (polar 
                        (polar (car 2-2_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-2_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-2_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-2_groove_angle_tri-A_))) (sin (deg-to-rad 2-2_groove_angle_tri-C_)) )
                      )
              )
              ;(command "point" 2-2point_1 )
              (setq 2-2point_2
                      (polar 
                        2-2point_1
                          (deg-to-rad 2-2_groove_angle_90V)
                          0.2
                      )
              )
              ;(command "point" 2-2point_2 )
              (setq 2-2point_3
                      (polar 
                        2-2point_2
                        (deg-to-rad (+ 90 2-2_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-2point_3 )
              (setq 2-2point_4
                      (polar 
                        2-2point_3
                        (deg-to-rad (+ 180 2-2_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-2point_4 )
              ; (command "_line" point_1 point_2 ""   )
              ; (command "_line" point_2    "")
              ; (command "_line" point_3    "")
              ; (command "_line" point_4    "")
              (setq new_ssset_(ssadd))
              (sslength new_ssset_)
              (setq new_ssset_ (ssadd 2-2_groove_angle_tri-C__ename_ new_ssset_  )  )
              (command "pline" 2-2point_1 2-2point_2 2-2point_3 2-2point_4 "")    
              (setq new_groove_ename (entlast))
              (setq new_ssset_ (ssadd new_groove_ename new_ssset_  )  )
              (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ 2-2point_1)
              
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_ (ssadd new_groove_ex1 new_ssset_  )  )
              (command "breakatpoint" 2-2_groove_angle_tri-C__ename_ 2-2point_4)
              (setq new_groove_ex2 (entlast))
              (vla-delete (vlax-ename->vla-object new_groove_ex2))
              
              
              (command "pedit" "m" new_ssset_ "" "J" "" "")
            ;
          ;
          ;2-3process
              (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
              (setq 2-3_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-3_groove_angle_tri-C__as10_list_))
              (setq 2-3_groove_angle_90V (rad-to-deg (angle (car 2-3_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
              (setq 2-3_groove_angle_90H (- 2-3_groove_angle_90V  90))
              (setq 2-3_groove_angle_tri-A_ (- 2-3_groove_angle_90H (cadr 2-3_groove_angle_tri-C__as10_list_val)))
              (setq 2-3_groove_angle_tri-B_ 90)
              (setq 2-3_groove_angle_tri-C_ (- 2-3_groove_angle_tri-B_ 2-3_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-3point_1 
                      (polar 
                        (polar (car 2-3_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-3_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-3_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-3_groove_angle_tri-A_))) (sin (deg-to-rad 2-3_groove_angle_tri-C_)) )
                      )
              )
              ;(command "point" 2-3point_1 )
              
              (setq 2-3point_2
                      (polar 
                        2-3point_1
                          (deg-to-rad 2-3_groove_angle_90V)
                          0.2
                      )
              )
              ;(command "point" 2-3point_2 )
              (setq 2-3point_3
                      (polar 
                        2-3point_2
                        (deg-to-rad (+ 90 2-3_groove_angle_90V))
                        0.2
                      )
              )
              ; (command "point" 2-3point_3 )
              (setq 2-3point_4
                      (polar 
                        2-3point_3
                        (deg-to-rad (+ 180 2-3_groove_angle_90V))
                        0.2
                      )
              )
              ; (command "point" 2-3point_4 )
              (setq new_ssset_2(ssadd))
              (setq new_ssset_2 (ssadd 2-3_groove_angle_tri-C__ename_ new_ssset_2  )  )
              ; (command "pline" 2-3point_1 2-3point_2 2-3point_3 2-3point_4 "")    
              (command "pline" 2-3point_1 2-2point_2 2-2point_3 2-3point_4 "")
              (setq new_groove_ename (entlast))
              (setq new_ssset_2 (ssadd new_groove_ename new_ssset_2  )  )
              (command "breakatpoint" 2-3_groove_angle_tri-C__ename_ 2-3point_1)
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_2 (ssadd new_groove_ex1 new_ssset_2  )  )
              (command "breakatpoint" 2-3_groove_angle_tri-C__ename_ 2-3point_4)
              (setq new_groove_ex2 (entlast))
              (vla-delete (vlax-ename->vla-object new_groove_ex2))
              
              
              (command "pedit" "m" new_ssset_2 "" "J" "" "")
            ;
          ;
          ;2-4process
              (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
              (setq 2-4_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-4_groove_angle_tri-C__as10_list_))
              (setq 2-4_groove_angle_90V (rad-to-deg (angle (car 2-4_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
              (setq 2-4_groove_angle_90H (- 2-4_groove_angle_90V  90))
              (setq 2-4_groove_angle_tri-A_ (- 2-4_groove_angle_90H (cadr 2-4_groove_angle_tri-C__as10_list_val)))
              (setq 2-4_groove_angle_tri-B_ 90)
              (setq 2-4_groove_angle_tri-C_ (- 2-4_groove_angle_tri-B_ 2-4_groove_angle_tri-A_ ))
            ;TRIGONMETRY-PROCESS
              (setq 2-4point_1 
                      (polar 
                        (polar (car 2-4_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-4_groove_angle_90H) 0.1 )
                        (deg-to-rad (+ 2-4_groove_angle_90V 180))
                        (/ (* 0.10000 (sin (deg-to-rad 2-4_groove_angle_tri-A_))) (sin (deg-to-rad 2-4_groove_angle_tri-C_)) )
                      )
              )
              ; (command "point" 2-4point_1 )    
              (setq 2-4point_2
                      (polar 
                        2-4point_1
                          (deg-to-rad 2-4_groove_angle_90V)
                          0.2
                      )
              )
              ; (command "point" 2-4point_2 )
              (setq 2-4point_3
                      (polar 
                        2-4point_2
                        (deg-to-rad (+ 90 2-4_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-4point_3 )
              (setq 2-4point_4
                      (polar 
                        2-4point_3
                        (deg-to-rad (+ 180 2-4_groove_angle_90V))
                        0.2
                      )
              )
              ;(command "point" 2-4point_4 )
              (setq new_ssset_3(ssadd))
              (setq new_ssset_3 (ssadd 2-4_groove_angle_tri-C__ename_ new_ssset_3  )  )
              ; (command "pline" 2-4point_1 2-4point_2 2-4point_3 2-4point_4 "")    
              (command "pline" 2-4point_1 2-2point_2 2-2point_3 2-4point_4 "")
              (setq new_groove_ename (entlast))
              (setq new_ssset_3 (ssadd new_groove_ename new_ssset_3  )  )
              (command "breakatpoint" 2-4_groove_angle_tri-C__ename_ 2-4point_1)
              (setq new_groove_ex1 (entlast))
              (setq new_ssset_3 (ssadd new_groove_ex1 new_ssset_3  )  )
              (command "breakatpoint" 2-4_groove_angle_tri-C__ename_ 2-4point_4)
              (setq new_groove_ex2 (entlast))
              (vla-delete (vlax-ename->vla-object new_groove_ex2))
              
              
              (command "pedit" "m" new_ssset_3 "" "J" "" "")
            ;
            
            

          ;
          ; ;2-5process
          ;   (setq acp_main_1_as10_list_val (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
          ;   (if 
          ;     (and
          ;       (= acp_main_1_as10_i 1)
          ;       (<=
          ;         (distance
          ;           (car (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
          ;           (car (nth  (-  acp_main_1_as10_i 1)  acp_main_1_as10_list_))
          ;         )
          ;         20
          ;       )
          ;     )
          ;     (progn
          ;       (setq 2-5_groove_angle_tri-C__as10_list_val (nth  (-  acp_main_1_as10_i 1)   2-5_groove_angle_tri-C__as10_list_))
          ;     )
          ;   )
          ;   (if 
          ;     (and
          ;       (/= acp_main_1_as10_i 1)
          ;       (/= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;       (>
          ;         (distance
          ;           (car (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
          ;           (car (nth  (-  acp_main_1_as10_i 1)  acp_main_1_as10_list_))
          ;         )
          ;         20
          ;       )
          ;     )
          ;     (progn
          ;       (setq 2-5_groove_angle_tri-C__as10_list_val (nth  acp_main_1_as10_i  2-5_groove_angle_tri-C__as10_list_))
          ;     )                
          ;   )
          ;   (if 
          ;     (and
          ;       (= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;       (>
          ;         (distance
          ;           (car (nth  acp_main_1_as10_i  acp_main_1_as10_list_))
          ;           (car (nth  (-  acp_main_1_as10_i 1)  acp_main_1_as10_list_))
          ;         )
          ;         20
          ;       )
          ;     )
          ;     (progn
          ;       (setq 2-5_groove_angle_tri-C__as10_list_val (nth  (-  acp_main_1_as10_i 1)   2-5_groove_angle_tri-C__as10_list_))
          ;     )                
          ;   )
          ;   (setq 2-5_groove_angle_90V (rad-to-deg (angle (car 2-5_groove_angle_tri-C__as10_list_val) (car acp_main_1_as10_list_val))))
          ;   (setq 2-5_groove_angle_90H (- 2-5_groove_angle_90V  90))
          ;   (setq 2-5_groove_angle_tri-A_ (- 2-5_groove_angle_90H (cadr 2-5_groove_angle_tri-C__as10_list_val)))
          ;   (setq 2-5_groove_angle_tri-B_ 90)
          ;   (setq 2-5_groove_angle_tri-C_ (- 2-5_groove_angle_tri-B_ 2-5_groove_angle_tri-A_ ))
          ;   ;TRIGONMETRY-PROCESS
          ;     (setq 2-5point_1 
          ;             (polar 
          ;               (polar (car 2-5_groove_angle_tri-C__as10_list_val) (deg-to-rad 2-5_groove_angle_90H) 0.1 )
          ;               (deg-to-rad (+ 2-5_groove_angle_90V 180))
          ;               (/ (* 0.10000 (sin (deg-to-rad 2-5_groove_angle_tri-A_))) (sin (deg-to-rad 2-5_groove_angle_tri-C_)) )
          ;             )
          ;     )
          ;     ;(command "point" 2-5point_1 )    
          ;     (setq 2-5point_2
          ;             (polar 
          ;               2-5point_1
          ;                 (deg-to-rad 2-5_groove_angle_90V)
          ;                 0.2
          ;             )
          ;     )
          ;     ;(command "point" 2-5point_2 )
          ;     (setq 2-5point_3
          ;             (polar 
          ;               2-5point_2
          ;               (deg-to-rad (+ 90 2-5_groove_angle_90V))
          ;               0.2
          ;             )
          ;     )
          ;     ;(command "point" 2-5point_3 )
          ;     (setq 2-5point_4
          ;             (polar 
          ;               2-5point_3
          ;               (deg-to-rad (+ 180 2-5_groove_angle_90V))
          ;               0.2
          ;             )
          ;     )
          ;     ;(command "point" 2-5point_4 )
          ;   ;
          ;   ;create process
          ;     (setq new_ssset_3 (ssadd))
          ;     (setq new_ssset_3 (ssadd 2-5_groove_angle_tri-C__ename_ new_ssset_3  )  )
          ;     ; (command "pline" 2-5point_1 2-5point_2 2-5point_3 2-5point_4 "") 
          ;     (cond
          ;       (;start
          ;         (and
          ;           (=  (- acp_main_1_as10_i 1) 0)
                    
          ;         )
          ;         (progn
          ;           (command "pline" 2-5point_1 2-4point_1 2-2point_2 2-2point_3 2-4point_4  "")
          ;         )
                  
          ;       )      
          ;       (;end
          ;         (and
          ;           (= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
                    
          ;         )
          ;         (progn
          ;           (command "pline" 2-4point_1 2-2point_2 2-2point_3 2-4point_4 2-5point_4 "")
          ;         )
                  
          ;       )      
          ;       (;middle
          ;         (and
          ;           (/= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;           (/=  (- acp_main_1_as10_i 1) 0)
          ;         )
          ;         (progn
          ;           (command "pline" 2-5point_1 2-4point_1 2-2point_2 2-2point_3 2-4point_4 2-5point_4 "")
          ;         )
          ;       )      
          ;     )
              
          ;     (setq new_groove_ename (entlast))
              
          ;     (setq new_ssset_3 (ssadd new_groove_ename new_ssset_3  )  )
          ;     (cond
          ;       (;first_wave_case_1
          ;         (and
          ;             (= 0 (- acp_main_1_as10_i 1))
                      
          ;         )
          ;         (progn
          ;             (command "breakatpoint" 2-5_groove_angle_tri-C__ename_ 2-5point_1)
          ;             (setq new_groove_ex1 (entlast))
                      
          ;             (setq new_ssset_3 (ssadd new_groove_ex1 new_ssset_3  )  )
          ;             (vla-delete 2-5_groove_angle_tri-C__obj_)
          ;             (command "pedit" "m" new_ssset_3 "" "J" "" "")
          ;             (setq new_man_ (entlast))
                    
                      
          ;             (princ "first_wave_case_1")
          ;         )
          ;       )
          ;       (;first_wave_case_2
          ;         (and
          ;             (/= 0 (- acp_main_1_as10_i 1))
          ;             (/= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;         )
          ;         (progn
                      
          ;             (command "breakatpoint" new_man_ 2-5point_1)
                      
          ;             (setq new_groove_ex1 (entlast))
          ;             (command "breakatpoint" new_groove_ex1 2-5point_4)
          ;             (setq new_groove_ex2 (entlast))
                    
          ;             (setq new_ssset_3 (ssadd new_man_ new_ssset_3  )  )
          ;             (setq new_ssset_3 (ssadd new_groove_ex2 new_ssset_3  )  )
              

          ;             (vla-delete (vlax-ename->vla-object new_groove_ex1))
                      
                      
          ;             (command "pedit" "m" new_ssset_3 "" "J" "" "")
          ;             (setq new_man_ (entlast))
          ;             (princ "first_wave_case_2")
          ;         )
          ;       )
          ;       (;first_wave_case_3
          ;         (and
          ;           (= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;           (<=
          ;             (distance 
          ;               (car (nth acp_main_1_as10_i acp_main_1_as10_list_))
          ;               (car (nth (- (length acp_main_1_as10_list_ ) 1 ) acp_main_1_as10_list_))
          ;             )
          ;             20
          ;           )
          ;         )
          ;         (progn
          ;           (setq newline_ename_ (entlast))
          ;           (setq newline_obj_ (vlax-ename->vla-object newline_ename_))
          ;           (setq new_man_obj_ (vlax-ename->vla-object new_man_))
          ;           (command "pselect"  new_man_ "" )
          ;           (command "extend" new_man_  "" newline_ename_ "")
          ;           (command "breakatpoint" new_man_ (car (LM:intersections+0 newline_obj_ new_man_obj_ acextendnone )) )
          ;           (vla-delete (vlax-ename->vla-object new_man_))
          ;           (setq new_cut (entlast))
          ;           (setq new_ssset_4 (ssadd))
          ;           (setq new_ssset_4 (ssadd new_cut new_ssset_4 ))
          ;           (setq new_ssset_4 (ssadd newline_ename_ new_ssset_4 ))
          ;           (command "pedit" "m" new_ssset_4 "" "J" "" "")
                    
                    
                    
                    
                    
          ;         )
          ;       )
          ;       (;first_wave_case_4
          ;         (and
          ;           (= acp_main_1_as10_i (- (length acp_main_1_as10_list_ ) 2 ) )
          ;           (> 
          ;             (distance 
          ;               (car (nth acp_main_1_as10_i acp_main_1_as10_list_))
          ;               (car (nth (- (length acp_main_1_as10_list_ ) 1 ) acp_main_1_as10_list_))
          ;             )
          ;             20
          ;           )
          ;         )
          ;         (progn
          ;           (setq newline_ename_ (entlast))
          ;           (setq newline_obj_ (vlax-ename->vla-object newline_ename_))
          ;           (setq new_man_obj_ (vlax-ename->vla-object new_man_))
          ;           (command "pselect"  new_man_ "" )
          ;           (command "breakatpoint" new_man_ (car (LM:intersections+0 newline_obj_ new_man_obj_ acextendnone )) )
          ;           (command "extend" new_man_  "" newline_ename_ "")
                    
          ;           (vla-delete (vlax-ename->vla-object new_man_))
          ;           (setq new_cut (entlast))
          ;           (setq new_ssset_4 (ssadd))
          ;           (setq new_ssset_4 (ssadd new_cut new_ssset_4 ))
          ;           (setq new_ssset_4 (ssadd newline_ename_ new_ssset_4 ))
          ;           (command "pedit" "m" new_ssset_4 "" "J" "" "")
                    
                    
                    
                    
                    
          ;         )
          ;       )
          ;     )
          ;   ;
          ; ;
          (setq acp_main_1_as10_i (+ acp_main_1_as10_i 1))
        
      )
    ;

    (setvar "osmode" 1215)
  )
  (defun c:wash ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq acp_level_ ())
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_obj_area_ (vla-get-area ss_pre_filter_set_xx_obj_))
      
        (setq acp_level_ (cons  (list ss_pre_filter_set_xx_ename_ ss_pre_filter_set_xx_obj_area_ ) acp_level_ ) )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq acp_level_sorted_ (vl-sort acp_level_  ;bigest open indent list
                                              (function 
                                                (lambda (a b) 
                                                  (> (cadr a) (cadr b))
                                                )
                                              )
                          ) ;bigest close indent list
    )
  ;
  ;new copy acp plastic line
    (setq acp_plastic_ssset (ssadd))
      (setq acp_level_sorted_lv2_obj_ (vlax-ename->vla-object (car (nth 1 acp_level_sorted_))))
      (vla-copy acp_level_sorted_lv2_obj_)
    (setq acp_plastic_line_lv1 (entlast))
    (setq acp_plastic_ssset (ssadd  acp_plastic_line_lv1 acp_plastic_ssset))
      (setq acp_level_sorted_lv3_obj_ (vlax-ename->vla-object (car (nth 2 acp_level_sorted_))))
      (vla-copy acp_level_sorted_lv3_obj_)
    (setq acp_plastic_line_lv2 (entlast))
    (setq acp_plastic_ssset (ssadd  acp_plastic_line_lv2 acp_plastic_ssset))
    
  ;
  ;plastic_joint
    (setq acp_plastic_line_lv1_pt (cadr (TA:Get_Pline_vertext_ins_point_ acp_plastic_line_lv1 )))
    (setq acp_plastic_line_lv2_pt (cadr (TA:Get_Pline_vertext_ins_point_ acp_plastic_line_lv2 )))
    (command "pline" (car acp_plastic_line_lv1_pt)  (car acp_plastic_line_lv2_pt) "")
    (setq acp_plastic_ssset (ssadd  (entlast) acp_plastic_ssset))

    (setq acp_plastic_line_lv1_pt (reverse (cadr (TA:Get_Pline_vertext_ins_point_ acp_plastic_line_lv1 ))))
    (setq acp_plastic_line_lv2_pt (reverse (cadr (TA:Get_Pline_vertext_ins_point_ acp_plastic_line_lv2 ))))
    (command "pline" (car acp_plastic_line_lv1_pt)  (car acp_plastic_line_lv2_pt) "")
    (setq acp_plastic_ssset (ssadd  (entlast) acp_plastic_ssset))

    (command "_.pedit" "_m" acp_plastic_ssset "" "_j" "" "")
    (setq ename_for_hatch_ (entlast))
    (TA:CREATE_HATCH_ANSI37 ename_for_hatch_ 1 1)
    (vla-put-color (setq ename_for_hatch_obj_ (vlax-ename->vla-object ename_for_hatch_)) 1)
    (command "draworder" ename_for_hatch_ "" "F")
  ;

  ;front_sheet_process_
    (setq acp_front_sheet_ssset_ (ssadd))
    (setq acp_front_sheet_line_lv1_pt_Head (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 0 acp_level_sorted_ )) )))
    (setq acp_front_sheet_line_lv2_pt_Head (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 1 acp_level_sorted_ )) )))
    (command "pline"  (car acp_front_sheet_line_lv1_pt_Head ) (car acp_front_sheet_line_lv2_pt_Head) "")
    (setq acp_front_sheet_ssset_ (ssadd (entlast) acp_front_sheet_ssset_))

    
    (setq acp_front_sheet_line_lv1_pt_bot (reverse (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 0 acp_level_sorted_ )) ))))
    (setq acp_front_sheet_line_lv2_pt_bot (reverse (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 1 acp_level_sorted_ )) ))))
    (command "pline"  (car acp_front_sheet_line_lv1_pt_bot ) (car acp_front_sheet_line_lv2_pt_bot) "")
    (setq acp_front_sheet_ssset_ (ssadd (entlast) acp_front_sheet_ssset_))
    
    (setq acp_front_sheet_ssset_ (ssadd (car (nth 0 acp_level_sorted_ )) acp_front_sheet_ssset_))
    (setq acp_front_sheet_ssset_ (ssadd (car (nth 1 acp_level_sorted_ )) acp_front_sheet_ssset_))
    
    (command "_.pedit" "_m" acp_front_sheet_ssset_ "" "_j" "" "")
    (setq acp_front_sheet_for_hatch_ (entlast))
    (TA:CREATE_HATCH_ acp_front_sheet_for_hatch_ 1 1)
    (vla-put-color (setq acp_front_sheet_for_hatch_obj_ (vlax-ename->vla-object acp_front_sheet_for_hatch_)) 3)
    (command "draworder" acp_front_sheet_for_hatch_ "" "F")
  ;
  ;back_sheet_process_
    (setq acp_back_sheet_ssset_ (ssadd))
    (setq acp_back_sheet_line_lv3_pt_Head (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 2 acp_level_sorted_ )) )))
    (setq acp_back_sheet_line_lv4_pt_Head (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 3 acp_level_sorted_ )) )))
    (command "pline"  (car acp_back_sheet_line_lv3_pt_Head ) (car acp_back_sheet_line_lv4_pt_Head) "")
    (setq acp_back_sheet_ssset_ (ssadd (entlast) acp_back_sheet_ssset_))

    
    (setq acp_back_sheet_line_lv3_pt_bot (reverse (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 2 acp_level_sorted_ )) ))))
    (setq acp_back_sheet_line_lv4_pt_bot (reverse (cadr (TA:Get_Pline_vertext_ins_point_ (car (nth 3 acp_level_sorted_ )) ))))
    (command "pline"  (car acp_back_sheet_line_lv3_pt_bot ) (car acp_back_sheet_line_lv4_pt_bot) "")
    (setq acp_back_sheet_ssset_ (ssadd (entlast) acp_back_sheet_ssset_))
    
    (setq acp_back_sheet_ssset_ (ssadd (car (nth 2 acp_level_sorted_ )) acp_back_sheet_ssset_))
    (setq acp_back_sheet_ssset_ (ssadd (car (nth 3 acp_level_sorted_ )) acp_back_sheet_ssset_))
    
    (command "_.pedit" "_m" acp_back_sheet_ssset_ "" "_j" "" "")
    (setq acp_back_sheet_for_hatch_ (entlast))
    (TA:CREATE_HATCH_ acp_back_sheet_for_hatch_ 1 1)
    (vla-put-color (setq acp_back_sheet_for_hatch_obj_ (vlax-ename->vla-object acp_back_sheet_for_hatch_)) 3)
    (command "draworder" acp_back_sheet_for_hatch_ "" "F")
  ;
  )

;




(defun c:unisc_ ()
  (setq uni_ename_ (car (entsel )))
  (setq uni_obj_ (vlax-ename->vla-object uni_ename_))
  (setq user_input_name_block (LM:effectivename uni_obj_))
  (TA:set-block-blockscaling user_input_name_block acUniform) 
)










(defun c:Matchangle_Mang_ ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "insert") ;type of object
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
                                            (cons 0 "insert") ;type of object
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
  ;user_input_
    (setq mathc_angle_ (car (entsel "specify angle Object")))
    (setq mathc_angle_obj_ (vlax-ename->vla-object mathc_angle_))
    (setq match_angle_obj_val (rad-to-deg (vla-get-angle mathc_angle_obj_)))
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (vla-put-rotation ss_pre_filter_set_xx_hon_obj_ (deg-to-rad match_angle_obj_val)   ) 
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)







(defun c:TEMP_stamp_on_intersec_CT05_ ()
  (setvar "osmode" 0)
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline,xline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline,xline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;user_input_
    ;while_select_block_on_condition_
        (setq main_intersec_pline_ename_ nil)
        (while (= main_intersec_pline_ename_ nil)
          (setq main_intersec_pline_ename_ (car (entsel "specify main_intersec_pline_ename Object")))
          (if
            (and ;conditional_rule_for_select_object
              (/= main_intersec_pline_ename_ nil)
              (setq main_intersec_pline_ename_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
              (= (vla-get-objectname main_intersec_pline_ename_obj_ ) "AcDbPolyline")
              ; (= (vla-get-isdynamicblock main_intersec_pline_ename_obj_ ) :vlax-true)
              ; (/= (LM:getdynpropallowedvalues main_intersec_pline_ename_obj_ "view") nil)
            )
            (progn
              (setq main_intersec_pline_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
            )
            (setq main_intersec_pline_ename_ nil)
          )
        )
    ;
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq insertion_point_val_ (car (LM:intersections+0 ss_pre_filter_set_xx_hon_obj_ main_intersec_pline_obj_  acextendnone)))
      
      (command "insert" "xsa1" insertion_point_val_ 0.5 0)
  
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;)
  (setvar "osmode" 1215)
)
(defun c:TEMP_stamp_on_intersec_CT1_ ()
  (setvar "osmode" 0)
  ;user_input_
    (initget "Yes No")
    (setq ss_ ;Detect the variable's result in case of a function error
      (vl-catch-all-apply 
        (function 
          (lambda () 
            (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
          )
        )
      )
    )
    (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
      (progn
        ;selection_set_for_fillter_blk_name
          (if  ;pre_select_ssget_or_post_select_ssget
            (=
              (setq ss_pre_filter_set_xx_ (ssget "i"
                                                (list
                                                  (cons 0 "line") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 0)           ;kind of color call sign with color code index
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
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 0)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
      )
      (setq ss_pre_filter_set_xx_ ss_pre_filter_set_xx_)
    )
  ;
 
  ;user_input_
    ;while_select_block_on_condition_
        (setq main_intersec_pline_ename_ nil)
        (while (= main_intersec_pline_ename_ nil)
          (setq main_intersec_pline_ename_ (car (entsel "specify main_intersec_pline_ename Object")))
          (if
            (and ;conditional_rule_for_select_object
              (/= main_intersec_pline_ename_ nil)
              (setq main_intersec_pline_ename_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
              (= (vla-get-objectname main_intersec_pline_ename_obj_ ) "AcDbPolyline")
              ; (= (vla-get-isdynamicblock main_intersec_pline_ename_obj_ ) :vlax-true)
              ; (/= (LM:getdynpropallowedvalues main_intersec_pline_ename_obj_ "view") nil)
            )
            (progn
              (setq main_intersec_pline_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
              
              
              
            )
            (setq main_intersec_pline_ename_ nil)
          )
        )
    ;
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_hon_obj_angle (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_hon_obj_)))
      (setq insertion_point_val_ (car (LM:intersections+0 ss_pre_filter_set_xx_hon_obj_ main_intersec_pline_obj_  acextendnone)))
      
      (if ;lv1
        (and 
          (= 3 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 1 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 1 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if ;lv2
        (and 
          (= 1 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 2 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 0.75 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if ;lv2-1
        (and 
          (= 4 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 2 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 1 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if ;lv3
        (and 
          (= 3 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 4 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 0.50 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if ;lv4
        (and 
          (= 1 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 5 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 0.50 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if ;lv4-1
        (and 
          (= 4 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 5 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 0.50 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      (if 
        (and 
          (= 3 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
          (= 6 (vla-get-color main_intersec_pline_obj_))
        )
        (progn
          (command "insert" "xsa1" insertion_point_val_ 0.50 ss_pre_filter_set_xx_hon_obj_angle)
        )
      )
      
  
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;)
  (setvar "osmode" 1215)
)    
(defun c:TEMP_stamp_on_intersec_CT3_ ()
  (setvar "osmode" 0)
  ;user_input_
    (initget "Yes No")
    (setq ss_ ;Detect the variable's result in case of a function error
      (vl-catch-all-apply 
        (function 
          (lambda () 
            (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
          )
        )
      )
    )
    (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
      (progn
        ;selection_set_for_fillter_blk_name
          (if  ;pre_select_ssget_or_post_select_ssget
            (=
              (setq ss_pre_filter_set_xx_ (ssget "i"
                                                (list
                                                  (cons 0 "line") ;type of object
                                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 0)           ;kind of color call sign with color code index
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
                                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                  ; (cons 62 0)           ;kind of color call sign with color code index
                                                )
                                          )
              )
            )
            (sslength ss_pre_filter_set_xx_)
          )
        ;
      )
      (setq ss_pre_filter_set_xx_ ss_pre_filter_set_xx_)
    )
  ;
 
  ;user_input_
    ;while_select_block_on_condition_
        (setq main_intersec_pline_ename_ nil)
        (while (= main_intersec_pline_ename_ nil)
          (setq main_intersec_pline_ename_ (car (entsel "specify main_intersec_pline_ename Object")))
          (if
            (and ;conditional_rule_for_select_object
              (/= main_intersec_pline_ename_ nil)
              (setq main_intersec_pline_ename_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
              (= (vla-get-objectname main_intersec_pline_ename_obj_ ) "AcDbPolyline")
              ; (= (vla-get-isdynamicblock main_intersec_pline_ename_obj_ ) :vlax-true)
              ; (/= (LM:getdynpropallowedvalues main_intersec_pline_ename_obj_ "view") nil)
            )
            (progn
              (setq main_intersec_pline_obj_ (vlax-ename->vla-object main_intersec_pline_ename_))
              
              
              
            )
            (setq main_intersec_pline_ename_ nil)
          )
        )
    ;
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_hon_obj_angle (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_hon_obj_)))
      (setq insertion_point_val_ (car (LM:intersections+0 ss_pre_filter_set_xx_hon_obj_ main_intersec_pline_obj_  acextendnone)))
      
      (cond
        ( ;lv1
          (and 
            (= 3 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
            (= 1 (vla-get-color main_intersec_pline_obj_))
          )
          (progn
            (command "insert" "xsa1" insertion_point_val_ 1 ss_pre_filter_set_xx_hon_obj_angle)
          )
        )
        ( ;lv2
          (and 
            (= 1 (vla-get-color ss_pre_filter_set_xx_hon_obj_))
            (= 2 (vla-get-color main_intersec_pline_obj_))
          )
          (progn
            (command "insert" "xsa1" insertion_point_val_ 1 ss_pre_filter_set_xx_hon_obj_angle)
          )
        )
      )
      
  
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      (setq ss_pre_filter_set_xx_i (- ss_pre_filter_set_xx_i 1))
    )
  ;)
  (setvar "osmode" 1215)
)    
(defun c:TMEP_line_sorting_CT2 ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq line_list_ ())
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
      )
      (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_ename_ ))
      (setq line_list_ (cons sumlist_  line_list_ ))
      
      
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ (vl-sort line_list_  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;
  ;preloop_and_while
    (setq line_list_sorted_i 0)
    (setq line_list_sorted_i-color_ 1)
    (while (< line_list_sorted_i (length line_list_sorted_))
      (setq line_list_sorted_ename_ (cadr (nth  line_list_sorted_i line_list_sorted_)))
      (setq line_list_sorted_obj_ (vlax-ename->vla-object line_list_sorted_ename_))
      
      (cond
        (;_case_1
           (and
              (= line_list_sorted_i-color_ 1)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 1)
             (princ "_case_1")
           )
        )
        (;_case_2
           (and
              (= line_list_sorted_i-color_ 2)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 3)
             (princ "_case_2")
           )
        )
        (;_case_3
           (and
              (= line_list_sorted_i-color_ 3)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 4)
             (princ "_case_3")
           )
        )
        (;_case_4
           (and
              (= line_list_sorted_i-color_ 4)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 3)
             (princ "_case_4")
           )
        )
        (;_case_5
           (and
              (= line_list_sorted_i-color_ 5)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 1)
             (setq line_list_sorted_i-color_ 1)
             (princ "_case_5")
           )
        )

      )
      
      
      (setq line_list_sorted_i-color_ (+ line_list_sorted_i-color_ 1))
      (setq line_list_sorted_i (+ line_list_sorted_i 1))
    )
  ;
  
)
(defun c:TMEP_line_sorting_CT4 ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
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
                                            (cons 0 "line,lwpolyline") ;type of object
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
    (setq line_list_ ())
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq obj_type_ (vla-get-objectname ss_pre_filter_set_xx_hon_obj_))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_ename_
                            )
             )
             (setq line_list_ (cons sumlist_  line_list_ ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_ename_ ))
              (setq line_list_ (cons sumlist_  line_list_ ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ (vl-sort line_list_  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;
  ;preloop_and_while
    (setq line_list_sorted_i 0)
    (setq line_list_sorted_i-color_ 1)
    (while (< line_list_sorted_i (length line_list_sorted_))
      (setq line_list_sorted_ename_ (cadr (nth  line_list_sorted_i line_list_sorted_)))
      (setq line_list_sorted_obj_ (vlax-ename->vla-object line_list_sorted_ename_))
      
      (cond
        (;_case_1
           (and
              (= line_list_sorted_i-color_ 1)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 1)
             (princ "_case_1")
           )
        )
        (;_case_2
           (and
              (= line_list_sorted_i-color_ 2)
           )
           (progn
             (vla-put-color line_list_sorted_obj_ 2)
             (setq line_list_sorted_i-color_ 0)
             (princ "_case_2")
           )
        )
        ; (;_case_3
        ;    (and
        ;       (= line_list_sorted_i-color_ 3)
        ;    )
        ;    (progn
        ;      (vla-put-color line_list_sorted_obj_ 4)
        ;      (princ "_case_3")
        ;    )
        ; )
        ; (;_case_4
        ;    (and
        ;       (= line_list_sorted_i-color_ 4)
        ;    )
        ;    (progn
        ;      (vla-put-color line_list_sorted_obj_ 3)
        ;      (princ "_case_4")
        ;    )
        ; )
        ; (;_case_5
        ;    (and
        ;       (= line_list_sorted_i-color_ 5)
        ;    )
        ;    (progn
        ;      (vla-put-color line_list_sorted_obj_ 1)
        ;      (setq line_list_sorted_i-color_ 1)
        ;      (princ "_case_5")
        ;    )
        ; )

      )
      
      
      (setq line_list_sorted_i-color_ (+ line_list_sorted_i-color_ 1))
      (setq line_list_sorted_i (+ line_list_sorted_i 1))
    )
  ;
  
)
(defun c:TMEP_line_sorting_CT5 ()
  ;user_input_
    (setq input_sc_ (cond ( (getint (strcat "\nspecify get block scale \n<" (rtos (setq input_sc_ (cond (input_sc_) (1.0) ) ) ) "> : " ) ) ) (input_sc_) ) )
  ;
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_hon_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_hon_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_hon_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_hon_i 0)
    (setq line_list_hon ())
    (while (< ss_pre_filter_set_xx_hon_i (sslength ss_pre_filter_set_xx_hon_))
      (setq ss_pre_filter_set_xx_hon_ename_ (ssname ss_pre_filter_set_xx_hon_ ss_pre_filter_set_xx_hon_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_hon_ename_
                            )
             )
             (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_hon_ename_ ))
              (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_hon_i (+ ss_pre_filter_set_xx_hon_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_hon_ (vl-sort line_list_hon  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ver_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_ver_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_ver_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_ver_i 0)
    (setq line_list_ver ())
    (while (< ss_pre_filter_set_xx_ver_i (sslength ss_pre_filter_set_xx_ver_))
      (setq ss_pre_filter_set_xx_ver_ename_ (ssname ss_pre_filter_set_xx_ver_ ss_pre_filter_set_xx_ver_i))
      (setq ss_pre_filter_set_xx_ver_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_ver_ename_
                            )
             )
             (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_ver_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_ver_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_ver_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_ver_obj_startpt_ ss_pre_filter_set_xx_ver_ename_ ))
              (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_ver_i (+ ss_pre_filter_set_xx_ver_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ver_ (vl-sort line_list_ver  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 1 (car a)) (nth 1 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;preloop_and_while
    (setq line_list_sorted_hon_i 0)
    (while (< line_list_sorted_hon_i (length line_list_sorted_hon_))
      (setq line_list_sorted_hon_ename_ (cadr (nth  line_list_sorted_hon_i line_list_sorted_hon_)))
      (setq line_list_sorted_hon_obj_ (vlax-ename->vla-object line_list_sorted_hon_ename_))
      (setq line_list_sorted_hon_obj_color_ (vla-get-color line_list_sorted_hon_obj_))
      
      
        ;preloop_and_while
          (setq line_list_sorted_ver_i 0)
          ; (setq blk_scale_i (* (length line_list_sorted_ver_) 0.20))
          
          (setq blk_scale_i input_sc_)
          (while (< line_list_sorted_ver_i (length line_list_sorted_ver_))
            (setq line_list_sorted_ver_ename_ (cadr (nth  line_list_sorted_ver_i line_list_sorted_ver_)))
            (setq line_list_sorted_ver_obj_ (vlax-ename->vla-object line_list_sorted_ver_ename_))
            (setq line_list_sorted_ver_obj_color_ (vla-get-color line_list_sorted_ver_obj_))
            
            
            
              (cond
                (;color_case_1
                   (and
                      (= line_list_sorted_hon_obj_color_ 1)
                      (= line_list_sorted_ver_obj_color_ 1)
                      
                   )
                   (progn
                     (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                     (command "insert" "xsa1" insertion_point_val_ blk_scale_i 90)
                     (princ "color_case_1")
                   )
                )
                (;color_case_2
                   (and
                      (= line_list_sorted_hon_obj_color_ 2)
                      (= line_list_sorted_ver_obj_color_ 2)
                      
                   )
                   (progn
                     (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                     (command "insert" "xsa1" insertion_point_val_ blk_scale_i 90)
                     (princ "color_case_2")
                   )
                )
              )
            
            (if (=  "0.2" (rtos blk_scale_i 2 1))
              (progn
                (princ "S")
              )
                (setq blk_scale_i (- blk_scale_i 0.2))
            )
            
            (setq line_list_sorted_ver_i (+ line_list_sorted_ver_i 1))
          )
        ;
      
      (setq line_list_sorted_hon_i (+ line_list_sorted_hon_i 1))
    )
  ;
)
(defun c:TMEP_line_sorting_CT6 ()
  ;user-input_name_block_
    (setq user_input_name_block "X10")
    (TA:set-block-blockscaling user_input_name_block acUniform)
  ;
  ;user_input_
    ; (setq input_sc_ (cond ( (getint (strcat "\nspecify get block scale \n<" (rtos (setq input_sc_ (cond (input_sc_) (1.0) ) ) ) "> : " ) ) ) (input_sc_) ) )
  ;
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_hon_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_hon_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_hon_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_hon_i 0)
    (setq line_list_hon ())
    (while (< ss_pre_filter_set_xx_hon_i (sslength ss_pre_filter_set_xx_hon_))
      (setq ss_pre_filter_set_xx_hon_ename_ (ssname ss_pre_filter_set_xx_hon_ ss_pre_filter_set_xx_hon_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_hon_ename_
                            )
             )
             (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_hon_ename_ ))
              (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_hon_i (+ ss_pre_filter_set_xx_hon_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_hon_ (vl-sort line_list_hon  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (> (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ver_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_ver_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_ver_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_ver_i 0)
    (setq line_list_ver ())
    (while (< ss_pre_filter_set_xx_ver_i (sslength ss_pre_filter_set_xx_ver_))
      (setq ss_pre_filter_set_xx_ver_ename_ (ssname ss_pre_filter_set_xx_ver_ ss_pre_filter_set_xx_ver_i))
      (setq ss_pre_filter_set_xx_ver_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_ver_ename_
                            )
             )
             (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_ver_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_ver_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_ver_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_ver_obj_startpt_ ss_pre_filter_set_xx_ver_ename_ ))
              (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_ver_i (+ ss_pre_filter_set_xx_ver_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ver_ (vl-sort line_list_ver  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 1 (car a)) (nth 1 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;preloop_and_while
    (setq line_list_sorted_hon_i 0)
    (while (< line_list_sorted_hon_i (length line_list_sorted_hon_))
      (setq line_list_sorted_hon_ename_ (cadr (nth  line_list_sorted_hon_i line_list_sorted_hon_)))
      (setq line_list_sorted_hon_obj_ (vlax-ename->vla-object line_list_sorted_hon_ename_))
      (setq line_list_sorted_hon_obj_color_ (vla-get-color line_list_sorted_hon_obj_))
      
      
        ;preloop_and_while
          (setq line_list_sorted_ver_i 0)
          ; (setq blk_scale_i (* (length line_list_sorted_ver_) 0.20))
          (setq blk_scale_i 1)
      
          (while (< line_list_sorted_ver_i (length line_list_sorted_ver_))
            (setq line_list_sorted_ver_ename_ (cadr (nth  line_list_sorted_ver_i line_list_sorted_ver_)))
            (setq line_list_sorted_ver_obj_ (vlax-ename->vla-object line_list_sorted_ver_ename_))
            (setq line_list_sorted_ver_obj_color_ (vla-get-color line_list_sorted_ver_obj_))
            
            
            
              (cond
                (;color_case_1
                   (and
                      (= line_list_sorted_hon_obj_color_ 1)
                      (= line_list_sorted_ver_obj_color_ 1)
                      
                   )
                   (progn
                     (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                     (command "insert" user_input_name_block insertion_point_val_ blk_scale_i 0)
                     (princ "color_case_1")
                   )
                )
                (;color_case_2
                   (and
                      (= line_list_sorted_hon_obj_color_ 2)
                      (= line_list_sorted_ver_obj_color_ 2)
                      
                   )
                   (progn
                     (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                     (command "insert" user_input_name_block insertion_point_val_ blk_scale_i 0)
                     (princ "color_case_2")
                   )
                )
              )
            
            ; (if (=  "0.2" (rtos blk_scale_i 2 1))
            ;   (progn
            ;     (princ "S")
            ;   )
            ;     (setq blk_scale_i (- blk_scale_i 0.2))
            ; )
            
            (setq line_list_sorted_ver_i (+ line_list_sorted_ver_i 1))
          )
        ;
      
      (setq line_list_sorted_hon_i (+ line_list_sorted_hon_i 1))
    )
  ;
  
)
(defun c:TMEP_line_sorting_CT61 ()
  ; ;user-input_name_block_
    ;   (setq user_input_name_block "X10")
    ;   (TA:set-block-blockscaling user_input_name_block acUniform)
  ; ;
  ;user_input_
    ; (setq input_sc_ (cond ( (getint (strcat "\nspecify get block scale \n<" (rtos (setq input_sc_ (cond (input_sc_) (1.0) ) ) ) "> : " ) ) ) (input_sc_) ) )
    (setq input_dia_ (cond ( (getreal (strcat "\nspecify get block scale \n<" (rtos (setq input_dia_ (cond (input_dia_) (1.0) ) ) ) "> : " ) ) ) (input_dia_) ) )
  ;
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_hon_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_hon_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_hon_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_hon_i 0)
    (setq line_list_hon ())
    (while (< ss_pre_filter_set_xx_hon_i (sslength ss_pre_filter_set_xx_hon_))
      (setq ss_pre_filter_set_xx_hon_ename_ (ssname ss_pre_filter_set_xx_hon_ ss_pre_filter_set_xx_hon_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_hon_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_hon_ename_
                            )
             )
             (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_hon_ename_ ))
              (setq line_list_hon (cons sumlist_  line_list_hon ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_hon_i (+ ss_pre_filter_set_xx_hon_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_hon_ (vl-sort line_list_hon  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (> (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ver_ (ssget "i"
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
        nil
      )
      (progn
        (setq ss_pre_filter_set_xx_ver_ (ssget 
                                          (list
                                            (cons 0 "line,lwpolyline") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_ver_)
    )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_ver_i 0)
    (setq line_list_ver ())
    (while (< ss_pre_filter_set_xx_ver_i (sslength ss_pre_filter_set_xx_ver_))
      (setq ss_pre_filter_set_xx_ver_ename_ (ssname ss_pre_filter_set_xx_ver_ ss_pre_filter_set_xx_ver_i))
      (setq ss_pre_filter_set_xx_ver_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_))
      (setq obj_type_ (vla-get-objectname (vlax-ename->vla-object ss_pre_filter_set_xx_ver_ename_)))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_ver_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_ver_ename_
                            )
             )
             (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_ver_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_ver_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_ver_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_ver_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_ver_obj_startpt_ ss_pre_filter_set_xx_ver_ename_ ))
              (setq line_list_ver (cons sumlist_  line_list_ver ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_ver_i (+ ss_pre_filter_set_xx_ver_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ver_ (vl-sort line_list_ver  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 1 (car a)) (nth 1 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;

  ;preloop_and_while
    (setq line_list_sorted_hon_i 0)
    (setq intersec_list_ ())
    (while (< line_list_sorted_hon_i (length line_list_sorted_hon_))
      (setq line_list_sorted_hon_ename_ (cadr (nth  line_list_sorted_hon_i line_list_sorted_hon_)))
      (setq line_list_sorted_hon_obj_ (vlax-ename->vla-object line_list_sorted_hon_ename_))
      (setq line_list_sorted_hon_obj_color_ (vla-get-color line_list_sorted_hon_obj_))
      
      
        ;preloop_and_while
          (setq line_list_sorted_ver_i 0)
          ; (setq blk_scale_i (* (length line_list_sorted_ver_) 0.20))
          (setq blk_scale_i 1)
      
          (while (< line_list_sorted_ver_i (length line_list_sorted_ver_))
            (setq line_list_sorted_ver_ename_ (cadr (nth  line_list_sorted_ver_i line_list_sorted_ver_)))
            (setq line_list_sorted_ver_obj_ (vlax-ename->vla-object line_list_sorted_ver_ename_))
            (setq line_list_sorted_ver_obj_color_ (vla-get-color line_list_sorted_ver_obj_))
            
            
            
              (cond
                (;color_case_1
                   (and
                      (= line_list_sorted_hon_obj_color_ 1)
                      (= line_list_sorted_ver_obj_color_ 1)
                      
                   )
                   (progn
                      (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                      (setq intersec_list_ (cons insertion_point_val_ intersec_list_ ))
                    ;  (command "insert" user_input_name_block insertion_point_val_ blk_scale_i 0)
                     (princ "color_case_1")
                   )
                )
                (;color_case_2
                   (and
                      (= line_list_sorted_hon_obj_color_ 2)
                      (= line_list_sorted_ver_obj_color_ 2)
                      
                   )
                   (progn
                     (setq insertion_point_val_ (car (LM:intersections+0 line_list_sorted_hon_obj_ line_list_sorted_ver_obj_  acextendnone)))
                    ;  (command "insert" user_input_name_block insertion_point_val_ blk_scale_i 0)
                     (princ "color_case_2")
                   )
                )
              )
            
            ; (if (=  "0.2" (rtos blk_scale_i 2 1))
            ;   (progn
            ;     (princ "S")
            ;   )
            ;     (setq blk_scale_i (- blk_scale_i 0.2))
            ; )
            
            (setq line_list_sorted_ver_i (+ line_list_sorted_ver_i 1))
          )
        ;
      
      (setq line_list_sorted_hon_i (+ line_list_sorted_hon_i 1))
    )
  ;
  
  ;foreach intersec circle
    (foreach i intersec_list_ 
      (TA:add_full_circle_ i (/  input_dia_ 2))
    )
  ;
)

(defun c:c:TMEP_line_sorting_CT7 ()
  ;user-input_name_block_
    (setq user_input_name_block "X15")
    (TA:set-block-explodable user_input_name_block acany)
    (TA:set-block-explodable user_input_name_block acUniform)
  ;
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq line_list_ ())
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_hon_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq obj_type_ (vla-get-objectname ss_pre_filter_set_xx_hon_obj_))
      (cond
        (;obj_type_case_1
           (and
              (= obj_type_ "AcDbPolyline" )

           )
           (progn
             (setq sumlist_ (list   
                              (list 
                                (car (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-coordinates ss_pre_filter_set_xx_hon_obj_ ))))
                                0
                              )
                              ss_pre_filter_set_xx_ename_
                            )
             )
             (setq line_list_ (cons sumlist_  line_list_ ))
             (princ "obj_type_case_1")
           )
        )
        (;obj_type_case_2
           (and
              (= obj_type_ "AcDbLine" )

           )
           (progn
             (setq ss_pre_filter_set_xx_hon_obj_start_end_pt_ (list
                                      (setq ss_pre_filter_set_xx_hon_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                               )
                                      )
                                      (setq ss_pre_filter_set_xx_hon_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_hon_obj_ ))
                                                             )
                                      )
                                    )
              )
              (setq sumlist_ (list ss_pre_filter_set_xx_hon_obj_startpt_ ss_pre_filter_set_xx_ename_ ))
              (setq line_list_ (cons sumlist_  line_list_ ))
             (princ "obj_type_case_2")
           )
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq line_list_sorted_ (vl-sort line_list_  ;bigest open indent list
                                    (function 
                                      (lambda (a b) 
                                        (< (nth 0 (car a)) (nth 0 (car b)))
                                      )
                                    )
                            ) ;bigest close indent list
    )
  ;
  ;preloop_and_while
    (setq line_list_sorted_i 0)
    (setq line_list_sorted_i-color_ 1)
    (while (< line_list_sorted_i (length line_list_sorted_))
      (setq line_list_sorted_ename_ (cadr (nth  line_list_sorted_i line_list_sorted_)))
      (setq line_list_sorted_obj_ (vlax-ename->vla-object line_list_sorted_ename_))
      (setq line_list_sorted_obj_angle (rad-to-deg (vla-get-angle line_list_sorted_obj_)))
      (setq line_list_sorted_obj_color (vla-get-color line_list_sorted_obj_))
      (setq line_list_sorted_obj_start_end_pt_ (list
                                      (setq line_list_sorted_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint line_list_sorted_obj_ ))
                                                               )
                                      )
                                      (setq line_list_sorted_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint line_list_sorted_obj_ ))
                                                             )
                                      )
                                    )
      )
      
      
      
   
      
      (cond
        (;_case_1
           (and
              (= line_list_sorted_obj_color 1)
           )
           (progn
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (* 0)
                      )
                     1
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (* 236 1)
                      )
                      0.6
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (* 236 2)
                      )
                      0.2
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (* 236 3)
                      )
                      0.2
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (princ "_case_1")
           )
        )
        (;_case_2
           (and
              (= line_list_sorted_obj_color 2)
           )
           (progn
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (+ 118 (*  0 118))
                      )
                      0.80
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (+ 118 (*  2 118))
                      )
                      0.4
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (+ 118 (*  4 118))
                      )
                      0.2
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             (command "insert" 
                      user_input_name_block
                      (polar line_list_sorted_obj_startpt_ 
                             (vla-get-angle line_list_sorted_obj_)
                             (+ 118 (*  6 118))
                      )
                      0.2
                      (rad-to-deg (vla-get-angle line_list_sorted_obj_))
             )
             
             
             (princ "_case_2")
           )
        )
        
      )
      
      
      (setq line_list_sorted_i-color_ (+ line_list_sorted_i-color_ 1))
      (setq line_list_sorted_i (+ line_list_sorted_i 1))
    )
  ;
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
(defun c:add_text_length_to_point_TT1 ()
  

  
  ;userinput_
    (setq newline_ (car (entsel)))
    (setq text_height (cond ( (getreal (strcat "\nspecify text height<" (rtos (setq text_height (cond (text_height) (1.0) ) ) ) "> : " ) ) ) (text_height) ) )
  ;
  ;getdata_
    (setq newline_pt_ (cadr (TA:Get_Pline_vertext_ins_point_ newline_)))
    (setq text_height_ 2)
    (setq newline_length_ (TA:get_vertex_len_ newline_))
  ;
  
  
  ;preloop_and_while
    (setq text_loop_i 1)
    (while (< text_loop_i (length newline_pt_))
      (setq text_loop_pt_ (nth  text_loop_i newline_pt_))
      (setq text_loop_L_ (nth  (- text_loop_i 1) newline_length_))
      
        ; (create-text-vla 
          ;   (setq ins_pt (vlax-3d-point 
          ;                  (list 
          ;                    (car text_loop_pt_)
          ;                    (cadr text_loop_pt_)
          ;                    0
          ;                  )
          ;                )
          ;   ) 
          ;   (rtos text_loop_L_ 2 2) 
          ;   text_height_  
          ;   24 
        ; )
      (command "text" 
                (list 
                  (car text_loop_pt_)
                  (cadr text_loop_pt_)
                  0
                )
                1
                0
                (rtos text_loop_L_ 2 2)
      )
      (setq text_loop_i (+ text_loop_i 1))
    )
  ;
)



(defun c:temp_inc ()
  

  ;user_input_
    (setq increment_number_ (cond ( (getint (strcat "\nSpecify increment_number_   <" (rtos (setq increment_number_ (cond (increment_number_) (1.0) ) ) ) "> : " ) ) ) (increment_number_) ) )
  ;
  ;user_att_
    (setq att_name_1 "NO.BUBBLE_VALUE_1")
    (setq att_name_2 "NO_VALUE_1")
    (setq axis_sort "Y" )
    (setq efname_blk "000 - DYNAMIC_SYMBOL_FAMELINE_MATTERIAL_BOX_AND_BUBBLE" )

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
  ;filter_and_sort_selection_by_efname_
    (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ efname_blk))
    (setq ss_pre_filter_set_xx_sorted_ (reverse (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx_ axis_sort )))
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_sorted_i 0)
    (while (< ss_pre_filter_set_xx_sorted_i (length ss_pre_filter_set_xx_sorted_))
      (setq ss_pre_filter_set_xx_sorted_ename_ (car (nth  ss_pre_filter_set_xx_sorted_i ss_pre_filter_set_xx_sorted_)))
      (setq ss_pre_filter_set_xx_sorted_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_sorted_ename_))
      
      
      (LM:vl-setattributevalue ss_pre_filter_set_xx_sorted_obj_ att_name_1 increment_number_ )
      (LM:vl-setattributevalue ss_pre_filter_set_xx_sorted_obj_ att_name_2 increment_number_ )
      
      
      (setq increment_number_ (+ increment_number_ 1))
      (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
    )
  ;
)
(defun c:temp_inc1 ()
  

  ;user_input_
    (setq increment_number_ (cond ( (getint (strcat "\nSpecify increment_number_   <" (rtos (setq increment_number_ (cond (increment_number_) (1.0) ) ) ) "> : " ) ) ) (increment_number_) ) )
  ;
  ;user_att_
    (setq att_name_ "no_value_1")
    (setq axis_sort "Y" )
    (setq efname_blk "A$C09877c55" )

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
  ;filter_and_sort_selection_by_efname_
    (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ efname_blk))
    (setq ss_pre_filter_set_xx_sorted_ (reverse (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx_ axis_sort )))
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_sorted_i 0)
    (while (< ss_pre_filter_set_xx_sorted_i (length ss_pre_filter_set_xx_sorted_))
      (setq ss_pre_filter_set_xx_sorted_ename_ (car (nth  ss_pre_filter_set_xx_sorted_i ss_pre_filter_set_xx_sorted_)))
      (setq ss_pre_filter_set_xx_sorted_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_sorted_ename_))
      
      (LM:vl-setattributevalue ss_pre_filter_set_xx_sorted_obj_ att_name_ increment_number_ )
      
      
      (setq increment_number_ (+ increment_number_ 1))
      (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
    )
  ;
)




(defun c:resc ()
  

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
  ;user_input_
    (setq input_sc_ (cond ( (getreal (strcat "\nspecify angle<" (rtos (setq input_sc_ (cond (input_sc_) (1.2) ) ) ) "> : " ) ) ) (input_sc_) ) )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))

        (setq ss_pre_filter_set_xx_obj_xsc_ (vla-get-xscalefactor ss_pre_filter_set_xx_obj_))

        (setq new_input_sc_ (* input_sc_ ss_pre_filter_set_xx_obj_xsc_))
      
        (vla-put-xscalefactor ss_pre_filter_set_xx_obj_ new_input_sc_)
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)

(defun c:temp_tte2 ()
  

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
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
      (setq ex_H_ (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H"))
      (setq new_H_ (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "H" (- ex_H_ 3.5) ))
      (setq new_ang_ (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "distance1" 25 ))
      
      
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;


)

(defun c:c:temp_tte3 ()
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
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (if (= (LM:Effectivename ss_pre_filter_set_xx_obj_ ) "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint")
        (progn
          (setq H (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H"))
          (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "H" (+ H 50))
        )
      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  
)




(defun c:temp_PPLINE_FOLDING_ALSHEET_FAL_ ()
  

  (setq ALsheet_ename_ (car (entsel "specify Object")))

  (setq get_width_panel_ (cond ( (getint (strcat "\nspecify get get_width_panel_ <" (rtos (setq get_width_panel_ (cond (get_width_panel_) (1.0) ) ) ) "> : " ) ) ) (get_width_panel_) ) )
  
  (if 
    (and
      (= (vla-get-objectname (vlax-ename->vla-object ALsheet_ename_)) 
        "AcDbBlockReference"
      )
      (= (LM:effectivename (vlax-ename->vla-object ALsheet_ename_)) 
        "000 - PP_LINE_DYN_LENGTH_PANEL_CUT_VIEW"
      )
    )
    (progn
      (setq ALsheet_obj_ (vlax-ename->vla-object ALsheet_ename_) )
      (setq main_width_0 0)
      (setq main_width_1 0)
      (cond
        (;visibility_case_1
          (and
              (= (LM:getdynpropvalue ALsheet_obj_ "view" ) "1_")

          )
          (progn
            (setq main_width_0 (LM:getdynpropvalue ALsheet_obj_ "main_width_0" ) )
            (setq wing_right_ (LM:getdynpropvalue ALsheet_obj_ "wing_right_" ) )
            (setq wing_left_ (LM:getdynpropvalue ALsheet_obj_ "wing_left_0" ) )
            (princ "visibility_case_1")
          )
        )
        (;visibility_case_2
          (and
              (= (LM:getdynpropvalue ALsheet_obj_ "view" ) "2_")
          )
          (progn
            (setq main_width_0 (LM:getdynpropvalue ALsheet_obj_ "main_width_0" ) )
            (setq main_width_1 (LM:getdynpropvalue ALsheet_obj_ "main_width_1" ) )
            (setq wing_right_ (LM:getdynpropvalue ALsheet_obj_ "wing_right_" ) )
            (setq wing_left_ (LM:getdynpropvalue ALsheet_obj_ "wing_left_1" ) )
            (princ "visibility_case_2")
          )
        )
        ; (;visibility_case_3
        ;    (and
        ;       ()
        ;       ()
        ;    )
        ;    (progn
        ;      ()
        ;      (princ "visibility_case_3")
        ;    )
        ; )
        ; (;visibility_case_4
        ;    (and
        ;       ()
        ;       ()
        ;    )
        ;    (progn
        ;      ()
        ;      (princ "visibility_case_4")
        ;    )
        ; )
      )
      ;insertion_blk_process_
        (setq ins_point_ (getpoint "specify point for insertion point "))
        (command "insert" "000 - PP_LINE_DYN_LENGTH_PANEL_FRONT_VIEW" ins_point_ 1 0 )
        (setq new_dyn_ename_ (entlast))
        (setq new_dyn_obj_ (vlax-ename->vla-object new_dyn_ename_))
          (LM:setdynpropvalue new_dyn_obj_ "main_width_total_X" get_width_panel_)
          (LM:setdynpropvalue new_dyn_obj_ "wing_right_" wing_right_)
          (LM:setdynpropvalue new_dyn_obj_ "wing_left_" wing_left_)
          (LM:setdynpropvalue new_dyn_obj_ "main_width_total_Y" (+ main_width_0 main_width_1) )
          (LM:setdynpropvalue new_dyn_obj_ "view" "2_" )
          
          (LM:vl-setattributevalue new_dyn_obj_ "PART_NAME_" (strcat 
                                                               "AL.SHEET_PANEL_" 
                                                               (rtos (+ main_width_0 main_width_1) 2 2)
                                                               "x"
                                                               (rtos get_width_panel_ 2 2)
                                                               "mm."
                                                             )
          )
          ;
            
            
            
          ;
        
    )
  )
  (if 
    (and
      (= (vla-get-objectname (vlax-ename->vla-object ALsheet_ename_)) 
        "AcDbPolyline"
      )
    )
    (progn
      (setq list_width_ (TA:get_vertex_len_ ALsheet_ename_))
      (setq wing_left_ (car (TA:get_vertex_len_ ALsheet_ename_)))
      (setq wing_right_ (car (reverse (TA:get_vertex_len_ ALsheet_ename_))))
      ;preloop_and_while
        (setq list_width_i 1)
        (setq list_width_val_sum_ 0)
        (while (< list_width_i (- (length list_width_) 1))
          (setq list_width_val_ (nth  list_width_i list_width_))
          (setq list_width_val_sum_ (+ list_width_val_sum_ list_width_val_ ))
      
          (setq list_width_i (+ list_width_i 1))
        )
      ;
      (setq ins_point_ (getpoint "specify point for insertion point "))
      (command "insert" "000 - PP_LINE_DYN_LENGTH_PANEL_FRONT_VIEW" ins_point_ 1 0 )
      (setq new_dyn_ename_ (entlast))
      (setq new_dyn_obj_ (vlax-ename->vla-object new_dyn_ename_))
        (LM:setdynpropvalue new_dyn_obj_ "main_width_total_X" get_width_panel_)
        (LM:setdynpropvalue new_dyn_obj_ "wing_right_" wing_right_)
        (LM:setdynpropvalue new_dyn_obj_ "wing_left_" wing_left_)
        (LM:setdynpropvalue new_dyn_obj_ "main_width_total_Y" list_width_val_sum_ )
        (LM:setdynpropvalue new_dyn_obj_ "view" "2_" )
        
        (LM:vl-setattributevalue new_dyn_obj_ "PART_NAME_" (strcat 
                                                            "AL.SHEET_PANEL_" 
                                                            (rtos list_width_val_sum_ 2 2)
                                                            "x"
                                                            (rtos get_width_panel_ 2 2)
                                                            "mm."
                                                          )
        )
    )
  )
  
)


(defun c:temp_PPLINE_INSERTION_BRACKET_INSBKK_ ()
  (setq PLine_ename_ (car (entsel "specify Object")))

  (if (= (vla-get-objectname (vlax-ename->vla-object PLine_ename_)) "AcDbPolyline")
    (progn
      (setq pline_pt_ (TA:Get_Pline_vertext_ins_point_ PLine_ename_))
      ; (TA:get_vertex_len_ PLine_ename_)
    ;
      (setq pline_pt_1pt (TA:midpoint (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
      (setq pline_pt_1dist (distance (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
      (setq pline_pt_1angle (angle (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
        (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_1pt 1 0 )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
        (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_1dist )
        (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
        
      (setq pline_pt_2pt (TA:midpoint (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
      (setq pline_pt_2dist (distance (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
      (setq pline_pt_2angle (rad-to-deg (angle (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) )))
        (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_2pt 1 pline_pt_2angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
        (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_2dist )
        (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
          
      
      
      (setq pline_pt_3pt (TA:midpoint (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
      (setq pline_pt_3dist (distance (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
      (setq pline_pt_3angle (rad-to-deg (angle (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) )))
        (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_3pt 1 pline_pt_3angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
        (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_3dist )
        (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
      
      (setq pline_pt_4pt (TA:midpoint (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
      (setq pline_pt_4dist (distance (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
      (setq pline_pt_4angle (rad-to-deg (angle (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) )))
        (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_4pt 1 pline_pt_4angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
        (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_4dist )
        (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
    ;
      
    )
  )
  (if (= (vla-get-objectname (vlax-ename->vla-object PLine_ename_)) "AcDbBlockReference")
    (progn
      (setq new_dyn_obj_ (vlax-ename->vla-object PLine_ename_))
      (setq new_dyn_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint new_dyn_obj_))))
      (setq point1_xy_
        (list
          (setq point1_x (+ (car new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position1 x" )))
          (setq point1_y (+ (cadr new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position1 y" )))
        )
      )
      (setq point2_xy_
        (list
          (setq point2_x (+ (car new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position2 x" )))
          (setq point2_y (+ (cadr new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position2 y" )))
        )
      )
      (setq point3_xy_
        (list
          (setq point3_x (+ (car new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position3 x" )))
          (setq point3_y (+ (cadr new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position3 y" )))
        )
      )
      (setq point4_xy_
        (list
          (setq point4_x (+ (car new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position4 x" )))
          (setq point4_y (+ (cadr new_dyn_obj_ins_) (LM:getdynpropvalue new_dyn_obj_ "position4 y" )))
        )
      )
      (setq pline_pt_ (list "sss" (list point1_xy_ point2_xy_ point3_xy_ point4_xy_ )))
      ;
        (setq pline_pt_1pt (TA:midpoint (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
        (setq pline_pt_1dist (distance (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
        (setq pline_pt_1angle (angle (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
          (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_1pt 1 0 )
          (setq new_bk_ename_ (entlast))
          (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
          (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
          (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_1dist )
          (LM:setdynpropvalue new_bk_obj_ "move" 50 )
          (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
          
        (setq pline_pt_2pt (TA:midpoint (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
        (setq pline_pt_2dist (distance (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
        (setq pline_pt_2angle (rad-to-deg (angle (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) )))
          (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_2pt 1 pline_pt_2angle )
          (setq new_bk_ename_ (entlast))
          (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
          (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
          (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_2dist )
          (LM:setdynpropvalue new_bk_obj_ "move" 50 )
          (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
            
        
        
        (setq pline_pt_3pt (TA:midpoint (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
        (setq pline_pt_3dist (distance (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
        (setq pline_pt_3angle (rad-to-deg (angle (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) )))
          (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_3pt 1 pline_pt_3angle )
          (setq new_bk_ename_ (entlast))
          (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
          (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
          (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_3dist )
          (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
        
        (setq pline_pt_4pt (TA:midpoint (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
        (setq pline_pt_4dist (distance (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
        (setq pline_pt_4angle (rad-to-deg (angle (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) )))
          (command "insert" "000 - DYNAMIC_VIEW_ASSCESSORIES_FAMELINE_ALEXTR._EQ.ANGLE-PROFILE_25x25mm._AG-25_COMBINE_VIEW" pline_pt_4pt 1 pline_pt_4angle )
          (setq new_bk_ename_ (entlast))
          (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
          (LM:setdynpropvalue new_bk_obj_ "view" "4_side_L_view")
          (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_4dist )
          (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
      ;
    )
  )
)
(defun c:temp_PPLINE_INSERTION_STLINE ()
  (setq PLine_ename_ (car (entsel "specify Object")))
  (if (= (vla-get-objectname (vlax-ename->vla-object PLine_ename_)) "AcDbPolyline")
    (progn
      (setq pline_pt_ (TA:Get_Pline_vertext_ins_point_ PLine_ename_))
      (TA:get_vertex_len_ PLine_ename_)
    ;
      (setq pline_pt_1pt (TA:midpoint (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
      (setq pline_pt_1dist (distance (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) ))
      (setq pline_pt_1angle (rad-to-deg (angle (nth 0 (nth 1 pline_pt_)) (nth 1 (nth 1 pline_pt_)) )))
        (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" (nth 0 (nth 1 pline_pt_)) 1 pline_pt_1angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "h" pline_pt_1dist)
        (LM:setdynpropvalue new_bk_obj_ "w" 50)
        ; (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_1dist )
        ; (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
        
      (setq pline_pt_2pt (TA:midpoint (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
      (setq pline_pt_2dist (distance (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) ))
      (setq pline_pt_2angle (rad-to-deg (angle (nth 1 (nth 1 pline_pt_)) (nth 2 (nth 1 pline_pt_)) )))
        (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" (nth 1 (nth 1 pline_pt_)) 1 pline_pt_2angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "h" pline_pt_2dist)
        (LM:setdynpropvalue new_bk_obj_ "w" 50)
        ; (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_2dist )
        ; (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
          
      
      
      (setq pline_pt_3pt (TA:midpoint (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
      (setq pline_pt_3dist (distance (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) ))
      (setq pline_pt_3angle (rad-to-deg (angle (nth 2 (nth 1 pline_pt_)) (nth 3 (nth 1 pline_pt_)) )))
        (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" (nth 2 (nth 1 pline_pt_)) 1 pline_pt_3angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "h" pline_pt_3dist)
        (LM:setdynpropvalue new_bk_obj_ "w" 50)
        ; (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_3dist )
        ; (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
      
      (setq pline_pt_4pt (TA:midpoint (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
      (setq pline_pt_4dist (distance (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) ))
      (setq pline_pt_4angle (rad-to-deg (angle (nth 3 (nth 1 pline_pt_)) (nth 0 (nth 1 pline_pt_)) )))
        (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" (nth 3 (nth 1 pline_pt_)) 1 pline_pt_4angle )
        (setq new_bk_ename_ (entlast))
        (setq new_bk_obj_ (vlax-ename->vla-object new_bk_ename_))
        (LM:setdynpropvalue new_bk_obj_ "h" pline_pt_4dist)
        (LM:setdynpropvalue new_bk_obj_ "w" 50)
        ; (LM:setdynpropvalue new_bk_obj_ "array_width" pline_pt_4dist )
        ; (LM:setdynpropvalue new_bk_obj_ "Flip state1" 1 )
    ;
      
    )
  )
)
(defun c:temp_PPLINE_INSERTION_SSTLINE ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ;user_input_
    (setq get_steel_size (cond ( (getint (strcat "\nspecify get get_steel_size" (rtos (setq get_steel_size (cond (get_steel_size) (1.0) ) ) ) "> : " ) ) ) (get_steel_size) ) )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq PLine_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq PLine_obj_ (vlax-ename->vla-object PLine_ename_))
      (setq pline_obj_start_end_pt_ (list
                                      (setq pline_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint pline_obj_ ))
                                                               )
                                      )
                                      (setq pline_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint pline_obj_ ))
                                                             )
                                      )
                                    )
      )
      (setq pline_obj_get_angle (rad-to-deg (vla-get-angle pline_obj_)))
      (setq pline_obj_get_length (vla-get-length pline_obj_))
      (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID_midpoint" pline_obj_startpt_ 1 pline_obj_get_angle )
      (setq pline_new_ename_ (entlast))
      (setq pline_new_obj_ (vlax-ename->vla-object pline_new_ename_))
      (LM:setdynpropvalue pline_new_obj_ "H" pline_obj_get_length )
      (LM:setdynpropvalue pline_new_obj_ "W" get_steel_size )
      
        
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)
(defun c:temp_PPLINE_INSERTION_GGLINE ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
  ;
  ; ;user_input_
  ;   (setq get_steel_size (cond ( (getint (strcat "\nspecify get get_steel_size" (rtos (setq get_steel_size (cond (get_steel_size) (1.0) ) ) ) "> : " ) ) ) (get_steel_size) ) )
  ; ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq PLine_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq PLine_obj_ (vlax-ename->vla-object PLine_ename_))
      (setq pline_obj_start_end_pt_ (list
                                      (setq pline_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint pline_obj_ ))
                                                               )
                                      )
                                      (setq pline_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint pline_obj_ ))
                                                             )
                                      )
                                    )
      )
      (if ;reverse object process
        (or 
          (> (car pline_obj_startpt_) (car pline_obj_endpt_))
          (< (cadr pline_obj_startpt_) (cadr pline_obj_endpt_))
        )
        (progn
          (command "reverse" PLine_ename_ "")
          (setq pline_obj_start_end_pt_ (list
                                      (setq pline_obj_startpt_ (vlax-safearray->list
                                                                 (vlax-variant-value ( vla-get-startpoint pline_obj_ ))
                                                               )
                                      )
                                      (setq pline_obj_endpt_ (vlax-safearray->list
                                                                (vlax-variant-value ( vla-get-endpoint pline_obj_ ))
                                                             )
                                      )
                                    )
          )
        )
      )
      
      
      (setq pline_obj_get_angle (rad-to-deg (vla-get-angle pline_obj_)))
      (setq pline_obj_get_length (vla-get-length pline_obj_))
      (command "insert" "000-GRID_LINE_DYN" pline_obj_startpt_ 1 (+ 90 pline_obj_get_angle) )
      
      (setq pline_new_ename_ (entlast))
      (setq pline_new_obj_ (vlax-ename->vla-object pline_new_ename_))
      (vla-put-layer pline_new_obj_ "000 - GRID")
      (vla-put-color pline_new_obj_ 256)
      
      (LM:setdynpropvalue pline_new_obj_ "H" pline_obj_get_length )
      (LM:setdynpropvalue pline_new_obj_ "offset" 250 )
      
      
      (vla-delete pline_obj_)
        
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)


(defun c:temp_PPLINE_summary_matterial_summ_ ()
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
  ;filler process
    (setq blk_ename_ (car (entsel "specify filter Object")))
    (setq blk_obj_ (vlax-ename->vla-object blk_ename_))
      (if 
        (and
          (= (vla-get-objectname blk_obj_) "AcDbBlockReference")
          (= (vla-get-hasattributes blk_obj_) :vlax-true)
          
        )
        (progn
          (setq blk_filter_ (LM:vl-getattributevalue blk_obj_ "PART_NAME_") )
        )
      )
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (setq TTL_ 0)
    (setq qty_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
      
      (if (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "PART_NAME_") blk_filter_  )
        (progn
          
          (if (/= (setq TL_ (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H")) nil)
            (progn
              (setq TTL_ (+ TTL_ TL_))
            )
          )
          
          (setq code_name_ (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "CODE_NAME_"))
          (setq qty_i (+ qty_i 1 ))
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;fillter_summary_list_name_object_
    (setq summary_list_ename_ nil)
    (while (= summary_list_ename_ nil)
      ;user_input_data
        (setq summary_list_ename_ (car (entsel "specify summary_list_ename_ Object")))
      ;
      ;fillter_object_type_
        (if 
          (and
            (if (/= summary_list_ename_ nil) (progn (setq summary_list_obj_ (vlax-ename->vla-object summary_list_ename_)) ))
              (= (vla-get-objectname summary_list_obj_) "AcDbBlockReference")
              (= (vla-get-hasattributes summary_list_obj_) :vlax-true )
              (= (LM:effectivename summary_list_obj_) "000 - DAYNAMIC_DATA_FAMELINE_SUMMARY_LIST" )
            )
          (progn
            (setq summary_list_tag_list_ (LM:vl-getattributevalues summary_list_obj_))
            (LM:vl-setattributevalue summary_list_obj_ "PART_NAME_" blk_filter_  )
            (LM:vl-setattributevalue summary_list_obj_ "CODE_NAME_" code_name_  )
            (LM:vl-setattributevalue summary_list_obj_ "QTY_VAL" qty_i  )
            (LM:vl-setattributevalue summary_list_obj_ "LENGTH_VAL" (rtos (/ TTL_ 1000) 2 2)  )
            (setq get_total_ (cond ( (getint (strcat "\nspecify get_total_ <" (rtos (setq get_total_ (cond (get_total_) (1.0) ) ) ) "> : " ) ) ) (get_total_) ) )
            (LM:vl-setattributevalue summary_list_obj_ "SET_VALUE" get_total_  )
            
          )
          (setq summary_list_ename_ nil)
        )
        (if
          (or
            (= summary_list_ename_ nil)
            (/= (vla-get-objectname summary_list_obj_) "AcDbBlockReference")
            (if (= (vla-get-objectname summary_list_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes summary_list_obj_) :vlax-true ) ))
          )
          (progn
            (alert "Please selection attribute object")
          )
        )
      ;
    )
  ;
  
)


(defun c:line_2_line_L2L_()
  (setq p1_ename_ (car (entsel "specify Object")))
  (setq p1_obj_ (vlax-ename->vla-object p1_ename_))
  (setq p2_ename_ (car (entsel "specify Object")))
  (setq p2_obj_ (vlax-ename->vla-object p2_ename_))
  
  (setq p1_obj_start_end_pt_ (list
                                  (setq p1_obj_startpt_ (vlax-safearray->list
                                                             (vlax-variant-value ( vla-get-startpoint p1_obj_ ))
                                                           )
                                  )
                                  (setq p1_obj_endpt_ (vlax-safearray->list
                                                            (vlax-variant-value ( vla-get-endpoint p1_obj_ ))
                                                         )
                                  )
                                )
  )
  (setq p2_obj_start_end_pt_ (list
                                  (setq p2_obj_startpt_ (vlax-safearray->list
                                                             (vlax-variant-value ( vla-get-startpoint p2_obj_ ))
                                                           )
                                  )
                                  (setq p2_obj_endpt_ (vlax-safearray->list
                                                            (vlax-variant-value ( vla-get-endpoint p2_obj_ ))
                                                         )
                                  )
                                )
  )
  (command "line" p1_obj_startpt_ p2_obj_startpt_ "")
  (command "line" p1_obj_endpt_ p2_obj_endpt_ "")
)


(defun c:Asum_ ()
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
  ;filler process
    (setq blk_ename_ (car (entsel "specify filter Object")))
    (setq blk_obj_ (vlax-ename->vla-object blk_ename_))
      (if 
        (and
          (= (vla-get-objectname blk_obj_) "AcDbBlockReference")
          (= (vla-get-hasattributes blk_obj_) :vlax-true)
          
        )
        (progn
          (setq blk_filter_ (LM:vl-getattributevalue blk_obj_ "PART_NAME_") )
        )
      )
  ;
  
  
  (initget "Yes No")
  (setq ss_ ;Detect the variable's result in case of a function error
    (vl-catch-all-apply 
      (function 
        (lambda () 
          (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
        )
      )
    )
  )
  (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
    (progn
      (setq input_sc_ (cond ( (getint (strcat "\nspecify get block scale \n<" (rtos (setq input_sc_ (cond (input_sc_) (1.0) ) ) ) "> : " ) ) ) (input_sc_) ) )
      (setq user_input_inspt_ (getpoint "specify point for insertion data list "))
        (command "insert" "000 - DAYNAMIC_DATA_FAMELINE_SUMMARY_LIST" user_input_inspt_ input_sc_ 0)
        (setq new_blk_ename1_ (entlast))
        (setq new_blk_obj1_ (vlax-ename->vla-object new_blk_ename1_))
        (LM:setdynpropvalue new_blk_obj1_ "visibility1" "head_only" )
    )
    (setq user_input_inspt_ 
                                    (list 
                                      (car user_input_inspt_)
                                      (- (cadr user_input_inspt_) (* input_sc_ 5))
                                      (caddr user_input_inspt_)
                                    )
            )
  )
  
  
  
 
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
      
      (if (= (setq part_name_ (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "PART_NAME_")) blk_filter_  )
        (progn
          ;getdata_
            (if 
              (and
                (/= (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H") nil)
                (/= (setq LENGTH_VAL (/ (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H") 1000 ) ) nil )
              )
              (progn 
                (setq TL_ TL_)
                (setq UNIT "m.")
                (setq LENGTH_VAL (RTOS LENGTH_VAL 2 2))
              )
              (setq TL_  ""
                    UNIT "pc."
                    LENGTH_VAL ""
              )
            )
            (setq QTY_ "1")
            (setq SET_VAL "")
            (setq TOTAL_VALUE "")
          
            (command "insert" "000 - DAYNAMIC_DATA_FAMELINE_SUMMARY_LIST" user_input_inspt_ input_sc_ 0)
            (setq new_blk_ename_ (entlast))
            (setq new_blk_obj_ (vlax-ename->vla-object new_blk_ename_))
              (LM:vl-setattributevalue new_blk_obj_ "PART_NAME_" part_name_ )
              (LM:vl-setattributevalue new_blk_obj_ "QTY_VAL" QTY_ )
              (LM:vl-setattributevalue new_blk_obj_ "LENGTH_VAL" (RTOS LENGTH_VAL 2 2) )
              (LM:vl-setattributevalue new_blk_obj_ "UNIT_VALUE" UNIT )
              (LM:vl-setattributevalue new_blk_obj_ "SET_VALUE" SET_VAL )
              (LM:vl-setattributevalue new_blk_obj_ "TOTAL_VALUE" TOTAL_VALUE )

            (setq user_input_inspt_ 
                                    (list 
                                      (car user_input_inspt_)
                                      (- (cadr user_input_inspt_) (* input_sc_ 5))
                                      (caddr user_input_inspt_)
                                    )
            )
          
          
          
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)

(defun c:sumset_ ()
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
    (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000 - DAYNAMIC_DATA_FAMELINE_SUMMARY_LIST" ))
  ;
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (if 
        (and
          (= 1 1)
          ; (/= (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "visibility1" ) "SHOW_HEAD_TABLE" )
        )
        (progn
          (cond
            (;UNIT_case_1
               (and
                  (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "UNIT_VALUE") "pc.")
               )
               (progn
                (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_VALUE"
                  (strcat 
                    (rtos 
                      (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_VALUE"
                        (* 
                          (atoi (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "QTY_VAL" ) ) 
                          (atoi (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "SET_VALUE" ) ) 
                        )
                      )
                      2 0
                    )
                    " pc."
                  )
                )
                 (princ "UNIT_case_1")
               )
            )
            (;UNIT_case_2
               (and
                  (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "UNIT_VALUE") "m.")
               )
               (progn
                  (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_VALUE"
                    (strcat 
                      (rtos 
                        (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "TOTAL_VALUE"
                          (* 
                            (atoi (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "QTY_VAL" ) ) 
                            (atof (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "LENGTH_VAL" ) ) 
                          )
                        )
                        2 2
                      )
                      " m."
                    )
                  )
                 (princ "UNIT_case_2")
               )
            )

          )
          
        )
      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)



;PURPLE_LINE_
  (defun c:insertion_AL_Ceiling_300SK_skins_ ()
    
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
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
      (setq ss_pre_filter_set_xx_ii 1)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        
          ;insertion_by_center_recline
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_ename_centroid_ (LM:PolyCentroid ss_pre_filter_set_xx_ename_ ))
          (setq Bo_line_ (TA:Find_boundary_line_ ss_pre_filter_set_xx_ename_ ))
          (setq dist_x
                (distance 
                  (car Bo_line_)
                  (list (car (cadr bo_line_)) (cadr (car bo_line_)))
                )
          )
          (setq dist_y
                (distance 
                  
                  (list (car (cadr bo_line_)) (cadr (car bo_line_)))
                  (cadr Bo_line_)
                )
          )
          ; (TA:set-block-blockscaling "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW" acUniform)
          (TA:vla-insertblock "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"  ss_pre_filter_set_xx_ename_centroid_ 1 0)
          
          
          (setq new_dynamic_blk_ (entlast))
          (setq new_dynamic_blk_obj_ (vlax-ename->vla-object new_dynamic_blk_))
          (LM:setdynpropvalue new_dynamic_blk_obj_ "ALL_X" dist_x )
          (LM:setdynpropvalue new_dynamic_blk_obj_ "ALL_Y" dist_y)

          (vla-delete ss_pre_filter_set_xx_obj_ ) 

        ;screen process
          (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
          ; (princ 
          ;   (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n")
          ; ) 
          (cond
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 1)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 2)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 3)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 4)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 5)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 6)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 7)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 8)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 9)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 10)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
            
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 20)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
                (setq ss_pre_filter_set_xx_ii 1)
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 19)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 18)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 17)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 16)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 15)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 14)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 13)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 12)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 11)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
          )
          
          (textscr)
        ; 
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        (setq ss_pre_filter_set_xx_ii (+ ss_pre_filter_set_xx_ii 1))
      )
    ;
  )
  (defun c:temp_ppline_start-end_insk1 ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (+ (cadr user_input_pt_) 75)
                        )
    )

    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_center_insk2 ()
    (setq user_input_pt_ (getpoint "specify point "))
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_" user_input_pt_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_mk300sk ()
    ; (setq line_ename_ (car (entsel "specify Object")))
    ; (setq line_obj_ (vlax-ename->vla-object line_ename_))
    ; (setq line_obj_length (vla-get-length line_obj_))
    
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_1 (list 
                          (+ (car user_input_pt_) 150)
                          (cadr user_input_pt_)
                          (caddr user_input_pt_)
                        )
    )
    ; (setq H (- (* 2 (getdist user_input_pt_ )) 150))
    (setq H (- (* 2 (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_" user_input_pt_1 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
    
    
    
  )
  (defun c:temp_ppline_mk400sk ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (+ (cadr user_input_pt_) 75)
                        )
    )
    ; (setq H (- (getdist user_input_pt_new_ ) 100))
    (setq H (- (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_mk500sk ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (- (cadr user_input_pt_) 75)
                        )
    )
    ; (setq H (- (getdist user_input_pt_new_ ) 100))
    (setq H (- (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_new_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "Flip state1" 1)
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_convert_dynamic_side_condyn ()
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
      (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"))
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq get_dyn_all_x (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_x"))
        (setq get_dyn_all_y (rtos (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_y") 2 0))
        ;input back_
          (if (= get_dyn_all_y "300")
            (progn
              (vla-put-rotation ss_pre_filter_set_xx_obj_ (deg-to-rad 90))
              (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "all_y" get_dyn_all_x)
              (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "all_x" get_dyn_all_y)
            )
          )
          
        ;
        ;screen process
        (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
        (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (textscr)
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:temp_ppline_getalllength_ ()
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
        (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"))
      ;
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (setq sum_ ())
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        
        (setq get_dyn_all_y (rtos (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_y") 2 2))
        (setq sum_ (cons get_dyn_all_y sum_))
        ;screen process
          (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
          (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
          (textscr)
        ; 
          ; (princ)
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;)
    ;TA:stanndard_lambda_sorting
      (setq sum_sorted_ (vl-sort sum_  ;bigest open indent list
                                (function 
                                  (lambda (a b) 
                                    (< a b)
                                  )
                                )
                        ) ;bigest close indent list
      )
    ;
    
    (princ (TA:Count_item_in_list_ sum_sorted_ ))
    (princ (setq sum_length_all_ (LM:CountItems sum_sorted_ )))
      (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (setq Excel_obj_ (TA:New_Excel_File_NEX_ vla-obj_app ))
    ;preloop_and_while
      (setq sum_length_all_i 0)
      (setq row_i 3)
      (while (< sum_length_all_i (length sum_length_all_))
        (setq sum_length_all_main_ (car (nth  sum_length_all_i sum_length_all_)))
        (setq sum_length_all_sub_ (cdr (nth  sum_length_all_i sum_length_all_)))

        (setq Excel_entlast_ (caddr (car (nth 3 (car (reverse (TA:Excel_Assembly_ALL-obj_list_ Excel_obj_ )))))))
        
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 1 "AL.300SK" )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 2 sum_length_all_main_ )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 3 sum_length_all_sub_ )
        
        (setq row_i (+ row_i 1))
        (setq sum_length_all_i (+ sum_length_all_i 1))
      )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 1 "Matt." )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 2 "legnth" )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 3 "qty." )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 1 1 (getvar "dwgname") )
        
    ;
    
  )
  (defun c:c+ ()
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
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
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq get_l (LM:getdynpropvalue sss_obj_ "array_width" ) )
                  (LM:setdynpropvalue sss_obj_ "array_width" (+ get_l 250) )
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_)) 
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ) )
            (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq get_l (LM:getdynpropvalue sss_obj_ "array_width"))
            (LM:setdynpropvalue sss_obj_ "array_width" (+ get_l 250))
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )
        )
        
      )
    ;

  )
  (defun c:c- ()
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
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
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq get_l (LM:getdynpropvalue sss_obj_ "array_width" ) )
                  (LM:setdynpropvalue sss_obj_ "array_width" (- get_l 250) )
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_)) 
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ) )
            (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq get_l (LM:getdynpropvalue sss_obj_ "array_width"))
            (LM:setdynpropvalue sss_obj_ "array_width" (- get_l 250))
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )
        )
        
      )
    ;

  )
  (defun c:y+ ()
    ;for moving support frame Fameline celing SK300 (0.025 / 1 enter)
    ;set paremeter for moving
      (setq move_length_ 25)
    ;
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
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
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (+ (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (+ (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;

  )
  (defun c:y- ()
    ;for moving support frame Fameline celing SK300 (0.025 / 1 enter)
    ;set paremeter for moving
      (setq move_length_ 25)
    ;
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
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
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (- (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (- (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;

  )
  (defun c:TEMP:copy_from_basepoint_cfb_ () 
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
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                    (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                  )
                tar_result_      (list (car tar_copy_obj_ins) 
                                        (cadr tar_copy_obj_ins)
                                  )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
              tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
              tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
              tar_result_ (list (car tar_copy_obj_ins)(cadr tar_copy_obj_ins))
        )
        
        
        (command "_copy" "m" tar_copy_obj_ins   )
        
        
      )
      (princ "\n")
    )
  )
  (defun c:TEMP:copy_from_basepoint_cfs_ () 
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
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 75)
                                      0
                                )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 75)
                                      0
                                )
          )
        
        
        
        ; (command "_copy" "m" tar_copy_ "" tar_copy_obj_ins  )
        (command "_copy" "m" tar_copy_obj_ins   )
        
      )
      (princ "\n")
    )
  )
  (defun c:TEMP:copy_from_basepoint_cfs1_ () 
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
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 0)
                                      0
                                )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 0)
                                      0
                                )
          )
        
        
        
        ; (command "_copy" "m" tar_copy_ "" tar_copy_obj_ins  )
        (command "_copy" "m" tar_copy_obj_ins   )
        
      )
      (princ "\n")
    )
  )
  (defun c:Resupport_spacing_DP1_ ()
    (setq insert_int_spcaing_dist_ (getint "insert_int_spcaing_dist_" ))
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
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
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
              (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            
              (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
              (setq sss_obj_rotation_ (rad-to-deg (vla-get-rotation sss_obj_ )))
              
              ;get-dyn-blk_process
                (setq H (LM:getdynpropvalue sss_obj_ "H"))
                (setq array_width (LM:getdynpropvalue sss_obj_ "array_width"))
              ;
              (setq insert_blk_name_ (strcat "000TYP-LINE_AND_ARRAY_@" 
                                            (rtos insert_int_spcaing_dist_ 2 0)
                                            "mm._start"
                                    )
              )
              ;insertion-new_object_
                (command "insert" insert_blk_name_ sss_obj_ins_ 1 sss_obj_rotation_ )
                (setq new_ename_ (entlast))
                (setq new_obj_ (vlax-ename->vla-object new_ename_))
              
                (LM:setdynpropvalue new_obj_ "H" H)
                (LM:setdynpropvalue new_obj_ "array_width" array_width)
              ;
              ;delete main_obj_
                (vla-delete sss_obj_ )
              ;

              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
              (if (= (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)) nil)
                (progn
                  (setq sss_obj_ (vlax-ename->vla-object (entlast)))
                )
              )
              
            
              (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
              (setq sss_obj_rotation_ (rad-to-deg (vla-get-rotation sss_obj_ )))
              
              ;get-dyn-blk_process
                (setq H (LM:getdynpropvalue sss_obj_ "H"))
                (setq array_width (LM:getdynpropvalue sss_obj_ "array_width"))
              ;
              (setq insert_blk_name_ (strcat "000TYP-LINE_AND_ARRAY_@" 
                                            (rtos insert_int_spcaing_dist_ 2 0)
                                            "mm._start"
                                    )
              )
              ;insertion-new_object_
                (command "insert" insert_blk_name_ sss_obj_ins_ 1 sss_obj_rotation_ )
                (setq new_ename_ (entlast))
                (setq new_obj_ (vlax-ename->vla-object new_ename_))
              
                (LM:setdynpropvalue new_obj_ "H" H)
                (LM:setdynpropvalue new_obj_ "array_width" array_width)
              ;
              ;delete main_obj_
                (vla-delete sss_obj_ )
              ;
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;
  )
  (defun c:Assembly_blcok_ ()
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
    
  
  
  )
  ;000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET
  (defun c:PPLINE_300hk_ins300hk ()
    (setq line_ename_ (car (entsel "specify Object")))
    (setq line_obj_ (vlax-ename->vla-object line_ename_))
    ;main_idea_coding_code
      (if (= (vla-get-objectname line_obj_) "AcDbLine")
        (progn
          ;standard properties line
          (setq line_obj_start_end_pt_ (list
                                          (setq line_obj_startpt_ (vlax-safearray->list
                                                                     (vlax-variant-value ( vla-get-startpoint line_obj_ ))
                                                                   )
                                          )
                                          (setq line_obj_endpt_ (vlax-safearray->list
                                                                    (vlax-variant-value ( vla-get-endpoint line_obj_ ))
                                                                 )
                                          )
                                        )
          )
          (setq line_obj_midpt_ (TA:midpoint line_obj_startpt_ line_obj_endpt_))
          (setq line_obj_angle (rad-to-deg (vla-get-angle line_obj_ )))
          (setq new_line_obj_angle (TA:ModifyRotation line_obj_angle -90))
          (setq line_obj_length (vla-get-length line_obj_))
          ;
          ;insertion process
            (TA:vla-insertblock "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" line_obj_midpt_ 1 (deg-to-rad new_line_obj_angle) )
            ;new_blk_entlast object
              (setq new300HK_ename (entlast))
              (setq new300HK_obj_ (vlax-ename->vla-object new300HK_ename))
              ;input data
                (cond
                  (;total_length_process_case_1
                     (and
                        ; (> line_obj_length 3000)
                        (= (LM:effectivename new300HK_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" )
                        (= "this dynamic block was create by me" "this dynamic block was create by me")
                     )
                     (progn
                       (LM:setdynpropvalue new300HK_obj_ "view" "2_top_view")
                       (LM:setdynpropvalue new300HK_obj_ "array_panel_length" line_obj_length)
                       (LM:setdynpropvalue new300HK_obj_ "panel_width" (getint "spectify panel_width "))
                       (command "draworder" new300HK_ename "" "F" )
                       
                       (princ "total_length_process_case_1")
                     )
                  )
                  (;total_length_process_case_2
                     (and
                        (> line_obj_length 3000)
                        (= (LM:effectivename new300HK_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" )
                        (/= "this dynamic block was create by me" "this dynamic block was create by me")
                     )
                     (progn
                       (princ "total_length_process_case_2")
                     )
                  )
                )    
              ;
            ;
          ;
        )
      )
    ;
  )
  ;
  ;000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02AA_COMBINE_VIEW
  (defun c:PPLINE_300HK_insHR_ ()
    (setq 300HK_ename_ (car (entsel "specify Object")))
    (setq 300HK_obj_ (vlax-ename->vla-object 300HK_ename_))
    ;main_idea_coding_code
      (if (= (LM:Effectivename 300hk_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET")
        (progn
          ;get_ex-insertion_pt_300HK
            (list 
              (setq HR-02A_top_point_x (LM:getdynpropvalue 300hk_obj_ "HR-02A_top_point x" ) )
              (setq HR-02A_top_point_y (LM:getdynpropvalue 300hk_obj_ "HR-02A_top_point y" ) )
            )
            (list 
              (setq HR-02B_top_point_x (LM:getdynpropvalue 300hk_obj_ "HR-02B_top_point x" ) ) 
              (setq HR-02B_top_point_y (LM:getdynpropvalue 300hk_obj_ "HR-02B_top_point Y" ) )
            )
            (setq 300hk_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint 300hk_obj_))))
            (setq 300hk_obj_rotation_ (TA:ModifyRotation (rad-to-deg (vla-get-rotation 300hk_obj_ ) ) 180))
          ;
          ;insertion_panel_insertion_point_
            (setq HR-02A_top_point_preins_ (list 
                                              (+ ( car 300hk_obj_ins_ ) HR-02A_top_point_x  )
                                              (+ ( cadr 300hk_obj_ins_ ) 0  )
                                            )
            )
            (setq HR-02B_top_point_preins_ (list 
                                              (+ ( car 300hk_obj_ins_ ) HR-02B_top_point_x  )
                                              (+ ( cadr 300hk_obj_ins_ ) 0  )
                                            )
            )
          ;
          ;get_dynamic_data_
            (setq array_panel_length (LM:getdynpropvalue 300hk_obj_ "array_panel_length"))
          ; 
          ;insertion_HR_side_A
            (TA:vla-insertblock "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02AA_COMBINE_VIEW" HR-02A_top_point_preins_ 1 (deg-to-rad 90)) 
            (setq new_ename_ (entlast))
            (setq new_obj_ (vlax-ename->vla-object new_ename_)) 
            (command "rotate" new_ename_ "" 300hk_obj_ins_ (rad-to-deg (vla-get-rotation 300hk_obj_ ) ))         
            (LM:setdynpropvalue new_obj_ "view" "2_top_view")
            (setq array_length (LM:setdynpropvalue new_obj_ "array_length" (* (fix (/ array_panel_length 2400)) 2400 ) ))
            (cond ;visibility&length condition
              ( ;visibility condition
                (or 
                  (> array_panel_length 3000)
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "2_top_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_left"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_right"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                )
              )
              ( ;visibility condition
                (and 
                  (< array_panel_length 3000)
                  
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "7_dynamic_length_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_middle"   
                    (- array_panel_length 100)
                  )
                )
              )
            )
          ;
          ;
          ;insertion_HR_side_B
            (TA:vla-insertblock "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02B_COMBINE_VIEW" HR-02B_top_point_preins_ 1 (deg-to-rad 90))
            (setq new_ename_ (entlast))
            (setq new_obj_ (vlax-ename->vla-object new_ename_))   
            (command "rotate" new_ename_ "" 300hk_obj_ins_ (rad-to-deg (vla-get-rotation 300hk_obj_ ) ))
            (LM:setdynpropvalue new_obj_ "view" "2_top_view")
            (setq array_length (LM:setdynpropvalue new_obj_ "array_length" (* (fix (/ array_panel_length 2400)) 2400 ) ))
            (cond ;visibility&length condition
              ( ;visibility condition
                (or 
                  (> array_panel_length 2400)
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "2_top_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_left"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_right"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                )
              )
              ( ;visibility condition
                (and 
                  (< array_panel_length 2400)
                  
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "7_dynamic_length_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_middle"   
                    (- array_panel_length 100)
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_array_length"   
                    array_panel_length
                  )
                )
              )
            )
            
        )
      )
    ;
  )
  ;
  ;000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET
  (defun c:PPLINE_300HK_insHanging_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
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
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      ;get-rotaton_object_
        (setq angle_ename_ (car (entsel "specify Object")))
        (setq angle_obj_ (vlax-ename->vla-object angle_ename_))
      ;
    
      (foreach x (LM:intersectionsinset ss_pre_filter_set_xx_)
        (TA:vla-insertblock 
          "000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET"
          x
          1
          (cond ;get rotate condition
            (;Line PLine condition 
              (or
                (= "AcDbLine" (vla-get-objectname angle_obj_))
                (= "AcDbPolyline" (vla-get-objectname angle_obj_ ))   
              )
              (progn
                (vla-get-angle angle_obj_)
              )
            )
            (;BLK condition 
              (and
                (= "AcDbBlockReference" (vla-get-objectname angle_obj_))
                (= "AcDbBlockReference" (vla-get-objectname angle_obj_ ))   
              )
              (progn
                (vla-get-rotation angle_obj_)
              )
            )
          )
        )
          ;new_blk_entlast object
          (setq new_hanging_ename (entlast))
          (setq new_hanging_obj_ (vlax-ename->vla-object new_hanging_ename))
          (LM:setdynpropvalue new_hanging_obj_ (LM:getvisibilityparametername new_hanging_obj_) "2_top_view" )
          ;
        
        
      )
    ;
  )
  (defun c:PPLINE_300HK_insCHanging_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
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
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      ;user_input_
        (setq distance_measure_ 1200)
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          ;main Line PLine Properties
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
            (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq ss_pre_filter_set_xx_obj_angle  (vla-get-angle ss_pre_filter_set_xx_obj_))
            (setq ss_pre_filter_set_xx_obj_length (vla-get-length ss_pre_filter_set_xx_obj_))
            (setq distance_measure_ (if (< ss_pre_filter_set_xx_obj_length 1200) (progn (fix (- ss_pre_filter_set_xx_obj_length 100 ))) distance_measure_)) 
            (setq ss_pre_filter_set_xx_obj_fragment_length_ (/ (- ss_pre_filter_set_xx_obj_length (* (fix (/ ss_pre_filter_set_xx_obj_length distance_measure_)) distance_measure_) ) 2))
            (setq ss_pre_filter_set_xx_obj_time_full_length_ (fix (/ ss_pre_filter_set_xx_obj_length distance_measure_)))
            (if (= ss_pre_filter_set_xx_obj_fragment_length_ 0) ;incase for number spcaing distance
              (progn
                (setq ss_pre_filter_set_xx_obj_time_full_length_ ss_pre_filter_set_xx_obj_time_full_length_ )
              )
              (setq ss_pre_filter_set_xx_obj_time_full_length_ (+ 2 ss_pre_filter_set_xx_obj_time_full_length_) )
            )
            ;preloop_and_while measure_spcaing_distance
              (setq measure_length_ ())
              (setq time_i 0)
              (while (< time_i ss_pre_filter_set_xx_obj_time_full_length_)
                (if 
                  (or (= time_i 0) (= time_i (- ss_pre_filter_set_xx_obj_time_full_length_ 1)) )
                  (progn
                    (setq measure_length_ (cons ss_pre_filter_set_xx_obj_fragment_length_ measure_length_))
                  )
                  (setq measure_length_ (cons distance_measure_ measure_length_))
                )
                (setq time_i (+ time_i 1))
              )       
            ;
            ;preloop_and_while measure_val_distance
              (setq measure_length_i 0)
              (setq finish_measure_length_ 0)
              (setq finish_measure_length_result ())
              (while (< measure_length_i (length measure_length_))
                (setq measure_length_ename_ (nth  measure_length_i measure_length_))
                (setq finish_measure_length_ (+ finish_measure_length_ measure_length_ename_)) 
                (setq finish_measure_length_result (cons finish_measure_length_ finish_measure_length_result))
                (setq measure_length_i (+ measure_length_i 1))
              )
            ;
          ;
          ;insertion_process
            (foreach x (vl-remove (nth (- (length finish_measure_length_result) 1) (reverse finish_measure_length_result) ) (reverse finish_measure_length_result) )
              (TA:vla-insertblock 
                "000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET"
                (vlax-curve-getpointatdist ss_pre_filter_set_xx_obj_ x)
                1
                ss_pre_filter_set_xx_obj_angle
              )
              ;new_blk_entlast object
                (setq newblk_ename (entlast))
                (setq newblk_obj (vlax-ename->vla-object newblk_ename))
                (LM:setdynpropvalue newblk_obj (LM:getvisibilityparametername new_hanging_obj_) "2_top_view" )
              ;
            )
          ;
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    ;
  )

  ;
  ;000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW
  ; (defun c:PPLINE_300HK_inscc01_ ()
  ;   ;selection_set_for_fillter_blk_name
  ;     (if  ;pre_select_ssget_or_post_select_ssget
  ;       (=
  ;         (setq ss_pre_filter_set_xx_ (ssget "i"
  ;                                           (list
  ;                                             (cons 0 "LINE") ;type of object
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
  ;                                             (cons 0 "LINE") ;type of object
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
  ;   ;
  ;   ;preloop_and_while
  ;     (setq ss_pre_filter_set_xx_i 0)
  ;     (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
  ;       (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
  ;       (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
  ;       (setq obj_ ss_pre_filter_set_xx_obj_)
  ;       ;get-propertie_data
  ;         (setq ss_pre_filter_set_xx_obj_angle_ (vla-get-angle ss_pre_filter_set_xx_obj_))  
  ;         (setq ss_pre_filter_set_xx_obj_length_ (vla-get-length ss_pre_filter_set_xx_obj_))
  ;         (setq ss_pre_filter_set_xx_obj_start_end_pt_ (list
  ;                                         (setq ss_pre_filter_set_xx_obj_startpt_ (vlax-safearray->list
  ;                                                                    (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_obj_ ))
  ;                                                                  )
  ;                                         )
  ;                                         (setq ss_pre_filter_set_xx_obj_endpt_ (vlax-safearray->list
  ;                                                                   (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_obj_ ))
  ;                                                                )
  ;                                         )
  ;                                       )
  ;         )
        
  ;       ;
  ;       (TA:vla-insertblock
  ;         "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW" 
  ;         (TA:midpoint ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_obj_endpt_ )
  ;         1
  ;         ss_pre_filter_set_xx_obj_angle_
  ;       )
  ;       ;new_blk_entlast object
  ;         (setq newinsert_ename (entlast))
  ;         (setq newinsert_obj_ (vlax-ename->vla-object newinsert_ename))
  ;         (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "2_top_view" )
  ;         (LM:setdynpropvalue newinsert_obj_ "array_length" (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) )
  ;         (LM:setdynpropvalue newinsert_obj_ "fragment_left" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
  ;         (LM:setdynpropvalue newinsert_obj_ "fragment_right" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
  ;       ;
  ;       (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
  ;     )
  ;   ;
    
  ;   ;
  ; )
  (defun c:PPLINE_300HK_inscc01_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
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
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;while_select_block_on_condition_
        (setq direction_ename_ nil)
        (while (= direction_ename_ nil)
          (setq direction_ename_ (car (entsel "specify direction_ename Object")))
          (if
            (and ;conditional_rule_for_select_object
              (/= direction_ename_ nil)
              (setq direction_ename_obj_ (vlax-ename->vla-object direction_ename_))
              (= (vla-get-objectname direction_ename_obj_ ) "AcDbLine")
            )
            (progn
              (setq direction_ename_obj_angle_ (vla-get-angle direction_ename_obj_) )
            )
            (alert "Object invalid Please try again")
          )
        )
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq obj_ ss_pre_filter_set_xx_obj_)
        ;get-propertie_data
          (setq ss_pre_filter_set_xx_obj_angle_ (vla-get-angle ss_pre_filter_set_xx_obj_))  
          (setq ss_pre_filter_set_xx_obj_length_ (vla-get-length ss_pre_filter_set_xx_obj_))
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
        
        ;
        (TA:vla-insertblock
          "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW" 
          (TA:midpoint ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_obj_endpt_ )
          1
          ss_pre_filter_set_xx_obj_angle_
        )
        ;new_blk_entlast object
          (setq newinsert_ename (entlast))
          (setq newinsert_obj_ (vlax-ename->vla-object newinsert_ename))
          (cond ;visibility&length condition
            ( (> ss_pre_filter_set_xx_obj_length_ 3000 ) ;visibility&length condition
              (progn
                (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "2_top_view" )
                (LM:setdynpropvalue newinsert_obj_ "array_length" (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) )
                (LM:setdynpropvalue newinsert_obj_ "fragment_left" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
                (LM:setdynpropvalue newinsert_obj_ "fragment_right" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
              )
            )
            ( (<= ss_pre_filter_set_xx_obj_length_ 3000 ) ;visibility&length condition
              (progn
                (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "7_dynamic_length_view" )
                (LM:setdynpropvalue newinsert_obj_ "fragment_middle" ss_pre_filter_set_xx_obj_length_ )
              )
            )
          )
          
        ;
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
    
    ;
  )
  ;
;

;Method Installation
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
  (defun c:MATTERIAL_BOX_INCREMENT_NUMBER_MATINC ()

    ;user_input_axis
      (initget "X Y")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq axis_sort (not (eq "Y" (getkword "\nKeep Existing point with object? [X/Y] <X>: "))))
            )
          )
        )
      )
      (if (= axis_sort T)
        (progn
          (setq axis_sort "X")
        )
        (setq axis_sort "Y")
      )
    ;
    ;user_input_
      (setq increment_number_ (cond ( (getint (strcat "\nSpecify increment_number_   <" (rtos (setq increment_number_ (cond (increment_number_) (1.0) ) ) ) "> : " ) ) ) (increment_number_) ) )
    ;
    ;user_att_in_code_line_
      (setq att_name_1 "NO.BUBBLE_VALUE_1")
      (setq att_name_2 "NO_VALUE_1")
      (setq efname_blk "000 - DYNAMIC_SYMBOL_FAMELINE_MATTERIAL_BOX_AND_BUBBLE" )
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
    ;filter_and_sort_selection_by_efname_
      (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ efname_blk))
      (setq ss_pre_filter_set_xx_sorted_ (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx_ axis_sort ))
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_sorted_i 0)
      (while (< ss_pre_filter_set_xx_sorted_i (length ss_pre_filter_set_xx_sorted_))
        (setq ss_pre_filter_set_xx_sorted_ename_ (car (nth  ss_pre_filter_set_xx_sorted_i ss_pre_filter_set_xx_sorted_)))
        (setq ss_pre_filter_set_xx_sorted_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_sorted_ename_))
        
        (LM:vl-setattributevalue ss_pre_filter_set_xx_sorted_obj_ att_name_1 increment_number_ )
        (LM:vl-setattributevalue ss_pre_filter_set_xx_sorted_obj_ att_name_2 increment_number_ )
        
        
        (setq increment_number_ (+ increment_number_ 1))
        (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
      )
    ;
  )
;

(defun c:mtext_renumber_mtre_ ()
  

  (setq old_numtext (getstring "specify old_numtext <" old_numtext ">:"))
  (setq new_numtext (getstring "specify new_numtext"))
  (setq mtext_ename_ (car (entsel "specify Object")))
  (setq mtext_obj_ (vlax-ename->vla-object mtext_ename_))

  (setq mtext_obj_textstring_ (vla-get-textstring mtext_obj_))
  (setq old (strcat ";" old_numtext "."))
  (setq new (strcat ";" new_numtext "."))

  (vla-put-textstring mtext_obj_ (LM:StringSubst  new old mtext_obj_textstring_ ))
)





(defun c:interset ( / sel )
    (if (setq sel (ssget))
        (foreach pnt (LM:intersectionsinset sel)
            (entmake (list '(0 . "POINT") (cons 10 pnt)))
        )
    )
    (princ)
)
(vl-load-com) (princ)

(defun c:inter ( / obj1 obj2 )    
    (if (and (setq obj1 (car (entsel "\nSelect 1st Object: ")))
             (setq obj2 (car (entsel "\nSelect 2nd Object: ")))
        )
        (foreach pnt (LM:intersections (vlax-ename->vla-object obj1) (vlax-ename->vla-object obj2) acextendnone)
            (entmake (list '(0 . "POINT") (cons 10 pnt)))
        )
    )
    (princ)
)
(vl-load-com) (princ)

;; Intersections in Set  -  Lee Mac
;; Returns a list of all points of intersection between all objects in a supplied selection set.
;; sel - [sel] Selection Set

(defun LM:intersectionsinset ( sel / id1 id2 ob1 ob2 rtn )
    (repeat (setq id1 (sslength sel))
        (setq ob1 (vlax-ename->vla-object (ssname sel (setq id1 (1- id1)))))
        (repeat (setq id2 id1)
            (setq ob2 (vlax-ename->vla-object (ssname sel (setq id2 (1- id2))))
                  rtn (cons (LM:intersections ob1 ob2 acextendnone) rtn)
            )
        )
    )
    (apply 'append (reverse rtn))
)

(defun c:ints1 ( )
    (setvar "osmode" 0)
    (textscr)
    (if (setq sel (ssget))
        (foreach pnt (LM:intersectionsinset sel)
            ; (entmake (list '(0 . "POINT") (cons 10 pnt)))
            (command "insert" "x8" pnt 1 0)
        )
        (princ (length (LM:intersectionsinset sel)))
        
    )
    (princ)
    (setvar "osmode" 1215)
)



(defun c:point2point_p2p ()
  
  (setq p1_ename_ (car (entsel "specify Object")))
  (setq p1_obj_ (vlax-ename->vla-object p1_ename_))
  (setq p2_ename_ (car (entsel "specify Object")))
  (setq p2_obj_ (vlax-ename->vla-object p2_ename_))
  
  (setq p1_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint p1_obj_))))
  (setq p2_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint p2_obj_))))
  
  (command "line" p1_obj_ins_ p2_obj_ins_ "")
)

(defun c:p2pset ()
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
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_1 (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_1 (vlax-ename->vla-object ss_pre_filter_set_xx_ename_1))
      (setq ss_pre_filter_set_xx_obj_ins_1 (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_1))))
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_ii 0)
        (while (< ss_pre_filter_set_xx_ii (sslength ss_pre_filter_set_xx_))
          (setq ss_pre_filter_set_xx_ename_2 (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_ii))
          (setq ss_pre_filter_set_xx_obj_2 (vlax-ename->vla-object ss_pre_filter_set_xx_ename_2))
          (setq ss_pre_filter_set_xx_obj_ins_2 (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_2))))
          
          (if (and 
                (/= (vl-princ-to-string  ss_pre_filter_set_xx_ename_2) (vl-princ-to-string ss_pre_filter_set_xx_ename_1))
                (= (car ss_pre_filter_set_xx_obj_ins_1) (car ss_pre_filter_set_xx_obj_ins_2))
              )
            (progn
              (command "line" ss_pre_filter_set_xx_obj_ins_1 ss_pre_filter_set_xx_obj_ins_2 "")
            )
          )
          
          
          (setq ss_pre_filter_set_xx_ii (+ ss_pre_filter_set_xx_ii 1))
        )
      ;
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)





(defun c:kl1_list-nested-blocks ()
  (setq ss (ssget '((0 . "INSERT")))) ; เลือกบล็อกทั้งหมด
  (setq i 0)
  (setq blk_list_ nil)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq blkname (cdr (assoc 2 (entget ent))))
    (princ (strcat "\nBlock: " blkname))
    
    ;; ดึง definition ของบล็อก
    (setq bdef (tblobjname "BLOCK" blkname))
    (if bdef
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
    (setq i (1+ i))
  )
  
)


(defun c:nested_block_nsblock ()
  (setq ss (ssget '((0 . "INSERT")))) ; เลือกบล็อกทั้งหมด
  (setq i 0)
  (setq blk_list_ nil)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq blkname (cdr (assoc 2 (entget ent))))
    (princ (strcat "\nBlock: " blkname))
    
    ;nested block process
      (setq bdef (tblobjname "BLOCK" blkname))
      (if bdef
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
    ;
    (setq i (1+ i))
  )
    (setq sum_sorted_ blk_list_ )
    (princ (TA:Count_item_in_list_ sum_sorted_ ))
    (princ (setq sum_length_all_ (LM:CountItems sum_sorted_ )))
    (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (setq Excel_obj_ (TA:New_Excel_File_NEX_ vla-obj_app ))
    
    ;preloop_and_while
      (setq sum_length_all_i 0)
      (setq row_i 4)
      (while (< sum_length_all_i (length sum_length_all_))
        (setq sum_length_all_main_ (car (nth  sum_length_all_i sum_length_all_)))
        (setq sum_length_all_sub_ (cdr (nth  sum_length_all_i sum_length_all_)))

        (setq Excel_entlast_ (caddr (car (nth 3 (car (reverse (TA:Excel_Assembly_ALL-obj_list_ Excel_obj_ )))))))
        
        (if (= sum_length_all_i 0)
          (progn
            (TA:Excel_input_data_R1C1 Excel_entlast_ 1 1 
              (strcat 
                "LOCATION FILE PATH : "
                (getvar "dwgprefix") (getvar "dwgname")
              ) 
            )
            (TA:Excel_input_data_R1C1 Excel_entlast_ 2 1
              (strcat 
                "NAME FILE  : "
                (getvar "dwgname")
              ) 
            )
          )
        )
        
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 2 sum_length_all_main_ )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 3 sum_length_all_sub_ )
        
        (setq row_i (+ row_i 1))
        (setq sum_length_all_i (+ sum_length_all_i 1))
      )
      ;preset format
        (TA:EXCEL_put_columnwidth Excel_entlast_ 2 80 )
      ;
      
        
    ;
)


 

(defun c:select_last_entity_lasten ()
  (command "pselect"  (entlast) "" )
)













(defun c:selection_pline_PL90270_ ()


  (setq ss_pre_filter_set_xx_ (ssget 
                                    (list 
                                      (cons 0 "line") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                      ; (cons 62 1)           ;kind of color call sign with color code index
                                    )
                              )
  )


  ;preloop_and_while
    (setq newss_pre_filter_set_xx_ (ssadd))
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
        (if 
          (or 
            (= (fix (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_obj_))) 90)
            (= (fix (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_obj_))) 270)
          )
          (progn
            (setq newss_pre_filter_set_xx_ (ssadd ss_pre_filter_set_xx_ename_ newss_pre_filter_set_xx_))
          )
        )
      
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  (command "pselect" newss_pre_filter_set_xx_  "" )
)


(defun c:Hatch_to_Excel_H2Excel ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "HATCH") ;type of object
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
    (setq patt_name_mainlist_ ())
    (setq patt_name_name_hatch_list_ ())
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
      (setq ss_pre_filter_set_xx_obj_patt_name_ (vla-get-patternname ss_pre_filter_set_xx_obj_))
      (setq ss_pre_filter_set_xx_obj_Harea_ (/ (vla-get-area ss_pre_filter_set_xx_obj_) 1000000 ))
      
      (setq patt_name_name_hatch_list_ (cons  ss_pre_filter_set_xx_obj_patt_name_ patt_name_name_hatch_list_))
      (setq patt_name_sublist_ (list ss_pre_filter_set_xx_obj_patt_name_ ss_pre_filter_set_xx_obj_Harea_  ))
      (setq patt_name_mainlist_ (cons patt_name_sublist_ patt_name_mainlist_))
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;preloop_and_while
    (setq mainlist_Ceiling (list 
                            "000_C01"
                            "000_C02"
                            "000_C03"
                            "000_C04"
                            "000_C05"
                            "000_C05A"
                            "000_C06"
                            "000_C07"
                            "000_C08"
                            "000_C09"
                            "000_C10"
                            "000_C11"
                            "000_C12"
                            "000_W01"
                            "000_W02"
                            "000_W03"
                            "000_W04"
                            "000_W05"
                            "000_W06"
                            "000_W07"
                            "000_W08"
                            "000_W09"
                            "000_W10"
                            "000_W11"
                            "000_W12"
                            "000_W13"
                            "000_W14"
                            "000_W15"
                            "000_W16"
                            "000_W17"
                            "000_W18"
                            "000_W19"
                            "000_W20"
                            "000_W21"
                            "000_W22"
                           )
    )
    (setq patt_name_mainlist_area_C01 0
          patt_name_mainlist_area_C02 0
          patt_name_mainlist_area_C03 0
          patt_name_mainlist_area_C04 0
          patt_name_mainlist_area_C05 0
          patt_name_mainlist_area_C05A 0
          patt_name_mainlist_area_C06 0
          patt_name_mainlist_area_C07 0
          patt_name_mainlist_area_C08 0
          patt_name_mainlist_area_C09 0
          patt_name_mainlist_area_C10 0
          patt_name_mainlist_area_C11 0
          patt_name_mainlist_area_C12 0
          patt_name_mainlist_area_W01 0
          patt_name_mainlist_area_W02 0
          patt_name_mainlist_area_W03 0
          patt_name_mainlist_area_W04 0
          patt_name_mainlist_area_W05 0
          patt_name_mainlist_area_W06 0
          patt_name_mainlist_area_W07 0
          patt_name_mainlist_area_W08 0
          patt_name_mainlist_area_W09 0
          patt_name_mainlist_area_W10 0
          patt_name_mainlist_area_W11 0
          patt_name_mainlist_area_W12 0
          patt_name_mainlist_area_W13 0
          patt_name_mainlist_area_W14 0
          patt_name_mainlist_area_W15 0
          patt_name_mainlist_area_W16 0
          patt_name_mainlist_area_W17 0
          patt_name_mainlist_area_W18 0
          patt_name_mainlist_area_W19 0
          patt_name_mainlist_area_W20 0
          patt_name_mainlist_area_W21 0
          patt_name_mainlist_area_W22 0
    )
    ;preloop_and_while
      (setq patt_name_mainlist_i 0)
      (while (< patt_name_mainlist_i (length patt_name_mainlist_))
        (setq patt_name_mainlist_name_ (car (nth  patt_name_mainlist_i patt_name_mainlist_)))
        (setq patt_name_mainlist_area_ (cadr (nth  patt_name_mainlist_i patt_name_mainlist_)))
          (cond 
            ( ;Harea_case_0
            (and (= patt_name_mainlist_name_ (nth 0 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C01 (+ patt_name_mainlist_area_C01 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_1
            (and (= patt_name_mainlist_name_ (nth 1 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C02 (+ patt_name_mainlist_area_C02 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_2
            (and (= patt_name_mainlist_name_ (nth 2 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C03 (+ patt_name_mainlist_area_C03 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_3
            (and (= patt_name_mainlist_name_ (nth 3 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C04 (+ patt_name_mainlist_area_C04 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_4
            (and (= patt_name_mainlist_name_ (nth 4 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C05 (+ patt_name_mainlist_area_C05 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_5
            (and (= patt_name_mainlist_name_ (nth 5 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C05A (+ patt_name_mainlist_area_C05A patt_name_mainlist_area_)))
            )
            ( ;Harea_case_6
            (and (= patt_name_mainlist_name_ (nth 6 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C06 (+ patt_name_mainlist_area_C06 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_7
            (and (= patt_name_mainlist_name_ (nth 7 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C07 (+ patt_name_mainlist_area_C07 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_8
            (and (= patt_name_mainlist_name_ (nth 8 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C08 (+ patt_name_mainlist_area_C08 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_9
            (and (= patt_name_mainlist_name_ (nth 9 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C09 (+ patt_name_mainlist_area_C09 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_10
            (and (= patt_name_mainlist_name_ (nth 10 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C10 (+ patt_name_mainlist_area_C10 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_11
            (and (= patt_name_mainlist_name_ (nth 11 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C11 (+ patt_name_mainlist_area_C11 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_12
            (and (= patt_name_mainlist_name_ (nth 12 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C12 (+ patt_name_mainlist_area_C12 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W1
            (and (= patt_name_mainlist_name_ (nth 13 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W01 (+ patt_name_mainlist_area_W01 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W2
            (and (= patt_name_mainlist_name_ (nth 14 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W02 (+ patt_name_mainlist_area_W02 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W3
            (and (= patt_name_mainlist_name_ (nth 15 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W03 (+ patt_name_mainlist_area_W03 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W4
            (and (= patt_name_mainlist_name_ (nth 16 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W04 (+ patt_name_mainlist_area_W04 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W5
            (and (= patt_name_mainlist_name_ (nth 17 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W05 (+ patt_name_mainlist_area_W05 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W6
            (and (= patt_name_mainlist_name_ (nth 18 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W06 (+ patt_name_mainlist_area_W06 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W7
            (and (= patt_name_mainlist_name_ (nth 19 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W07 (+ patt_name_mainlist_area_W07 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W8
            (and (= patt_name_mainlist_name_ (nth 20 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W08 (+ patt_name_mainlist_area_W08 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W9
            (and (= patt_name_mainlist_name_ (nth 21 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W09 (+ patt_name_mainlist_area_W09 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W10
            (and (= patt_name_mainlist_name_ (nth 22 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W10 (+ patt_name_mainlist_area_W10 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W11
            (and (= patt_name_mainlist_name_ (nth 23 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W11 (+ patt_name_mainlist_area_W11 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W12
            (and (= patt_name_mainlist_name_ (nth 24 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W12 (+ patt_name_mainlist_area_W12 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W13
            (and (= patt_name_mainlist_name_ (nth 25 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W13 (+ patt_name_mainlist_area_W13 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W14
            (and (= patt_name_mainlist_name_ (nth 26 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W14 (+ patt_name_mainlist_area_W14 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W15
            (and (= patt_name_mainlist_name_ (nth 27 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W15 (+ patt_name_mainlist_area_W15 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W16
            (and (= patt_name_mainlist_name_ (nth 28 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W16 (+ patt_name_mainlist_area_W16 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W17
            (and (= patt_name_mainlist_name_ (nth 29 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W17 (+ patt_name_mainlist_area_W17 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W18
            (and (= patt_name_mainlist_name_ (nth 30 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W18 (+ patt_name_mainlist_area_W18 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W19
            (and (= patt_name_mainlist_name_ (nth 31 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W19 (+ patt_name_mainlist_area_W19 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W20
            (and (= patt_name_mainlist_name_ (nth 32 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W20 (+ patt_name_mainlist_area_W20 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W21
            (and (= patt_name_mainlist_name_ (nth 33 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W21 (+ patt_name_mainlist_area_W21 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W22
            (and (= patt_name_mainlist_name_ (nth 34 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W22 (+ patt_name_mainlist_area_W22 patt_name_mainlist_area_)))
            )
          )

        
        (setq patt_name_mainlist_i (+ patt_name_mainlist_i 1))
      )
    ;
    (setq all_area_list_ (list 
                           (list  patt_name_mainlist_area_C01 
                                  patt_name_mainlist_area_C02 
                                  patt_name_mainlist_area_C03 
                                  patt_name_mainlist_area_C04 
                                  patt_name_mainlist_area_C05 
                                  patt_name_mainlist_area_C06 
                                  patt_name_mainlist_area_C07 
                                  patt_name_mainlist_area_C08 
                                  patt_name_mainlist_area_C09 
                                  patt_name_mainlist_area_C10 
                                  patt_name_mainlist_area_C11
                                  patt_name_mainlist_area_C11 
                                  patt_name_mainlist_area_C12 
                                  patt_name_mainlist_area_W01 
                                  patt_name_mainlist_area_W02 
                                  patt_name_mainlist_area_W03 
                                  patt_name_mainlist_area_W04 
                                  patt_name_mainlist_area_W05 
                                  patt_name_mainlist_area_W06 
                                  patt_name_mainlist_area_W07 
                                  patt_name_mainlist_area_W08 
                                  patt_name_mainlist_area_W09
                                  patt_name_mainlist_area_W10 
                                  patt_name_mainlist_area_W11 
                                  patt_name_mainlist_area_W12 
                                  patt_name_mainlist_area_W13 
                                  patt_name_mainlist_area_W14 
                                  patt_name_mainlist_area_W15 
                                  patt_name_mainlist_area_W16 
                                  patt_name_mainlist_area_W17 
                                  patt_name_mainlist_area_W18 
                                  patt_name_mainlist_area_W19 
                                  patt_name_mainlist_area_W20 
                                  patt_name_mainlist_area_W21 
                                  patt_name_mainlist_area_W22
                           )
                         )
    )
    (length (car all_area_list_))
    (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1))
    (TA:EXCEL_autofit_column_ Excel_lasted_worksheet_obj_)
    (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 3 2 (getvar "dwgname"))
    (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 2 5 (list mainlist_Ceiling ))
    (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 3 5 all_area_list_)
  ;
)
(defun c:Hatch_to_Excel_H3Excel ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "HATCH") ;type of object
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
    (setq patt_name_mainlist_ ())
    (setq patt_name_name_hatch_list_ ())
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_obj_patt_name_ (vla-get-patternname ss_pre_filter_set_xx_obj_))
      (if 
        (or 
          (= (wcmatch ss_pre_filter_set_xx_obj_patt_name_ "000_C*") T)
          (= (wcmatch ss_pre_filter_set_xx_obj_patt_name_ "000_W*") T)
        )
        (progn
          (setq ss_pre_filter_set_xx_obj_Harea_ (/ (vla-get-area ss_pre_filter_set_xx_obj_) 1000000 ))
          (setq patt_name_name_hatch_list_ (cons  ss_pre_filter_set_xx_obj_patt_name_ patt_name_name_hatch_list_))
          (setq patt_name_sublist_ (list ss_pre_filter_set_xx_obj_patt_name_ ss_pre_filter_set_xx_obj_Harea_  ))
          (setq patt_name_mainlist_ (cons patt_name_sublist_ patt_name_mainlist_))
        )
      )
      
      
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;preloop_and_while
    (setq mainlist_Ceiling (list 
                            "000_C01"
                            "000_C02"
                            "000_C03"
                            "000_C04"
                            "000_C05"
                            "000_C05A"
                            "000_C06"
                            "000_C07"
                            "000_C08"
                            "000_C09"
                            "000_C10"
                            "000_C11"
                            "000_C12"
                            "000_W01"
                            "000_W02"
                            "000_W03"
                            "000_W04"
                            "000_W05"
                            "000_W06"
                            "000_W07"
                            "000_W08"
                            "000_W09"
                            "000_W10"
                            "000_W11"
                            "000_W12"
                            "000_W13"
                            "000_W14"
                            "000_W15"
                            "000_W16"
                            "000_W17"
                            "000_W18"
                            "000_W19"
                            "000_W20"
                            "000_W21"
                            "000_W22"
                           )
    )
    (setq patt_name_mainlist_area_C01 0
          patt_name_mainlist_area_C02 0
          patt_name_mainlist_area_C03 0
          patt_name_mainlist_area_C04 0
          patt_name_mainlist_area_C05 0
          patt_name_mainlist_area_C05A 0
          patt_name_mainlist_area_C06 0
          patt_name_mainlist_area_C07 0
          patt_name_mainlist_area_C08 0
          patt_name_mainlist_area_C09 0
          patt_name_mainlist_area_C10 0
          patt_name_mainlist_area_C11 0
          patt_name_mainlist_area_C12 0
          patt_name_mainlist_area_W01 0
          patt_name_mainlist_area_W02 0
          patt_name_mainlist_area_W03 0
          patt_name_mainlist_area_W04 0
          patt_name_mainlist_area_W05 0
          patt_name_mainlist_area_W06 0
          patt_name_mainlist_area_W07 0
          patt_name_mainlist_area_W08 0
          patt_name_mainlist_area_W09 0
          patt_name_mainlist_area_W10 0
          patt_name_mainlist_area_W11 0
          patt_name_mainlist_area_W12 0
          patt_name_mainlist_area_W13 0
          patt_name_mainlist_area_W14 0
          patt_name_mainlist_area_W15 0
          patt_name_mainlist_area_W16 0
          patt_name_mainlist_area_W17 0
          patt_name_mainlist_area_W18 0
          patt_name_mainlist_area_W19 0
          patt_name_mainlist_area_W20 0
          patt_name_mainlist_area_W21 0
          patt_name_mainlist_area_W22 0
    )
    (length (car all_area_list_))
    ;preloop_and_while
      (setq patt_name_mainlist_i 0)
      (while (< patt_name_mainlist_i (length patt_name_mainlist_))
        (setq patt_name_mainlist_name_ (car (nth  patt_name_mainlist_i patt_name_mainlist_)))
        (setq patt_name_mainlist_area_ (cadr (nth  patt_name_mainlist_i patt_name_mainlist_)))
          (cond 
            ( ;Harea_case_0
            (and (= patt_name_mainlist_name_ (nth 0 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C01 (+ patt_name_mainlist_area_C01 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_1
            (and (= patt_name_mainlist_name_ (nth 1 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C02 (+ patt_name_mainlist_area_C02 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_2
            (and (= patt_name_mainlist_name_ (nth 2 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C03 (+ patt_name_mainlist_area_C03 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_3
            (and (= patt_name_mainlist_name_ (nth 3 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C04 (+ patt_name_mainlist_area_C04 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_4
            (and (= patt_name_mainlist_name_ (nth 4 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C05 (+ patt_name_mainlist_area_C05 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_5
            (and (= patt_name_mainlist_name_ (nth 5 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C05A (+ patt_name_mainlist_area_C05A patt_name_mainlist_area_)))
            )
            ( ;Harea_case_6
            (and (= patt_name_mainlist_name_ (nth 6 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C06 (+ patt_name_mainlist_area_C06 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_7
            (and (= patt_name_mainlist_name_ (nth 7 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C07 (+ patt_name_mainlist_area_C07 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_8
            (and (= patt_name_mainlist_name_ (nth 8 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C08 (+ patt_name_mainlist_area_C08 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_9
            (and (= patt_name_mainlist_name_ (nth 9 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C09 (+ patt_name_mainlist_area_C09 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_10
            (and (= patt_name_mainlist_name_ (nth 10 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C10 (+ patt_name_mainlist_area_C10 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_11
            (and (= patt_name_mainlist_name_ (nth 11 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C11 (+ patt_name_mainlist_area_C11 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_12
            (and (= patt_name_mainlist_name_ (nth 12 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_C12 (+ patt_name_mainlist_area_C12 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W1
            (and (= patt_name_mainlist_name_ (nth 13 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W01 (+ patt_name_mainlist_area_W01 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W2
            (and (= patt_name_mainlist_name_ (nth 14 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W02 (+ patt_name_mainlist_area_W02 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W3
            (and (= patt_name_mainlist_name_ (nth 15 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W03 (+ patt_name_mainlist_area_W03 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W4
            (and (= patt_name_mainlist_name_ (nth 16 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W04 (+ patt_name_mainlist_area_W04 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W5
            (and (= patt_name_mainlist_name_ (nth 17 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W05 (+ patt_name_mainlist_area_W05 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W6
            (and (= patt_name_mainlist_name_ (nth 18 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W06 (+ patt_name_mainlist_area_W06 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W7
            (and (= patt_name_mainlist_name_ (nth 19 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W07 (+ patt_name_mainlist_area_W07 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W8
            (and (= patt_name_mainlist_name_ (nth 20 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W08 (+ patt_name_mainlist_area_W08 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W9
            (and (= patt_name_mainlist_name_ (nth 21 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W09 (+ patt_name_mainlist_area_W09 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W10
            (and (= patt_name_mainlist_name_ (nth 22 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W10 (+ patt_name_mainlist_area_W10 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W11
            (and (= patt_name_mainlist_name_ (nth 23 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W11 (+ patt_name_mainlist_area_W11 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W12
            (and (= patt_name_mainlist_name_ (nth 24 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W12 (+ patt_name_mainlist_area_W12 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W13
            (and (= patt_name_mainlist_name_ (nth 25 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W13 (+ patt_name_mainlist_area_W13 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W14
            (and (= patt_name_mainlist_name_ (nth 26 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W14 (+ patt_name_mainlist_area_W14 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W15
            (and (= patt_name_mainlist_name_ (nth 27 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W15 (+ patt_name_mainlist_area_W15 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W16
            (and (= patt_name_mainlist_name_ (nth 28 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W16 (+ patt_name_mainlist_area_W16 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W17
            (and (= patt_name_mainlist_name_ (nth 29 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W17 (+ patt_name_mainlist_area_W17 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W18
            (and (= patt_name_mainlist_name_ (nth 30 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W18 (+ patt_name_mainlist_area_W18 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W19
            (and (= patt_name_mainlist_name_ (nth 31 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W19 (+ patt_name_mainlist_area_W19 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W20
            (and (= patt_name_mainlist_name_ (nth 32 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W20 (+ patt_name_mainlist_area_W20 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W21
            (and (= patt_name_mainlist_name_ (nth 33 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W21 (+ patt_name_mainlist_area_W21 patt_name_mainlist_area_)))
            )
            ( ;Harea_case_W22
            (and (= patt_name_mainlist_name_ (nth 34 mainlist_Ceiling)))
            (progn (setq patt_name_mainlist_area_W22 (+ patt_name_mainlist_area_W22 patt_name_mainlist_area_)))
            )
          )

        
        (setq patt_name_mainlist_i (+ patt_name_mainlist_i 1))
      )
    ;
    (setq all_area_list_ (list 
                           (list  patt_name_mainlist_area_C01 
                                  patt_name_mainlist_area_C02 
                                  patt_name_mainlist_area_C03 
                                  patt_name_mainlist_area_C04 
                                  patt_name_mainlist_area_C05 
                                  patt_name_mainlist_area_C05A 
                                  patt_name_mainlist_area_C06 
                                  patt_name_mainlist_area_C07 
                                  patt_name_mainlist_area_C08 
                                  patt_name_mainlist_area_C09 
                                  patt_name_mainlist_area_C10 
                                  patt_name_mainlist_area_C11
                                  patt_name_mainlist_area_C12 
                                  patt_name_mainlist_area_W01 
                                  patt_name_mainlist_area_W02 
                                  patt_name_mainlist_area_W03 
                                  patt_name_mainlist_area_W04 
                                  patt_name_mainlist_area_W05 
                                  patt_name_mainlist_area_W06 
                                  patt_name_mainlist_area_W07 
                                  patt_name_mainlist_area_W08 
                                  patt_name_mainlist_area_W09
                                  patt_name_mainlist_area_W10 
                                  patt_name_mainlist_area_W11 
                                  patt_name_mainlist_area_W12 
                                  patt_name_mainlist_area_W13 
                                  patt_name_mainlist_area_W14 
                                  patt_name_mainlist_area_W15 
                                  patt_name_mainlist_area_W16 
                                  patt_name_mainlist_area_W17 
                                  patt_name_mainlist_area_W18 
                                  patt_name_mainlist_area_W19 
                                  patt_name_mainlist_area_W20 
                                  patt_name_mainlist_area_W21 
                                  patt_name_mainlist_area_W22
                           )
                         )
    )
    (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1))
    (TA:EXCEL_autofit_column_ Excel_lasted_worksheet_obj_)
    (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 3 2 (getvar "dwgname"))
    (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 2 6 (list mainlist_Ceiling ))
    (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 3 6 all_area_list_)
  ;
  (c:Special-Hatch_to_Excel_SH2Excel)
)
(defun c:select_hacth_SsH ()
  (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "HATCH") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )

  (command "pselect"  ss_pre_filter_set_xx_  "" )
)
(defun c:select_hacth_SSB ()
  (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )

  (command "pselect"  ss_pre_filter_set_xx_  "" )
)

(defun c:finderrorhatcharea_fear_ ()
  

  (setq sss_ (ssget 
                                (list 
                                  (cons 0 "hatch") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                              )
  )
  ;preloop_and_while
    (setq sss_i 0)
    (setq sss_set_ (ssadd))
    (while (< sss_i (sslength sss_))
      (setq sss_ename_ (ssname sss_ sss_i))
      (setq sss_obj_ (vlax-ename->vla-object sss_ename_))
      
      (if 
        (= (wcmatch (vl-prin1-to-string (vl-catch-all-apply (function (lambda () (vla-get-area sss_obj_)))) ) "*error*" ) T )
        (progn 
          (setq sss_set_ (ssadd sss_ename_ sss_set_))
          (sslength sss_set_)
        )
      )
      
      
      (setq sss_i (+ sss_i 1))
    )
  ;
  (command "pselect"  sss_set_ "" )
)

(defun c:Special-Hatch_to_Excel_SH2Excel ()
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
  (if (/= ss_pre_filter_set_xx_ nil)
    (progn
      (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP_MATT_LENGTH"))
    )
  )
  
  
  ;preloop_and_while
    (setq spec_mat (list 
                    "LIGTHING BOOM"
                    "SC05A"
                    "SC05B"
                   )
    )
    (setq LIGTHING_boom_val 0
          SC05A_val          0
          SC05B_val          0
    )
    (setq ss_pre_filter_set_xx_i 0)
    (while 
      (and 
        (/= ss_pre_filter_set_xx_ nil)
        (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      )
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq mat_name_ (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "MATT_NAME"))
      (setq H (/ (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "H") 1000))
      (cond
          ( ;Harea_case_W22
            (and (= mat_name_ (nth 0 spec_mat)))
            (progn (setq LIGTHING_boom_val (+ LIGTHING_boom_val H)))
          )
          ( ;Harea_case_W22
            (and (= mat_name_ (nth 1 spec_mat)))
            (progn (setq SC05A_val (+ SC05A_val H)))
          )
          ( ;Harea_case_W22
            (and (= mat_name_ (nth 2 spec_mat)))
            (progn (setq SC05B_val (+ SC05B_val H)))
          )
      )
      (setq spec-matt_val_list_ (list 
                                  LIGTHING_boom_val
                                  SC05A_val
                                  SC05B_val
                                )
      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
      ; (setq Excel_lasted_worksheet_obj_ (TA:Excel_Open-Excel_ 1 1))
      ; (TA:EXCEL_autofit_column_ Excel_lasted_worksheet_obj_)
      (TA:Excel_input_data_R1C1 Excel_lasted_worksheet_obj_ 3 2 (getvar "dwgname"))
      (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 2 3 (list spec_mat ))
      (TA:Excel_foreach_input_list_dataset_R1C1 Excel_lasted_worksheet_obj_ 3 3 (list spec-matt_val_list_))
  ;
)







(defun c:stamplevel_slv_ ()
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
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
      (setq ss_pre_filter_set_xx_obj_ins_trans_xyz (trans ss_pre_filter_set_xx_obj_ins_ 0 1))
      (LM:vl-setattributevalue ss_pre_filter_set_xx_obj_ "MAT_LV" (rtos (cadr ss_pre_filter_set_xx_obj_ins_trans_xyz) 2 2 ))
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;



)


(defun c:hex1 ()
  

  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "LWPOLYLINE") ;type of object
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
                                            (cons 0 "LWPOLYLINE") ;type of object
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
  (textscr)
  (getstring "\nHit [ENTER] to continue...")
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (setq new_ssset (ssadd))
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      
      
      (if 
        (and 
          (= (vla-get-closed ss_pre_filter_set_xx_obj_ ) :vlax-true)
          (= (length (cadr (TA:Get_Pline_vertext_ins_point_ ss_pre_filter_set_xx_ename_))) 6)
        )
        (progn
          (setq new_ssset (ssadd ss_pre_filter_set_xx_ename_ new_ssset))
        )
      )
      ; (command "pselect" ss_pre_filter_set_xx_ename_   "" )
      
      (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_pre_filter_set_xx_i (/ 100 (float (sslength ss_pre_filter_set_xx_)))) 2 0) "%\n"))
      
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  (command "pselect"  new_ssset "" )
)

       
(defun c:Lasted_selection_set_LSS_ ()
  (command "pselect"  ss_pre_filter_set_xx_ "" )
)
      
(defun c:Select_by_color_SBC_ ()
  (setq input_color_ (cond ( (getint (strcat "\nspecify get color \n<" (rtos (setq input_color_ (cond (input_color_) (1.0) ) ) ) "> : " ) ) ) (input_color_) ) )
  (if  ;pre_select_ssget_or_post_select_ssget
    (= 
      (setq ss_pre_filter_set_xx_ (ssget "i" 
                                        (list 
                                          ;  (cons 0 "insert") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                          (cons 62 (fix input_color_))           ;kind of color call sign with color code index
                                        )
                                  )
      )
      nil
    )
    (progn 
      (setq ss_pre_filter_set_xx_ (ssget 
                                    (list 
                                      ; (cons 0 "insert") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                      (cons 62 (fix input_color_))           ;kind of color call sign with color code index
                                    )
                                  )
      )
    )
    (sslength ss_pre_filter_set_xx_)
  )
  (command "pselect"  ss_pre_filter_set_xx_ "" )
)
(defun c:Change_object_color_CHC_ ()
  (setq Return_color_ (cond ( (getint (strcat "\nspecify get rotation\n<" (rtos (setq Return_color_ (cond (Return_color_) (1.0) ) ) ) "> : " ) ) ) (Return_color_) ) )
  (if  ;pre_select_ssget_or_post_select_ssget
    (= 
      (setq ss_pre_filter_set_xx_ (ssget "i" 
                                        (list 
                                          ;  (cons 0 "insert") ;type of object
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
                                      ; (cons 0 "insert") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                      ; (cons 62 1)           ;kind of color call sign with color code index
                                    )
                                  )
      )
    )
    (sslength ss_pre_filter_set_xx_)
  )
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_i 0)
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (vla-put-color ss_pre_filter_set_xx_obj_ Return_color_)
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  
  
)




  (defun TA:asssembly_3Dpolyline_ ()
    ;Explode process_
      ;selection_set_for_fillter_blk_name
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq ssset_3DPL_ (ssget "X"
                                (list 
                                  (cons 0 "LWPOLYLINE,POLYLINE,LINE") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                  (cons 410 (getvar "CTAB"))           
                                )
                              )
            )
            nil
          )
          (progn 
            (setq ssset_3DPL_ (ssget "X"
                                (list 
                                  (cons 0 "LWPOLYLINE,POLYLINE,LINE") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                  (cons 410 (getvar "CTAB"))
                                )
                              )
            )
          )
          (sslength ssset_3DPL_)
        )
      ;


      ;preloop_and_while
        (textscr)
        ; (getstring "\nHit [ENTER] to continue...")
        (setq ssset_3DPL_i 0)
        (while (and 
                 (/= ssset_3DPL_ nil)
                 (< ssset_3DPL_i (sslength ssset_3DPL_))
               )
          (setq ssset_3DPL_ename_ (ssname ssset_3DPL_ ssset_3DPL_i))
          (setq ssset_3DPL_obj_ (vlax-ename->vla-object ssset_3DPL_ename_))
          (if (= (wcmatch (vl-princ-to-string ssset_3DPL_obj_) "*IAcad3DPolyline*") T)
            (progn
              
              (vla-explode ssset_3DPL_obj_ )
              (vla-erase  ssset_3DPL_obj_)
              
            )
          )
          (setq percent (fix (* (/ (float ssset_3DPL_i) (sslength ssset_3DPL_)) 100)))
          (if (= (substr (rtos (/ (float ssset_3DPL_i) 2 ) 2 1) 3 1) "0")
            (progn
              (princ (strcat "tranfering " (rtos ssset_3DPL_i 2 0) "/" (rtos (sslength ssset_3DPL_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
            )
            (princ (strcat "                                tranfering " (rtos ssset_3DPL_i 2 0) "/" (rtos (sslength ssset_3DPL_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
          )
          (setq ssset_3DPL_i (+ ssset_3DPL_i 1))
        )
      ;
    ;
  )
  (defun TA:reaxis-z_ ()
    ;return Z 0
      ;selection_set_for_fillter_blk_name
        (if  ;pre_select_ssget_or_post_select_ssget
          (=
            (setq ss_pre_filter_set_xx_ (ssget "x"
                                              (list
                                                ; (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                                (cons 410 (getvar "CTAB"))
                                              )
                                        )
            )
            nil
          )
          (progn
            (setq ss_pre_filter_set_xx_ (ssget  "X"
                                              (list
                                                ; (cons 0 "INSERT") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                                (cons 410 (getvar "CTAB"))
                                              )
                                        )
            )
          )
          
        )
      ;
      ;preloop_and_while
        (textscr)
        ; (getstring "\nHit \nHit \nHit [ENTER] to continue...")
        (setq ss_pre_filter_set_xx_i 0)
        (while (and 
                 (/= ss_pre_filter_set_xx_ nil)
                 (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
               )
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
          ;main_idea_
            (cond
              (;type_name_object_case_1
                (and
                    (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadLine" "*")) T)
                    (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbLine" ) T)
                )
                (progn
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
                  (vla-put-startpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_startpt_) (cadr ss_pre_filter_set_xx_obj_startpt_) 0))
                  (vla-put-endpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_endpt_) (cadr ss_pre_filter_set_xx_obj_endpt_) 0))
                  (princ "type_name_object_case_1")
                )
              )
              (;type_name_object_case_2
                (and
                    (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadLWPolyline" "*")) T)
                    (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbPolyline") T)
                )
                (progn
                  (vla-put-elevation ss_pre_filter_set_xx_obj_ 0)
                  (princ "type_name_object_case_2")
                )
              )
              (;type_name_object_case_3
                (and
                    (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadBlockReference " "*")) T)
                    (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbBlockReference") T)
                )
                (progn
                  (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
                  (vla-put-insertionpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_ins_) (cadr ss_pre_filter_set_xx_obj_ins_) 0) )
                  (princ "type_name_object_case_3")
                )
              )
              (;type_name_object_case_4
                (and
                    (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadHatch" "*")) T)
                    (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbHatch") T)
                )
                (progn
                  (vla-put-elevation ss_pre_filter_set_xx_obj_  0)
                  (princ "type_name_object_case_4")
                )
              )
              (;type_name_object_case_5
                (and
                    (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                    (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadArc" "*")) T)
                    (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbArc") T)
                )
                (progn
                  (setq ss_pre_filter_set_xx_obj_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_pre_filter_set_xx_obj_) ) ) )
                  (vla-put-center ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_center_) (cadr ss_pre_filter_set_xx_obj_center_) 0 ) )
                  (princ "type_name_object_case_5")
                )
              )
            )
          ;
          (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))
          (if (= (substr (rtos (/ (float ss_pre_filter_set_xx_i) 2 ) 2 1) 3 1) "0")
            (progn
              (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
            )
            (princ (strcat "                                tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
          )
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    ;
  )
(defun c:axisZ0_ ()
  (TA:asssembly_3Dpolyline_)
  (TA:reaxis-z_)
  (setq all_nameblock_ (TA:get_name_block_in_drawing_to_list))
  ;preloop_and_while
    (if (wcmatch ;detect all_nameblock_i
          (vl-princ-to-string
            (setq error_detect_ ;Detect the variable's result in case of a function error
                    (vl-catch-all-apply 
                      (function 
                        (lambda () 
                          (setq all_nameblock_i (cond ( (getint (strcat "\nspecify get all_nameblock_i \n<" (rtos (setq all_nameblock_i (cond (all_nameblock_i) (1.0) ) ) ) "> : " ) ) ) (all_nameblock_i) ) )
                        )
                      )
                    )
            )
          )
          "*error*"
        )
      (progn
        (setq all_nameblock_i 0)
        
      )
      (princ all_nameblock_i)
    )
    (while (< all_nameblock_i (length all_nameblock_))
      (setq all_nameblock_ename_ (nth  all_nameblock_i all_nameblock_))
        (command "bedit" all_nameblock_ename_  )
        (TA:asssembly_3Dpolyline_)
        (TA:reaxis-z_)
        (command "bclose" "s" )
      (setq all_nameblock_i (+ all_nameblock_i 1))
    )
  ;
)


(defun c:Selection-set_reverse_ssreverse ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (if (> (rad-to-deg (vla-get-angle ss_pre_filter_set_xx_obj_)) 90)
        (progn
          (command "reverse" ss_pre_filter_set_xx_ename_ "" )
        )
      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)

(defun c:drawline_startpoint_DLST_ ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
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
    (setq ss_pre_filter_set_xx_startpt_list_ ())
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
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
      (setq ss_pre_filter_set_xx_startpt_list_ (cons ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_startpt_list_))
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
  ;TA:stanndard_lambda_sorting
    (setq ss_pre_filter_set_xx_startpt_list_sorted_ (vl-sort ss_pre_filter_set_xx_startpt_list_  ;bigest open indent list
                                                             (function 
                                                               (lambda (a b) 
                                                                 (> (cadr a) 
                                                                    (cadr b)
                                                                 )
                                                               )
                                                             )
                                                    ) ;bigest close indent list
    )
  ;
  (create-pline ss_pre_filter_set_xx_startpt_list_sorted_ )
)


; (setq gh_ename_ (car (entsel "specify Object")))
; (setq gh_obj_ (vlax-ename->vla-object gh_ename_))
; (entget gh_ename_)
; (vlax-dump-object gh_obj_)
; (vla-get-area gh_obj_ )
; (vlax-property-available-p gh_obj_ "area" ) 
; (command "area" "o" gh_ename_  )

; (setq ent (car (entsel)))
; (setq obj (vlax-ename->vla-object ent))
; (setq props (vla-GetMassProperties obj))
; (setq area (vlax-variant-value (vla-Item props "SurfaceArea"))) 

; (getvar "lastprompt")

(defun c:point-to-point_PP2P_ ()
  (setq pl-1_ename_ (car (entsel "specify Object")))
  (setq pl-1_obj_ (vlax-ename->vla-object pl-1_ename_))
  (setq pl-1-obj_vtp_ (cadr(TA:Get_Pline_vertext_ins_point_ pl-1_ename_ )))
  (setq pl-2_ename_ (car (entsel "specify Object")))
  (setq pl-2_obj_ (vlax-ename->vla-object pl-2_ename_))
  (setq pl-2-obj_vtp_ (cadr (TA:Get_Pline_vertext_ins_point_ pl-2_ename_ )))

  ;preloop_and_while
    (setq i 0)
    (while (< i (length pl-1-obj_vtp_))
      (setq ename_1 (nth  i pl-1-obj_vtp_))
      (setq ename_2 (nth  i pl-2-obj_vtp_))
      (command "line" ename_1 ename_2 "")
      (setq i (+ i 1))
    )
  ;
)
(defun c:point-to-point_PL2P_ ()
  (setq pl-1_ename_ (car (entsel "specify Object")))
  (setq pl-1_obj_ (vlax-ename->vla-object pl-1_ename_))
  (setq pl-1-obj_vtp_ (cadr(TA:Get_Pline_vertext_ins_point_ pl-1_ename_ )))
  (setq pl-2_ename_ (car (entsel "specify Object")))
  (setq pl-2_obj_ (vlax-ename->vla-object pl-2_ename_))
  (setq pl-2-obj_vtp_ (cadr (TA:Get_Pline_vertext_ins_point_ pl-2_ename_ )))

  ; ;preloop_and_while
  ;   (setq i 0)
  ;   (while (< i (length pl-1-obj_vtp_))
      (setq ename_1 (nth  0 pl-1-obj_vtp_))
      (setq ename_3 (nth  0 pl-2-obj_vtp_))
      (setq ename_2 (nth  (- (length pl-1-obj_vtp_) 1) pl-1-obj_vtp_))
      (setq ename_4 (nth  (- (length pl-1-obj_vtp_) 1) pl-2-obj_vtp_))
      (command "line" ename_1 ename_3 "")
      (command "line" ename_2 ename_4 "")
  ;     (setq i (+ i 1))
  ;   )
  ; ;
)
(defun c:point-to-point_L2REC_ ()
  (setq pl-1_ename_ (car (entsel "specify Object")))
  (setq pl-1_obj_ (vlax-ename->vla-object pl-1_ename_))
  (setq pl-1-obj_vtp_ (cadr(TA:Get_Pline_vertext_ins_point_ pl-1_ename_ )))
  (setq pl-2_ename_ (car (entsel "specify Object")))
  (setq pl-2_obj_ (vlax-ename->vla-object pl-2_ename_))
  (setq pl-2-obj_vtp_ (cadr (TA:Get_Pline_vertext_ins_point_ pl-2_ename_ )))

  ; ;preloop_and_while
  ;   (setq i 0)
  ;   (while (< i (length pl-1-obj_vtp_))
      (setq 1st-1 (car pl-1-obj_vtp_))
      (setq 1st-2 (car (reverse pl-1-obj_vtp_)))
  
      (setq 2nd-1 (car pl-2-obj_vtp_))
      (setq 2nd-2 (cadr pl-2-obj_vtp_))
  
      (command "line" 1st-1 2nd-1 "")
      (command "line" 1st-2 2nd-2 "")

  
  ;     (setq i (+ i 1))
  ;   )
  ; ;
)


(defun c:Rehatch_color_RHC ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "hatch") ;type of object
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
                                            (cons 0 "hatch") ;type of object
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
    (setq ssset_ ())
    (setq ssset_ (ssadd))
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (cond 
        ( ;vla-get-color_case_1
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C01")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 70)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_1")
         )
        )
        ( ;vla-get-color_case_2
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C02")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 90)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_2")
         )
        )
        ( ;vla-get-color_case_3
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C03")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_3")
         )
        )
        ( ;vla-get-color_case_4
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C04")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 130)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_4")
         )
        )
        ( ;vla-get-color_case_5
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C05")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_5")
         )
        )
        ( ;vla-get-color_case_5
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C05A")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_5")
         )
        )
        ( ;vla-get-color_case_6
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C06")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 170)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_6")
         )
        )
        ( ;vla-get-color_case_7
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C07")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 190)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_7")
         )
        )
        ( ;vla-get-color_case_8
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C08")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 210)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_8")
         )
        )
        ( ;vla-get-color_case_9
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C09")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 230)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_9")
         )
        )
        ( ;vla-get-color_case_10
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C10")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 235)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_10")
         )
        )
        ( ;vla-get-color_case_11
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C11")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 240)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_11")
         )
        )
        ( ;vla-get-color_case_12
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W01")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_12")
         )
        )
        ( ;vla-get-color_case_13
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W02")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 40)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_13")
         )
        )
        ( ;vla-get-color_case_14
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W03")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 60)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_14")
         )
        )
        ( ;vla-get-color_case_15
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W04")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 80)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_15")
         )
        )
        ( ;vla-get-color_case_16
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W05")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 100)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_16")
         )
        )
        ( ;vla-get-color_case_17
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W06")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 120)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_17")
         )
        )
        ( ;vla-get-color_case_18
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W07")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 140)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_18")
         )
        )
        ( ;vla-get-color_case_19
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W08")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_19")
         )
        )
        ( ;vla-get-color_case_20
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W09")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 180)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_20")
         )
        )
        ( ;vla-get-color_case_21
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W10")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 200)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_21")
         )
        )
        ( ;vla-get-color_case_22
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W11")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 220)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_22")
         )
        )
        ( ;vla-get-color_case_23
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W12")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 240)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_23")
         )
        )
        ( ;vla-get-color_case_24
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W13")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 20)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_24")
         )
        )
        ( ;vla-get-color_case_25
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W14")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 40)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_25")
         )
        )
        ( ;vla-get-color_case_26
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W15")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 60)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_26")
         )
        )
        ( ;vla-get-color_case_27
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W16")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 80)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_27")
         )
        )
        ( ;vla-get-color_case_28
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W17")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 100)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_28")
         )
        )
        ( ;vla-get-color_case_29
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W18")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 120)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_29")
         )
        )
        ( ;vla-get-color_case_30
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W19")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 140)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_30")
         )
        )
        ( ;vla-get-color_case_31
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W20")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 160)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_31")
         )
        )
        ( ;vla-get-color_case_32
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W21")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 180)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_32")
         )
        )
        ( ;vla-get-color_case_33
         (and 
           (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_W22")
         )
         (progn 
           (vla-put-color ss_pre_filter_set_xx_obj_ 200)
           (setq ssset_ (ssadd ss_pre_filter_set_xx_ename_ ssset_))
           (princ "vla-get-color_case_33")
         )
        )
      )
      
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
    (command "pselect"  ssset_ "" )
  ;
)
(defun c:Rehatch_color_RHCx ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
                                          (list
                                            (cons 0 "hatch") ;type of object
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
                                            (cons 0 "hatch") ;type of object
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
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (cond 
        ( ;vla-get-color_case_1
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C01")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 70)
          (princ "vla-get-color_case_1")
        )
        )
        ( ;vla-get-color_case_2 
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C02")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 90)
          (princ "vla-get-color_case_2")
        )
        )
        ( ;vla-get-color_case_3
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C03")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 20)
          (princ "vla-get-color_case_3")
        )
        )
        ( ;vla-get-color_case_4
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C04")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 130)
          (princ "vla-get-color_case_4")
        )
        )
        ( ;vla-get-color_case_5
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C05")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 20)
          (princ "vla-get-color_case_5")
        )
        )
        ( ;vla-get-color_case_5
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C05A")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 20)
          (princ "vla-get-color_case_5")
        )
        )
        ( ;vla-get-color_case_6
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C06")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 170)
          (princ "vla-get-color_case_6")
        )
        )
        ( ;vla-get-color_case_7
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C07")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 190)
          (princ "vla-get-color_case_7")
        )
        )
        ( ;vla-get-color_case_8
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C08")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 210)
          (princ "vla-get-color_case_8")
        )
        )
        ( ;vla-get-color_case_9
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C09")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 230)
          (princ "vla-get-color_case_9")
        )
        )
        ( ;vla-get-color_case_10
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C10")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 235)
          (princ "vla-get-color_case_10")
        )
        )
        ( ;vla-get-color_case_11
        (and 
          (= (vla-get-patternname ss_pre_filter_set_xx_obj_) "000_C11")
        )
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 240)
          (princ "vla-get-color_case_11")
        )
        )
        (;vla-get-color_case_12
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W01")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 20)
            (princ "vla-get-color_case_12")
          )
        )
        (;vla-get-color_case_13
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W02")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 40)
            (princ "vla-get-color_case_13")
          )
        )
        (;vla-get-color_case_14
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W03")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 60)
            (princ "vla-get-color_case_14")
          )
        )
        (;vla-get-color_case_15
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W04")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 80)
            (princ "vla-get-color_case_15")
          )
        )
        (;vla-get-color_case_16
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W05")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 100)
            (princ "vla-get-color_case_16")
          )
        )
        (;vla-get-color_case_17
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W06")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 120)
            (princ "vla-get-color_case_17")
          )
        )
        (;vla-get-color_case_18
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W07")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 140)
            (princ "vla-get-color_case_18")
          )
        )
        (;vla-get-color_case_19
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W08")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 20)
            (princ "vla-get-color_case_19")
          )
        )
        (;vla-get-color_case_20
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W09")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 180)
            (princ "vla-get-color_case_20")
          )
        )
        (;vla-get-color_case_21
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W10")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 200)
            (princ "vla-get-color_case_21")
          )
        )
        (;vla-get-color_case_22
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W11")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 220)
            (princ "vla-get-color_case_22")
          )
        )
        (;vla-get-color_case_23
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W12")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 240)
            (princ "vla-get-color_case_23")
          )
        )
        (;vla-get-color_case_24
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W13")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 20)
            (princ "vla-get-color_case_24")
          )
        )
        (;vla-get-color_case_25
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W14")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 40)
            (princ "vla-get-color_case_25")
          )
        )
        (;vla-get-color_case_26
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W15")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 60)
            (princ "vla-get-color_case_26")
          )
        )
        (;vla-get-color_case_27
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W16")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 80)
            (princ "vla-get-color_case_27")
          )
        )
        (;vla-get-color_case_28
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W17")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 100)
            (princ "vla-get-color_case_28")
          )
        )
        (;vla-get-color_case_29
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W18")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 120)
            (princ "vla-get-color_case_29")
          )
        )
        (;vla-get-color_case_30
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W19")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 140)
            (princ "vla-get-color_case_30")
          )
        )
        (;vla-get-color_case_31
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W20")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 160)
            (princ "vla-get-color_case_31")
          )
        )
        (;vla-get-color_case_32
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W21")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 180)
            (princ "vla-get-color_case_32")
          )
        )
        (;vla-get-color_case_33
          (and
            (= (vla-get-patternname ss_pre_filter_set_xx_obj_ ) "000_W22")
          )
          (progn
            (vla-put-color ss_pre_filter_set_xx_obj_ 200)
            (princ "vla-get-color_case_33")
          )
        )

      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)


(defun c:PPLINE-ITD_detect_wall_Sym_dewall ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
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
  ; (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "A$C441E3E01")) ;METHOD 1
  (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "A$C441E3E01")) ;METHOD 2
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_filter_i 0)
    (while (< ss_pre_filter_set_xx_filter_i (sslength ss_pre_filter_set_xx_filter_))
      (setq ss_pre_filter_set_xx_filter_ename_ (ssname ss_pre_filter_set_xx_filter_ ss_pre_filter_set_xx_filter_i))
      (setq ss_pre_filter_set_xx_filter_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_filter_ename_))
      
        (cond 
        ;
          (;vla-get-color_case_12a
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W00")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_12")
            )
          )
          (;vla-get-color_case_12
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W01")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_12")
            )
          )
          (;vla-get-color_case_13
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W02")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_13")
            )
          )
          (;vla-get-color_case_14
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W03")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_14")
            )
          )
          (;vla-get-color_case_15
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W04")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_15")
            )
          )
          (;vla-get-color_case_16
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W05")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_16")
            )
          )
        ;
          (;vla-get-color_case_17
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W06")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 120)
              (princ "vla-get-color_case_17")
            )
          )
        ;
          (;vla-get-color_case_18
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W07")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
              (princ "vla-get-color_case_18")
            )
          )
        ;
          (;vla-get-color_case_19
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W08")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 20)
              (princ "vla-get-color_case_19")
            )
          )
          (;vla-get-color_case_20
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W09")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 180)
              (princ "vla-get-color_case_20")
            )
          )
          (;vla-get-color_case_21
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W10")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 200)
              (princ "vla-get-color_case_21")
            )
          )
          (;vla-get-color_case_22
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W11")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 220)
              (princ "vla-get-color_case_22")
            )
          )
          (;vla-get-color_case_23
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W12")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 240)
              (princ "vla-get-color_case_23")
            )
          )
          ;
            (;vla-get-color_case_24
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W13")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_24")
              )
            )
            (;vla-get-color_case_25
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W14")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_25")
              )
            )
            (;vla-get-color_case_26
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W15")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_26")
              )
            )
            (;vla-get-color_case_27
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W16")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_27")
              )
            )
            (;vla-get-color_case_28
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W17")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_28")
              )
            )
          ;
          (;vla-get-color_case_29
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W18")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 120)
              (princ "vla-get-color_case_29")
            )
          )
          ;
            (;vla-get-color_case_30
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W19")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_30")
              )
            )
          ;
          (;vla-get-color_case_31
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W20")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 160)
              (princ "vla-get-color_case_31")
            )
          )
          (;vla-get-color_case_32
            (and
              (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W21")
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 180)
              (princ "vla-get-color_case_32")
            )
          )
          ;
            (;vla-get-color_case_33
              (and
                (= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_"W17" ) "W22")
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 251)
                (princ "vla-get-color_case_33")
              )
            )
          ;

        )
      
      (setq ss_pre_filter_set_xx_filter_i (+ ss_pre_filter_set_xx_filter_i 1))
    )
  ;
)
(defun c:PPLINE-ITD_detect_celing_Sym_cewall () 
  ;selection_set_for_fillter_blk_name
  (if  ;pre_select_ssget_or_post_select_ssget
    (= 
      (setq ss_pre_filter_set_xx_ (ssget "x" 
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
  ; (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "A$C441E3E01")) ;METHOD 1
  (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "qerg" ) ) ;METHOD 2
  ;preloop_and_while
  (setq ss_pre_filter_set_xx_filter_i 0)
  (while (< ss_pre_filter_set_xx_filter_i (sslength ss_pre_filter_set_xx_filter_)) 
    (setq ss_pre_filter_set_xx_filter_ename_ (ssname ss_pre_filter_set_xx_filter_ 
                                                     ss_pre_filter_set_xx_filter_i
                                             )
    )
    (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_filter_ename_))

    (cond 
      ( ;vla-get-color_case_
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C00")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 251)
         (princ "vla-get-color_case_1")
       )
      )
      ( ;vla-get-color_case_1
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C01")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 70)
         (princ "vla-get-color_case_1")
       )
      )
      ( ;vla-get-color_case_2
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C02")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 90)
         (princ "vla-get-color_case_2")
       )
      )
      ( ;vla-get-color_case_3
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C03")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 20)
         (princ "vla-get-color_case_3")
       )
      )
      ( ;vla-get-color_case_4
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C04")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 130)
         (princ "vla-get-color_case_4")
       )
      )
      ( ;vla-get-color_case_5
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C05")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 20)
         (princ "vla-get-color_case_5")
       )
      )
      ( ;vla-get-color_case_5
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C05A")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 20)
         (princ "vla-get-color_case_5")
       )
      )
      ( ;vla-get-color_case_6
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C06")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 170)
         (princ "vla-get-color_case_6")
       )
      )
      ( ;vla-get-color_case_7
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C07")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 190)
         (princ "vla-get-color_case_7")
       )
      )
      ( ;vla-get-color_case_8
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C08")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 210)
         (princ "vla-get-color_case_8")
       )
      )
      ( ;vla-get-color_case_9
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C09")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 230)
         (princ "vla-get-color_case_9")
       )
      )
      ( ;vla-get-color_case_10
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C10")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 235)
         (princ "vla-get-color_case_10")
       )
      )
      ( ;vla-get-color_case_11
       (and 
         (= (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C00") "C11")
       )
       (progn 
         (vla-put-color ss_pre_filter_set_xx_obj_ 240)
         (princ "vla-get-color_case_11")
       )
      )
    )


    (setq ss_pre_filter_set_xx_filter_i (+ ss_pre_filter_set_xx_filter_i 1))
  )
  ;
)
(defun c:PPLINE-ITD_detect_bubble_Sym_asf ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
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
  (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "ROOM"))
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_filter_i 0)
    (while (< ss_pre_filter_set_xx_filter_i (sslength ss_pre_filter_set_xx_filter_))
      (setq ss_pre_filter_set_xx_filter_ename_ (ssname ss_pre_filter_set_xx_filter_ ss_pre_filter_set_xx_filter_i))
      (setq ss_pre_filter_set_xx_filter_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_filter_ename_))
      
        (cond
          (;vla-get-color_case_17
            (or 
              (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "room_name") (strcat "*" "TICKET"    "*"))   T)
              
              (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "room_name") (strcat "*" "OPERATIONS"    "*"))   T)
 
            )
            (progn
              (vla-put-color ss_pre_filter_set_xx_filter_obj_ 241)
              (setq ssbox (TA:ename+vla-getboundingbox  ss_pre_filter_set_xx_filter_obj_))
              (command "ATTSYNC" "S" ss_pre_filter_set_xx_filter_ename_ "Y" )
               
              (setq ssssset_ (ssget "C" (cadr ssbox)  (caddr ssbox)
                                            (list 
                                              (cons 0 "LEADER") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                          )
              )
              ;preloop_and_while
                (setq ssssset_i 0)
                (while (and (/= ssssset_ nil )(< ssssset_i (sslength ssssset_)))
                  (setq ssssset_ename_ (ssname ssssset_ ssssset_i))
                  (setq ssssset_obj_ (vlax-ename->vla-object ssssset_ename_))
                    (if 
                      (= (vla-get-objectname ssssset_obj_) "AcDbLeader")
                      (progn 
                        (vla-put-color ssssset_obj_ 241)
                        (vla-put-dimensionlinecolor ssssset_obj_ 241)
                        
                      )
                    )
                  (setq ssssset_i (+ ssssset_i 1))
                )
              ;
              (princ "vla-get-color_case_17")
            )
          )
        )
        
      
      (setq ss_pre_filter_set_xx_filter_i (+ ss_pre_filter_set_xx_filter_i 1))
    )
  ;
  (c:PPLINE-ITD_detect_room_num_Rsf)
)
(defun c:PPLINE-ITD_detect_bubble_Sym_bsf ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
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
  ; (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "A$C51E56039"))
  ; (setq ss_pre_filter_set_xx_filter_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "q"))
  (setq ss_pre_filter_set_xx_filter_ ss_pre_filter_set_xx_)
  ;preloop_and_while
    (setq ss_pre_filter_set_xx_filter_i 0)
    (while (< ss_pre_filter_set_xx_filter_i (sslength ss_pre_filter_set_xx_filter_))
      (setq ss_pre_filter_set_xx_filter_ename_ (ssname ss_pre_filter_set_xx_filter_ ss_pre_filter_set_xx_filter_i))
      (setq ss_pre_filter_set_xx_filter_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_filter_ename_))
      (if 
        (and 
          (= (vla-get-hasattributes ss_pre_filter_set_xx_filter_obj_) :vlax-true)
          (/= (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") nil)
        )
        (progn
          (cond
            (;vla-get-color_case_17
              (or 
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6004"    "*"))   T)
                
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6103"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6104"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6150"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6151"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6152"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6153"    "*"))   T)

                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6250"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6251"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6252"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6264"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6271"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6272"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6274"    "*"))   T)

                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6301"    "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6307"    "*"))   T)

                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6403"  "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6408"  "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6409"  "*"))   T)
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6414"  "*"))   T)

                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6500"    "*"))   T)
                
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6505"    "*"))   T)
                
                (= (wcmatch (LM:vl-getattributevalue ss_pre_filter_set_xx_filter_obj_ "NOOO") (strcat "*" "6600"    "*"))   T)
              )
              (progn
                (vla-put-color ss_pre_filter_set_xx_filter_obj_ 241)
                (setq ssbox (TA:ename+vla-getboundingbox  ss_pre_filter_set_xx_filter_obj_))
                (command "ATTSYNC" "S" ss_pre_filter_set_xx_filter_ename_ "Y" )
                
                (setq ssssset_ (ssget "C" (cadr ssbox)  (caddr ssbox)
                                              (list 
                                                (cons 0 "LEADER") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                            )
                )
                ;preloop_and_while
                  (setq ssssset_i 0)
                  (while (and (/= ssssset_ nil )(< ssssset_i (sslength ssssset_)))
                    (setq ssssset_ename_ (ssname ssssset_ ssssset_i))
                    (setq ssssset_obj_ (vlax-ename->vla-object ssssset_ename_))
                      (if 
                        (= (vla-get-objectname ssssset_obj_) "AcDbLeader")
                        (progn 
                          (vla-put-color ssssset_obj_ 241)
                          (vla-put-dimensionlinecolor ssssset_obj_ 241)
                          
                        )
                      )
                    (setq ssssset_i (+ ssssset_i 1))
                  )
                ;
                (princ "vla-get-color_case_17")
              )
            )
          )
        )
      )
        
      
      (setq ss_pre_filter_set_xx_filter_i (+ ss_pre_filter_set_xx_filter_i 1))
    )
  ;
)
(defun c:PPLINE-ITD_detect_room_num_Rsf ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
                                          (list
                                            (cons 0 "text") ;type of object
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
                                            (cons 0 "text") ;type of object
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
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (setq ss_pre_filter_set_xx_text_val_ (vla-get-textstring ss_pre_filter_set_xx_obj_))
      (if 
        (or 
          (= (wcmatch ss_pre_filter_set_xx_text_val_ (strcat "*" "6563"    "*"))   T)
          (= (wcmatch ss_pre_filter_set_xx_text_val_ (strcat "*" "6562"    "*"))   T)
        )
        (progn
          (vla-put-color ss_pre_filter_set_xx_obj_ 241)
        )
      )
  
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
    )
  ;
)
(defun c:PPLINE-ITD_REPLACE_WALL-SYM_REPWALL_ ()
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
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
    (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ (LM:effectivename (vlax-ename->vla-object (car (entsel "specify Object"))))))
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (if (= (vla-get-hasattributes ss_pre_filter_set_xx_obj_) :vlax-true)
          (progn
            (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
            (setq ss_pre_filter_set_xx_obj_rotation_ (vla-get-rotation ss_pre_filter_set_xx_obj_ ))
            (setq wallnum_ (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "W10" ))
            (TA:vla-insertblock "A$C441E3E01" ss_pre_filter_set_xx_obj_ins_ 1 ss_pre_filter_set_xx_obj_rotation_ )
            ;new_blk_entlast object
              (setq blknew_ename_ (entlast))
              (setq blknew_obj_ (vlax-ename->vla-object blknew_ename_))
              (LM:vl-setattributevalue blknew_obj_ "W17"  wallnum_)
            ;
            ;delete existing object
              (vla-delete  ss_pre_filter_set_xx_obj_ )
            ;
          )
          
          
        )
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
)
(defun c:PPLINE-ITD_REPLACE_WALL-SYM_REPC1ELL_ ()
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
                                          (list
                                            (cons 0 "ATTDEF") ;type of object
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
                                            (cons 0 "ATTDEF") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
    ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ (LM:effectivename (vlax-ename->vla-object (car (entsel "specify Object"))))))
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (if (= (wcmatch (setq wallnum_ (vla-get-tagstring ss_pre_filter_set_xx_obj_  )) "*C*") T)
          (progn
            (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
            
            (setq ss_pre_filter_set_xx_obj_rotation_ (vla-get-rotation ss_pre_filter_set_xx_obj_ ))
            (setq wallnum_ (vla-get-tagstring ss_pre_filter_set_xx_obj_  ))
            (TA:vla-insertblock "qerg" ss_pre_filter_set_xx_obj_ins_ 1 ss_pre_filter_set_xx_obj_rotation_ )
            ;new_blk_entlast object
              (setq blknew_ename_ (entlast))
              (setq blknew_obj_ (vlax-ename->vla-object blknew_ename_))
              (LM:vl-setattributevalue blknew_obj_ "C00"  wallnum_)
            ;
            ;delete existing object
              (vla-delete  ss_pre_filter_set_xx_obj_ )
            ;
          )
          
          
        )
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
)
(defun c:PPLINE-ITD_REPLACE_WALL-SYM_REPC2ELL_ ()
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "x"
                                          (list
                                            (cons 0 "insert") ;type of object
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
                                            (cons 0 "insert") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                    )
        )
      )
      (sslength ss_pre_filter_set_xx_)
    )
    (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ (LM:effectivename (vlax-ename->vla-object (car (entsel "specify Object"))))))
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (if 
          ; (= (wcmatch (setq wallnum_ (vla-get-tagstring ss_pre_filter_set_xx_obj_  )) "*C0*") T)
          (= 1 1 )
          (progn
            (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
            
            (setq ss_pre_filter_set_xx_obj_rotation_ (vla-get-rotation ss_pre_filter_set_xx_obj_ ))
            (setq wallnum_ (LM:vl-getattributevalue ss_pre_filter_set_xx_obj_ "C03" ))
            (TA:vla-insertblock "qerg" ss_pre_filter_set_xx_obj_ins_ 1 ss_pre_filter_set_xx_obj_rotation_ )
            ;new_blk_entlast object
              (setq blknew_ename_ (entlast))
              (setq blknew_obj_ (vlax-ename->vla-object blknew_ename_))
              (LM:vl-setattributevalue blknew_obj_ "C00"  wallnum_)
            ;
            ;delete existing object
              (vla-delete  ss_pre_filter_set_xx_obj_ )
            ;
          )
          
          
        )
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
)


(defun c:240 ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "LEADER,INSERT") ;type of object
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
                                            (cons 0 "LEADER,INSERT") ;type of object
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
    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
      (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
      (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
      (if (= (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbLeader") 
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 241)
          (vla-put-dimensionlinecolor ss_pre_filter_set_xx_obj_ 241)
          
        )
      )
      (if (= (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbBlockReference") 
        (progn 
          (vla-put-color ss_pre_filter_set_xx_obj_ 241)
          
          
        )
      )
      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      
    )
  ;
)



(defun C:selection_return_axis_Z_SSreaz_ ()
  ;return Z 0
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              ; (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                              (cons 410 (getvar "CTAB"))
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget  
                                            (list
                                              ; (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                              (cons 410 (getvar "CTAB"))
                                            )
                                      )
          )
        )
        
      )
    ;
    ;preloop_and_while
      (textscr)
      ; (getstring "\nHit \nHit \nHit [ENTER] to continue...")
      (setq ss_pre_filter_set_xx_i 0)
      (while (and 
                (/= ss_pre_filter_set_xx_ nil)
                (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              )
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        ;main_idea_
          (cond
            (;type_name_object_case_1
              (and
                  (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadLine" "*")) T)
                  (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbLine" ) T)
              )
              (progn
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
                (vla-put-startpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_startpt_) (cadr ss_pre_filter_set_xx_obj_startpt_) 0))
                (vla-put-endpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_endpt_) (cadr ss_pre_filter_set_xx_obj_endpt_) 0))
                (princ "type_name_object_case_1")
              )
            )
            (;type_name_object_case_2
              (and
                  (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadLWPolyline" "*")) T)
                  (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbPolyline") T)
              )
              (progn
                (vla-put-elevation ss_pre_filter_set_xx_obj_ 0)
                (princ "type_name_object_case_2")
              )
            )
            (;type_name_object_case_3
              (and
                  (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadBlockReference " "*")) T)
                  (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbBlockReference") T)
              )
              (progn
                (setq ss_pre_filter_set_xx_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_pre_filter_set_xx_obj_))))
                (vla-put-insertionpoint ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_ins_) (cadr ss_pre_filter_set_xx_obj_ins_) 0) )
                (princ "type_name_object_case_3")
              )
            )
            (;type_name_object_case_4
              (and
                  (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadHatch" "*")) T)
                  (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbHatch") T)
              )
              (progn
                (vla-put-elevation ss_pre_filter_set_xx_obj_  0)
                (princ "type_name_object_case_4")
              )
            )
            (;type_name_object_case_5
              (and
                  (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (= (wcmatch (vl-princ-to-string ss_pre_filter_set_xx_obj_) (strcat "*" "IAcadArc" "*")) T)
                  (= (wcmatch (vla-get-objectname ss_pre_filter_set_xx_obj_) "AcDbArc") T)
              )
              (progn
                (setq ss_pre_filter_set_xx_obj_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_pre_filter_set_xx_obj_) ) ) )
                (vla-put-center ss_pre_filter_set_xx_obj_ (vlax-3d-point (car ss_pre_filter_set_xx_obj_center_) (cadr ss_pre_filter_set_xx_obj_center_) 0 ) )
                (princ "type_name_object_case_5")
              )
            )
          )
        ;
        (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))
        (if (= (substr (rtos (/ (float ss_pre_filter_set_xx_i) 2 ) 2 1) 3 1) "0")
          (progn
            (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
          )
          (princ (strcat "                                tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        )
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  ;
)


(defun c:foldPLine_FPLL ()
  (setq pl_ename_ (car (entsel "specify Object 1")))
  (setq pl_obj_ (vlax-ename->vla-object pl_ename_))
  (setq pl_obj_length_ (vla-get-length pl_obj_))
  (setq input_pt (getpoint "specify point"))
  
  (setq pl2_ename_ (car (entsel "specify Object 2")))
  (setq pl2_obj_ (vlax-ename->vla-object pl2_ename_))
  (setq pl2_obj_length_ (vla-get-length pl2_obj_))
  

  (setq pt_3 (polar input_pt 0 pl_obj_length_))
  (setq pt_4 (polar pt_3 1.57  pl2_obj_length_))
  (command "rectangle" input_pt  pt_4)
  
  
)

