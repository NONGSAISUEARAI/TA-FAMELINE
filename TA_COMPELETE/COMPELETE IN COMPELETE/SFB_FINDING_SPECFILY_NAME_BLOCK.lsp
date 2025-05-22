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
; (defun c:JUST_EXAMPLE ()
  ;   (setq x (getint "ป้อนค่า x: "))
  ;   (setq y (getint "ป้อนค่า y: "))

  ;   (if (and (= x 10) (= y 10))
  ;       (progn
  ;         (princ "x และ y คู่ค่าเท่ากับ 10")
  ;         ; ทำอะไรสักอย่างเมื่อ x และ y คู่ค่าเท่ากับ 10
  ;         )
  ;       (princ "x และ y ไม่คู่ค่าเท่ากับ 10")
  ;       ; ทำอะไรสักอย่างเมื่อ x และ y ไม่คู่ค่าเท่ากับ 10
  ;   )
; )
;HEAD_OF_CUSTOM_FUNC._BY_LEE_MACDONEL
(defun LM:effectivename ( obj )
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
)
;BOT_OF_CUSTOM_FUNC._BY_LEE_MACDONEL

;START_POINT_OF_FINDING_SPECFILY_NAME_BLOCK
(defun c:SFB_FINDING_SPECFILY_NAME_BLOCK ()
  ;selection_set
    (if 
      (= (setq my-selection-set (ssget "_I" 
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
        (setq my-selection-set (ssget 
                            (list 
                              (cons 0 "INSERT") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
        )
      )
      (princ my-selection-set)
     )
  ;

  ; (setq ef_name "000-GRID_LINE_DYN")
  ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
  
  (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
  (setq select_name_obj (vlax-ename->vla-object select_name))
  (setq ef_name (LM:effectivename select_name_obj))

  (setq total_ssget (sslength my-selection-set))
  (setq ie 0)
  (setq ename-list '())

  (while 
    (< ie total_ssget)
    (setq blk_obj (ssname my-selection-set ie))
    (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
    (setq ss (LM:effectivename blk_obj_Set))

      (if (= ef_name ss)
        (progn
          (setq ename-list (cons blk_obj ename-list)) 
        )
        (princ "\n")

      )
    (setq ie (+ ie 1))
  )
  (princ "\n")
  (princ (setq total_ename-list (length ename-list)))
  (setq ff (ssadd))
    (foreach ename ename-list
      (ssadd ename ff)
    )
  (command "_pselect" ff "")
  (command "regen")
)
(defun c:SFVB1_FINDING_SPECFILY_STRNAME_ALLBLOCK ()
  ; ;selection_set
  ;   (if 
  ;     (= (setq my-selection-set (ssget "x"
  ;                                       (list 
  ;                                         (cons 0 "INSERT") ;type of object
  ;                                         ; (cons 8 "000 - GRID")   ;kind of layer
  ;                                         ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                                         ; (cons 62 1)           ;kind of color call sign with color code index
  ;                                       )
  ;                                )
  ;          )
  ;       nil
  ;     )
  ;     (progn
  ;       (setq my-selection-set (ssget "X"
  ;                           (list 
  ;                             (cons 0 "INSERT") ;type of object
  ;                             ; (cons 8 "000 - GRID")   ;kind of layer
  ;                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                             ; (cons 62 1)           ;kind of color call sign with color code index
  ;                           )
  ;                         )
  ;       )
  ;     )
  ;     (princ my-selection-set)
  ;     (sslength my-selection-set)
  ;    )
  ; ;
  (setq my-selection-set (ssget "X"
                      (list 
                        (cons 0 "INSERT") ;type of object
                        ; (cons 8 "000 - GRID")   ;kind of layer
                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                        ; (cons 62 1)           ;kind of color call sign with color code index
                      )
                    )
  )
  ; (setq ef_name "000-GRID_LINE_DYN")
  ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
  
  ; (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
  ; (setq select_name_obj (vlax-ename->vla-object select_name))
  ; (setq ef_name (LM:effectivename select_name_obj))
  
  ;TA:getstring_and_keep_previous
    (setq stringname_value_ (strcase "*air supply*")) ;::::method 2 input by user
    ; (setq stringname_value_ (strcase "*multi*")) ;::::method 2 input by user
  ;
  

  (setq total_ssget (sslength my-selection-set))
  (setq ie 0)
  (setq ename-list '())

  (while 
    (< ie total_ssget)
    (setq blk_obj (ssname my-selection-set ie))
    (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
    ; (setq ef_name (LM:effectivename select_name_obj))
    (setq ss (or 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) stringname_value_)
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*air supply*")) ;if more +2 variable it s mean u are using multiname 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*luminaire*")) ;if more +2 variable it s mean u are using multiname 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*Horizontal with Gap*")) ;if more +2 variable it s mean u are using multiname 
               (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*System Panel - DPA_Metal Ceiling Rectangular Panel*")) ;if more +2 variable it s mean u are using multiname 
             )
    )

      (if (= ss T)
        (progn
          (setq ename-list (cons blk_obj ename-list)) 
        )
        (princ "\n")

      )
    (setq ie (+ ie 1))
  )
  (princ "\n")
  (princ (setq total_ename-list (length ename-list)))
  (setq ff (ssadd))
    (foreach ename ename-list
      (ssadd ename ff)
    )
  (command "_pselect" ff "")
  (command "regen")
)
(defun c:SFVB2_FINDING_SPECFILY_STRNAME_ALLBLOCK ()
  ; ;selection_set
  ;   (if 
  ;     (= (setq my-selection-set (ssget "x"
  ;                                       (list 
  ;                                         (cons 0 "INSERT") ;type of object
  ;                                         ; (cons 8 "000 - GRID")   ;kind of layer
  ;                                         ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                                         ; (cons 62 1)           ;kind of color call sign with color code index
  ;                                       )
  ;                                )
  ;          )
  ;       nil
  ;     )
  ;     (progn
  ;       (setq my-selection-set (ssget "X"
  ;                           (list 
  ;                             (cons 0 "INSERT") ;type of object
  ;                             ; (cons 8 "000 - GRID")   ;kind of layer
  ;                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                             ; (cons 62 1)           ;kind of color call sign with color code index
  ;                           )
  ;                         )
  ;       )
  ;     )
  ;     (princ my-selection-set)
  ;     (sslength my-selection-set)
  ;    )
  ; ;
  (setq my-selection-set (ssget "X"
                      (list 
                        (cons 0 "INSERT") ;type of object
                        ; (cons 8 "000 - GRID")   ;kind of layer
                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                        ; (cons 62 1)           ;kind of color call sign with color code index
                      )
                    )
  )
  ; (setq ef_name "000-GRID_LINE_DYN")
  ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
  
  ; (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
  ; (setq select_name_obj (vlax-ename->vla-object select_name))
  ; (setq ef_name (LM:effectivename select_name_obj))
  
  ;TA:getstring_and_keep_previous
    (setq stringname_value_ (strcase "*air supply*")) ;::::method 2 input by user
    ; (setq stringname_value_ (strcase "*multi*")) ;::::method 2 input by user
  ;
  

  (setq total_ssget (sslength my-selection-set))
  (setq ie 0)
  (setq ename-list '())

  (while 
    (< ie total_ssget)
    (setq blk_obj (ssname my-selection-set ie))
    (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
    ; (setq ef_name (LM:effectivename select_name_obj))
    (setq ss (or 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) stringname_value_)
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*air supply*")) ;if more +2 variable it s mean u are using multiname 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*luminaire*")) ;if more +2 variable it s mean u are using multiname 
              ;  (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*Horizontal with Gap*")) ;if more +2 variable it s mean u are using multiname 
               (wcmatch (strcase (LM:effectivename blk_obj_Set)) (strcase "*System Panel - DPA-AR_Metal*")) ;if more +2 variable it s mean u are using multiname 
             )
    )

      (if (= ss T)
        (progn
          (setq ename-list (cons blk_obj ename-list)) 
        )
        (princ "\n")

      )
    (setq ie (+ ie 1))
  )
  (princ "\n")
  (princ (setq total_ename-list (length ename-list)))
  (setq ff (ssadd))
    (foreach ename ename-list
      (ssadd ename ff)
    )
  (command "_pselect" ff "")
  (command "regen")
)

(defun c:SFO_FINDING_SPECFILY_OBJECT ()
  (setq my-selection-set (ssget 
                (list 
                  ; (cons 0 "INSERT")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  ; (setq ef_name "000-GRID_LINE_DYN")
  ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
  
  (setq select_name (car (entsel "select_Object"))) ;::::method 2
  (setq select_get (entget select_name))
  (setq select_get_as0 (cdr (assoc 0 select_get)))
  (setq select_name_obj (vlax-ename->vla-object select_name))
  (setq select_name_obj_layer (vla-get-layer select_name_obj))
  

  (setq total_ssget (sslength my-selection-set))
  (setq ie 0)
  (setq ename-list '())

  (while 
    (< ie total_ssget)
    (setq blk_obj (ssname my-selection-set ie))
    (setq blk_obj_get (entget blk_obj))
    (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
    (setq blk_obj_layer (vla-get-layer blk_obj_Set))
    (setq ss (cdr (assoc 0 blk_obj_get)))

      (if (= ss "line")
        (progn
          (setq ename-list (cons blk_obj ename-list)) 
        )
        (princ "\n")

      )
    (setq ie (+ ie 1))
  )
  (princ "\n")
  (princ (setq total_ename-list (length ename-list)))
  (setq ff (ssadd))
    (foreach ename ename-list
      (ssadd ename ff)
    )
  (command "_pselect" ff "")
)
;END_POINT_OF_FINDING_SPECFILY_NAME_BLOCK


(defun c:tt1 () 

  (setq my-selection-set (ssget 
                           (list 
                             (cons 0 "LWpolyline") ;type of object
                             (cons 8 "0,1") ;kind of layer
                             ; (cons 2 "SSSS")       ;kind of nameblock
                             ; (cons 62 1)           ;kind of color call sign with color code index
                           )
                         )
  )
  (command "pselect" my-selection-set "")
)



(defun c:q2 ()
   
    ; (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
    (setq scl 0 )
    
    ; ;selection_set
    ;   (if 
    ;     (= (setq ss_fillter_set_ (ssget "_I" 
    ;                                       (list 
    ;                                         (cons 0 "INSERT") ;type of object
    ;                                         ; (cons 8 "000 - GRID")   ;kind of layer
    ;                                         ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
    ;                                         ; (cons 62 1)           ;kind of color call sign with color code index
    ;                                       )
    ;                               )
    ;         )
    ;       nil
    ;     )
    ;     (progn
    ;       (setq ss_fillter_set_ (ssget 
    ;                           (list 
    ;                             (cons 0 "INSERT") ;type of object
    ;                             ; (cons 8 "000 - GRID")   ;kind of layer
    ;                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
    ;                             ; (cons 62 1)           ;kind of color call sign with color code index
    ;                           )
    ;                         )
    ;       )
    ;     )
    ;     (princ ss_fillter_set_)
    ;   )
    ; ;
    (setq ref_ (car (entsel)))
    (command "zoom" "o" ref_ "")
    (setq ref_obj_ (vlax-ename->vla-object ref_))
    (setq ref_obj_bo_ (TA:ename+vla-getboundingbox ref_obj_))
    (setq ss_fillter_set_ (ssget "_c" (nth 1 ref_obj_bo_ ) (nth 2 ref_obj_bo_ ) '((0 . "insert"))))
    (setq Selection_list_ (TA:EF_Filter_ss_set_  ss_fillter_set_ "001 - ALU.ANGLE DYNAMIC - VIEW 1"))
    (setq ss (TA:Prop_Filter_ss_set_ Selection_list_ "rotation" (deg-to-rad scl)))
    (command "zoom" "p")
    (command "pselect" ss "")
  
)
  
(defun c:Mxy_match_ins_xyz ()
  (setq ref_target_ename_ (car (entsel)))
  (setq ref_target_obj_ (vlax-ename->vla-object ref_target_ename_))
  (setq ref_target_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ref_target_obj_))))
  
  (setq new_target_ename_ (car (entsel)))
  (setq new_target_obj_ (vlax-ename->vla-object new_target_ename_))
  (setq new_target_obj_ins_ (vla-put-insertionpoint new_target_obj_ (vlax-3d-point  ref_target_obj_ins_)))
  
  
)


(defun c:select_line_sfli ()
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)
(defun c:select_line_sfpl ()
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "lwpolyline")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)
(defun c:select_line_sfin ()
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "insert")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)
(defun c:select_dims_sfd_ ()
  (setq my-selection-set_ (ssget 
                (list 
                  (cons 0 "dimension")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect"  my-selection-set_ "") 
)
(defun c:select_by_layer__SBL ()
  (setq layer_ename_ (car (entsel "specify layer_ename_ Object")))
  (setq layer_obj_ (vlax-ename->vla-object layer_ename_))
  (setq layer_obj_layer_name_ (vla-get-layer layer_obj_))
  
  (setq my-selection-set (ssget 
                (list 
                  ; (cons 0 "insert,dimension")       ;type of object
                  (cons 8 layer_obj_layer_name_)   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)
(defun c:select_grid_sfg ()
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "insert,dimension")       ;type of object
                  (cons 8 "000 - GRID,000 - D I M")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)

(defun c:change_text_size_chtx_ ()
  ;selection_set
    (if 
      (= (setq my-selection-set_ (ssget "_I" 
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
        (setq my-selection-set_ (ssget 
                            (list 
                              (cons 0 "text") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
        )
      )
      (princ my-selection-set_)
    )
  ;
  ;preloop_and_while_main_loop
    (setq x nil)
    (setq symbol_ nil)
    (while (and  (/= x "CANCEL"))
      ;user-input for specify value and *CANCEL* method 
        (setq result (vl-catch-all-apply 
                       (function 
                         (lambda () 
                           (setq symbol_ (cond 
                                           ((getint 
                                              (strcat "\n plus = 1 \nno minus = 0  \n<" 
                                                      (rtos 
                                                        (setq symbol_ (cond 
                                                                        (symbol_)
                                                                        (0.0)
                                                                      )
                                                        )
                                                      )
                                                      "> : "
                                              )
                                            )
                                           )
                                           (symbol_)
                                         )
                           )
                         )
                       ) ; ใช้ lambda เพื่อจับคำสั่ง getint
                     )
        )
        (if (vl-catch-all-error-p result)
          (if (wcmatch (strcase (vl-catch-all-error-message result)) "*CANCEL*")
            (setq x "CANCEL")  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
          )
            
        )
      ; 
      ;preloop_and_while
      (if (= symbol_ 1)
        (progn
          (setq sym_num_ +)
        )
        (setq sym_num_ -)
      )
      (if
          (and 
            (/= x "CANCEL")
          )
          (progn
            (setq my-selection-set_i 0)
          
            (while (< my-selection-set_i (sslength my-selection-set_))
              (setq my-selection-set_ename_ (ssname my-selection-set_ my-selection-set_i))
              (setq my-selection-set_obj_ (vlax-ename->vla-object my-selection-set_ename_))
              (setq my-selection-set_obj_get_txH (vla-get-height my-selection-set_obj_))
              (setq my-selection-set_obj_put_txH (vla-put-height my-selection-set_obj_ (sym_num_ my-selection-set_obj_get_txH 0.001)))
              (setq my-selection-set_i (+ my-selection-set_i 1))
            )
          )
          (princ "s")
      )
      ;
    )
)




(defun c:temp_+select_by_layer_SBL1 ()
  (setq my-selection-set (ssget 
                (list 
                  ; (cons 0 "insert,dimension")       ;type of object
                  (cons 8 "A-GLAZ-CURT")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (command "pselect" my-selection-set "")
)
(defun c:temp_+select_by_layer_SBLL ()
  ;user_input_and_get_data_
    (setq specify_layer_ename_ (car (entsel "specify Object for layer")))
    (setq specify_layer_obj_ (vlax-ename->vla-object specify_layer_ename_))
    ;getdata_
      (setq speccify_layer_obj_layer_ (vla-get-layer specify_layer_obj_) )
    ;
  ;
  
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "HATCH")       ;type of object
                  (cons 8 speccify_layer_obj_layer_)   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (command "pselect" my-selection-set "")
)
(defun c:temp_+select_by_layer_SBLL ()
  ;user_input_and_get_data_
    (setq specify_layer_ename_ (car (entsel "specify Object for layer")))
    (setq specify_layer_obj_ (vlax-ename->vla-object specify_layer_ename_))
    ;getdata_
      (setq speccify_layer_obj_layer_ (vla-get-layer specify_layer_obj_) )
    ;
  ;
  
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "line,hatch")       ;type of object
                  (cons 8 "A-ANNO-NOTE")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (command "pselect" my-selection-set "")
)
(defun c:temp_+select_by_layer_SBLL ()
  ;user_input_and_get_data_
    (setq specify_layer_ename_ (car (entsel "specify Object for layer")))
    (setq specify_layer_obj_ (vlax-ename->vla-object specify_layer_ename_))
    ;getdata_
      (setq speccify_layer_obj_layer_ (vla-get-layer specify_layer_obj_) )
    ;
  ;
  
  (setq my-selection-set (ssget 
                (list 
                  ; (cons 0 "line,hatch")       ;type of object
                  ; (cons 8 "A-ANNO-NOTE")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  (cons 62 3)           ;kind of color call sign with color code index
                )
              )
  )

  (command "pselect" my-selection-set "")
)



(defun c:select_all_by_layer_azx_ ()
  
  (setq selection_ename_ (car (entsel "specify Object")))
  (setq selection_obj_ (vlax-ename->vla-object selection_ename_))
  (setq selectiion_obj_layer_ (vla-get-layer selection_obj_ ))
  (setq my-selection-set (ssget "X"
                (list 
                  ; (cons 0 "mtext,line,hatch,insert")       ;type of object
                  (cons 8 selectiion_obj_layer_)   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 256)           ;kind of color call sign with color code index
                )
              )
  )
  ; (setq my-selection-set (ssget 
  ;               (list 
  ;                 (cons 0 "insert")       ;type of object
  ;                 (cons 8 "A-GLAZ-CURT")   ;kind of layer 
  ;                 ; (cons 2 "SSSS")       ;kind of nameblock
  ;                 ; (cons 62 256)           ;kind of color call sign with color code index
  ;               )
  ;             )
  ; )

  (command "pselect" my-selection-set "")
)

(defun c:ins-area_box_as1_ ()
  (setq getstpoint_ (getpoint "specify point"))
  (setq getdist_y (getpoint "specify point for height"))
  (setq getdist_x (getpoint "specify point for width") )
  
  
  (setq getdist_x_result 
    (distance 
      getstpoint_ 
      (list 
        (car getdist_x)
        (cadr getstpoint_)
        0
      )
    )
  )
  (setq getdist_y_result 
    (distance 
      getstpoint_ 
      (list 
        (car getstpoint_)
        (cadr getdist_y)
        0
      )
    )
  )
  (if 
    (= (rad-to-deg (angle getstpoint_ getdist_y) ) 180)
    (progn
      (setq rotation_ 90)
      (setq getdist_x_result (distance 
                               getstpoint_
                               (list 
                                 (car getstpoint_)
                                 (cadr getdist_x)
                                 0
                               )
                             )
      )
      (setq getdist_y_result (distance 
                              getstpoint_
                              (list 
                                (car getdist_y)
                                (cadr getstpoint_)
                                0
                              )
                            )
      )
    )
    (setq rotation_ 0)
  )
  (command "insert" "000 - Box_for_area" getstpoint_ 1 rotation_ )
  ;new_blk_entlast object
    (setq new_ename (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename))
    (LM:setdynpropvalue new_obj_ "x" getdist_x_result)
    (LM:setdynpropvalue new_obj_ "y" (* 2 getdist_y_result) )
    (LM:vl-attribute new_obj_ "y" (* 2 getdist_y_result) )
          
  ;
  
  
)

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (mapcar 'min (vlax-safearray->list llp) (cond (ls1) ((vlax-safearray->list llp))))
                  ls2 (mapcar 'max (vlax-safearray->list urp) (cond (ls2) ((vlax-safearray->list urp))))
            )
        )
    )
    (if (and ls1 ls2) (list ls1 ls2))
)

(defun c:ssbx_ ( / box obj sel spc )
    (if (and (setq sel (ssget))
             (setq box (LM:ssboundingbox sel))
        )
        (progn
            (setq spc
                (vlax-get-property (vla-get-activedocument (vlax-get-acad-object))
                    (if (= 1 (getvar 'cvport))
                        'paperspace
                        'modelspace
                    )
                )
            )
            (if (equal 0.0 (apply '- (mapcar 'caddr box)) 1e-6)
                (progn
                    (setq obj
                        (vlax-invoke spc 'addlightweightpolyline
                            (apply 'append
                                (mapcar '(lambda ( x ) (mapcar '(lambda ( y ) ((eval y) box)) x))
                                   '(
                                        (caar   cadar)
                                        (caadr  cadar)
                                        (caadr cadadr)
                                        (caar  cadadr)
                                    )
                                )
                            )
                        )
                    )
                    (vla-put-closed obj :vlax-true)
                    (vla-put-elevation obj (caddar box))
                )
                (apply 'vlax-invoke 
                    (vl-list* spc 'addbox
                        (apply 'mapcar (cons '(lambda ( a b ) (/ (+ a b) 2.0)) box))
                        (apply 'mapcar (cons '- (reverse box)))
                    )
                )
            )
        )
    )
    (princ)
  
    ;TA if 
      (setq delete_set_ (cond ( (getint (strcat "\nspecify fllet_rec_<" (rtos (setq delete_set_ (cond (delete_set_) (1.0) ) ) ) "> : " ) ) ) (delete_set_) ) )
      (if (= delete_set_ 1)
        (progn
          
          (command "erase"  sel "" )
        )
      )
  
    ;
)



