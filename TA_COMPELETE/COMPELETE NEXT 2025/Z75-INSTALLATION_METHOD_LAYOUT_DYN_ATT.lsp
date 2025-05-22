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
      ; (setq sss_ (ssget))
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
    (defun LM:GetTrueContent ( RegExp entity *dtextstring *mtextstring / _Replace _AllowsFormatting _GetTextString )

      (defun _Replace ( new old string )
        (vlax-put-property RegExp 'pattern old) (vlax-invoke RegExp 'replace string new)
      )

      (defun _AllowsFormatting ( entity / object )    
        (or (wcmatch (cdr (assoc 0 (entget entity))) "MTEXT,MULTILEADER")      
          (and
            (eq "ATTRIB" (cdr (assoc 0 (entget entity))))
            (vlax-property-available-p (setq object (vlax-ename->vla-object entity)) 'MTextAttribute)
            (eq :vlax-true (vla-get-MTextAttribute object))
          )
        )
      )

      (defun _GetTextString ( entity )
        (
          (lambda ( entity / _type elist )
            (cond
              ( (wcmatch (setq _type (cdr (assoc 0 (setq elist (entget entity))))) "TEXT,*DIMENSION")
              
                (cdr (assoc 1 (reverse elist)))
              )
              ( (eq "MULTILEADER" _type)

                (cdr (assoc 304 elist))
              )
              ( (wcmatch _type "ATTRIB,MTEXT")

                (
                  (lambda ( string )
                    (mapcar
                      (function
                        (lambda ( pair )
                          (if (member (car pair) '(1 3))
                            (setq string (strcat string (cdr pair)))
                          )
                        )
                      )
                      elist
                    )
                    string
                  )
                  ""
                )
              )
            )
          )
          (if (eq 'VLA-OBJECT (type entity))
            (vlax-vla-object->ename entity)
            entity
          )
        )
      )

      (
        (lambda ( string )
          (if string
            (progn
              (mapcar
                (function
                  (lambda ( x ) (vlax-put-property RegExp (car x) (cdr x)))
                )
                (list (cons 'global actrue) (cons 'ignorecase acfalse) (cons 'multiline actrue))
              )
              (if (_AllowsFormatting entity)
                (mapcar
                  (function
                    (lambda ( x ) (setq string (_Replace (car x) (cdr x) string)))
                  )
                '(
                    ("Ð"       . "\\\\\\\\")
                    (" "       . "\\\\P|\\n|\\t")
                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                    ("$1"      . "[\\\\]({)|{")
                  )
                )
                (setq string (_Replace "" "%%[OoUu]" (_Replace "Ð" "\\\\" string)))
              )
              (set *mtextstring (_Replace "\\\\" "Ð" (_Replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" string)))
              (set *dtextstring (_Replace "\\"   "Ð" string))
            )
          )
        )
        (_GetTextString entity)
      )
      ; nil
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
        (if (= (getvar "CTAB") "Model")
          (progn
            (setq modelSpace (vla-get-ModelSpace doc))
          )
          (setq modelSpace (vla-get-PaperSpace  doc))
        )
        
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
    (defun c:dynamic_rewidth_array_ ()
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
            (setq blk_raw_efname_ (LM:StringSubst "" "COMBINE" blk_raw_efname_))
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
  ;mtext_FUNC_
    (defun c:mtext_renumber_mtre_ ()
      (setq old_numtext (getstring "specify old_numtext"))
      (setq new_numtext (getstring "specify new_numtext"))
      (setq mtext_ename_ (car (entsel "specify Object")))
      (setq mtext_obj_ (vlax-ename->vla-object mtext_ename_))

      (setq mtext_obj_textstring_ (vla-get-textstring mtext_obj_))
      (setq old (strcat ";" old_numtext "."))
      (setq new (strcat ";" new_numtext "."))

      (vla-put-textstring mtext_obj_ (LM:StringSubst  new old mtext_obj_textstring_ ))
    )
  ;

;000 - DYNAMIC_SYMBOL_FAMELINE_MATTERIAL_BOX_AND_BUBBLE
  (defun c:MATTERIAL_BOX_INCREMENT_NUMBER_MATINC1 ()

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
;
;000 - DYNAMIC_SYMBOL_FAMELINE_MATTERIAL_BOX_AND_CONTENT
  (defun c:MATTERIAL_BOX_INCREMENT_NUMBER_MATINC2 ()
    ;user_input_
      (setq increment_number_ (cond ( (getint (strcat "\nSpecify increment_number_   <" (rtos (setq increment_number_ (cond (increment_number_) (1.0) ) ) ) "> : " ) ) ) (increment_number_) ) )
    ;
    ;user_att_
      (setq att_name_1 "NO_VALUE_1")
      (setq axis_sort "Y" )
      (setq efname_blk "000 - DYNAMIC_SYMBOL_FAMELINE_MATTERIAL_BOX_AND_CONTENT" )

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

        
        
        (setq increment_number_ (+ increment_number_ 1))
        (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
      )
    ;
  )
;
;000 - DYNAMIC_LAYOUT_FAMELINE_INSTALLATION_METHOD_A4_PROTAIT
  (defun c:INSTALLATION_INCREMENT_FOOTER_FTINC_ ()
    ;user_input_
      (setq increment_number_ (cond ( (getint (strcat "\nSpecify increment_number_   <" (rtos (setq increment_number_ (cond (increment_number_) (1.0) ) ) ) "> : " ) ) ) (increment_number_) ) )
    ;
    ;user_att_
      (setq att_name_1 "FOOTER_NO_VAL_")
      (setq axis_sort "X" )
      (setq efname_blk "000 - DYNAMIC_LAYOUT_FAMELINE_INSTALLATION_METHOD_A4_PROTAIT" )
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

        
        
        (setq increment_number_ (+ increment_number_ 1))
        (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
      )
    ;
  )
;

(defun c:INSTALLATION_PLOT_INPLOT ()
  (setvar "osmode" 0)
  ;user_input_
    (setq att_name_1 "FOOTER_NO_VAL_")
    (setq axis_sort "X" )
    (setq efname_blk "000 - DYNAMIC_LAYOUT_FAMELINE_INSTALLATION_METHOD_A4_PROTAIT" )
    (setq paper_size "ISO_full_bleed_A4_(210.00_x_297.00_MM)")
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
  ;preloop_and_while_data_for_plot
    (setq ss_pre_filter_set_xx_sorted_i 0)
    (setq subfix_page_i 100)
    (while (< ss_pre_filter_set_xx_sorted_i (length ss_pre_filter_set_xx_sorted_))
      (setq ss_pre_filter_set_xx_sorted_ename_ (car (nth  ss_pre_filter_set_xx_sorted_i ss_pre_filter_set_xx_sorted_)))
      (setq ss_pre_filter_set_xx_sorted_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_sorted_ename_))
      
      ;data_process_
        
        (setq ss_pre_filter_set_xx_sorted_obj_bol_ (cdr (TA:ename+vla-getboundingbox ss_pre_filter_set_xx_sorted_obj_ ) ) ) ;get_boundingbox
        (setq file (strcat (getvar 'DWGPREFIX)  ;namefile
                           (substr (setq dwg (getvar 'DWGNAME)) 
                                   1
                                   (- (strlen dwg) 4)
                           )
                           "_"
                           (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "FOOTER_TEXT_VAL_" )
                           "_"
                           (LM:vl-getattributevalue ss_pre_filter_set_xx_sorted_obj_ "FOOTER_NO_VAL_" )
                          ;  (itoa (setq subfix_page_i (1+ subfix_page_i))) ;ไม่ใช่วิธีนี้ ให้อิงกับ FOOTER_NO_VAL_
                           ".pdf"
                   )
        )
        (if (findfile file) 
          (vl-file-delete file)
        )
      ;
      ;
        (command "-plot" 
                  "Yes" ;Detailed Plot Configuration?
                  (getvar "ctab") ;Model or Layout
                  "DWG To PDF.pc3" ;Printer Name
                  ; "ISO_full_bleed_A3_(420.00_x_297.00_mm)" ;Paper Size
                  paper_size ;Paper Size
                  "m" ;Paper Units Millimeters
                  "portrait" ;Orientation
                  "No" ;Plot Upside Down
                  "Window" ;Plot Area
                  (car ss_pre_filter_set_xx_sorted_obj_bol_) ;vla-getboundingbox
                  (cadr ss_pre_filter_set_xx_sorted_obj_bol_) ;vla-getboundingbox
                  "Fit" ;Plot Scale	"Fit"
                  "Center" ;Plot Offset
                  "yes" ;Use Plot Style?
                  "TA3-INSTALLATION.ctb" ;Plot Style Name
                  "Yes" ;Plot Lineweights?
                  "" ;Enter shade plot setting ["As displayed"/"Wireframe"/"Hidden"/"Visual styles"/"Rendered"]
                  "no" ;Plot paper space first
                  "no" ;Hide paperspace objects
                  file ; Name file and Location
                  "n"
                  "y"
        )
      ;
      
      
      
      (setq ss_pre_filter_set_xx_sorted_i (+ ss_pre_filter_set_xx_sorted_i 1))
    )
  ;
  (setvar "osmode" 1215)
  
  
  
)

(defun c:TNM ()
  (setq prefix_text_ "000 - ASSCESSORIES_FAMELINE_")
  (setq subfix_text_ "_ISOMATRIC_VIEW")
  (setq Vtext_ename_ (car (entsel "specify Object")))
  (setq Vtext_obj_ (vlax-ename->vla-object Vtext_ename_))
  (setq Vtext_obj_1 (vla-get-textstring Vtext_obj_))
  (setq Vtext_obj_2 (LM:StringSubst "_" " " Vtext_obj_1))
  (setq Vtext_obj_3 (LM:StringSubst "_" "__" Vtext_obj_2))
  (setq Vtext_obj_4 (LM:StringSubst "mm." "MM." Vtext_obj_3))
  (setq Vtext_obj_5 (LM:StringSubst "x" "X" Vtext_obj_4))
  
  (setq sum_text_ (strcat prefix_text_ Vtext_obj_5 subfix_text_))
  (create-text-vla 
    (vlax-3d-point (getpoint)) 
    sum_text_
    2.4
    0
  )
)

(defun c:Lobj_ ()
  
  (command "pselect" (entlast)  "" )
)
  
(defun c:re_boudary_viewport_revp_ ()
  (setq rectangle_blk_ename_ (car (entsel "specify rectangle_blk_ename_ Object")))
  (setq rectangle_blk_obj_ (vlax-ename->vla-object rectangle_blk_ename_))
  (if (= (vla-get-objectname rectangle_blk_obj_) "AcDbBlockReference")
    (progn
      (setq rectangle_blk_obj_getboundingbox (vla-getboundingbox rectangle_blk_obj_ 'min_ 'max_)) 
      (setq rectangle_blk_min_ (vlax-safearray->list min_)) 
      (setq rectangle_blk_max_ (vlax-safearray->list max_))
      (command "rectangle" rectangle_blk_min_ rectangle_blk_max_ )
      (setq new_entlast_ename_ (entlast))
      (setq new_entlast_ename_center_ (LM:PolyCentroid new_entlast_ename_))
      (vla-delete (vlax-ename->vla-object new_entlast_ename_))
    )
  )
  (setq vp_ename_ (car (entsel "specify rectangle_blk_ename_ Object")))
  (entget vp_ename_)
  (setq vp_obj_ (vlax-ename->vla-object vp_ename_))
  (if (= (vla-get-objectname vp_obj_) "AcDbViewport")
    (progn
      (vlax-dump-object vp_obj_)
      (vlax-safearray->list (vlax-variant-value (vla-get-center vp_obj_ )))
      (vla-put-center vp_obj_ (vlax-3d-point new_entlast_ename_center_) )
      ; (vla-put-center vp_obj_ (vlax-3d-point new_entlast_ename_center_) )
      (vla-put-height vp_obj_ (float 137.75325984) )
      (vla-put-width vp_obj_ (float 169.90503743) )
      
      (vla-put-customscale vp_obj_ 0.025  )
      
    )
  )
)
(defun c:temp_move_dmz ()
  ;selection_set_for_fillter_blk_name
    (if  ;pre_select_ssget_or_post_select_ssget
      (=
        (setq ss_pre_filter_set_xx_ (ssget "i"
                                          (list
                                            (cons 0 "dimension") ;type of object
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
                                            (cons 0 "dimension") ;type of object
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
  (setq dim_xyz (cdr (assoc 10 (entget (ssname ss_pre_filter_set_xx_ 0)))))
  
  (setq dim_ename_ (car (entsel "specify grid line Object")))
  (setq dim_obj_ (vlax-ename->vla-object dim_ename_))
  (setq dim_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint dim_obj_))))
  (setq preset_move_2 (list (car dim_xyz) 
                           (cadr dim_xyz)
                           (caddr dim_obj_ins_)
                     )
  )
  (command "move" ss_pre_filter_set_xx_ ""  dim_xyz   preset_move_2 )
)



(defun c:make_2Dwireframe_mk2d_ ()
  (setq vpt_ename_ (car (entsel "specify Object")))
  (setq vpt_obj_ (vlax-ename->vla-object vpt_ename_) )

  (if (= (vla-get-objectname vpt_obj_) "AcDbViewport")
    (progn
      (vlax-dump-object vpt_obj_)
      (vla-put-visualstyle 1)
    )
    (alert "this object not viewport")
  )
)



(defun c:module_for_what_ ()
  (textscr)
  (setq total_i (getint))
  ;preloop_and_while
    (setq ss_i 0)
    (setq ss_ii 1)
    (while (< ss_i total_i)
      
      (setq percent (fix (* (/ (float ss_i) total_i) 100)))
      (princ 
        (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "\n" )
      ) 
      
          (cond
            (;_case_1
              (and
                (= ss_ii 1)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 2)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 3)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 4)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 5)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 6)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 7)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 8)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 9)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 10)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
            
            (;_case_1
              (and
                (= ss_ii 20)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
                (setq ss_ii 1)
              )
            )
            (;_case_1
              (and
                (= ss_ii 19)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 18)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 17)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 16)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 15)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 14)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 13)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 12)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_ii 11)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_i 2 0) "/" (rtos ss_i 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
          )
      
  
      (setq ss_i (+ ss_i 1))
      (setq ss_ii (+ ss_ii 1))
    )
  ;
)

(defun c:GT5 ()
  

(setq Textblk_ename_ (car (entsel "specify Object")))
(setq Textblk_obj_ (vlax-ename->vla-object Textblk_ename_))
  (TA:copytext->clipboard 
    (strcat
      (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_1")
      (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_2")
      (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_3")
      " "
      "(no."
      (LM:vl-getattributevalue Textblk_obj_ "NO_VALUE_1")
      ")"
    )
  )
  ; (create-text-vla 
  ;   (vlax-3d-point (getpoint))
  ;   (strcat 
  ;     (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_1")
  ;     (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_2")
  ;     (LM:vl-getattributevalue Textblk_obj_ "CONTENT_VALUE_3")
  ;   )
  ;   10
  ;   0
  ; )
)
(defun c:GT6 ()
  
  (while (and (setq src (car (nentsel "\nSelect Source Object: ")))
              (not (wcmatch (cdr (assoc 0 (entget src))) "*TEXT,ATTRIB,MULTILEADER")))
    (princ "\n** Source Object must Contain Text **")
  )
  (while (and (setq des (car (nentsel "\nSelect Destination Object: ")))
              (not (wcmatch (cdr (assoc 0 (entget des))) "*TEXT,ATTRIB,MULTILEADER")))
    (princ "\n** Destination Object must Contain Text **")
  )
  (if (and src des)
    (progn
      (setq RegExp (vlax-get-or-create-object "VBScript.RegExp"))      
      (TA:copytext->clipboard (LM:GetTrueContent RegExp src 'text 'mtext))

      
    )
  )
 

)


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

(defun c:ch_color_cch ()
  (setq sss_ename_ (car (entsel "specify Object")))
  (setq sss_obj_ (vlax-ename->vla-object sss_ename_))
  (vla-get-textstring sss_obj_ )
  ;preloop_and_while
    (setq i 0)
    (while (< i 256)
      (setq text_change_ (strcat "C" (rtos i 2 0)))
      (vla-put-textstring sss_obj_ (LM:StringSubst "C256" text_change_ (vla-get-textstring sss_obj_) ))
      
      (setq i (+ i 1))
    )
  ;
)