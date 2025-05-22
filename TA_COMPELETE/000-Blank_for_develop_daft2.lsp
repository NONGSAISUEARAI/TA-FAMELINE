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
;
;sub_func
  (defun TA:vla-addpoint_ (point) 
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
    (vla-addpoint modelSpace (vlax-3d-point point))
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
  (defun TA:make_segment_mkseg_ (cir_ename_obj_ segment_time_) 
    ;comment for tesing
      ; (setq cir_ename_ (car (entsel)))
    ;
    ;reset VAR
      (setq getold_osmode (getvar "osmode"))
      (setvar "osmode" 0)
    ;
    ;get_data standard_
      ; (setq cir_ename_obj_ (vlax-ename->vla-object cir_ename_))
      (setq cir_ename_obj_arclength_ (vla-get-arclength cir_ename_obj_)) ;= 4.71
      (setq cir_ename_obj_get_startpoint_ (vlax-safearray->list (vlax-variant-value (vla-get-startpoint cir_ename_obj_)) ) )
      (setq cir_ename_obj_get_endpoint_ (vlax-safearray->list (vlax-variant-value (vla-get-endpoint cir_ename_obj_)) ) )
    ;
    ;user_input_
      ; (setq segment_time_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq segment_time_ (cond (segment_time_) (0.0) ) ) ) "> : " ) ) ) (segment_time_) ) )
      (princ (setq segment_length_ (/ cir_ename_obj_arclength_ segment_time_)))
    ;
    ;preloop_and_while_
      (setq segment_time_i 1)
      (setq segment_data_ ())
      (while (< segment_time_i segment_time_)
      ;get_segment_data_
        (setq cir_ename_obj_get_param_ (vlax-curve-getParamAtDist cir_ename_obj_ (* segment_length_ segment_time_i)))
        (setq cir_ename_obj_get_segments_point (vlax-curve-getPointAtParam cir_ename_obj_ cir_ename_obj_get_param_))
        (setq segment_data_ (cons cir_ename_obj_get_segments_point segment_data_))
      ;
      (setq segment_time_i (+ segment_time_i 1))
      )
    ;
    ;constructs data
      (setq segment_data_ (reverse segment_data_))
      (setq segment_data_result_ (append (list cir_ename_obj_get_startpoint_ ) segment_data_ (list cir_ename_obj_get_endpoint_ )))
    ;
    ;run
      (create-pline segment_data_result_)
    ;
    ;return VAR
      (setvar "osmode" getold_osmode)
    ;
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
  (defun TA:arc-to-segment (seg-time_)
    ;user_input
      ; (setq seg-time_ (cond ( (getint (strcat "\nspecify segment_time_  \n<" (rtos (setq seg-time_ (cond (seg-time_) (0.0) ) ) ) "> : " ) ) ) (seg-time_) ))
    ;
    ;Explode process
      ;selection set
        (setq ssset_EXPLODE_ (ssget "x" 
                                      (list 
                                        ; (cons 0 "pline") ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                                )
        )
        (sslength ssset_EXPLODE_)
      ;
      (vla-explode (setq ssset_EXPLODE_obj_ (vlax-ename->vla-object (ssname ssset_EXPLODE_ 0)) ))
      (vla-delete ssset_EXPLODE_obj_)
    ;
    ;tran line>pline process
      ;selection set
        (setq ssset_tran>line_ (ssget "x" 
                                      (list 
                                        (cons 0 "line") ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                                )
        )
        (sslength ssset_tran>line_)
      ;
      (command "pedit" "m" ssset_tran>line_ "" "y" "" )
    ; 
    ;TA:make_segment_mkseg_ process
      ;selection_set
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq ss_pre_filter_set_xx_ (ssget "x" 
                                              (list 
                                                (cons 0 "arc") ;type of object
                                                ; (cons 8 "000 - GRID")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
            nil
          )
          (progn 
            (setq ss_pre_filter_set_xx_ (ssget "x"
                                          (list 
                                            (cons 0 "arc") ;type of object
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
      ;preloop_and_while_ for running TA:make_segment_mkseg_
      (if (/= ss_pre_filter_set_xx_ nil)
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
            (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (TA:make_segment_mkseg_ ss_pre_filter_set_xx_obj_ seg-time_)
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )  
        )
        (command "bedit" "D" "?" "*" )
      )
      ;
    ;
    ;Delete arc process
      (if (/= ss_pre_filter_set_xx_ nil) 
        (progn 
          (setq ssset_delete_arc_ (ssget "x" 
                                        (list 
                                          (cons 0 "arc") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                  )
          )
          (sslength ssset_delete_arc_)
          ;preloop_and_while
          (setq ssset_delete_arc_i 0)
          (while (< ssset_delete_arc_i (sslength ssset_delete_arc_)) 
            (setq ssset_delete_arc_obj_ (vlax-ename->vla-object (ssname ssset_delete_arc_ ssset_delete_arc_i ) ) )
            (vla-delete ssset_delete_arc_obj_)
            (setq ssset_delete_arc_i (+ ssset_delete_arc_i 1))
          )
          ;
        )
        (alert "1")
        ; (command "bedit" "D" "?" "*")
      )
    ;
    ;Joint_polyline process
      (if (/= ss_pre_filter_set_xx_ nil) 
        (progn 
          ;selection set
          (setq ssset_joint_ (ssget "x" 
                                    (list 
                                      ; (cons 0 "pline") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                      ; (cons 62 1)           ;kind of color call sign with color code index
                                    )
                            )
          )
          (sslength ssset_joint_)
          ;
          (command "pedit" "m" ssset_joint_ "" "j" "" "")
          ;
        )
        (setq logic_result "Fail")
        ; (command "bedit" "D" "?" "*")
      )
    ;
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
  (defun TA:Find_boundary_line_ (main_border_) -
    (setq main_border_ (car (entsel)))
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
  )
;

(defun c:mk_bo () 
  ;user-input_data pocess
    ;selection_set
      (setq main_border_ nil)
      (while (= main_border_ nil)
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq main_border_ (ssget "I" 
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
            (princ (setq main_border_ (car (entsel "\nplease select Polyline "))))
          )
          (sslength main_border_)
        )
      )      
    ;
    (setq seg-time_ (cond ( (getint (strcat "\nspecify segment_time_  \n<" (rtos (setq seg-time_ (cond (seg-time_) (0.0) ) ) ) "> : " ) ) ) (seg-time_) ))
  ;
  ;generate_data pocess
    (setq center_point (TA:find_center main_border_))
    (setq name-blk_ (strcat "000-temp-blk-" (rtos (getvar "cdate") 2 8)))
  ;
  ;make_block_ pocess
    (command "_block" name-blk_ center_point main_border_ "")
  ;
  ;edit_block pocess
    (command "bedit" name-blk_ )
    ;make segment process 
      (setq result_ (TA:arc-to-segment seg-time_))
      (if (= result_ "Fail")
        (progn
         (alert "make segment process Fail")
        )
        (command "bedit" "S" "?" "*" )
      )
    ;
  ;
  ;insert_block pocess
    (if (= result_ "Fail") 
      (progn 
        (alert "insert_block pocess Fail")
      )
      (command "insert" name-blk_ center_point 1 1 0 )
    )
  ;
  ;entlast obj process
    (if (/= result_ "Fail") 
      (progn 
        (setq entlast_ename_ (entlast))
        (setq entlast_ename_obj_ (vlax-ename->vla-object entlast_ename_))
        (command "explode" entlast_ename_)
        
      )
      (alert "insert_block pocess Fail")
    )
    (if (= result_ "Fail") 
      (progn 
        (command "insert" name-blk_ center_point 1 1 0 )
        (setq entlast_ename_ (entlast))
        (setq entlast_ename_obj_ (vlax-ename->vla-object entlast_ename_))
        (command "explode" entlast_ename_)
        
      )
      (alert "insert_block pocess Complete")
    )
    ; (vla-explode entlast_ename_obj_)
    ; (vla-delete entlast_ename_obj_)
  ;
)

(defun c:DAFT:COPY_SAME_PLACE_CCS_ ()

  (setq s (car (entsel)))
  (setq sobj (vlax-ename->vla-object s))
  (setq i 1)
  (setq il (getint))
  (while (< i il)
    (vla-copy sobj)
    (setq sss (entlast))
    (setq sssobj (vlax-ename->vla-object sss))
    
    (setq sssrotation (vla-put-rotation sssobj (deg-to-rad i)))
    (setq i ( + i 20))
  )
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

(vlax-dump-object (vlax-ename->vla-object (car (entsel ))))



(defun c:add_vis_advs_ ()
  (setq input_ins_ (getpoint ))
  (setq name_vis_ "view")
  (command "BPARAMETER" "v" "L" name_vis_ input_ins_ 1)
  (command "-Bvstate" "s" "VisibilityState0")
  ;preloop_and_while_
  (setq visibility_list_ (list
                           "1"
                           "2"
                           "3"
                         )
  )
  (setq visibility_list_i 0)

  (while (< visibility_list_i (length visibility_list_))
    (setq visibility_list_SEQ (nth visibility_list_i visibility_list_))
    (command "-Bvstate" "n" visibility_list_SEQ "c" )
    (setq visibility_list_i (+ visibility_list_i 1))
  )
)


(defun c:DAFT_block_to_line_ ()
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


