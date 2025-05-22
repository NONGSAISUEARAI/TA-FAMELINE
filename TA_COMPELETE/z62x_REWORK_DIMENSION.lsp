(vl-load-com)
;Dynamic Funcion
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
;Effective Block Name Funcition
  ;; Effective Block Name  -  Lee Mac
    ;; obj - [vla] VLA Block Reference object
        (defun LM:effectivename ( obj )
            (vlax-get-property obj
                (if (vlax-property-available-p obj 'effectivename)
                    'effectivename
                    'name
                )
            )
        )
  ;; Effective Block Name  -  Lee Mac
    ;; ent - [ent] Block Reference entity

    (defun LM:al-effectivename ( ent / blk rep )
        (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
            (if
                (and
                    (setq rep
                        (cdadr
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
;Attribute Function
  ;; Get Attribute Value  -  Lee Mac
    ;; Returns the value held by the specified tag within the supplied block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; Returns: [str] Attribute value, else nil if tag is not found.

    (defun LM:vl-getattributevalue ( blk tag )
        (setq tag (strcase tag))
        (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Value  -  Lee Mac
    ;; Sets the value of the first attribute with the given tag found within the block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; val - [str] Attribute Value
    ;; Returns: [str] Attribute value if successful, else nil.

    (defun LM:vl-setattributevalue ( blk tag val )
        (setq tag (strcase tag))
        (vl-some
          '(lambda ( att )
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

    (defun LM:vl-getattributevalues ( blk )
        (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Values  -  Lee Mac
    ;; Sets attributes with tags found in the association list to their associated values.
    ;; blk - [vla] VLA Block Reference Object
    ;; lst - [lst] Association list of ((<tag> . <value>) ... )
    ;; Returns: nil

    (defun LM:vl-setattributevalues ( blk lst / itm )
        (foreach att (vlax-invoke blk 'getattributes)
            (if (setq itm (assoc (vla-get-tagstring att) lst))
                (vla-put-textstring att (cdr itm))
            )
        )
    )
;
;convert_value_
  (defun deg-to-rad (angle)
    (* angle (/ pi 180.0))
  )
  (defun rad-to-deg (rad)
    (/ (* rad 180) pi) 
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
    ;temp for testing
      ; (setq Selection_list_ ss_pre_filter_set_xx)
      ; (setq nameprop_ "effectivename")
      ; (setq propval_ "000-GRID_LINE_DYN")
      ; (command "pselect" temp_ssget_)
    ;
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
;sub_function
  ;corrdiante_
    (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
    )
    (defun sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (caddr a) (caddr b))))))
    )
    (defun sort_by_higest (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 4 a) (nth 4 b))))))
    )
    (defun sort_by_lowest (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 6 a) (nth 6 b))))))
    )
    (defun sort_by_higest_hon (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 3 a) (nth 3 b))))))
    )
    (defun sort_by_lowest_hon (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 6 a) (nth 6 b))))))
    )
    (defun sort_by_higest_ver (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 5 a) (nth 5 b))))))
    )
    (defun sort_by_lowest_ver (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 3 a) (nth 3 b))))))
    )
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
  ;list
    (defun LM:RemoveNth (n l / i) 
      (setq i -1)
    (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
    )
  ;
  ;chaning_dim
    (defun TA:z62A_change_single_dim (dim_obj_ dim_string)
      ; (setq obj  (car (entsel)))
      ; ; (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      ; ; (setq sc (* scl 10)) 
      ; (setq dim_obj_ (vlax-ename->vla-object obj))
      ;get_data
        ; (setq get_dim_sting (vla-get-stylename dim_obj_ ))
        (setq get_sc (vla-get-scalefactor dim_obj_))
        (setq get_text_int (vlax-safearray->list (vlax-variant-value (vla-get-textposition dim_obj_))))
      ;
      ;return_data_
        (vla-put-stylename    dim_obj_ dim_string)
        (vla-put-scalefactor  dim_obj_ get_sc)
        (vla-put-textposition dim_obj_ (vlax-3d-point get_text_int))
        
      ;
      
    )
    (defun TA:z62B_change_multi_dim (selection_set_ dim_string )
      (setq ss_dim_i 0)
      (while (< ss_dim_i (sslength selection_set_))
          
          (setq ss_dim_obj (vlax-ename->vla-object (ssname selection_set_ ss_dim_i)))
          (TA:z62A_change_single_dim ss_dim_obj dim_string)
          (setq ss_dim_i (+ ss_dim_i 1))
        )
    )
    (defun TA:z62C_reset_dim (reset_dim_ ss_fillter_set)
      ; (setq reset_dim_  (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
      (if (= reset_dim_ 0)
        (progn
          (setq ss_fillter_set_i 0)
          (while (< ss_fillter_set_i (sslength ss_fillter_set))
            (setq ss_fillter_set_ename_ (ssname ss_fillter_set ss_fillter_set_i))
            (setq get_text_movement_val (vla-get-textmovement (vlax-ename->vla-object ss_fillter_set_ename_)))
            (setq get_text_pos_val (vlax-safearray->list (vlax-variant-value (vla-get-textposition  (vlax-ename->vla-object ss_fillter_set_ename_)))))
            (if (or
                  (= get_text_movement_val 1)
                  (= get_text_movement_val 2)
                )
              (progn 
                (command "AIDIMTEXTMOVE" "1" ss_fillter_set_ename_ "" get_text_pos_val )
              )
              (princ "/n")
            )
            (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
          )
          
        )
        (command "dimedit" "H" ss_fillter_set "")
      )
    )
  ;
  ;reset_coord_dim.
    (defun TA:z62C_reset_X_single_dim (ref_dyn_block_obj_ tar_dyn_block_obj_ )
      ; (setq ref_dyn_block_obj_ (vlax-ename->vla-object (car (entsel))))
      ; (setq tar_dyn_block_obj_ (vlax-ename->vla-object (car (entsel))))
      (setq ref_ins_x_pt (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ref_dyn_block_obj_ )))))
        (setq tar_ins_pt_xyz  (vlax-3d-point 
                                (list
                                  ref_ins_x_pt
                                  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint tar_dyn_block_obj_ ))))
                                  0
                                )
                              )
        )
      (vla-put-insertionpoint tar_dyn_block_obj_ tar_ins_pt_xyz)
    
    )
    (defun TA:z62D_reset_X_multi_dim (selection_set_ ref_dyn_block_obj_  )
      (setq ss_dim_i 0)
      (while (< ss_dim_i (sslength selection_set_))
          
          (setq tar_dyn_block_obj_ (vlax-ename->vla-object (ssname selection_set_ ss_dim_i)))
          (TA:z62C_reset_X_single_dim ref_dyn_block_obj_ tar_dyn_block_obj_)
          (setq ss_dim_i (+ ss_dim_i 1))
        )
    )
    (defun TA:z62E_reset_Y_single_dim (ref_dyn_block_obj_ tar_dyn_block_obj_ )
      ; (setq ref_dyn_block_obj_ (vlax-ename->vla-object (car (entsel))))
      ; (setq tar_dyn_block_obj_ (vlax-ename->vla-object (car (entsel))))
      (setq ref_ins_x_pt (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ref_dyn_block_obj_ )))))
        (setq tar_ins_pt_xyz  (vlax-3d-point 
                                (list
                                  (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint tar_dyn_block_obj_ ))))
                                  ref_ins_x_pt
                                  
                                  0
                                )
                              )
        )
      (vla-put-insertionpoint tar_dyn_block_obj_ tar_ins_pt_xyz)
    
    )
    (defun TA:z62F_reset_Y_multi_dim (selection_set_ ref_dyn_block_obj_  )
      (setq ss_dim_i 0)
      (while (< ss_dim_i (sslength selection_set_))
          
          (setq tar_dyn_block_obj_ (vlax-ename->vla-object (ssname selection_set_ ss_dim_i)))
          (TA:z62E_reset_Y_single_dim ref_dyn_block_obj_ tar_dyn_block_obj_)
          (setq ss_dim_i (+ ss_dim_i 1))
        )
    )
  ;
;
;sub_command
  (defun c:TEMP:copy_from_basepoint_mfb_ () 
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
          
          (command "_move" tar_copy_ "" tar_copy_obj_ins)
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
        
        
        ; (command "_move" ss_pre_filter_set_xx "" tar_copy_obj_ins )
        (command "_move" tar_copy_obj_ins)
        
        
      )
      (princ "\n")
    )
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
  (defun c:TEMP:copy_from_basepoint_X_cfbx_ () 
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
        (setq get_coord_ename_ (car (car (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "X"))))
        (setq tar_copy_        get_coord_ename_
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
  (defun c:TEMP:copy_from_basepoint_Y_cfby_ () 
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
        (setq get_coord_ename_ (car (car (reverse (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "Y")))))
        (setq tar_copy_        get_coord_ename_
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
  (defun c:TEMP:ortho_gridline_copy_ogl ()
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= ;pre_select_ssget_or_post_select_ssget 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                     )
          )
          nil
        )
        (progn ;in-case ;pre_select_ssget_or_post_select_ssget = nil
          ;get_data_gridline
            (setq tar_copy_             (car (entsel))) ;get_data
            (setq tar_copy_obj_         (vlax-ename->vla-object tar_copy_) ;get_data vlax-ename>object
                  tar_copy_obj_ins      (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_)) )
                  tar_copy_obj_rotaion  (rad-to-deg (vla-get-rotation tar_copy_obj_))
                  tar_result_           (list 
                                          (car tar_copy_obj_ins)
                                          (cadr tar_copy_obj_ins)
                                        )
            )
            (setq x nil)
          ;
          ;preloop_and_while_specify_point_to_gridline
            (while (/= x "FUNCTION CANCELLED") 
              ;user-input for specify value and *CANCEL* method 
                (setq result (vl-catch-all-apply (function (lambda () (setq symbol_ (getpoint) ) ) ))) ; ใช้ lambda เพื่อจับคำสั่ง getint ) )
                (if (vl-catch-all-error-p result)
                  (if (= (strcase (vl-catch-all-error-message result)) "FUNCTION CANCELLED")
                    (setq x "FUNCTION CANCELLED")  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
                  )
                    
                )
              ; 
              ; (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins )
              (if (/= x "FUNCTION CANCELLED")
                (progn
                  (command "_copy" tar_copy_ "" tar_copy_obj_ins symbol_)
                  ;new_obj_
                    (setq new_obj_ (entlast))
                    (setq new_obj_get_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object new_obj_ ) ))))
                    (cond ;in-case_rotation_data
                      ((= tar_copy_obj_rotaion 0)
                        (progn
                          (setq new_obj_get_ins_x (car new_obj_get_ins_))
                          (setq new_obj_get_ins_y (cadr tar_copy_obj_ins))
                          (setq new_obj_get_ins_z 0)
                          (setq new_obj_put_ins_xyz (list new_obj_get_ins_x new_obj_get_ins_y new_obj_get_ins_z ) )
                        )
                      )
                      ((= tar_copy_obj_rotaion 90)
                        (progn
                          (setq new_obj_get_ins_x (car tar_copy_obj_ins))
                          (setq new_obj_get_ins_y (cadr new_obj_get_ins_))
                          (setq new_obj_get_ins_z 0)
                          (setq new_obj_put_ins_xyz (list new_obj_get_ins_x new_obj_get_ins_y new_obj_get_ins_z ) )
                        )
                      )
                    )
                        
                    (setq put_ins (vla-put-insertionpoint (vlax-ename->vla-object new_obj_ ) (vlax-3d-point new_obj_put_ins_xyz)))
                    (setq x nil)
                  ;
                )
                (princ "\n")
              )
            )
          ;
        )
        (princ "\n")
      )
    ;
    ;get_data_gridline
      (if ;in_case ssget_data_ not 1
        (= (length (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "Y")) 1)
        (progn
          (setq get_grid_ename_ (car (car (reverse (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "Y")))))
          (setq tar_copy_             get_grid_ename_) ;get_data
          (setq tar_copy_obj_         (vlax-ename->vla-object tar_copy_) ;get_data vlax-ename>object
                tar_copy_obj_ins      (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_)) )
                tar_copy_obj_rotaion  (rad-to-deg (vla-get-rotation tar_copy_obj_))
                tar_result_           (list 
                                        (car tar_copy_obj_ins)
                                        (cadr tar_copy_obj_ins)
                                      )
          )
          (setq x nil)
        )
        (setq x "FUNCTION CANCELLED")
      )
      
    ;
    ;preloop_and_while_specify_point_to_gridline
      (while (/= x "FUNCTION CANCELLED") 
        ;user-input for specify value and *CANCEL* method 
          (setq result (vl-catch-all-apply (function (lambda () (setq symbol_ (getpoint) ) ) ))) ; ใช้ lambda เพื่อจับคำสั่ง getint ) )
          (if (vl-catch-all-error-p result)
            (if (= (strcase (vl-catch-all-error-message result)) "FUNCTION CANCELLED")
              (setq x "FUNCTION CANCELLED")  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
            )
              
          )
        ; 
        ; (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins )
        (if (/= x "FUNCTION CANCELLED")
          (progn
            (command "_copy" tar_copy_ "" tar_copy_obj_ins symbol_)
            ;new_obj_
              (setq new_obj_ (entlast))
              (setq new_obj_get_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint (vlax-ename->vla-object new_obj_ ) ))))
              (cond ;in-case_rotation_data
                ((= tar_copy_obj_rotaion 0)
                  (progn
                    (setq new_obj_get_ins_x (car new_obj_get_ins_))
                    (setq new_obj_get_ins_y (cadr tar_copy_obj_ins))
                    (setq new_obj_get_ins_z (caddr tar_copy_obj_ins))
                    (setq new_obj_put_ins_xyz (list new_obj_get_ins_x new_obj_get_ins_y new_obj_get_ins_z ) )
                  )
                )
                ((= tar_copy_obj_rotaion 90)
                  (progn
                    (setq new_obj_get_ins_x (car tar_copy_obj_ins))
                    (setq new_obj_get_ins_y (cadr new_obj_get_ins_))
                    (setq new_obj_get_ins_z (caddr tar_copy_obj_ins))
                    (setq new_obj_put_ins_xyz (list new_obj_get_ins_x new_obj_get_ins_y new_obj_get_ins_z ) )
                  )
                )
              )
                  
              (setq put_ins (vla-put-insertionpoint (vlax-ename->vla-object new_obj_ ) (vlax-3d-point new_obj_put_ins_xyz)))
              (setq x nil)
            ;
          )
          (princ "\n")
        )
      )
    ;
  )
  (defun c:temp_movetextdim_ggm ()
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
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
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                ; tar_copy_obj_ins (vlax-safearray->list 
                ;                    (vlax-variant-value (vla-get-textposition tar_copy_obj_))
                ;                  )
                ; tar_result_      (list (car tar_copy_obj_ins) 
                ;                        (cadr tar_copy_obj_ins)
                ;                  )
          )
          (command "AIDIMTEXTMOVE" "1" tar_copy_ ""  )
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
              tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
              ; tar_copy_obj_ins (vlax-safearray->list 
              ;                    (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
              ;                  )
              ; tar_result_ (list (car tar_copy_obj_ins)(cadr tar_copy_obj_ins))
        )
        
        
        (command "AIDIMTEXTMOVE" "1" tar_copy_ ""  )
        
        
      )
      (princ "\n")
    )
  )
  (defun c:TEMP_select_gridlineR0_GI0 ()
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
    ;filter_ssget_
      (setq ssset_filter_efname_ (TA:Prop_Filter_ss_set_ ss_pre_filter_set_xx "effectivename" "000-GRID_LINE_DYN"))
      (setq ssset_filter_efname_rotation (TA:Prop_Filter_ss_set_ ssset_filter_efname_ "rotation" (deg-to-rad 0)))
      (command "pselect" ssset_filter_efname_rotation  "")
    ;
  )
  (defun c:TEMP_select_gridlineR0_GI90 ()
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
    ;filter_ssget_
      (setq ssset_filter_efname_ (TA:Prop_Filter_ss_set_ ss_pre_filter_set_xx "effectivename" "000-GRID_LINE_DYN"))
      (sslength ssset_filter_efname_)
      (setq ssset_filter_efname_rotation (TA:Prop_Filter_ss_set_ ssset_filter_efname_ "rotation" (deg-to-rad 90)))
      (command "pselect" ssset_filter_efname_rotation  "")
    ;
  )
  (defun c:DAFT:COPY_SAME_PLACE_CCS_ () 

    (setq s (car (entsel)))
    (setq sobj (vlax-ename->vla-object s))
    (setq i 0)
    (setq il (getint))
    (while (< i 1) 
      (vla-copy sobj)
      (setq sss (entlast))
      (setq sssobj (vlax-ename->vla-object sss))

      (setq sssrotation (vla-put-rotation sssobj (deg-to-rad i)))
      (setq i (+ i 1))
    )
  )
  (defun c:TMEP_DIMEDIT_DEDD ()
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx_ (ssget "I" 
                                            (list 
                                              (cons 0 "DIMENSION") ;type of object
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
                                          (cons 0 "DIMENSION") ;type of object
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
      (setq insertion_blk_point_1 (getpoint "specify point"))
      (setq insertion_blk_point_2 (getpoint insertion_blk_point_1 "specify point"))
      (setq insertion_blk_rotation_ (rad-to-deg (angle insertion_blk_point_1 insertion_blk_point_2)))  
      (LM:round insertion_blk_rotation_)
      (setq insertion_blk_dist_ (distance insertion_blk_point_1 insertion_blk_point_2))
    ;
    (command "dimedit" "ob" ss_pre_filter_set_xx_ "" (LM:round insertion_blk_rotation_))
    
  )
;
;main_commad
  (defun c:Dx ()
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
      (command "dimstyle" "r" "DIM E-E")
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
      (setq lv_add (* lv_ 1))
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
      (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ nil)
      (while (= ss_fillter_set_ nil)
        (setq ss_fillter_set_ (ssget 
                            (list 
                              (cons 0 "INSERT") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
          )
        (if (= ss_fillter_set_ nil)
          (progn
            (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
            (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
            (setq lv_add (* lv_ 1))
            (setq sc (* scl 10)) ; 5 = 50 2 = 20
            (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
            (setq hi_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
            (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
            (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
          )
          
        )
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  )
  (defun c:DDx ()
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
      (command "dimstyle" "r" "DIM E-E")
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
      (setq lv_add (* lv_ 1))
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
      (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
      (setvar "DIMSCALE" sc)
    ;
    
    ;selection_set
      (setq ss_fillter_set_ nil)
      (while (= ss_fillter_set_ nil)
        (setq ss_fillter_set_ (ssget 
                            (list 
                              (cons 0 "INSERT") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
          )
        (if (= ss_fillter_set_ nil)
          (progn
            (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
            (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
            (setq lv_add (* lv_ 1))
            (setq sc (* scl 10)) ; 5 = 50 2 = 20
            (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
            (setq hi_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
            (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
            (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
          )
          
        )
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  )
  (defun c:z621_r90c ()
    ;selection_set
      (if 
        (= (setq ss_fillter_set_ (ssget "_I" 
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
          (setq ss_fillter_set_ (ssget 
                              (list 
                                (cons 0 "INSERT") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set_)
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) 0)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;preloop_and_while copy_and_rotate
      (setq FILT_GLINE_set_i 0)
      (while
        ;get_data_
        (< FILT_GLINE_set_i (length FILT_GLINE_set_))
        (setq FILT_GLINE_set_list (car (nth FILT_GLINE_set_i FILT_GLINE_set_)))
        (setq FILT_GLINE_set_obj (vlax-ename->vla-object FILT_GLINE_set_list))
        ; (command "pselect" FILT_GLINE_set_list)
        ;
        ;new_data_
        (vla-copy FILT_GLINE_set_obj)
        (setq new_FILT_GLINE_set_obj (vlax-ename->vla-object (entlast )))
        (vla-put-rotation new_FILT_GLINE_set_obj (angtof "90"))
        ;
        (setq FILT_GLINE_set_i (+ FILT_GLINE_set_i 1))
      )
    ;
  )
  (defun c:z621_r0c ()
    ;selection_set
      (if 
        (= (setq ss_fillter_set_ (ssget "_I" 
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
          (setq ss_fillter_set_ (ssget 
                              (list 
                                (cons 0 "INSERT") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set_)
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) 90)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;preloop_and_while copy_and_rotate
      (setq FILT_GLINE_set_i 0)
      (while
        ;get_data_
        (< FILT_GLINE_set_i (length FILT_GLINE_set_))
        (setq FILT_GLINE_set_list (car (nth FILT_GLINE_set_i FILT_GLINE_set_)))
        (setq FILT_GLINE_set_obj (vlax-ename->vla-object FILT_GLINE_set_list))
        ; (command "pselect" FILT_GLINE_set_list)
        ;
        ;new_data_
        (vla-copy FILT_GLINE_set_obj)
        (setq new_FILT_GLINE_set_obj (vlax-ename->vla-object (entlast )))
        (vla-put-rotation new_FILT_GLINE_set_obj (angtof "0"))
        ;
        (setq FILT_GLINE_set_i (+ FILT_GLINE_set_i 1))
      )
    ;
  )
  (defun c:z622_reset_gridline_ins ()
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;REF_blk_for_GV2
      (setq REF_block_ nil)
      (while (not REF_block_)
          (setq REF_block_ (entsel "\nSelect a First BLOCK "))
          (if 
            (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
              (setq REF_block_ (car REF_block_))
              
              (progn
                (setq REF_block_ nil)
                (alert "Please select a BLOCK")
              )
          )
          (if
            (/= REF_block_ nil)
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
            )

            (princ "/n")
          )
          (if (= REF_block_ nil)
            (progn
              (alert "Please select a 000-GRID_LINE_DYN\n")
            )
            (cond
              ((and 
                (= (LM:effectivename REF_block_obj) 
                    "000-GRID_LINE_DYN"
                )
                (= (LM:effectivename REF_block_obj) "000-GRID_LINE_DYN")
              ) 
                (progn 
                  (setq REF_block_obj (vlax-ename->vla-object REF_block_))
                  (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
                )
                (princ "\n")
              )
              ((or 
                (/= (LM:effectivename REF_block_obj) 
                    "000-GRID_LINE_DYN"
                )
                (/= (LM:effectivename REF_block_obj) "000-GRID_LINE_DYN")
              ) 
                (progn 
                  (setq REF_block_ nil)
                  (alert "Please select a 000-GRID_LINE_DYN\n")
                )
                (princ "/n")
              )
            )
          )
      )
    ;
    ;selection_set
      (if 
        (= (setq ss_fillter_set_ (ssget "_I" 
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
          (setq ss_fillter_set_ (ssget 
                              (list 
                                (cons 0 "INSERT") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set_)
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;preloop_and_while copy_and_rotate
      (setq FILT_GLINE_set_i 0)
      (while
        ;get_data_
          (< FILT_GLINE_set_i (length FILT_GLINE_set_))
          (setq FILT_GLINE_set_list (car (nth FILT_GLINE_set_i FILT_GLINE_set_)))
          (setq FILT_GLINE_set_obj (vlax-ename->vla-object FILT_GLINE_set_list))
          (setq FILT_GLINE_set_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint FILT_GLINE_set_obj))))
          ; (command "pselect" FILT_GLINE_set_list)
        ;
        ;ref_ins_blk
          (cond ;create_new_ins_ref_ins_blk
            ((and 
              (= mode-direction-val 1)
            ) 
              (progn 
                (setq new_ins_REF_block_obj_ins (list 
                                                  (car FILT_GLINE_set_ins)
                                                  (cadr REF_block_obj_ins)
                                                  0
                                                )
                )
              )
            )
            ((and 
              (= mode-direction-val 2)
            ) 
              (progn 
                (setq new_ins_REF_block_obj_ins (list 
                                                  (car REF_block_obj_ins)
                                                  (cadr FILT_GLINE_set_ins)
                                                  0
                                                )
                )
              )
            )
          )
        ;
        ;new_data_
        (vla-put-insertionpoint FILT_GLINE_set_obj (vlax-3d-point new_ins_REF_block_obj_ins))
        (setq FILT_GLINE_set_i (+ FILT_GLINE_set_i 1))
      )
    ;
  )
  (defun c:DECV_multi_change_dim ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set)
      )
    ;
    ;user_input
      (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
    ;
    ;main_command
      (TA:z62B_change_multi_dim ss_fillter_set "DIM E-C")
      (TA:z62C_reset_dim reset_dim_ ss_fillter_set)
    ;
  )
  (defun c:DCEV_multi_change_dim ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set)
      )
    ;
    ;user_input
      (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
    ;
    ;main_command
      (TA:z62B_change_multi_dim ss_fillter_set "DIM C-E")
      (TA:z62C_reset_dim reset_dim_ ss_fillter_set)
    ;
  )
  (defun c:DEEV_multi_change_dim ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ ss_fillter_set)
      )
    ;
    ;user_input
      (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
    ;
    ;main_command
      (TA:z62B_change_multi_dim ss_fillter_set "DIM E-E")
      (TA:z62C_reset_dim reset_dim_ ss_fillter_set)
    ;
  )
  (defun c:DCCV_multi_change_dim ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ (sslength ss_fillter_set))
      )
    ;
    ;user_input
      (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
    ;
    ;main_command
      (TA:z62B_change_multi_dim ss_fillter_set "DIM C-C")
      (TA:z62C_reset_dim reset_dim_ ss_fillter_set)
    ;
  )
  (defun c:DCCV_multi_change_dim ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ (sslength ss_fillter_set))
      )
    ;
    ;user_input
      (setq reset_dim_  (cond ( (getint (strcat "\nreset dim = 1 \nno reset = 0  \n<" (rtos (setq reset_dim_ (cond (reset_dim_) (0.0) ) ) ) "> : " ) ) ) (reset_dim_) ) )
    ;
    ;main_command
      (TA:z62B_change_multi_dim ss_fillter_set "DIM C-C")
      (TA:z62C_reset_dim reset_dim_ ss_fillter_set)
    ;
  )
  (defun c:z623_reset_x_coord_rexx ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "INSERT") ;type of object
                                            (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "INSERT") ;type of object
                                (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ (sslength ss_fillter_set))
      )
      ;preloop_and_while_ selection_delete process
        (setq ss_fillter_set_i 0 )
        (while (< ss_fillter_set_i (sslength ss_fillter_set))
          (setq ss_fillter_set_ename (ssname ss_fillter_set ss_fillter_set_i))
          (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
          (setq ss_fillter_set_obj_get_rotation_ (vla-get-rotation ss_fillter_set_obj ))
          (setq ef_name (LM:effectivename ss_fillter_set_obj))
          (if (and (/= ef_name "000-GRID_LINE_DYN") (= ss_fillter_set_obj_get_rotation_ (deg-to-rad 90)))
            (progn
              (ssdel ss_fillter_set_ename ss_fillter_set)
            )
            (princ (sslength ss_fillter_set))
          
          )      
          (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
        )
      ;
    ;
    ;main_command
      (setq ref_dyn_block_ (car (entsel)))
        (if (= ref_dyn_block_ nil)
          (progn
            
            (setq get_grid_ename_ (car (car (reverse (TA:standard_list_croodinate_sorting ss_fillter_set "Y")))))
            (setq ref_dyn_block_obj_ (vlax-ename->vla-object get_grid_ename_))
          )
          (setq ref_dyn_block_obj_ (vlax-ename->vla-object ref_dyn_block_))
        )
      (TA:z62D_reset_X_multi_dim ss_fillter_set ref_dyn_block_obj_)
    ;
  )
  (defun c:z624_reset_y_coord_reyy ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "INSERT") ;type of object
                                            (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "INSERT") ;type of object
                                (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ (sslength ss_fillter_set))
      )
      ;preloop_and_while_ selection_delete process
        (setq ss_fillter_set_i 0 )
        (while (< ss_fillter_set_i (sslength ss_fillter_set))
          (setq ss_fillter_set_ename (ssname ss_fillter_set ss_fillter_set_i))
          (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
          (setq ef_name (LM:effectivename ss_fillter_set_obj))
          (if (and (/= ef_name "000-GRID_LINE_DYN") (= ss_fillter_set_obj_get_rotation_ (deg-to-rad 0)))
            (progn
              (ssdel ss_fillter_set_ename ss_fillter_set)
            )
            (princ (sslength ss_fillter_set))
          
          )      
          (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
        )
      ;
    ;
    ;main_command
      (setq ref_dyn_block_ (car (entsel)))
        (if (= ref_dyn_block_ nil)
          (progn
            (setq get_grid_ename_ (car (car (TA:standard_list_croodinate_sorting ss_fillter_set "X"))))
            (setq ref_dyn_block_obj_ (vlax-ename->vla-object get_grid_ename_))
          )
          (setq ref_dyn_block_obj_ (vlax-ename->vla-object ref_dyn_block_))
        )
      (TA:z62F_reset_Y_multi_dim ss_fillter_set ref_dyn_block_obj_)
    ;
  )
  (defun c:z625_reset_text_ins_coord ()
    ;selection_
      (if ;(= (setq ssfillter_set (ssget "I" ) nil)
        (= (setq ss_fillter_set (ssget "I" 
                                          (list 
                                            (cons 0 "DIMENSION") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
            )
          nil
        )
        (progn
          (setq ss_fillter_set (ssget 
                              (list 
                                (cons 0 "DIMENSION") ;type of object
                                ; (cons 8 "000 - GRID")   ;kind of layer
                                ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                ; (cons 62 1)           ;kind of color call sign with color code index
                              )
                            )
          )
        )
        (princ (sslength ss_fillter_set))
      )
    ;
    ;main_command
      (command "dimedit" "H" ss_fillter_set)
    ;
  )
  (defun c:z626_grid_line_length_GYY ()
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
      ;fillter_gridline_
        (setq s_set_0_ (TA:Prop_Filter_ss_set_ ss_pre_filter_set_xx "effectivename" "000-GRID_LINE_DYN" ))
        (setq s_set_1_ (TA:Prop_Filter_ss_set_ s_set_0_ "rotation" (deg-to-rad 0)))
        (sslength s_set_1_)
      ;
      ;make_length_
        (setq get_grid_ename_ (car (entsel)))
        (if (= get_grid_ename_ nil)
          (progn
            (setq get_grid_ename_ (car (car (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "X"))))
          )
          (princ "\n")
        )
        (setq get_grid_obj_ (vlax-ename->vla-object get_grid_ename_))
        (setq get_grid_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint get_grid_obj_ ))))
        (setq create_gridline_length_ (getdist get_grid_obj_ins_ ))
      ;
      ;preloop_and_while_ relength_gridline_dyn_blk
        (setq s_set_1_i 0)
        (while (< s_set_1_i (sslength s_set_1_))
          (setq s_set_1_ename_ (ssname s_set_1_ s_set_1_i))
          (setq s_set_1_obj_ (vlax-ename->vla-object s_set_1_ename_))
          (LM:setdynpropvalue s_set_1_obj_ "H" create_gridline_length_)
          (setq s_set_1_i (+ s_set_1_i 1))
        )
      ;
  )
  (defun c:z627_grid_line_length_GXX ()
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
      ;fillter_gridline_
        (setq s_set_0_ (TA:Prop_Filter_ss_set_ ss_pre_filter_set_xx "effectivename" "000-GRID_LINE_DYN" ))
        (setq s_set_1_ (TA:Prop_Filter_ss_set_ s_set_0_ "rotation" (deg-to-rad 90)))
        (sslength s_set_1_)
      ;
      ;make_length_
        (setq get_grid_ename_ (car (entsel)))
        (if (= get_grid_ename_ nil)
          (progn
            (setq get_grid_ename_ (car (car (reverse (TA:standard_list_croodinate_sorting ss_pre_filter_set_xx "Y")))))
          )
          (princ "\n")
        )
        (setq get_grid_obj_ (vlax-ename->vla-object get_grid_ename_))
        (setq get_grid_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint get_grid_obj_ ))))
        (setq create_gridline_length_ (getdist get_grid_obj_ins_ ))
      ;
      ;preloop_and_while_ relength_gridline_dyn_blk
        (setq s_set_1_i 0)
        (while (< s_set_1_i (sslength s_set_1_))
          (setq s_set_1_ename_ (ssname s_set_1_ s_set_1_i))
          (setq s_set_1_obj_ (vlax-ename->vla-object s_set_1_ename_))
          (LM:setdynpropvalue s_set_1_obj_ "H" create_gridline_length_)
          (setq s_set_1_i (+ s_set_1_i 1))
        )
      ;
  )
  (defun c:Dxm ()
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
      (command "dimstyle" "r" "PIA-DIM100")
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
      (setq lv_add (* lv_ 1))
      (setq sc (* scl 1)) ; 5 = 50 2 = 20
      (setq of_point 0.5); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 0.6); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
      (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget 
                          (list 
                            (cons 0 "INSERT") ;type of object
                            ; (cons 8 "000 - GRID")   ;kind of layer
                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                            ; (cons 62 1)           ;kind of color call sign with color code index
                          )
                        )
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) OF_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) OF_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) OF_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) OF_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth SRT-FILT_GLINE_set_ib SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  )
  (defun c:DDxm ()
    ;get_previous_var
      (setq old_osmode (getvar "osmode"))
      (command "dimstyle" "r" "PIA-DIM100")
    ;
    ;user_input_mode_val_for_gridline_direction
      (setq mode-direction-val nil)
      (while ;user_input_mode_val
        (not mode-direction-val)
        (if ;return_base_val_to_user_input
          (/= mode-direction-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-direction-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-direction-val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = 0 (honrizontal array) \nmode 2 = 90 (vertical array) \nmode 3 = Other<" (rtos (setq mode-direction-val-val (cond (mode-direction-val-val) (mode-direction-val) ) ) ) "> : " ) ) ) (mode-direction-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-direction-val 1)
            (/= mode-direction-val 2)
            (/= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
            (setq mode-direction-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-direction-val)
        )
        (if ;correct_case
          (or
            (= mode-direction-val 1)
            (= mode-direction-val 2)
            (= mode-direction-val 3)
          )
          (progn
            (setq mode-direction-val-val mode-direction-val )
          )
          (princ mode-direction-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-direction-val 1)
        )
        (progn
          (setq ori_rotate 0)
          (setq ori_direction "h")
        )
        )
        ((and
          (= mode-direction-val 2)
        )
        (progn
          (setq ori_rotate 90)
          (setq ori_direction "v")
        )
        )
        ((and
          (= mode-direction-val 3)
        )
        (progn
          (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
          (setq ori_direction "v")
        )
        )
      )
    ;
    ;user_input_mode_val_for_making_gridline_head_or_bot
      (setq mode_location_val nil)
      (while ;user_input_mode_val
        (not mode_location_val)
        (if ;return_base_val_to_user_input
          (/= mode-HorB nil)
          (progn
            (princ "/n")
          )
          (setq mode-HorB 1)
        )
        ;user_input_mode_val
          (setq mode_location_val (cond ( (getint (strcat "\nspecify_rotation_grid_line \nmode 1 = Dim at head \nmode 2 = Dim at bot \n<" (rtos (setq mode-HorB (cond (mode-HorB) (mode_location_val) ) ) ) "> : " ) ) ) (mode-HorB) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode_location_val 1)
            (/= mode_location_val 2)
            ; (/= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
            (setq mode_location_val nil)
            (alert "\n\n\n\Please input mode 1 or 2\n\n\n\n")
          )
          (princ mode_location_val)
        )
        (if ;correct_case
          (or
            (= mode_location_val 1)
            (= mode_location_val 2)
            ; (= mode_location_val 3)
          )
          (progn
            (setq mode-HorB mode_location_val )
          )
          (princ mode_location_val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode_location_val 1)
        )
        (progn
          ; (setq ori_rotate 0)
          (setq mk_dim_location "head")
        )
        )
        ((and
          (= mode_location_val 2)
        )
        (progn
          ; (setq ori_rotate 90)
          (setq mk_dim_location "bot")
        )
        )
        ; ((and
        ;   (= mode-val 3)
        ;  )
        ;  (progn
        ;   (setq ori_rotate (cond ( (getreal (strcat "\nspecify_rotation_grid_line<" (rtos (setq ori_rotate (cond (ori_rotate) (ori_rotate) ) ) ) "> : " ) ) ) (ori_rotate) ) )
        ;   (setq ori_locationion "v")
        ;  )
        ; )
      )
    ;
    ;use_input_dim_scale
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq lv_ (cond ( (getint (strcat "\nSpecify Level <" (rtos (setq lv_ (cond (lv_) (1.0) ) ) ) "> : " ) ) ) (lv_) ) )
      (setq lv_add (* lv_ 1))
      (setq sc (* scl 1)) ; 5 = 50 2 = 20
      (setq of_point 0.5); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 0.5); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (*(* (* scl 1) of_point) lv_add))
      (setq HI_DIM (*(* (* scl 1) hi_point) lv_add))
      (setvar "DIMSCALE" sc)
    ;
    ;selection_set
      (setq ss_fillter_set_ (ssget 
                          (list 
                            (cons 0 "INSERT") ;type of object
                            ; (cons 8 "000 - GRID")   ;kind of layer
                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                            ; (cons 62 1)           ;kind of color call sign with color code index
                          )
                        )
      )
    ;
    ;preloop_and_while filter_selection_set_
      (setq ori_efname "000-GRID_LINE_DYN")
      (setq ss_fillter_set_i 0)
      (setq FILT_GLINE_set_ ())
      (while
        (< ss_fillter_set_i (sslength ss_fillter_set_))
        (setq ss_fillter_set_ename (ssname ss_fillter_set_ ss_fillter_set_i))
        (setq ss_fillter_set_obj (vlax-ename->vla-object ss_fillter_set_ename))
        (setq ss_fillter_set_efname (LM:Effectivename ss_fillter_set_obj))
          (if 
            (and 
              (= ss_fillter_set_efname ori_efname)
              (= (atof (angtos (vla-get-rotation ss_fillter_set_obj))) ori_rotate)
            )
            (progn
              ;get_ins_data
                (setq base_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_fillter_set_obj))))
                (vla-getboundingbox ss_fillter_set_obj '1min_ins '2max_ins )
                (setq max_ins (vlax-safearray->list 2max_ins))
                (setq min_ins (vlax-safearray->list 1min_ins))
              ;
              ;assemble_ins_data
                (setq ename+ins 
                      (list 
                        ss_fillter_set_ename
                        (car base_ins)
                        (cadr base_ins)
                        (car max_ins)
                        (cadr max_ins)
                        (car min_ins)
                        (cadr min_ins)
                      )
                )
              ;
              ;summary_data_to_cons
                (setq FILT_GLINE_set_ (cons ename+ins FILT_GLINE_set_))
                (princ (strcat "\nfilter_selection_set_ is processing " (rtos (* ss_fillter_set_i (/ 100 (float (sslength ss_fillter_set_)))) 2 0) "%"))
                (setq FILT_GLINE_set_total_ (length FILT_GLINE_set_))
              ;
            )
            (princ "\n")
          )
        (setq ss_fillter_set_i (+ ss_fillter_set_i 1))
      )
      (princ (strcat "\nfilter_selection_set_ is processing " (rtos 100 2 0) "%") )
    ;
    ;sorting_ename_list
      (cond ;specify_efective_name_by_user_input
          ((and
            (= mode-direction-val 1)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (sort_by_X FILT_GLINE_set_)) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest (nth 0 (reverse (sort_by_higest FILT_GLINE_set_)))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest+scale (list ;sortting_highest_ename_
                                                    (nth 3 SRT-FILT_GLINE_set_highest)
                                                    (+ (nth 4 SRT-FILT_GLINE_set_highest) HI_DIM)
                                                  )
            )
            (setq SRT-FILT_GLINE_set_lowest-hon (nth 0 (sort_by_lowest_hon FILT_GLINE_set_)))
            (setq SRT-FILT_GLINE_set_lowest-hon+scale (list ;sortting_highest_ename_
                                                    (nth 5 SRT-FILT_GLINE_set_lowest-hon)
                                                    (- (nth 6 SRT-FILT_GLINE_set_lowest-hon) HI_DIM)
                                                  )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 2)
          )
          (progn
            (setq SRT-FILT_GLINE_set_ (reverse (sort_by_Y FILT_GLINE_set_))) ;sortting_ename
            (setq SRT-FILT_GLINE_set_highest-ver (nth 0 (sort_by_higest_ver FILT_GLINE_set_))) ;sortting_highest_ename_
            (setq SRT-FILT_GLINE_set_highest-ver+scale  (list ;sortting_highest_ename_
                                                          (- (nth 5 SRT-FILT_GLINE_set_highest-ver) HI_DIM)
                                                          (nth 6 SRT-FILT_GLINE_set_highest-ver)
                                                        )
            )
            (setq SRT-FILT_GLINE_set_lowest-ver (nth 0 (reverse (sort_by_lowest_ver FILT_GLINE_set_))))
            (setq SRT-FILT_GLINE_set_lowest-ver+scale (list ;sortting_highest_ename_                                                 
                                                        (+ (nth 3 SRT-FILT_GLINE_set_lowest-ver) HI_DIM)
                                                        (nth 4 SRT-FILT_GLINE_set_lowest-ver)
                                                      )
            )          
            (princ "your grid lines have been sorted by the x-axis neatly")
          )
          )
          ((and
            (= mode-direction-val 3)
          )
          (progn
            (alert "mode นี้ยากสุดแล้ว ยังทำไม่เสร็จ ขอโทษด้วย")
          )
          )
        )
    ;
    ;preloop_and_while_making_dimension
      (setq SRT-FILT_GLINE_set_ia 0)
      (setq SRT-FILT_GLINE_set_ib 1)
      (setvar "osmode" 0)
      (cond
        ((and ;0 dng + head
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;0 dng + bot
          (= mode-direction-val 1) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-hon+scale    
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + head
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 1) ;head or bot
        )
          (progn
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-a)
                      (nth 6 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 5 SRT-FILT_GLINE_set_list-b)
                      (nth 6 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_highest-ver+scale 
              )
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
          )
        )
        ((and ;90 dng + bot
          (= mode-direction-val 2) ;honrizontal or vertical
          (= mode_location_val 2) ;head or bot
        )
          (progn
            (setvar "dimtad" 1)
            (while
              ; (< SRT-FILT_GLINE_set_ia (- (length SRT-FILT_GLINE_set_) 1))
              (< SRT-FILT_GLINE_set_ia 1)
              (setq SRT-FILT_GLINE_set_list-a (nth SRT-FILT_GLINE_set_ia SRT-FILT_GLINE_set_))
              (setq SRT-FILT_GLINE_set_list-b (nth (- (length SRT-FILT_GLINE_set_) 1) SRT-FILT_GLINE_set_))
              
              (setq SRT-FILT_GLINE_set_list-aa 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-a)
                      (nth 4 SRT-FILT_GLINE_set_list-a)
                    )
              )
              (setq SRT-FILT_GLINE_set_list-bb 
                    (list
                      (nth 3 SRT-FILT_GLINE_set_list-b)
                      (nth 4 SRT-FILT_GLINE_set_list-b)
                    )
              )
              (command "dimlinear" 
                      SRT-FILT_GLINE_set_list-aa ;1st_ins_dim
                      SRT-FILT_GLINE_set_list-bb ;2nd_ins_dim
                      ori_direction ;directtion_ins_dim
                      SRT-FILT_GLINE_set_lowest-ver+scale 
              )
              (setq dim-last-ename (vlax-ename->vla-object (entlast)))
              (setq SRT-FILT_GLINE_set_ia (+ SRT-FILT_GLINE_set_ia 1))
              ; (setq SRT-FILT_GLINE_set_ib (+ SRT-FILT_GLINE_set_ib 1))
            )
            (setvar "dimtad" 1)
          )
        )
        
      )
      (setvar "osmode" 1215)
    ;
  )
  (defun c:z628_insert_grid_line_IGG ()
    
    ;user_input_
      (setq insertion_blk_point_1 (getpoint "specify point"))
      (setq insertion_blk_point_2 (getpoint insertion_blk_point_1 "specify point"))
      (setq insertion_blk_rotation_ (LM:round (rad-to-deg (angle insertion_blk_point_1 insertion_blk_point_2))))  
      (LM:round insertion_blk_rotation_)
      (setq insertion_blk_dist_ (distance insertion_blk_point_1 insertion_blk_point_2))
    ;
    ;value_for_insertion_command
      (setq sc_val_ 1)
    ;
    ;set_var_
      (setvar "osmode" 0)
      ; (command "-layer" "s" "000 - GRID" "")
      ; (command "-color" "bylayer" "")
    ;
    (cond ;insertion_girdlind_case
      (;gridline_case_1
          (or
            (= insertion_blk_rotation_ 360)
            (= insertion_blk_rotation_ 0)
            
         )
         (progn
           (command "insert"  "000-GRID_LINE_DYN" insertion_blk_point_1 sc_val_ 90 )
           (princ "gridline_case_1")
         )
      )
      (;gridline_case_2
         (or
            (= insertion_blk_rotation_ 270)
         )
         (progn
           (command "insert"  "000-GRID_LINE_DYN" insertion_blk_point_1 sc_val_ 0 )
           (princ "gridline_case_2")
         )
      )
      ; (;gridline_case_3
      ;    (and
      ;       ()
      ;       ()
      ;    )
      ;    (progn
      ;      ()
      ;      (princ "gridline_case_3")
      ;    )
      ; )
      ; (;gridline_case_4
      ;    (and
      ;       ()
      ;       ()
      ;    )
      ;    (progn
      ;      ()
      ;      (princ "gridline_case_4")
      ;    )
      ; )
    )
    
    ;put_data_dynnamic_grid_line
      (setq dynamic_blk_ename_ (entlast))
      (setq dynamic_blk_obj_ (vlax-ename->vla-object dynamic_blk_ename_))

      (vla-put-layer dynamic_blk_obj_ "000 - GRID" )
      (vla-put-color dynamic_blk_obj_ 256 )

      (LM:setdynpropvalue dynamic_blk_obj_ "H" insertion_blk_dist_)
      (LM:setdynpropvalue dynamic_blk_obj_ "Offset" 25)

    
    ;
    
    
    ;return_var
      (setvar "osmode" 1215)
    ;
  )
; 

(defun c:ALL_SETTING_PLOT_PROPERTIES_ALSP_ ()
  ; (setq dwg "NEW_BEE.dwg") ; เปลี่ยนเป็นชื่อไฟล์ที่ต้องการ
  (setq dwg (getvar "dwgname"))
  (setq layout-names (getdrawinglayouts dwg))
  (princ "\nรายชื่อ layouts ทั้งหมด:\n")
  (mapcar '(lambda (layout) (princ (strcat layout "\n"))) layout-names)
  (princ)
  (setq total_layout-names (length layout-names ))
  (setq i 1)
  (while 
    (< i total_layout-names)
    (setq layname_GO (nth i layout-names))
    (command "ctab" layname_GO)
    ; command_part
      (TA:runtoset_layout_)
      (defun TA:runtoset_layout_ ()
        ;set_var_plot
          ; (setq ss (car (entsel)))
          (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
          (setq lyout (vla-get-activelayout adoc))
          (setq styleSheet "TA3.ctb")
          (setq cfgName "AutoCAD PDF (High Quality Print).pc3")
          (vla-RefreshPlotDeviceInfo lyout)
          (vla-put-ConfigName lyout CfgName)
          
          (vla-put-PaperUnits lyout acMillimeters)
          (vla-put-plotrotation lyout ac0degrees)
          ; (vla-put-PlotType lyout acWindow)
          (vla-put-PlotType lyout acExtents)
          (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A4_(210.00_x_297.00_MM)")
          ; (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
          (vla-put-PlotWithLineweights lyout :vlax-true)
          (vla-put-PlotWithPlotStyles lyout :vlax-true)
          (vla-put-StyleSheet lyout styleSheet)
          (vla-put-CenterPlot lyout :vlax-true)
          (vla-put-standardscale lyout acScaleToFit )
          (vla-put-plotrotation lyout ac90degrees)
        
        
          (vla-put-PaperUnits lyout acMillimeters)
        ;
      )
    ; command_part
    (setq i (+ i 1))
  )
)



;9ตัวอย่าง
(defun c:cx1 ()
  (setq ent (car (entsel "\nSelect a DIMROTATED dimension: "))) ; เลือก DIMROTATED
  (setq entData (entget ent)) ; ดึงข้อมูลของ DIMROTATED

  ;; กำหนดค่าจุดใหม่ที่ต้องการ
  (setq newDimLinePoint (list 10 30000.0 -1600.0 0.0)) ; แก้ไข Dimension line point
  (setq newExtPoint1 (list 13 29990.0 -1600.0 0.0)) ; แก้ไข 1st extension defining point
  (setq newExtPoint2 (list 14 30010.0 -1600.0 0.0)) ; แก้ไข 2nd extension defining point

  ;; แก้ไขค่าจุดในรายการข้อมูล
  (setq entData (subst newDimLinePoint (assoc 10 entData) entData)) ; แก้ไข assoc 10
  (setq entData (subst newExtPoint1 (assoc 13 entData) entData)) ; แก้ไข assoc 13
  (setq entData (subst newExtPoint2 (assoc 14 entData) entData)) ; แก้ไข assoc 14

  ;; ส่งข้อมูลที่แก้ไขกลับไปที่วัตถุใน AutoCAD
  (entmod entData)
  (entupd ent) ; อัปเดตการแสดงผลของวัตถุใน AutoCAD
  
  (princ "\nDimension points modified successfully.")
  (princ)
)


