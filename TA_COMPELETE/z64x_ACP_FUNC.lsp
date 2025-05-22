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
;sub func
  (defun TA:Offset_line (ename num_ )
    ; (setq 1-PL_en_ (car (entsel)))
    (setq 1-PL_en_obj (vlax-ename->vla-object ename))
    (vla-Offset 1-PL_en_obj num_)
  )
  (defun TA:Get_as10 (A)
    (setq new-PL_en_get (entget (entlast)))
    (setq A (entlast))
    (setq new-PL_en_get_as10 (vl-remove-if-not '(lambda (x) (= 10 (car x))) new-PL_en_get))
    (setq new-PL_en_get_as10-vertex (mapcar 'cdr new-PL_en_get_as10))
    (setq bbb (cons (entlast) new-PL_en_get_as10-vertex))
  )
  (defun TA:Get_as10_type2 (ename_)
    (setq new-PL_en_get (entget ename_))
    (setq new-PL_en_get_as10 (vl-remove-if-not '(lambda (x) (= 10 (car x))) new-PL_en_get))
    (setq new-PL_en_get_as10-vertex (mapcar 'cdr new-PL_en_get_as10))
    (setq bbb (cons (entlast) new-PL_en_get_as10-vertex))
  )
  (defun TA:Join (A1 A2 A3 A4)
    (command "join" A1 A2 A3 A4 "")
  )
  (defun TA:midpoint (set1 set2)
    ; (setq set1 (list x1 y1))
    ; (setq set2 (list x2 y2))
    (setq set3 (list 
                 (/ (+ (car set1) (car set2)) 2.0)
                 (/ (+ (cadr set1) (cadr set2)) 2.0)
               )
    )
  )
  (defun create-pline (coords) 
    (if (= (length coords) 0) 
      nil
      (progn 
        ; สร้างพอลีไนจากค่าพิกัด
        (command "pline" (car coords))


        ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
        (foreach coord (cdr coords) 
          (command coord)
        )
      )
    )
  )
;
;main_func
  (defun c:z640_create_line_ACP ()
    (setvar "osmode" 0)
    ;user_input_mode_val_for_abs_val
      (setq mode-val nil)
      (while ;user_input_mode_val
        (not mode-val)
        (if ;return_base_val_to_user_input
          (/= mode-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-val (cond ( (getint (strcat "\nspecify_name_blk \nmode 1 = inside \nmode 2 = outside \n<" (rtos (setq mode-val-val (cond (mode-val-val) (mode-val) ) ) ) "> : " ) ) ) (mode-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-val 1)
            (/= mode-val 2)
          )
          (progn
            (setq mode-val-val mode-val )
            (setq mode-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-val)
        )
        (if ;correct_case
          (or
            (= mode-val 1)
            (= mode-val 2)
            (= mode-val 3)
          )
          (progn
            (setq mode-val-val mode-val )
          )
          (princ mode-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-val 1)
          )
          (progn
          (setq num_A 0.75)
          (setq num_B 3.25)
          (setq num_C 4.00)
          (setq num_D 2.50)
          )
        )
        ((and
          (= mode-val 2)
          )
          (progn
          (setq num_A -0.75)
          (setq num_B -3.25)
          (setq num_C -4.00)
          (setq num_D -2.50)
          )
        )
      )
    ;
    ;user_input_and_get_first_pick_pline_data
      (setq 1-PL_en_ (car (entsel)))
      (setq 1-PL_en_obj (vlax-ename->vla-object 1-PL_en_))
      (vla-copy 1-PL_en_obj)
      (setq 1-PL_en_get (entget 1-PL_en_))
      ;vla-getboundingbox
        (setq 1-PL_en_border_ (vla-getboundingbox 1-PL_en_obj 'ins_min 'ins_max))
        (setq 1-PL_en_border_min (vlax-safearray->list ins_min))
        (setq 1-PL_en_border_max (vlax-safearray->list ins_max))
      ;
      ;assoc 10
        (setq 1-PL_en_get_as10 (vl-remove-if-not '(lambda (x) (= 10 (car x))) 1-PL_en_get))
        (setq L1_vtx_pt_data (mapcar 'cdr 1-PL_en_get_as10))
        (setq L1_vtx_pt_data_ (cons 1-PL_en_ L1_vtx_pt_data))
      ;
    ;
    ;create_offset_pline
      (setvar "osmode" 0)
      (setq L2_vtx_pt_      (TA:Offset_line 1-PL_en_ num_A ))
      (setq L2_vtx_pt_data_  (TA:Get_as10 A))
      (setq L3_vtx_pt_      (TA:Offset_line 1-PL_en_ num_B ))
      (setq L3_vtx_pt_data_  (TA:Get_as10 B))
      (setq L4_vtx_pt_      (TA:Offset_line 1-PL_en_ num_C ))
      (setq L4_vtx_pt_data_  (TA:Get_as10 C))

      ; (setq L5_vtx_pt_      (TA:Offset_line 1-PL_en_ num_A ))
      ; (setq L5_vtx_pt_data_ (TA:Get_as10 D))
      ; (setq L6_vtx_pt_      (TA:Offset_line 1-PL_en_ num_B ))
      ; (setq L6_vtx_pt_data_ (TA:Get_as10 E))

      (setvar "osmode" 1215)
    ;
    ;preloop_and_while insert_acp_conner_front_sheet
      (setq Line_vtx_pt_data_start 1)
      (setq Line_vtx_pt_data_last (- (length L1_vtx_pt_data_) 1))
      (while  (< Line_vtx_pt_data_last (length L1_vtx_pt_data_))
        (setq L1_vtx_pt_list-s (nth Line_vtx_pt_data_start L1_vtx_pt_data_))
        (setq L2_vtx_pt_list-s (nth Line_vtx_pt_data_start L2_vtx_pt_data_))
        (setq L1_vtx_pt_list-l (nth Line_vtx_pt_data_last L1_vtx_pt_data_))
        (setq L2_vtx_pt_list-l (nth Line_vtx_pt_data_last L2_vtx_pt_data_))
        ; (setq L_ang (angtos (angle L1_vtx_pt_list L2_vtx_pt_list)))
        (command "line" L1_vtx_pt_list-s L2_vtx_pt_list-s "")
        (setq new_L_1 (entlast ))
        
        (command "line" L1_vtx_pt_list-l L2_vtx_pt_list-l "")
        
        (setq new_L_2 (entlast ))
        
        
          (TA:Join (car L1_vtx_pt_data_) (car L2_vtx_pt_data_) new_L_1 new_L_2 )
          (command "pselect" "")
          ; (command "join" (car L1_vtx_pt_data_)  (car L2_vtx_pt_data_) new_L_1 new_L_2 "") 
          ; (command "hatch" "ANSI32" 0.5 0 L1_vtx_pt_list-s "")
      
        (setq Line_vtx_pt_data_last (+ Line_vtx_pt_data_last 1))
      )
    ;
    ;preloop_and_while insert_acp_conner_back_sheet
      (setq Line_vtx_pt_data_start 1)
      (setq Line_vtx_pt_data_last (- (length L4_vtx_pt_data_) 1))
      (while  (< Line_vtx_pt_data_last (length L4_vtx_pt_data_))
        (setq L4_vtx_pt_list-s (nth Line_vtx_pt_data_start L4_vtx_pt_data_))
        (setq L3_vtx_pt_list-s (nth Line_vtx_pt_data_start L3_vtx_pt_data_))
        (setq L4_vtx_pt_list-l (nth Line_vtx_pt_data_last L4_vtx_pt_data_))
        (setq L3_vtx_pt_list-l (nth Line_vtx_pt_data_last L3_vtx_pt_data_))
        ; (setq L_ang (angtos (angle L1_vtx_pt_list L2_vtx_pt_list)))
        (command "line" L4_vtx_pt_list-s L3_vtx_pt_list-s "")
        (setq new_L_4 (entlast ))
        
        (command "line" L4_vtx_pt_list-l L3_vtx_pt_list-l "")
        (setq new_L_3 (entlast ))
        
        
        (TA:Join (car L4_vtx_pt_data_) (car L3_vtx_pt_data_) new_L_4 new_L_3 )
        ; (command "join" (car L1_vtx_pt_data_)  (car L2_vtx_pt_data_) new_L_1 new_L_2 "") 
        ; (command "hatch" "ANSI32" 0.5 0 L4_vtx_pt_list-s "")
      
        (setq Line_vtx_pt_data_last (+ Line_vtx_pt_data_last 1))
      )
    ;
    ;middle_
      ;preloop_and_while 
        (setvar "osmode" 0)
        (setq remove_num_ (nth 0 L2_vtx_pt_data_))
        (setq L5_vtx_pt_data_ (vl-remove remove_num_ L2_vtx_pt_data_))
        (create-pline L5_vtx_pt_data_ )
        (command "")
        (setq L5_ename (entlast))

        (setq L6_vtx_pt_      (TA:Offset_line L5_ename num_D))
        (setq L6_vtx_pt_data_ (TA:Get_as10 E))
        (setq remove_num_ (nth 0 L6_vtx_pt_data_))
        (setq L6_vtx_pt_data_ (vl-remove remove_num_ L6_vtx_pt_data_))
        (setq L6_ename (entlast))

        (setq Line_vtx_pt_data_start 0)
        (setq Line_vtx_pt_data_last (- (length L5_vtx_pt_data_) 1))
        (while  (< Line_vtx_pt_data_last (length L5_vtx_pt_data_))
          (setq L5_vtx_pt_list-s (nth Line_vtx_pt_data_start L5_vtx_pt_data_))
          (setq L6_vtx_pt_list-s (nth Line_vtx_pt_data_start L6_vtx_pt_data_))
          (setq L5_vtx_pt_list-l (nth Line_vtx_pt_data_last L5_vtx_pt_data_))
          (setq L6_vtx_pt_list-l (nth Line_vtx_pt_data_last L6_vtx_pt_data_))
          ; (setq L_ang (angtos (angle L1_vtx_pt_list L2_vtx_pt_list)))
          (command "line" L5_vtx_pt_list-s L6_vtx_pt_list-s "")
          (setq new_L_5 (entlast ))
          
          (command "line" L5_vtx_pt_list-l L6_vtx_pt_list-l "")
          (setq new_L_6 (entlast ))
          
          
          (TA:Join L5_ename L6_ename new_L_5 new_L_6 )
          ; (command "join" (car L1_vtx_pt_data_)  (car L2_vtx_pt_data_) new_L_1 new_L_2 "") 
          ; (command "hatch" "cross" 1 0 L5_vtx_pt_list-s "")
    
        
          (setq Line_vtx_pt_data_last (+ Line_vtx_pt_data_last 1))
        )
    
    ;
    ;preloop_and_while add rotation val
      (setq L1_vtx_pt_data_ename (nth 0 L1_vtx_pt_data_))
      (setq L1_vtx_pt_data_num_ (vl-remove L1_vtx_pt_data_ename L1_vtx_pt_data_))
      (setq L2_vtx_pt_data_ename (nth 0 L2_vtx_pt_data_))
      (setq L2_vtx_pt_data_num_ (vl-remove L2_vtx_pt_data_ename L2_vtx_pt_data_))
      (setq L1_vtx_pt_data_num_i 0)
      (setq L1_vtx_pt_data_num_ii 1)
      (setq ins_conner_data_ ())
      (while (< L1_vtx_pt_data_num_i (length L1_vtx_pt_data_num_))
        ;get_data
          (setq num_list1 (nth L1_vtx_pt_data_num_i L1_vtx_pt_data_num_))
          (setq num_list2 (nth L1_vtx_pt_data_num_ii L1_vtx_pt_data_num_))
          (setq num_list3 (nth L1_vtx_pt_data_num_i L2_vtx_pt_data_num_))
        ;
        ;analyze_data
          ;
            ; (if (< (- L1_vtx_pt_data_num_i 1) 0) 
            ;   (progn 
            ;     0
            ;   )
            ;   (setq analysis_pre_ang (atof (angtos (angle (nth (- L1_vtx_pt_data_num_i 1) L1_vtx_pt_data_num_) num_list3))))
            ; )
          ;
          (if (/= num_list2 nil)
            (progn
              (setq analysis_next_ang (atof (angtos (angle num_list1 num_list2 ))))
            )
            (prin1 "\n")
          )
          ; (command "point" num_list1)
          ; (command "pselect" "L" "")
          ; (command "point" num_list2)
          ; (command "pselect" "L" "")

          (setq conner_ang (atof (angtos (angle num_list1 num_list3 ))))
        ;
        ;mk_list
          (setq mk_list (list
                          num_list1
                          conner_ang

                          analysis_next_ang
                          
                        )
          )
          (setq ins_conner_data_ (cons mk_list ins_conner_data_))
          
        ; 
        (setq L1_vtx_pt_data_num_i (+ L1_vtx_pt_data_num_i 1))
        (setq L1_vtx_pt_data_num_ii (+ L1_vtx_pt_data_num_ii 1))
      )
      (setq ins_conner_data_ (reverse ins_conner_data_))
      (length ins_conner_data_ )
    ;
    ;preloop_and_wihle_ insert_ACP_CONNER
      ;mk_selection_set 
        (setq ename-list '())
        (setq dyn_block_ (ssadd))
      ;
      (setq ins_conner_data_i 1)
      (while (< ins_conner_data_i (- (length ins_conner_data_) 1))
        ;get_data
          (setq ins_conner_data_list (car (nth ins_conner_data_i ins_conner_data_)))
          (setq L_ang_val (cadr (nth ins_conner_data_i ins_conner_data_)))
          
          (setq analize_ang (caddr (nth ins_conner_data_i ins_conner_data_)))
          (setq pre_analize_ang (caddr (nth (- ins_conner_data_i 1) ins_conner_data_)))
        ;
        ;insert_commmad and get_data
          (command "insert" "ACP_CONNER 1-1" ins_conner_data_list 1 L_ang_val)
          (setq conner_ename (entlast))
          (setq conner_obj (vlax-ename->vla-object (entlast)))
        ;
        ;mk_selection_ss
          (setq ename-list (cons conner_ename ename-list))
          (foreach ename ename-list
            (ssadd ename dyn_block_)
          )
          (sslength dyn_block_)
        ;
        ; (setq ins_conner_data_i (+ ins_conner_data_i 1))
        ;logic for adjust flip
          (if (= mode-val 1) 
            (progn
            (cond
              ((and (= analize_ang 0) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 0) (= pre_analize_ang 90)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0)
              )
              )
              ((and (= analize_ang 0) (= pre_analize_ang 270)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 90) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 90) (= pre_analize_ang 180)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 180) (= pre_analize_ang 90)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 180) (= pre_analize_ang 270)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 270) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 270) (= pre_analize_ang 180)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
            ) 
            )
            (cond
              ((and (= analize_ang 0) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 0) (= pre_analize_ang 90)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1)
              )
              )
              ((and (= analize_ang 0) (= pre_analize_ang 270)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 90) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 90) (= pre_analize_ang 180)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 180) (= pre_analize_ang 90)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
              ((and (= analize_ang 180) (= pre_analize_ang 270)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 270) (= pre_analize_ang 0)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 1 )
              )
              )
              ((and (= analize_ang 270) (= pre_analize_ang 180)
              ) 
              (progn (LM:toggleflipstate conner_obj) (LM:setdynpropvalue conner_obj "Flip State1" 0 )
              )
              )
            )             
          )
        ;

      
        (setq ins_conner_data_i (+ ins_conner_data_i 1))
      )
    ;
    ; ;preloop_and_while flip_data
    ;   (if (= mode-val 2)
    ;     (progn
    ;       (setq dyn_block_i 0)
    ;       (while (< dyn_block_i (sslength dyn_block_))
    ;         (setq dyn_block_ename (ssname dyn_block_ dyn_block_i))
    ;           (setq dyn_block_obj (vlax-ename->vla-object dyn_block_ename))
    ;           (if (= (LM:toggleflipstate dyn_block_obj) 0)
    ;             (progn
    ;               (LM:setdynpropvalue conner_obj "Flip State1" 1)
    ;             )
    ;             (LM:setdynpropvalue conner_obj "Flip State1" 0)
    ;           )
    ;         (setq dyn_block_i (+ dyn_block_i 1))
    ;       )
    ;     )   
    ;     (princ "\n")
    ;   )
    ; ;
    ;; old_logic
      ; ;preloop_and_while insert_acp_conner
      ;   (setq Line_vtx_pt_data_i 2)
      ;   (while  (< Line_vtx_pt_data_i (- (length L1_vtx_pt_data_) 1))
      ;     (setq L1_vtx_pt_list (nth Line_vtx_pt_data_i L1_vtx_pt_data_))
      ;     (setq L2_vtx_pt_list (nth Line_vtx_pt_data_i L2_vtx_pt_data_))
      ;     (setq L_ang (angtos (angle L1_vtx_pt_list L2_vtx_pt_list)))
      ;     ;insert_data
      ;       (command "insert" "ACP_CONNER_1" L1_vtx_pt_list 1 L_ang) 
      ;     ; get_data
      ;       (setq insert_obj (vlax-ename->vla-object (entlast)))
      ;       ; (LM:SETDYNPROPVALUE insert_obj "ANGLE"  "NO")
      ;       (LM:SETDYNPROPVALUE insert_obj "ANGLE"  (rtos (atoi L_ang)  2 0))
      ;     ;
      ;     (setq Line_vtx_pt_data_i (+ Line_vtx_pt_data_i 1))
      ;   )
      ; ;
    ;; 
    (setvar "osmode" 1215)
  )
  (defun c:z641_create_hatch_ACP ()
    ;sub_func
      (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
    ;
    ;selection_set_pline
      (setq ss_pline_ (ssget 
                                (list 
                                  (cons 0 "POLYLINE,LWPOLYLINE") ;type of object
                                  ; (cons 8 "000 - GRID")   ;kind of layer
                                  ; (cons 2 "SSSS")       ;kind of nameblock
                                  ; (cons 62 1)           ;kind of color call sign with color code index
                                )
                              )
      )
    ;
    ;preloop_and_while_
      (setq ss_pline_i 0)
      (setq pline_total_area ())
      (while  (< ss_pline_i (sslength ss_pline_))
        (setq ss_pline_ename_ (ssname ss_pline_ ss_pline_i))
        ;get_data_pline
          (setq ss_pline_obj_ (vlax-ename->vla-object ss_pline_ename_))
          (setq ss_pline_area (vla-get-area ss_pline_obj_))
          (setq ss_pline_closed (vla-get-closed ss_pline_obj_))
        ;
        ;lsit_and_cons_data
          (if (= ss_pline_closed :vlax-true)
            (progn
              (setq pline_list (list 
                                ss_pline_ename_
                                ss_pline_area
                              )
              )
              (setq pline_total_area (cons pline_list pline_total_area))
            )
            (princ "\n")
          )
        ;
        (setq ss_pline_i (+ ss_pline_i 1))
      )
    ;
    ;preloop_and_while_plastic
      (setq sorted_PL_TL_AR_ (sort_by_X  pline_total_area ))
      (setq sorted_PL_TL_AR_i 0)
      (while  (< sorted_PL_TL_AR_i (- (length sorted_PL_TL_AR_) 1))
        (setq sorted_PL_TL_AR_list_ename_ (car (nth sorted_PL_TL_AR_i sorted_PL_TL_AR_)))
        (setq sorted_PL_TL_AR_list_num_ (cadr (nth sorted_PL_TL_AR_i sorted_PL_TL_AR_)))
          ;change_prop_hatch_set
            (command "hatch" "ANSI32" 0.5 0 sorted_PL_TL_AR_list_ename_ "")
            (command "CHPROP" (entlast) "" "LA" "000 - H A T C H" "")
            (command "CHPROP" (entlast) "" "C" 8 "")
            (command "CHPROP" (entlast) "" "LT" "bylayer" "") 
            (command "draworder" (entlast) "" "b")
          ;
          ;change_prop_line_set
            (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LA" "A01_ACP" "")
            (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "C" 3 "")
            (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LT" "bylayer" "") 
            (command "draworder" sorted_PL_TL_AR_list_ename_ "" "b")
          ;
          ;สำหรับใส่ชุดคำสั่งเปลี่ยนสีในอนาคต 
        (setq sorted_PL_TL_AR_i ( + sorted_PL_TL_AR_i 1))
      )
      (setq sorted_PL_TL_AR_list_ename_ (car (nth 2 sorted_PL_TL_AR_)))
      (setq sorted_PL_TL_AR_list_num_ (cadr (nth 2 sorted_PL_TL_AR_)))
      (command "hatch" "cross" 1 0 sorted_PL_TL_AR_list_ename_ "")
      ;change_prop_hatch_set
        (command "CHPROP" (entlast) "" "LA" "000 - H A T C H" "")
        (command "CHPROP" (entlast) "" "C" 8 "")
        (command "CHPROP" (entlast) "" "LT" "bylayer" "") 
        (command "draworder" (entlast) "" "b")
      ;
      ;change_prop_line_set
        (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LA" "A01_ACP" "")
        (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "C" 1 "")
        (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LT" "bylayer" "") 
        (command "draworder" sorted_PL_TL_AR_list_ename_ "" "b") 
        (command "draworder" sorted_PL_TL_AR_list_ename_ "" "b")
      ;

    ;
  )
  (defun c:z642_create_fold_ACP ()
    ;user_input_entsel and sub_func_for summary vertex in object
      (setq line_en (car (entsel)))
      (setq L1_vtx_ (TA:Get_as10_type2 line_en))
      ; (command "pselect" (car L1_vtx_))
    ;
    ;preloop_and_while summary_data for insertion acp_fold
      (setq L1_vtx_i 1)
      (setq L1_vtx_ii 2)
      (setq ins_data_ ())
      (while (< L1_vtx_i (- (length L1_vtx_) 1))
        ;get_data
          (setq insertion_mid_pt_val_ 
              (TA:midpoint (nth L1_vtx_i L1_vtx_) (nth L1_vtx_ii L1_vtx_))
          )
          (setq dist_val_         
                  (distance (nth L1_vtx_i L1_vtx_) (nth L1_vtx_ii L1_vtx_))
          )
          (setq ang_val_ 
                  (atof (angtos (angle (nth L1_vtx_i L1_vtx_) (nth L1_vtx_ii L1_vtx_))))
          )
        (setq ins_data  (list
                          insertion_mid_pt_val_
                          dist_val_
                          ang_val_
                        )
        )
        ;
        ;summary_cons
          (setq ins_data_ (cons ins_data ins_data_))
        ;
        (setq L1_vtx_i  (+ L1_vtx_i 1))
        (setq L1_vtx_ii (+ L1_vtx_ii 1))
      )
    ;
    ;preloop_and_while_intersection_
      (setq ins_data_i 1)
      (setq ename_ss (ssadd))
      (while  (< ins_data_i (- (length ins_data_) 1))
        ;get_data
          (setq ins_data_list (nth ins_data_i ins_data_))
          (if (< (- ins_data_i 1) 0)
            (progn
              (setq pre_ins_data_list (nth 0 ins_data_))
            )
            (setq pre_ins_data_list (nth (- ins_data_i 1) ins_data_))
          )
          (command "insert" "001 - DYNAMIC ACP FOLDING" (car ins_data_list) 1 (caddr ins_data_list))
          (setq insert_ename (entlast))
          (setq insert_obj (vlax-ename->vla-object (entlast)))
          (LM:setdynpropvalue insert_obj "distance2" (- (cadr ins_data_list) 10))
          (if (or
                (<= (cadr ins_data_list) 40)
                ; (<= (cadr pre_ins_data_list) 50)
              )
            (progn
              (vla-erase insert_obj)
            )
            (princ "\n")
          )
        ;
        ;mk_ss_set
          (setq SS_ (ssadd insert_ename ename_ss ) )
          (sslength ename_ss)
        ;
        (setq ins_data_i (+ ins_data_i 1))
      )
      (command "pselect" SS_ "")
    ;
  )
;


(defun c:z643_ins_text_to_pt ()
  ;sub_func_add_text
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
  ;user_input_ename
    (setq line_ename_ (car (entsel)))
    (setq text_height (cond ( (getreal (strcat "\nspecify text height<" (rtos (setq text_height (cond (text_height) (1.0) ) ) ) "> : " ) ) ) (text_height) ) )
  ;
  ;get_as10_data_and_remove_ename (1st list)
    (setq line_ename_get10_list_ (TA:Get_as10_type2 line_ename_))
    (setq line_ename_get10_ename (nth 0 line_ename_get10_list_))
    (setq line_ename_get10_list_num_ (vl-remove line_ename_get10_ename line_ename_get10_list_ ))
  ;
  ;preloop_and_while_analysis_data
    (setq line_ename_get10_list_num_A ())
    (setq line_ename_get10_list_num_i 0)
    (setq line_ename_get10_list_num_ii 1)
    (while (< line_ename_get10_list_num_i (- (length line_ename_get10_list_num_) 1))
      (setq front_list  (nth line_ename_get10_list_num_i line_ename_get10_list_num_))
      (setq back_list   (nth line_ename_get10_list_num_ii line_ename_get10_list_num_))
      (setq analysis_next_ang (atof (angtos (angle front_list back_list ))))
      
      (setq line_ename_get10_list_num_i (+ line_ename_get10_list_num_i 1))
      (setq line_ename_get10_list_num_ii (+ line_ename_get10_list_num_ii 1))
      (setq sum_data (list
                      front_list
                      analysis_next_ang
                      )
      )
      (setq line_ename_get10_list_num_A (cons sum_data line_ename_get10_list_num_A))
      (length line_ename_get10_list_num_A)
    )
  ;
  ;preloop_and_while_iinsertion_text_to_point
    (setq line_ename_get10_list_num_A (reverse line_ename_get10_list_num_A))
    (setq line_ename_get10_list_num_i 0)
    (while (< line_ename_get10_list_num_i (length line_ename_get10_list_num_A))
      (setq num_list (car (nth line_ename_get10_list_num_i line_ename_get10_list_num_A)))
      (setq text (strcat (rtos (car num_list) 2 2) "," (rtos (cadr num_list) 2 2)))
      (create-text-vla  
        (setq ins_pt (vlax-3d-point 
                      (list 
                        (car num_list)
                        (cadr num_list)
                        0
                      )
                    )
        )
        (strcat 
          (rtos (car num_list) 2 2) "____" 
          (rtos (cadr num_list) 2 2 ) 
          "__________["
          (rtos (cadr (nth (if 
                             (< (- line_ename_get10_list_num_i 1) 0)
                             (progn
                              0
                             )
                             (- line_ename_get10_list_num_i 1)
                           ) 
                           line_ename_get10_list_num_A)) 2 2)
          "]"
          "___["
          (rtos (cadr (nth line_ename_get10_list_num_i line_ename_get10_list_num_A)) 2 2)
          
          "]"
        ) 
        text_height
        120
      )
      (setq line_ename_get10_list_num_i (+ line_ename_get10_list_num_i 1))
    )
  ;
) 
(vl-load-com)
(defun c:Example_AddText()
    ;; This example creates a text object in model space.
    
  
    ;; Define the text object
    (setq insertionPoint (vlax-3d-point 2 2 0)  
          textString "Hello, World."
          height 0.5)
    
    ;; Create the text object in model space
    
    (setq textObj (vla-AddText modelSpace textString insertionPoint height))  
    (vla-ZoomAll acadObj)
)



