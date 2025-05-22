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
  (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth 0 (cadr a)) (nth 0(cadr b)))))))
  )
(defun c:xt1_xtemp1 ()
  ;user_input_set PLINE
    (setq ss_LWPLINE_set_ename1 (car (entsel)))
    (setq ss_LWPLINE_set_ename2 (car (entsel)))
  ;
  ;get_data for pline_get_10th set1
    (setq pline_vertex_data_T1 ())
    (setq ss_LWPLINE_set_get (entget ss_LWPLINE_set_ename1))
    (setq ss_LWPLINE_set_get_10th (vl-remove-if-not '(lambda (x) (= 10 (car x))) ss_LWPLINE_set_get))
    (setq pline_vertex_data_T1 ss_LWPLINE_set_get_10th)
  ;
  ;get_data for pline_get_10th set2
    (setq pline_vertex_data_T2 ())
    (setq ss_LWPLINE_set_get (entget ss_LWPLINE_set_ename2))
    (setq ss_LWPLINE_set_get_10th (vl-remove-if-not '(lambda (x) (= 10 (car x))) ss_LWPLINE_set_get))
    (setq pline_vertex_data_T2 ss_LWPLINE_set_get_10th)
  ;
  ;pre_loop_and_while calculate_rotaion_data
    (setq pline_vertex_data_T1_i 1)
    (while (< pline_vertex_data_T1_i (- (length pline_vertex_data_T1) 1))
      ;get_data
        (setq pline_vertex_data_list_T1 (cdr (nth pline_vertex_data_T1_i pline_vertex_data_T1)))
        (setq pline_vertex_data_list_T2 (cdr (nth pline_vertex_data_T1_i pline_vertex_data_T2)))
      ;
      ;indicate_data_for_slope
  
        (setq rotation_val (angtos (angle pline_vertex_data_list_T1 pline_vertex_data_list_T2)))  
            
      ;
      ;insert_data
        (command "insert" "ACP_CONNER_1" pline_vertex_data_list_T1 1 rotation_val) 
        ; get_data
          (setq insert_obj (vlax-ename->vla-object (entlast)))
          (LM:SETDYNPROPVALUE insert_obj "ANGLE"  (rtos (atoi rotation_val)  2 0))
        ;
      ;
      (setq pline_vertex_data_T1_i (+ pline_vertex_data_T1_i 1))
    )
    
    (setq pline_vertex_data_i (+ pline_vertex_data_i 1))
  ;
)
(defun c:xt2_xtemp1 ()
  ;user_input_set PLINE
    (setq ss_LWPLINE_set_ename1 (car (entsel)))
    (setq ss_LWPLINE_set_ename2 (car (entsel)))
  ;
  ;get_data for pline_get_10th set1
    (setq pline_vertex_data_T1 ())
    (setq ss_LWPLINE_set_get (entget ss_LWPLINE_set_ename1))
    (setq ss_LWPLINE_set_get_10th (vl-remove-if-not '(lambda (x) (= 10 (car x))) ss_LWPLINE_set_get))
    (setq pline_vertex_data_T1 ss_LWPLINE_set_get_10th)
  ;
  ;get_data for pline_get_10th set2
    (setq pline_vertex_data_T2 ())
    (setq ss_LWPLINE_set_get (entget ss_LWPLINE_set_ename2))
    (setq ss_LWPLINE_set_get_10th (vl-remove-if-not '(lambda (x) (= 10 (car x))) ss_LWPLINE_set_get))
    (setq pline_vertex_data_T2 ss_LWPLINE_set_get_10th)
  ;
  ;pre_loop_and_while calculate_rotaion_data
    (setq pline_vertex_data_T1_i (- (length pline_vertex_data_T1) 1))
    (while (< pline_vertex_data_T1_i (length pline_vertex_data_T1))
      ;get_data
        (setq pline_vertex_data_list_T1 (cdr (nth 0 pline_vertex_data_T1)))
        (setq pline_vertex_data_list_T2 (cdr (nth 0 pline_vertex_data_T2)))
        (setq pline_vertex_data_list_T3 (cdr (nth (- (length pline_vertex_data_T1) 1) pline_vertex_data_T1)))
        (setq pline_vertex_data_list_T4 (cdr (nth (- (length pline_vertex_data_T2) 1) pline_vertex_data_T2)))
      ;
      ;indicate_data_for_slope
  
        (setq rotation_val (angtos (angle pline_vertex_data_list_T1 pline_vertex_data_list_T2)))  
            
      ;
      ;insert_data
        (command "line" pline_vertex_data_list_T1 pline_vertex_data_list_T2 "" )
          (setq insert_ename_1 (entlast))
        (command "line" pline_vertex_data_list_T3 pline_vertex_data_list_T4 "" )
          (setq insert_ename_2 (entlast))
        ; (command "pselect" ss_LWPLINE_set_ename1 ss_LWPLINE_set_ename2 insert_ename_1 insert_ename_2)
        (command "join" ss_LWPLINE_set_ename1 ss_LWPLINE_set_ename2 insert_ename_1 insert_ename_2 "")
        ; get_data

        ;
      ;
      (setq pline_vertex_data_T1_i (+ pline_vertex_data_T1_i 1))
    )
)

(defun c:xt3_temp ()
  (setq ss_LWPLINE_set_ename1 (vlax-ename->vla-object (car (entsel))))
  (command "pselect" ss_LWPLINE_set_ename1 "")
  (setq ss (getpoint))
  (command "offset" ss 15 ss_LWPLINE_set_ename1 15 "" "")
  
  (setq points (vlax-make-safearray vlax-vbDouble '(0 . 11)))
  (vla-offset ss_LWPLINE_set_ename1 (vlax-3d-point 15 0 0))
)
    
  ;




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
(defun TA:Get_as10_type2 (A)
  (setq new-PL_en_get (entget A))
  (setq new-PL_en_get_as10 (vl-remove-if-not '(lambda (x) (= 10 (car x))) new-PL_en_get))
  (setq new-PL_en_get_as10-vertex (mapcar 'cdr new-PL_en_get_as10))
  (setq bbb (cons (entlast) new-PL_en_get_as10-vertex))
)
(defun TA:Join (A1 A2 A3 A4)
  (command "join" A1 A2 A3 A4 "")
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

(defun c:xt4_temp1 ()
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
  ;preloop_and_while insert_acp_conner
    (setq Line_vtx_pt_data_i 2)
    (while  (< Line_vtx_pt_data_i (- (length L1_vtx_pt_data_) 1))
      (setq L1_vtx_pt_list (nth Line_vtx_pt_data_i L1_vtx_pt_data_))
      (setq L2_vtx_pt_list (nth Line_vtx_pt_data_i L2_vtx_pt_data_))
      (setq L_ang (angtos (angle L1_vtx_pt_list L2_vtx_pt_list)))
      ;insert_data
        (command "insert" "ACP_CONNER_1" L1_vtx_pt_list 1 L_ang) 
      ; get_data
        (setq insert_obj (vlax-ename->vla-object (entlast)))
        ; (LM:SETDYNPROPVALUE insert_obj "ANGLE"  "NO")
        (LM:SETDYNPROPVALUE insert_obj "ANGLE"  (rtos (atoi L_ang)  2 0))
      ;
      (setq Line_vtx_pt_data_i (+ Line_vtx_pt_data_i 1))
    )
  ;
  
)
(defun c:xt5_temp ()
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
  ;preloop_and_while
    (setq ss_pline_i 0)
    (setq pline_total_area ())
    (while  (< ss_pline_i (sslength ss_pline_))
      (setq ss_pline_ename_ (ssname ss_pline_ ss_pline_i))
      ;get_data_pline
        (setq ss_pline_obj_ (vlax-ename->vla-object ss_pline_ename_))
        (setq ss_pline_area (vla-get-area ss_pline_obj_))
      ;
      ;lsit_and_cons_data
        (setq pline_list (list 
                          ss_pline_ename_
                          ss_pline_area
                        )
        )
        (setq pline_total_area (cons pline_list pline_total_area) )
      ;
      (setq ss_pline_i (+ ss_pline_i 1))
    )
  ;
  ;preloop_and_while_
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
        ;
        ;change_prop_line_set
          (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LA" "A01_ACP" "")
          (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "C" 3 "")
          (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LT" "bylayer" "") 
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
    ;
    ;change_prop_line_set
      (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LA" "A01_ACP" "")
      (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "C" 1 "")
      (command "CHPROP" sorted_PL_TL_AR_list_ename_ "" "LT" "bylayer" "") 
    ;

  ;
)


(defun TA:insert_acp_fold (en)
  ; (setq line_ename_obj (vlax-ename->vla-object (car (entsel))))
  (setq line_ename_obj (vlax-ename->vla-object en))
  (setq line_vrx_data (TA:Get_as10 en))
  
  (setq Dist1st  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getStartPoint line_ename_obj)))
  (setq Dist2nd  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getEndPoint line_ename_obj)))
  (setq lineLen (- Dist2nd Dist1st))
  (setq PointMid (vlax-curve-getPointAtDist line_ename_obj (+ Dist1st (* 0.5 lineLen))))
)

(defun c:xt6_temp ()
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
  (setq ins_data_i 0)
  (setq ename_ss (ssadd))
  (while  (< ins_data_i (length ins_data_))
    ;get_data
      (setq ins_data_list (nth ins_data_i ins_data_))
      (command "insert" "A$C49443ade" (car ins_data_list) 1 (caddr ins_data_list))
      (setq insert_ename (entlast))
      (setq insert_obj (vlax-ename->vla-object (entlast)))
      (LM:setdynpropvalue insert_obj "distance2" (- (cadr ins_data_list) 20))
    ;
    (setq ins_data_i (+ ins_data_i 1))
    ;mk_ss_set
    (setq SS_ (ssadd insert_ename ename_ss ) )
    (sslength ename_ss)
  )
  (command "pselect" SS_ "")
)

;



  (setq line_ename_obj (vlax-ename->vla-object (car (entsel))))
  (setq Dist1st  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getStartPoint line_ename_obj)))
  (setq Dist2nd  (vlax-curve-getDistAtPoint line_ename_obj (vlax-curve-getEndPoint line_ename_obj)))
  (setq lineLen (- Dist2nd Dist1st))
  (setq PointMid (vlax-curve-getPointAtDist line_ename_obj (+ Dist1st (* 0.5 lineLen))))

  (defun TA:midpoint (set1 set2)
    ; (setq set1 (list x1 y1))
    ; (setq set2 (list x2 y2))
    (setq set3 (list 
                 (/ (+ (car set1) (car set2)) 2.0)
                 (/ (+ (cadr set1) (cadr set2)) 2.0)
               )
    )
  )
  (setq set1 (nth 1 L1_vtx_))
  (setq set2 (nth 2 L1_vtx_))
  (TA:midpoint (nth 1 L1_vtx_) (nth 2 L1_vtx_))


  (defun DOF_DOUBLE_OFFSET (ename_offset_obj)
    (setq ename_offset_obj (vlax-ename->vla-object (car( entsel))))
    (vla-offset ename_offset_obj 5)
    (vla-offset ename_offset_obj -5)
    (vla-erase ename_offset_obj)
  )
    (defun c:SSDOF_Selection_set_DOUBLE_OFFSET ()
      ;sub_func
        (defun DOF_DOUBLE_OFFSET (ename_offset_obj) 
          (setq ename_offset_obj (vlax-ename->vla-object (car (entsel))))
          (vla-offset ename_offset_obj 5)
          (vla-offset ename_offset_obj -5)
          (vla-erase ename_offset_obj)
        )
      ;
      ;main_func
        (setq ss_xline_ (ssget 
                                  (list 
                                    (cons 0 "xline") ;type of object
                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                    ; (cons 2 "SSSS")       ;kind of nameblock
                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                  )
                                )
        )
        (setq ss_xline_i 0)
        (while  (< ss_xline_i (sslength ss_xline_))
          (setq ss_xline_ename (ssname ss_xline_ ss_xline_i)
                ss_xline_obj (vlax-ename->vla-object ss_xline_ename)
          )
          (DOF_DOUBLE_OFFSET ss_xline_obj)
          (setq ss_xline_i (+ ss_xline_i 1))
        )
      ;
    )




