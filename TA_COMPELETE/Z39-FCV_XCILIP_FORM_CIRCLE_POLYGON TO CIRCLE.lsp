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

;
;convert_value_
  (defun deg-to-rad (angle)
    (* angle (/ pi 180.0))
  )
  (defun rad-to-deg (rad)
    (/ (* rad 180) pi) 
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
                                        (cons 0 "lwpolyline") ;type of object
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
;


(defun c:FCEN_FIND_CENTER ( / ss )
    (if
        (setq ss
            (ssget "_+.:E:S:L"
               '(
                    (0 . "LWPOLYLINE")
                    (-4 . "&=")
                    (70 . 1)
                    (-4 . "<NOT")
                    (-4 . "<>")
                    (42 . 0.0)
                    (-4 . "NOT>")
                )
            )
        )
        (entmake (list '(0 . "POINT") (cons 10 (LM:PolyCentroid (ssname ss 0)))))
    )
    (princ)
)
(defun c:xcv_xcilp_circle ()
    
  (setvar "osmode" 0)
    (setq SEG_XC (cond ( (getint (strcat "\nSpecify SEGMENT <" (rtos (setq SEG_XC (cond (SEG_XC) (1.0) ) ) ) "> : " ) ) ) (SEG_XC) ) )
    ; select circle part
      (setq REF_CIRCLE nil)
      (while (not REF_CIRCLE)
        (setq REF_CIRCLE (entsel "\nSelect a circle "))
        (if (and REF_CIRCLE (= (cdr (assoc 0 (entget (car REF_CIRCLE)))) "CIRCLE"))
            (setq REF_CIRCLE (car REF_CIRCLE))
            (progn
              (setq REF_CIRCLE nil)
              (alert "Please select a circle.")
            )
        )
      )
    ; select circle part
    
    ; select block part
      (setq REF_BLK nil)
      (while (not REF_BLK)
        (setq REF_BLK (entsel "\nSelect a block "))
        (if (and REF_BLK (= (cdr (assoc 0 (entget (car REF_BLK)))) "INSERT"))
            (setq REF_BLK (car REF_BLK))
            (progn
              (setq REF_BLK nil)
              (alert "Please select a block.")
            )
        )
      )
    ; select block part

    (setq REF_CIRCLE_OBJ (vlax-ename->vla-object REF_CIRCLE))
    (vla-get-center REF_CIRCLE_OBJ)
    (setq REF_CIRCLE_OBJ_RADISUS (vla-get-radius REF_CIRCLE_OBJ))
    (setq REF_CIRCLE_OBJ_CENPOINT (vlax-variant-value (vla-get-Center REF_CIRCLE_OBJ)))
      (setq REF_CIRCLE_OBJ_CENPOINT_X (rtos (nth 0 (vlax-safearray->list REF_CIRCLE_OBJ_CENPOINT))))
      (setq REF_CIRCLE_OBJ_CENPOINT_Y (rtos (nth 1 (vlax-safearray->list REF_CIRCLE_OBJ_CENPOINT))))
      (setq REF_CIRCLE_OBJ_CENPOINT_XY (strcat REF_CIRCLE_OBJ_CENPOINT_X "," REF_CIRCLE_OBJ_CENPOINT_Y))
    
    
    
    (command "polygon" SEG_XC REF_CIRCLE_OBJ_CENPOINT_XY "I" REF_CIRCLE_OBJ_RADISUS)
    (setq NEW_POLYGON (entlast))
    (setq NEW_POLYGON_OBJ (vlax-ename->vla-object NEW_POLYGON))
    (command "xclip" REF_BLK "" "DELETE" )
    (command )
    ; (command "xclip" REF_BLK "" "n" "y" "s")
    (command "xclip" REF_BLK "" "n" "s" NEW_POLYGON) 
    
    (vla-delete NEW_POLYGON_OBJ)
    
    
    (setvar "osmode" 1215)
  
    
    
)


(defun c:xcv_xcilp_circle_rev02  

)
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



(defun c:testsave ()
  (SETQ FILENAME (GETVAR 'DWGNAME))
  (setq location "C:/TA/TA_WORK/2/nn22229" )

  
  (setq save_location nil)
    (while (not save_location)
      (setq save_location (entsel "\nSelect a circle "))
      
      (if (and save_location (= (cdr (assoc 0 (entget (car save_location)))) "insert") (= ef_tittle ef_name))
          (setq save_location (car save_location))
          (progn
            (setq save_location nil)
            (alert "Please select a circle.")
          )
      )
    )
  
  
  
  
    (COMMAND "_SAVEAS" "2010" location (SETQ FILENAME (GETVAR 'DWGNAME)))
)

