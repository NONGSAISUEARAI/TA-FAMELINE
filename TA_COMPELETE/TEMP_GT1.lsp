  (defun TA:Get_Pline_vertext_ins_point_ (ename_) ;must be (car (entsel))
    (setq Get_Pline_ (entget ename_))
    (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
    (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
    ; (setq ename+Get_Pline_vtx_pt_ (list
    ;                                 ; ename_
    ;                                 Get_Pline_vtx_pt_
    ;                               )
    
    ; )
  )
  (defun TA:Offset_line+area_ (ename num_ )
      ; (setq 1-PL_en_ (car (entsel)))
      (setq 1-PL_en_obj (vlax-ename->vla-object ename))
      (vla-Offset 1-PL_en_obj num_)

  )
  (defun TA:get_vertex_len_ (ename_)
    ; (setq ref_line (car (entsel)))
    (setq vertex_total (length (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq sum_ ())

    (setq vertex_i 0)
    (setq vertex_ii 1)

    (while (< vertex_i vertex_total)
      ;get_data_len
      (setq len_i (vlax-curve-getDistatParam ename_ vertex_i) )
      (setq len_ii (vlax-curve-getDistatParam ename_ vertex_ii))
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
  (defun LM:effectivename_MO (obj) 
    (setq result (vl-catch-all-apply 
                   (function 
                     (lambda () 
                       (vlax-get-property obj 
                                          (if 
                                            (/= 
                                              (vlax-property-available-p 
                                                obj
                                                'effectivename
                                              )
                                              nil
                                            )
                                            (progn 
                                              'effectivename
                                              'name
                                            )
                                          )
                       )
                     )
                   ) ; ใช้ lambda เพื่อจับคำสั่ง getint
                 )
    )
  
    ; (if (vl-catch-all-error-p result) 
    ;   (if (wcmatch (strcase (vl-catch-all-error-message result)) "*ERROR*") 
    ;     (setq x "CANCEL") ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
    ;   )
    ; )
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

  (defun c:gt2 ()
    ;user input name block 
      (setq prefix_name "special_block")
      (setq num_i (rtos (getvar "date" )2 4))
      (setq name_block_ (strcat prefix_name num_i))
    ;
    ;user_input
      (setq ref_block (car (entsel)))
    ;
    ;generate_border_line
      (setq block_border_ (cdr (TA:ename+vla-getboundingbox (vlax-ename->vla-object ref_block ))))
      
      ; (command "rectangle" ref_block_bo_line_min ref_block_bo_line_max)
      ; (setq big_bo (entlast))
    ;
    ;generate_vertex_as10_xclip_border_
      (command "xclip" ref_block "" "p")
      (setq ename_bo_line_ (entlast))
      ; (command "pselect" ename_bo_line_)
      (setq bo_line_ (TA:Get_Pline_vertext_ins_point_ ename_bo_line_))
        (setq offset_bo_line_1 (TA:Offset_line+area_ ename_bo_line_ 0.01))
        (setq rec1_ (entlast))
        (setq rec1_as10_ (TA:Get_Pline_vertext_ins_point_ rec1_))
        (setq rec1_area_ (vla-get-area (vlax-ename->vla-object rec1_)))
        (setq offset_bo_line_2 (TA:Offset_line+area_ ename_bo_line_ -0.01))
        (setq rec2_ (entlast))
        (setq rec2_as10_ (TA:Get_Pline_vertext_ins_point_ rec2_))
        (setq rec2_area_ (vla-get-area (vlax-ename->vla-object rec2_)))
        (cond
          ((< rec1_area_ rec2_area_)
            (progn
              (vla-erase (vlax-ename->vla-object rec1_))
              (setq offset_bo_line3_ (cdr (TA:ename+vla-getboundingbox (vlax-ename->vla-object rec2_))))
              (setq rec3_as10 rec2_as10_ )
              ((vla-erase (vlax-ename->vla-object rec2_)))
            )
          )
          ((> rec1_area_ rec2_area_)
            (progn
              (vla-erase (vlax-ename->vla-object rec2_))
              (setq offset_bo_line3_ (cdr (TA:ename+vla-getboundingbox (vlax-ename->vla-object rec1_))))
              (setq rec3_as10 rec1_as10_ )
              (vla-erase (vlax-ename->vla-object rec1_))
              
            )
          )
        )
      
      ; (vla-erase (vlax-ename->vla-object (entlast)))
    ;
    ;explode_delete_block
      (vla-explode (vlax-ename->vla-object ref_block))
      (vla-erase (vlax-ename->vla-object ref_block))
    ;
    ;trim_method
      (command "regenall")
      (command "zoom" "w" "0,0" "10,10" )
      (command "regenall")
      (command "trim" ename_bo_line_ "" (car offset_bo_line3_) (cadr offset_bo_line3_) "") 
      (command "trim" ename_bo_line_ "" (car offset_bo_line3_) (cadr offset_bo_line3_) "") 
      (command "trim" ename_bo_line_ "" (car offset_bo_line3_) (cadr offset_bo_line3_) "") 
      (command "trim" ename_bo_line_ "" (car offset_bo_line3_) (cadr offset_bo_line3_) "") 
      (command "trim" ename_bo_line_ "" (car offset_bo_line3_) (cadr offset_bo_line3_) "") 
       
      (command "zoom" "p")
      
      
    ;
  ;selection_set_xclip_border_
    (command "regenall")
    
    
    (setq ss_s_non_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                             (append bo_line_ (list (car bo_line_)))
                             (list 
                               (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                               ; (cons 8 "000 - GRID")   ;kind of layer
                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                               ; (cons 62 1)           ;kind of color call sign with color code index
                             )
                      )
    )
    (setq ss_s_block_ (ssget "_F" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                             (append bo_line_ (list (car bo_line_)))
                             (list 
                               (cons 0 "INSERT") ;type of object
                               ; (cons 8 "000 - GRID")   ;kind of layer
                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                               ; (cons 62 1)           ;kind of color call sign with color code index
                             )
                      )
    )
  ;
  ;preloop_and_while xclip_block_object
    (if (/= ss_s_block_ nil)
      (progn
        (setq ss_s_block_i 0)
        (while  (< ss_s_block_i (sslength ss_s_block_))
          (if (= (vla-get-objectname (vlax-ename->vla-object (ssname ss_s_block_ ss_s_block_i))) "AcDbBlockReference")
            (progn
              (command "xclip" (ssname ss_s_block_ ss_s_block_i) "" "n"  "s" ename_bo_line_)
            )
            (princ "\n")
          )
          (setq ss_s_block_i (+ ss_s_block_i 1))
        )
      )
      (princ "\n")
    )    
  ;
  
  ;make_block_
    (command "pselect" ss_s_non_block_ "")
    ; (command "pselect" ss_s_block_ "")
    ; (command "pselect" ss_s_block_ ss_s_non_block_ "")
    (setq ss_s_make_block_ (ssget "_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                             (append bo_line_ (list (car bo_line_)))
                             (list 
                              ;  (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                               ; (cons 8 "000 - GRID")   ;kind of layer
                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                               ; (cons 62 1)           ;kind of color call sign with color code index
                             )
                      )
    )
    (command "_block" name_block_ (car bo_line_)  ss_s_make_block_ "" )
  ;

  
  
  
  
  (command "rectangle" (car block_border_) (cadr block_border_))
  (setq main_border_ (entlast))
  (setq main_border_vertex_ (TA:Get_Pline_vertext_ins_point_ main_border_))
  (setq ss_ref_block_ (ssget "_CP"  ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                             (append main_border_vertex_ (list (car main_border_vertex_)))
                             (list 
                               ;  (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                               ; (cons 8 "000 - GRID")   ;kind of layer
                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                               ; (cons 62 1)           ;kind of color call sign with color code index
                             )
                      )
  )
 
  ; (command "pselect" ss_ref_block_)
  ;preloop_and_while
    (setq ss_ref_block_i 0)
    (while (< ss_ref_block_i (sslength ss_ref_block_ ) )
      (if (/= (LM:effectivename_MO (vlax-ename->vla-object (ssname ss_ref_block_ ss_ref_block_i))) name_block_) 
        (progn
          (setq del_obj_ (vlax-ename->vla-object  (ssname ss_ref_block_ ss_ref_block_i) ))
          ; (command "pselect" (ssname ss_ref_block_ ss_ref_block_i) )
          (vla-erase del_obj_)
        )
        (princ "\n")
      
      )
      (setq ss_ref_block_i (+ ss_ref_block_i 1))
    ) 
  
  ; (setq ptt (getpoint ))
  (command "insert" name_block_ (car bo_line_) 1 1 0 )
  ; (command "insert" name_block_ ptt 1 1 0 )

)

(defun c:gt3 ()
  
  

 (setq ref_block1 (car (entsel)))
  (setq ref_block (car (entsel)))
  (setq ref_block_bo_line (vla-getboundingbox (vlax-ename->vla-object ref_block ) 'mn 'mx))
  (setq ref_block_bo_line_min (vlax-safearray->list mn))
  (setq ref_block_bo_line_max (vlax-safearray->list mx))
(setq srsr (TA:Get_Pline_vertext_ins_point_ ref_block))
(command "regenall")
(command "trim" ref_block1 "" "C" ref_block_bo_line_min ref_block_bo_line_max   )
(command "trim" ref_block1 "" "C" srsr   )
  
  

)

(defun c:gt4 ()
  ;sub_func
    (defun TA:Offset_line (ename num_ )
      ; (setq 1-PL_en_ (car (entsel)))
      (setq 1-PL_en_obj (vlax-ename->vla-object ename))
      (vla-Offset 1-PL_en_obj num_)
    )
  ;
  ;user_input
    (setq ref_block1 (car (entsel)))
    ; (setq num_input (getreal))
  ;
  ;offset_len_line _metheod
    ;get_data
      ; (setq max_len_ (TA:get_vertex_len_ ref_block1))
      ; (setq max_val (car (vl-sort max_len_ '>)))
      ; (TA:Offset_line ref_block1 (* max_val 0.4))
    ;
    ; ;zoom_obj metheod
    ;   (setq temp_ent (entsel "specify offset object"))
    ;   (setq zoom_obj_ (entlast))
    ;   (setq zoom_obj_bo_line (vla-getboundingbox (vlax-ename->vla-object zoom_obj_ ) 'zmn 'zmx))
    ;   (setq zoom_obj_bo_line_min (vlax-safearray->list zmn))
    ;   (setq zoom_obj_bo_line_max (vlax-safearray->list zmx))
    ;
  ;
  
  
  
  ;offset_func for trim
    ;get_data
      (TA:Offset_line ref_block1  -0.0001)
      (setq ref_block (entlast))
      (setq ref_block_bo_line (vla-getboundingbox (vlax-ename->vla-object ref_block ) 'mn 'mx))
      (setq ref_block_bo_line_min (vlax-safearray->list mn))
      (setq ref_block_bo_line_max (vlax-safearray->list mx))
      (setq srsr (TA:Get_Pline_vertext_ins_point_ ref_block))
    ;
    ;main_func_
      (command "regenall")
      ; (command "zoom" "_W" zoom_obj_bo_line_min zoom_obj_bo_line_max)
      (command "zoom" "_W" "0,0" "10,10")
      (vla-erase (vlax-ename->vla-object ref_block ))
      (command "trim" ref_block1 "" "C" ref_block_bo_line_min ref_block_bo_line_max  "")
      ; (vla-erase (vlax-ename->vla-object zoom_obj_ ))
      (command "zoom" "pr")
    ;
    ;select_func
      (setq ss_select (ssget "_CP" srsr))
      (command "pselect" ss_select "")
    ;
)
(defun c:gt5 ()
  ;sub_func
    (defun TA:Offset_line (ename num_ )
      ; (setq 1-PL_en_ (car (entsel)))
      (setq 1-PL_en_obj (vlax-ename->vla-object ename))
      (vla-Offset 1-PL_en_obj num_)
    )
  ;
  ;user_input
    (setq ref_block1 (car (entsel)))
    ; (setq num_input (getreal))
  ;
  ; ;offset_len_line _metheod
  ;   ;get_data
  ;     (setq max_len_ (TA:get_vertex_len_ ref_block1))
  ;     (setq max_val (car (vl-sort max_len_ '>)))
  ;     (TA:Offset_line ref_block1 (* max_val 0.4))
  ;   ;
  ;   ;zoom_obj metheod
  ;     (setq temp_ent (entsel "specify offset object"))
  ;     (setq zoom_obj_ (entlast))
  ;     (setq zoom_obj_bo_line (vla-getboundingbox (vlax-ename->vla-object zoom_obj_ ) 'zmn 'zmx))
  ;     (setq zoom_obj_bo_line_min (vlax-safearray->list zmn))
  ;     (setq zoom_obj_bo_line_max (vlax-safearray->list zmx))
  ;   ;
  ; ;
  
  
  
  ;offset_func for trim
    ;get_data
      (TA:Offset_line ref_block1  0.0001)
      (setq ref_block (entlast))
      (setq ref_block_bo_line (vla-getboundingbox (vlax-ename->vla-object ref_block ) 'mn 'mx))
      (setq ref_block_bo_line_min (vlax-safearray->list mn))
      (setq ref_block_bo_line_max (vlax-safearray->list mx))
      (setq srsr (TA:Get_Pline_vertext_ins_point_ ref_block))
    ;
    ;main_func_
      (command "regenall")
      ; (command "zoom" "_W" zoom_obj_bo_line_min zoom_obj_bo_line_max)
      (command "zoom" "_W" "0,0" "10,10")
      (vla-erase (vlax-ename->vla-object ref_block ))
      (command "trim" ref_block1 "" "C" ref_block_bo_line_min ref_block_bo_line_max  "")
      ; (vla-erase (vlax-ename->vla-object zoom_obj_ ))
      (command "zoom" "pr")
    ;
    ;select_func
      (setq ss_select (ssget "_CP" srsr))
      (command "pselect" ss_select "")
    ;
)


(defun c:gt6 ()
  ;user_input
    (setq ent_block_ (entsel "specify block"))
  ;
  
)


(vl-load-com)
(defun c:Example_AddPolyline()
    ;; This example creates a polyline in model space.
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
  
    ;; Define the 2D polyline points
    (setq points (vlax-make-safearray vlax-vbDouble '(0 . 14)))
    (vlax-safearray-fill points '(1 1 0
                                  1 2 0
                                  2 2 0
                                  3 2 0
                                  4 4 0
                                 )
    )
        
    ;; Create a lightweight Polyline object in model space
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq plineObj (vla-AddPolyline modelSpace points))
    (vla-ZoomAll acadObj)
)
(defun c:gt6 ()
(setq ename_ (car (entsel)))
(defun TA:Get_Pline_vertext_ins_point_full_loop_ (ename_) ;must be (car (entsel))
    (setq Get_Pline_ (entget ename_))
    (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
    (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
    (setq Get_Pline_vtx_pt_ (append Get_Pline_vtx_pt_ (list (car Get_Pline_vtx_pt_))))
    ; (setq ename+Get_Pline_vtx_pt_ (list
    ;                                 ; ename_
    ;                                 Get_Pline_vtx_pt_
    ;                               )
    
    ; )
  )
  (setq pt_list_ (TA:Get_Pline_vertext_ins_point_full_loop_ ename_))
  (setq pt_list_i 0
        pt_list_ii 1
  )
  (while (< pt_list_i (length pt_list_))
    (setq pt_list_set1 (nth pt_list_i pt_list_)
          pt_list_set2 (nth pt_list_ii pt_list_)
    )
    (command "trim" ename_ "" "f" pt_list_set1 pt_list_set2 ""  "")
    (setq pt_list_i  (+ pt_list_i 1)
          pt_list_ii (+ pt_list_ii 1)
    )
  )
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


(defun LM:intersections ( ob1 ob2 mod / lst rtn )
    (if (and (vlax-method-applicable-p ob1 'intersectwith)
             (vlax-method-applicable-p ob2 'intersectwith)
             (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
        )
        (repeat (/ (length lst) 3)
            (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
                  lst (cdddr lst)
            )
        )
    )
    (reverse rtn)
)

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
(defun get_interpts (obj1 obj2 / iplist)
  (if (not (vl-catch-all-error-p
             (setq iplist (vl-catch-all-apply
                            'vlax-safearray->list
                            (list
                              (vlax-variant-value
                                (vla-intersectwith obj1 obj2 acextendnone)
                              ))))))
    iplist
  )
)
(vl-load-com) (princ)




(setq obj1 (vlax-ename->vla-object (car (entsel "\nSelect 1st Object: "))))
(setq obj2 (vlax-ename->vla-object (car (entsel "\nSelect 2nd Object: "))))
; (LM:intersections obj1 obj2 acextendnone)
(command "point" (car (LM:intersections obj1 obj2 acextendnone )))
(get_interpts obj1 obj2)
(LM:intersections obj1 obj2 acextendnone)

  (defun CO:sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
  )
  (defun CO:sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
  )
  (defun CO:sort_by_val_ (list_ val_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth val_ a) (nth val_ b))))))
  )
  (defun TA:vla-addarc_ (center_ rad_ point_list_ )
    ; point_list_ must be two coord set in list
    ; do not vlax-3d_point w/ agument
    ;sub_func_
      (defun CO:sort_by_val_ (val_ list_ )  ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (nth val_ a) (nth val_ b))))))
      )
    ;
    ;basic_get_data_
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))
    ;
    ;advance_get_data
      (setq point_list_ (reverse (append (CO:sort_by_val_ 1 point_list_) (list (car point_list_)))))
    ;
    ;preloop_and_while sorting arc_line
      (setq angle_list_ ())
      (setq point_list_i 0)
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
    (setq point_list_ (append (CO:sort_by_yy angle_list_) (list (car (CO:sort_by_val_ 1 angle_list_) ))) )
    (setq point_list_i 0)
    (setq point_list_ii 1)
    (while (< point_list_i (- (length point_list_) 1))
      (setq point_list_set_1 (car (nth point_list_i point_list_)))
      (setq point_list_set_2 (car (nth point_list_ii point_list_)))
      (setq start_ang_ (angle center_ point_list_set_1))
      (setq end_ang_ (angle center_ point_list_set_2))
      ; (setq rad_ (distance center_ point_list_set_1))
      (getpoint)
      (vla-addarc modelSpace (vlax-3d-point center_) rad_      end_ang_ start_ang_)
      (setq point_list_i (+ point_list_i 1))
      (setq point_list_ii (+ point_list_ii 1))
    )
    
  )


(setq main_border_  (car (entsel)))
(setq main_border_obj (vlax-ename->vla-object main_border_))
(setq ss_s_non_block_ (ssget ;"_CP" ;ชุดคำสั่ง selection set เพื่อ fillter block ไปทำ xclip สำเร็จแล้ว เหลือ ssget อีกรอบเพื่อ กรองเอาแต่ block
                             ;(append bo_line_ (list (car bo_line_)))
                             (list 
                               (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                               ; (cons 8 "000 - GRID")   ;kind of layer
                               ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                               ; (cons 62 1)           ;kind of color call sign with color code index
                             )
                      )
)
(setq ss_s_non_block_i 0)
(while (< ss_s_non_block_i (sslength ss_s_non_block_))
  (setq ss_s_non_block_ename (ssname ss_s_non_block_ ss_s_non_block_i))
  (setq ss_s_non_block_obj (vlax-ename->vla-object ss_s_non_block_ename))
  (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object ss_s_non_block_ename))) ;For Check objectname
  (setq get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_s_non_block_obj))))
  
  
  (setq intersec_point_list_ (LM:intersections main_border_obj ss_s_non_block_obj  acextendnone ))
  (setq intersec_point_list_ (append (CO:sort_by_y intersec_point_list_) (list (car intersec_point_list_))))
  
  (if (/= main_border_obj nil)
    (progn
      (setq intersec_point_list_i 0)
      (setq intersec_point_list_ii 1)
      (while (< intersec_point_list_i (length intersec_point_list_))
        (setq intersec_point_list_num_i   (nth intersec_point_list_i intersec_point_list_))
        (setq intersec_point_list_num_ii  (nth intersec_point_list_ii intersec_point_list_))
          
          (if(and
              (= type_obj_ "AcDbCircle") 
              (> (length intersec_point_list_) 1)
           )
            (progn
              (setq center_ get_center_)
              (setq point_list_start intersec_point_list_num_i)
              (setq point_list_ (append (CO:sort_by_y intersec_point_list_) (list (car intersec_point_list_))))
              
              (TA:vla-addarc_ get_center_  intersec_point_list_)
            )
            (princ "\n")
          )
          (setq intersec_point_list_i (+ intersec_point_list_i 1))
          (setq intersec_point_list_ii (+ intersec_point_list_ii 1))
                (defun TA:vla-addarc_ (center_ point_list_ )
                  ; point_list_ must be two coord set in list
                  ; do not vlax-3d_point w/ agument
                  ;basic_get_data_
                    (setq acadObj (vlax-get-acad-object))
                    (setq doc (vla-get-ActiveDocument acadObj))
                    (setq modelSpace (vla-get-ModelSpace doc))
                  ;
                  ;advance_get_data
                    (setq point_list_ (append (CO:sort_by_y point_list_) (list (car point_list_))))
                  ;
                  (setq point_list_i 0)
                  (setq point_list_ii 1)
                  (while (< point_list_i (- (length point_list_) 1))
                    (setq point_list_set_1 (nth point_list_i point_list_))
                    (setq point_list_set_2 (nth point_list_ii point_list_))
                    (setq start_ang_ (angle center_ point_list_set_1))
                    (setq end_ang_ (angle center_ point_list_set_2))
                    (setq rad_ (distance center_ point_list_set_1))
                    (vla-addarc modelSpace (vlax-3d-point center_) rad_  end_ang_   start_ang_)
                    (setq point_list_i (+ point_list_i 1))
                    (setq point_list_ii (+ point_list_ii 1))
                  )
                  
                )
          

      )
    )

    
  
  )
  (setq )
)

(create-pline obj3)
(defun c:ctrim_ ()
(setq obj1 (vlax-ename->vla-object (car (entsel "\nSelect main_border_ Object: "))))
(setq obj2 (vlax-ename->vla-object (car (entsel "\nSelect circle Object: "))))
(setq obj3 (LM:intersections obj1 obj2 acextendnone))
(setq get_center_ (vlax-safearray->list (vlax-variant-value (vla-get-center obj2))))
(setq get_rad_ (vla-get-radius obj2))
(vla-erase obj2)

(TA:vla-addarc_test get_center_ get_rad_ obj3)
(setq  center_ get_center_)
(setq  rad_ get_rad_)
(setq  point_list_ obj3 )
  (defun TA:vla-addarc_test (center_ rad_ point_list_ )
    ; point_list_ must be two coord set in list
    ; do not vlax-3d_point w/ agument
    ;basic_get_data_
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq modelSpace (vla-get-ModelSpace doc))
    ;
    ;advance_get_data
      (setq point_list_ (reverse (append (CO:sort_by_y point_list_) (list (car point_list_)))))
    ;
    ;preloop_and_while sorting arc_line
      (setq angle_list_ ())
      (setq point_list_i 0)
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
    (setq point_list_ (append (CO:sort_by_yy angle_list_) (list (car (CO:sort_by_yy angle_list_) ))) )
    (setq point_list_i 0)
    (setq point_list_ii 1)
    (while (< point_list_i (- (length point_list_) 1))
      (setq point_list_set_1 (car (nth point_list_i point_list_)))
      (setq point_list_set_2 (car (nth point_list_ii point_list_)))
      (setq start_ang_ (angle center_ point_list_set_1))
      (setq end_ang_ (angle center_ point_list_set_2))
      ; (setq rad_ (distance center_ point_list_set_1))
      (getpoint)
      (vla-addarc modelSpace (vlax-3d-point center_) rad_      end_ang_ start_ang_)
      (setq point_list_i (+ point_list_i 1))
      (setq point_list_ii (+ point_list_ii 1))
    )
    
  )
)

    ;preloop_and_while sorting arc_line
      (setq angle_list_ ())
      (setq point_list_i 0)
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

  
(setq list_ angle_list_)
(CO:sort_by_yy angle_list_)