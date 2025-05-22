(defun LM:getdynpropvalue (blk prp) 
  (setq prp (strcase prp))
  (vl-some 
    '(lambda (x) 
       (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)
(defun LM:vl-getattributevalue (blk tag) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
     )
    (vlax-invoke blk 'getattributes)
  )
)
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
(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
(defun LM:effectivename ( obj )
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
)

(vl-load-com)
(princ)


(defun c:SS3_SET_NAME_VIEWPORT ()
  (setq main (car (entsel)))

  (setq NAME_VIEWPORT_OLD (LM:vl-getattributevalue (vlax-ename->vla-object main) "NAME_VIEWPORT"))
  ; (setq DRAWING_NO2 (LM:vl-getattributevalue (vlax-ename->vla-object main) "DRAWING_NO2"))
  
  
  
  
  (setq my_real_ename (ssget (list 
                      ;  (cons 8 "0") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
  )
  (setq entityCount (sslength my_real_ename))
  
  (setq i 0)
  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq ejc (ssname my_real_ename i)) ; ดึง entity ที่ลำดับ i จาก my_real_ename
      (setq o (vlax-ename->vla-object ejc))

      ; (setq entity1 (ssname my_real_ename 1)) 
      ; (setq entity2 (ssname my_real_ename 2))
      ; (setq entity3 (ssname my_real_ename 0)) 
      ; (setq entity4 (ssname my_real_ename 3)) 

      (setq o (vlax-ename->vla-object ejc))
      (setq ii (1+ i)) ; เริ่มต้น i ที่ 101
      (setq NAME_VIEWPORT_NEW (strcat "M1" (if (< ii 10) "0" "") (itoa ii))) ; สร้างชื่อใหม่โดยใช้ i และแปลงเป็นสตริง
      (LM:vl-setattributevalue o "NAME_VIEWPORT" NAME_VIEWPORT_NEW)
     
      ; (setq M_D (strcat "M10" (itoa (1+ i))))
      ; (LM:vl-setattributevalue o "FAC" M_D)
  
      ; เพิ่มลำดับ
      (setq i (1+ i))
  )
)


;for continue command
(defun c:LLOP13 ()
  (command "ucs" "")
  (setvar "osmode" 0)
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
    ;selection_part
      (setq my-selection-set (ssget
                    (list 
                      (cons 0 "INSERT")       ;type of object
                      ; (cons 8 "000 - GRID")   ;kind of layer 
                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                      ; (cons 62 1)           ;kind of color call sign with color code index
                    )
                  )
      )
    ;selection_part

    ;setting_reference_name_part
      ;mode1
        (setq ef_name "GV1")
      ;end_mode1
      ;mode2
      ;   (setq ef_name (getstring "insert_NAME_BLOCK"))
      ;end_mode2
      ;mode3
        ; (setq ef_ename_1st (entsel "select_NAME_BLOCK"))
        ;   (setq ef_ename_2nd (car ef_ename_1st))
        ;   (setq ef_ename_obj (vlax-ename->vla-object ef_ename_2nd))
        ; (setq ef_name (LM:effectivename ef_ename_obj))
      ;end_mode3
    ;setting_reference_name_part
    (setq total_ssget (sslength my-selection-set))
    (setq i 0)
    (setq ename_list '())
    (while 
      (< i total_ssget)
      (setq blk_obj (ssname my-selection-set i))
      (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
      (setq ss (LM:effectivename blk_obj_Set))

        (if (= ef_name ss)
          (progn
            (setq ename_list (cons blk_obj ename_list)) 
          )
          (princ "\n")
            
        )
      (setq i (+ i 1))
    )
      (princ "\n")
    (princ (setq total_ename_list (length ename_list)))
    (setq my_real_ename (ssadd))
    (foreach ename ename_list
      (ssadd ename my_real_ename)
    )
  ; SFB_FINDING_SPECFILY_NAME_BLOCK

  ; part2
    ; (setq my_real_ename (ssget (list 
    ;                     (cons 8 "defpoints") 
    ;                     (cons 0 "INSERT") 
    ;                     ;  (cons 2 "d2d")
    ;                   ) 
    ;             ) 
    ; )
    (setq entityCount (sslength my_real_ename))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq MAI (ssname my_real_ename i)) ; ดึง entity ที่ลำดับ i จาก my_real_ename
      ; (setq entity1 (ssname my_real_ename 1)) 
      ; (setq entity2 (ssname my_real_ename 2))
      ; (setq entity3 (ssname my_real_ename 0)) 
      ; (setq entity4 (ssname my_real_ename 3)) 
      (setq MAI_EN (entget MAI))
      (setq MAI_INSERT_P (assoc 10 MAI_EN))
      
      (princ "\n STAR_X =")
      (princ (setq MAI_INSERT_X (cadr MAI_INSERT_P)))
      
      (princ "\n STAR_Y =")
      (princ (setq MAI_INSERT_Y (caddr MAI_INSERT_P)))  
    
      (setq MAI_INSERT_XY (strcat
                            (rtos MAI_INSERT_X 2 5)
                            ","
                            (rtos MAI_INSERT_Y 2 5)
                          )
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GET_DATA_PART
      (princ "\n DYN_BASE_P X =")
      (princ (setq KJ1 (LM:getdynpropvalue (vlax-ename->vla-object MAI) "BASE_P X")))
      (princ "\n DYN_BASE_P Y =")
      (princ (setq KJ2 (LM:getdynpropvalue (vlax-ename->vla-object MAI) "BASE_P Y")))

      (princ "\n DYN_H =")
      (princ (setq KJH (LM:getdynpropvalue (vlax-ename->vla-object MAI) "H")))
      (princ "\n DYN_L =")
      (princ (setq KJL (LM:getdynpropvalue (vlax-ename->vla-object MAI) "L")))

      (princ "\n DYN_SC =")
      (princ (setq KJsc (LM:getdynpropvalue (vlax-ename->vla-object MAI) "SC")))
      (princ (setq KJscsc (rtos KJsc 2 2)))

      (setq AA_1 (LM:vl-getattributevalue (vlax-ename->vla-object MAI) "NAME_VIEWPORT"))
      (princ AA_1)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GET_DATA_PART
    (princ "\n")

    (setq TOP_INSERT_X (+ KJL MAI_INSERT_X))
    (setq TOP_INSERT_Y (+ KJH MAI_INSERT_Y))

    (setq TOP_INSERT_XY (strcat (rtos TOP_INSERT_X 2 8) "," (rtos TOP_INSERT_Y 2 8)))

    (setq RE_X (/ KJ1 -1))
    (setq RE_Y (/ KJ2 -1))


    (setq MID_X (+ RE_X (/ KJL 2)))
    (setq MID_Y (+ RE_Y (/ KJH 2)))

    (setq MID_Xsc (/ MID_X KJsc))
    (setq MID_Ysc (/ MID_Y KJsc))

    ; (setq MID_Xsc (+ (+ RE_X (/ KJL 2)) KJsc))
    ; (setq MID_Ysc (+ (+ RE_Y (/ KJH 2)) KJsc))
    (setq MID_INSERT_XY (strcat (rtos MID_Xsc 2 5) "," (rtos MID_Ysc 2 5)))   
      
    (command "CTAB" AA_1)
    (command "MVIEW" "NEW" MAI_INSERT_XY TOP_INSERT_XY "" MID_INSERT_XY)
      (vl-load-com)
    (setq afc (entlast))
    (setq afc_gt (entget afc))
    (setq a_app (vlax-get-acad-object)
          a_doc (vla-get-ActiveDocument a_app)
          obj   (vlax-ename->vla-object afc) ; obj   (vlax-ename->vla-object (car (entsel "Select a pviewport entity: "))) ;OPTION 2
    )
    (setq cen_ (assoc 10 afc_gt))
    (setq cen_x (cadr cen_))
    (setq cen_y (caddr cen_))
    (princ (setq cen_xy (strcat (rtos cen_x) "," (rtos cen_y))))
    (princ (vla-get-height obj))


    (setq real_box (/ (/ KJH KJsc) (vla-get-height obj)))
      (setq re_real_box (rtos real_box 2 2))
    (setq inp_cus_sc (/ 1 KJsc))
    (vla-put-customscale obj inp_cus_sc)
    (command "_.SCALE" afc "" cen_xy re_real_box)

    ; (vla-ZoomCenter (vlax-get-acad-object) (vlax-3d-point obj) 1)
    (setq kk 0.2)
    (princ "\n")
    (princ (vla-get-customscale obj))
    (princ "\n anno")
    (princ (vla-get-height obj))
    (princ "\n vla1")
    (princ (vla-get-standardscale obj))
    (princ "\n vla2")
    (princ (vla-get-standardscale2 obj))
    
    (command "zoom" "Extents")
    (command "CTAB" "MODEL")
  
    
      ; เพิ่มลำดับ
      (setq i (1+ i))
    )
    (setvar "osmode" 1199)
    (command "ucs" "p")
  ; part2
)

(defun c:SS1S_set1_re_basepoint_plot () 
  ; Lee_macdonel_part
    (defun LM:setdynpropvalue (blk prp val) 
      (setq prp (strcase prp))
      (vl-some 
        '(lambda (x) 
          (if (= prp (strcase (vla-get-propertyname x))) 
            (progn 
              (vla-put-value x 
                              (vlax-make-variant val 
                                                (vlax-variant-type (vla-get-value x))
                              )
              )
              (cond (val) (t))
            )
          )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
      )
    )
  ; Lee_macdonel_part  
  ; part_ref_viewport
    (setq FP (entget (car (entsel "SELECT FOR REF_TITTLE_BLOCK"))))
    (setq enam (assoc -1 FP))
    (setq BBB (assoc 10 FP))

    (setq cbx (cadr BBB))
    (setq cby (caddr BBB))
  ; part_ref_viewport
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
    ;selection_part
      (setq my-set_ef_name_selection-set (ssget
                    (list 
                      (cons 0 "INSERT")       ;type of object
                      ; (cons 8 "000 - GRID")   ;kind of layer 
                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                      ; (cons 62 1)           ;kind of color call sign with color code index
                    )
                  )
      )
    ;selection_part
    ;setting_refernce_name_part
      ; mode1
        (setq ef_name "GV1")
      ; end_mode1
      ;mode2
        ; (setq ef_name (getstring "insert_NAME_BLOCK"))
      ;end_mode2
      ;mode3
        ; (setq ef_ename_1st (entsel "select_NAME_BLOCK"))
        ;   (setq ef_ename_2nd (car ef_ename_1st))
        ;   (setq ef_ename_obj (vlax-ename->vla-object ef_ename_2nd))
        ; (setq ef_name (LM:effectivename ef_ename_obj))
      ;end_mode3
    ;setting_refernce_name_part
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
  ; part_viewport
    (setq total_my_vp_blk_real_enam (sslength my-set_ef_name_selection-set))
    (setq no_vp 0)
    (while 
      (< no_vp total_my_vp_blk_real_enam)
      (setq vp_main (ssname my-set_ef_name_selection-set no_vp))
      (setq vp_main_entget (entget vp_main))
      (setq vp_main_obj (vlax-ename->vla-object vp_main))
      (setq vp_main_xy (cdr (assoc 10 vp_main_entget)))
      (setq vp_main_x (car vp_main_xy))
      (setq vp_main_y (cadr vp_main_xy))
      (setq wx (/(- vp_main_x cbx) -1))
      (setq wy (/ (- vp_main_y cby) -1))
      
      
      (LM:setdynpropvalue vp_main_obj "BASE_P X" (rtos wx 2 2))
      (LM:setdynpropvalue vp_main_obj "BASE_P Y" (rtos wy 2 2))
      (setq no_vp (+ no_vp 1))
    )
  ; part_viewport

)

(defun cn_counta_layout ()
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layout-count (vla-get-Count (vla-get-Layouts doc)))
  (setq layout-name (vla-get-name doc))
  (vla-put-name doc AAA)
  
  
  
  
  (princ (strcat "จำนวน layouts ทั้งหมด: " (itoa layout-count)))
  (princ)
  
  (setq layout-list '())
    (setq layout-name (vla-get-Name layout))
    (setq layout-list (cons layout-name layout-list))
  
  (foreach layout layout-list
    (princ (strcat layout "\n"))
  )
)

