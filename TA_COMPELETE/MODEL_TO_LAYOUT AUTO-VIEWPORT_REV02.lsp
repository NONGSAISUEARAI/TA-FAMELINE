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
(vl-load-com)
(princ)


(defun c:SS3_SET_NAME_VIEWPORT ()
  (setq main (car (entsel)))

  (setq NAME_VIEWPORT_OLD (LM:vl-getattributevalue (vlax-ename->vla-object main) "NAME_VIEWPORT"))
  ; (setq DRAWING_NO2 (LM:vl-getattributevalue (vlax-ename->vla-object main) "DRAWING_NO2"))
  
  
  
  
  (setq mySet (ssget (list 
                      ;  (cons 8 "0") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
  )
  (setq entityCount (sslength mySet))
  
  (setq i 0)
  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq ejc (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
      (setq o (vlax-ename->vla-object ejc))

      ; (setq entity1 (ssname mySet 1)) 
      ; (setq entity2 (ssname mySet 2))
      ; (setq entity3 (ssname mySet 0)) 
      ; (setq entity4 (ssname mySet 3)) 

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
(defun c:LLOP1 ()
  (command "ucs" "")
  (setvar "osmode" 0)
  (setq mySet (ssget (list 
                       (cons 8 "defpoints") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
  )
  (setq entityCount (sslength mySet))
  (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq MAI (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
    ; (setq entity1 (ssname mySet 1)) 
    ; (setq entity2 (ssname mySet 2))
    ; (setq entity3 (ssname mySet 0)) 
    ; (setq entity4 (ssname mySet 3)) 
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
  
  (command "CTAB" "MODEL")
 
  
    ; เพิ่มลำดับ
    (setq i (1+ i))
  )
  (setvar "osmode" 1215)
  (command "ucs" "p")
)

  ; (setq obj (vlax-ename->vla-object (ssname mySet (setq entityCount (1- entityCount)))))

  


; (setq cntr 1)
; (while (< cntr 10) 

;   (princ cntr)

;   (setq cntr (+ cntr 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:SS1S_set1_re_basepoint_plot () 
  (setq FP (entget (car (entsel))))
  (setq enam (assoc -1 FP))
  (setq BBB (assoc 10 FP))

  (setq cbx (cadr BBB))
  (setq cby (caddr BBB))

  ; (setq CB (vlax-ename->vla-object enam))

  ; (setq abx (entget (car (entsel "\n OK GO"))))
  ; (setq o (car (entsel "\n OK GO GO GO")))

  (setq o (car (entsel "\n OK GO")))
  (setq abx (entget o))

  (setq CCC (assoc 10 abx))
  (setq ccx (cadr CCC))
  (setq ccy (caddr CCC))


  (if 
    (and o 
         (setq o (vlax-ename->vla-object o))
         (if (not (eq (vla-get-objectname o) "AcDbBlockReference")) 
           (alert "Please select block object.")
           t
         )
         (if (not (eq (vla-get-isdynamicblock o) :vlax-true)) 
           (alert "Please select dynamic block.")
           t
         )
         (setq wx (- cbx ccx))
         (setq wy (- cby ccy))
    )
    (progn 
      (LM:setdynpropvalue o "BASE_P X" (rtos wx 2 2))
      (LM:setdynpropvalue o "BASE_P Y" (rtos wy 2 2))
    )
  )

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
)
