;sub-func
  (defun LM:str->lst (str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
  )
;

;dye_pin part
  (setq dye_pin nil)
  (while (not dye_pin)
    (setq dye_pin (entsel "\nPLEASE Select a 000-DYE_PIN_PROTOTYPE"))
    (if 
      (and 
        (= (cdr (assoc 0 (entget (car dye_pin)))) "INSERT")
        (= "000-dye_pin_prototype" (LM:effectivename (vlax-ename->vla-object (cdr (assoc -1 (entget (car dye_pin)))))))
        ; (= (vla-get-isdynamicblock (setq dye_pin_OBJ (vlax-ename->vla-object (car dye_pin)))))
      )
      (progn 
          (setq dye_ename (cdr (assoc -1 (entget (car dye_pin)))))
        
          (setq dye_ins_xyz (cdr (assoc 10 (entget (car dye_pin)))))
          (princ "\n dye_ins_x = ")
          (princ (setq dye_ins_x (cadr (assoc 10 (entget (car dye_pin))))))
          (princ "\n dye_ins_y = ")
          (princ (setq dye_ins_y (caddr (assoc 10 (entget (car dye_pin))))))
          (setq dye_ins_xy (strcat (rtos dye_ins_x 2 8) "," (rtos dye_ins_y 2 8)))
      )
      (setq dye_pin nil) ; เมื่อเงื่อนไขไม่ถูกต้องให้กำหนดค่า dye_pin เป็น nil และทำการ entsel ใหม่
    )
  )
;

;ref_sheet part
  (vl-load-com)
  (setq gghh '()) ; สร้างตัวแปรที่มีชื่อว่า gghh และกำหนดให้เป็นรายการว่าง
  (and (setq i -1 ss (ssget '((0 . "LWPOLYLINE"))))t)
  (setvar "osmode" 0)
  (while 
    (setq ent (ssname ss (setq i (1+ i))))
      (setq polyline (vlax-ename->vla-object ent))
      (setq length_total (vla-get-length polyline))
      
      (setq j (1- (vlax-curve-getStartParam ent)) str "0")

      (while (<= (setq j (1+ j)) (vlax-curve-getEndParam ent))
        (setq str
          (strcat str
            "_"
            (rtos (- (vlax-curve-getDistatParam ent j)
                        (if (zerop j) 0
                          (vlax-curve-getDistatParam ent (1- j)))) 2 2)
            
            ; (chr 9) มีไว้ทำไมไม่รู้
          )
        )
      )

            ; (setq gghh (cons (strcat str (rtos (vlax-curve-getDistatParam ent
            ;                           (vlax-curve-getEndParam ent)) 2 2) ) gghh))
  )
  (princ (reverse gghh))
  (setq ssf str)

  (setq delimiter "_")


  (princ "\n----------------------------------------------------------------------------------------")
  (princ "\n              Summary length objet")
  (princ "\n              List of PLINE length = ")
  (princ (setq ss-list (LM:str->lst ssf delimiter)))
  (princ "\n              Total length = ")
  (princ (setq length_total (vla-get-length polyline)))
  (princ "\n----------------------------------------------------------------------------------------")
;
  ;target_sheet part
    
    (setq target_SHEET nil)
    (while (not target_SHEET)
      (command "insert" "A$C0f772e0c" dye_ins_xy 1 0)
      (setq target_SHEET (entlast))
      (command "pselect" "L")
      ; (setq hxx (list hx hx))
      ; (setq hxxx (car hxx))
            
      
      
      (if 
        (and 
          (= (cdr (assoc 0 (entget target_SHEET))) "INSERT")
          (= "A$C0f772e0c" (LM:effectivename (vlax-ename->vla-object (cdr (assoc -1 (entget target_SHEET))))))
          (= (vla-get-isdynamicblock (setq target_SHEET_OBJ (vlax-ename->vla-object target_SHEET))))
          (= (setq get_folding_number (LM:getvisibilitystate (vlax-ename->vla-object target_SHEET))))
          
          (setq pi_value 3.1428571428571428571428571428571)
          (setq std_ang 180)
          (= (LM:round (setq target_angle1 (* (/ (setq target_angle1 (LM:getdynpropvalue (vlax-ename->vla-object target_SHEET ) "angle1")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle2 (* (/ (setq target_angle2 (LM:getdynpropvalue (vlax-ename->vla-object target_SHEET ) "angle2")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle3 (* (/ (setq target_angle3 (LM:getdynpropvalue (vlax-ename->vla-object target_SHEET ) "angle3")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle4 (* (/ (setq target_angle4 (LM:getdynpropvalue (vlax-ename->vla-object target_SHEET ) "angle4")) pi_value) std_ang))) 180)

        )
        (progn 
          (princ "\nxx")
            (setq target_ename (cdr (assoc -1 (entget target_SHEET))))
            (setq target_obj (vlax-ename->vla-object target_ename))
          
            (setq target_ins_xy (cdr (assoc 10 (entget target_SHEET))))
            (princ "\n target_ins_x = ")
            (princ (setq target_ins_x (cadr (assoc 10 (entget target_SHEET)))))
            (princ "\n target_ins_y = ")
            (princ (setq target_ins_x (caddr (assoc 10 (entget target_SHEET)))))
          
            (setq insertionPoint (vlax-3d-point dye_ins_x dye_ins_y 0))
            
            (vla-put-insertionpoint target_obj insertionPoint)
          
            ;get_ref_data
              (LM:SetVisibilityState (vlax-ename->vla-object target_SHEET) ref_Folding_number)
              (setq target_line_1 (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET ) "LINE1" ref_line_1))
              (setq target_line_2 (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET ) "LINE2" ref_line_2))
              (setq target_line_3 (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET ) "LINE3" ref_line_3))
              (setq target_line_4 (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET ) "LINE4" ref_line_4))
              (setq target_line_5 (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET ) "LINE5" ref_line_5))

            ;

            

        )
        (setq target_SHEET nil) ; เมื่อเงื่อนไขไม่ถูกต้องให้กำหนดค่า target_SHEET เป็น nil และทำการ entsel ใหม่
      )
    )

    (setq ilast (atoi ref_Folding_number))
    (setq i (- ilast 1))
    
      (while 
        (and (> i 0))
        (setq angleXX (strcat "Angle" (rtos i 2 0)))
        (setq get_angleXX (eval (read (strcat "GET_ANGLE" (rtos i 2 0)))))
        (setq positionXX (strcat "Position" (rtos i 2 0) " X"))
        
        
        (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET) positionXX 0)
        (LM:setdynpropvalue (vlax-ename->vla-object target_SHEET) angleXX get_angleXX)
        (setq kk (getstring "ddddddd"))
      

        ; ทำมาถึง ขั้น while เพื่อ ส่งค่าต่างๆ ไป ที่การพับองศา
        
        (setq i (* (- 1 i) -1))
      )

  ;