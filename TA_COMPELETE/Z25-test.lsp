(defun c:P3Len (/ i ss ent j str gghh)
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
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
  (vl-load-com)
  (setq gghh '()) ; สร้างตัวแปรที่มีชื่อว่า gghh และกำหนดให้เป็นรายการว่าง

  (cond 
    (
     (and (setq i -1 ss (ssget '((0 . "LWPOLYLINE"))))t)
        (setvar "osmode" 0)
        (while (setq ent (ssname ss (setq i (1+ i))))
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
    )

    ; แสดงความยาวทั้งหมดในตัวแปร gghh
    (princ (reverse gghh))
    (setq ssf str)

    (princ)
  )
  
  (defun LM:str->lst (str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
  )
  
  (setq ssf str)
    (setq delimiter "_")
    (setq ss-list (LM:str->lst ssf delimiter))
  (setq length_total_all length_total)

  ; PART สร้าง
    (setq total_ss-list (vl-list-length ss-list))
    ;  (command "line" "0,0" "0,10" "")
      (setq faa_x (atof (nth 2 ss-list)))
      (setq faa_x3 (atof (nth 3 ss-list)))
      (setq y 0)  
      (setq fv1 (strcat (rtos faa_x 2 8) "," (rtos y 2 8) "," (rtos y 2 8)))
  
      (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID" "0,0" 1 0)
      (setq BLANK_ALL (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_ALL) "W" length_total_all)
  
      (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID" "0,0" 1 0)
      (setq BLANK_1 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1) "W" faa_x)
      
      (command "copy" BLANK_1 "" "Displacement" fv1 "")
      (setq BLANK_1-1 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1-1) "W" faa_x3)
    (setq pl 3)
    (setq p 4)
    (setq pp 5)
  
    (while 
      ; (< p (- total_ss-list 1))
      (< p total_ss-list)
        (setq faa_p_x_p2 (atof (nth pl ss-list)))
        (setq faa_p_x (atof (nth p ss-list)))
        ; (setq faa_p_x_pp (atof (nth pp ss-list)))
        (setq faa_p_xy (strcat (rtos faa_p_x 2 8) "," (rtos 0 2 8)))
      
      
      (setq fv (strcat (rtos faa_p_x_p2 2 8) "," (rtos y 2 8) "," (rtos y 2 8)))
      (command "copy" "L" "" "Displacement" fv "")
      (setq BLANK_2 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_2) "W" faa_p_x)

      (setq pl (+ pl 1))
      (setq p (+ p 1))
      (setq pp (+ pp 1))
    )
    ; (command "insert" "001 - DYNAMIC REC&SSQ SHAPE SOLID" "0,0" 1 0)
    ; (setq soi (entlast))
    ; (setq H (rtos 10 2 8 ))

    ; (LM:setdynpropvalue (vlax-ename->vla-object soi) "W" length_total_all)
    ; (LM:setdynpropvalue (vlax-ename->vla-object soi) "H" H)
  (setvar "osmode" 1215)
)