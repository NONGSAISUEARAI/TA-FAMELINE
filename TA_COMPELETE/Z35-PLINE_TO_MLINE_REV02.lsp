;sub_function
  (defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )

;

(defun c:P2ML_PLINE_TO_MLINE ()
  (setq abx (entget (car (entsel "\n OK GO"))))

  ; (setq entData (entget abx)) ; เปลี่ยน someEntity เป็นรหัสของวัตถุที่คุณต้องการ

  ; ใช้ vl-remove-if-not เพื่อกรองรายการที่มีคีย์เป็น 11
  ; (setq eleventhData (vl-remove-if-not '(lambda (x) (= 11 (car x))) abx)) ;for mline
  (setq tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) abx)) ;for pline

  ; ใช้ mapcar เพื่อดึงข้อมูลออกมา
  (setq zdataList (mapcar 'cdr tenthData))
  
  ; นำข้อมูลที่ได้มาใช้ต่อ
  (foreach data zdataList
    ; ทำสิ่งที่คุณต้องการกับข้อมูลที่ดึงออกมาได้ที่ตัวแปร data
    (princ data) ; ตัวอย่าง: พิมพ์ข้อมูลที่ได้
  )

  (setq coord_data zdataList)

  (defun create-pline (coords)
    (if (= (length coords) 0)
        nil
      (progn
        ; สร้างพอลีไนจากค่าพิกัด
        (command "mline" (car coords))
        
        ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
        (foreach coord (cdr coords)
          (command coord)
        )
      )
    )
  )

  ; เรียกใช้ฟังก์ชันสร้างพอลีไน
  (create-pline coord_data)
  (command "")
)

(defun c:P2MML_PLINE_TO_MULTI_MLINE ()
  
  (setq steel_width (cond ( (getreal (strcat "\nSpecify Steel Width or Depth <" (rtos (setq steel_width (cond (steel_width) (1.0) ) ) ) "> : " ) ) ) (steel_width) ) )
  
  (setq select_multi_ssnam (ssget 
                (list 
                  (cons 0 "POLYLINE,LWPOLYLINE")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  
  (setq select_multi_count (sslength select_multi_ssnam))
  (setq line_i 0)
  
  (while      
    (< line_i select_multi_count)
    (setq select_multi_ename (ssname select_multi_ssnam line_i))
    (setq select_multi_get (entget select_multi_ename))
    
    
    (setq select_multi_obj (vlax-ename->vla-object select_multi_ename))
    
                                (setq select_multi_get (entget select_multi_ename))
                                (setq tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) select_multi_get))
                                
                                  ; ใช้ mapcar เพื่อดึงข้อมูลออกมา
                                (setq zdataList (mapcar 'cdr tenthData))

                                ; นำข้อมูลที่ได้มาใช้ต่อ
                                (foreach data zdataList
                                  ; ทำสิ่งที่คุณต้องการกับข้อมูลที่ดึงออกมาได้ที่ตัวแปร data
                                  (princ data) ; ตัวอย่าง: พิมพ์ข้อมูลที่ได้
                                )
    
                                (setq coord_data zdataList)
                                
                                (setq coord_data_0 (list (nth  0 coord_data))) ;สำหรับ ไม่จบ loop สี่เหลี่ยม
                                (setq coord_data_00 (append coord_data coord_data_0)) ;สำหรับ จบ loop สี่เหลี่ยม
 


    
    (defun create-pline (coords)
      (if (= (length coords) 0)
          nil
        (progn
          ; สร้างพอลีไนจากค่าพิกัด
          (command "mline" (car coords))
          
          ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
          (foreach coord (cdr coords)
            (command coord)
          )
        )
      )
    )
    
    (create-pline coord_data_00)
      (command "")
    (vla-delete select_multi_obj)
      (setq set_mline (entlast))
      (setq set_mline_obj (vlax-ename->vla-object set_mline))
      (vla-get-mlinescale set_mline_obj)
      (vla-put-mlinescale set_mline_obj steel_width)
    (setq line_i (1+ line_i))

     
  )
)
(defun c:P2MML1_PLINE_TO_MULTI_MLINE () ;end_loop
  
  (setq steel_width (cond ( (getreal (strcat "\nSpecify Steel Width or Depth <" (rtos (setq steel_width (cond (steel_width) (1.0) ) ) ) "> : " ) ) ) (steel_width) ) )
  
  (setq select_multi_ssnam (ssget 
                (list 
                  (cons 0 "POLYLINE,LWPOLYLINE")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  
  (setq select_multi_count (sslength select_multi_ssnam))
  (setq line_i 0)
  
  (while      
    (< line_i select_multi_count)
    (setq select_multi_ename (ssname select_multi_ssnam line_i))
    (setq select_multi_get (entget select_multi_ename))
    
    
    (setq select_multi_obj (vlax-ename->vla-object select_multi_ename))
    
                                (setq select_multi_get (entget select_multi_ename))
                                (setq tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) select_multi_get))
                                
                                  ; ใช้ mapcar เพื่อดึงข้อมูลออกมา
                                (setq zdataList (mapcar 'cdr tenthData))

                                ; นำข้อมูลที่ได้มาใช้ต่อ
                                (foreach data zdataList
                                  ; ทำสิ่งที่คุณต้องการกับข้อมูลที่ดึงออกมาได้ที่ตัวแปร data
                                  (princ data) ; ตัวอย่าง: พิมพ์ข้อมูลที่ได้
                                )
    
                                (setq coord_data zdataList)
                                
                                (setq coord_data_0 (list (nth  0 coord_data))) ;สำหรับ ไม่จบ loop สี่เหลี่ยม
                                (setq coord_data_00 (append coord_data coord_data_0)) ;สำหรับ จบ loop สี่เหลี่ยม
 


    
    (defun create-pline (coords)
      (if (= (length coords) 0)
          nil
        (progn
          ; สร้างพอลีไนจากค่าพิกัด
          (command "mline" (car coords))
          
          ; วนลูปผ่านพิกัดทั้งหมดและเชื่อมต่อเส้นต่อเนื่อง
          (foreach coord (cdr coords)
            (command coord)
          )
        )
      )
    )
    
    (create-pline coord_data_00)
      (command "")
    (vla-delete select_multi_obj)
      (setq set_mline (entlast))
      (setq set_mline_obj (vlax-ename->vla-object set_mline))
      (vla-get-mlinescale set_mline_obj)
      (vla-put-mlinescale set_mline_obj steel_width)
    (setq line_i (1+ line_i))

     
  )
)

(defun c:p3mm_make_seg ()
  ;selection part
    (setq select_multi_pline (ssget 
                  (list 
                    (cons 0 "POLYLINE,LWPOLYLINE")       ;type of object
                    ; (cons 8 "000 - GRID")   ;kind of layer 
                    ; (cons 2 "SSSS")       ;kind of nameblock
                    ; (cons 62 1)           ;kind of color call sign with color code index
                  )
                )
    )
    (setq total_multi_pline (sslength select_multi_pline))
    (setq multi_pline_i 0)
    (setq multi_pline_ii 1)
    (setvar "osmode" 0)
  ;
  ;line_1
    (setq select_multi_pline_enam_i (ssname select_multi_pline multi_pline_i))
    (setq select_multi_pline_enam_i_get (entget select_multi_pline_enam_i))
    (setq select_multi_pline_obj (vlax-ename->vla-object select_multi_pline_enam_i))
    
    (setq line1_tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) select_multi_pline_enam_i_get))
    (setq line1_total_tenthData (vl-list-length line1_tenthData))
    (setq line1_tenthData_i 0)
    (setq line1_tenthData_ii 0)
  ;
  ;line_2
    (setq select_multi_pline_enam_ii (ssname select_multi_pline multi_pline_ii))
    (setq select_multi_pline_enam_ii_get (entget select_multi_pline_enam_ii))
    (setq select_multi_pline_obj (vlax-ename->vla-object select_multi_pline_enam_ii))
    
    (setq line2_tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) select_multi_pline_enam_ii_get))
    (setq line2_total_tenthData (vl-list-length line2_tenthData))
    (setq line2_tenthData_i 0)
    (setq line2_tenthData_ii 0)
  ;
  ;loop_mode
    (while 
      (and 
        (< line1_tenthData_i line1_total_tenthData) ;line1
        (< line1_tenthData_ii line2_total_tenthData) ;line2
      )
      (setq line1_tenthData_ename (cdr (nth line1_tenthData_i line1_tenthData)))
      (setq line2_tenthData_ename (cdr (nth line2_tenthData_ii line2_tenthData)))
    
      (command "line" line1_tenthData_ename line2_tenthData_ename "")
      (setq make_line (entlast))
      (setq new_line make_line)
      (setq new_line_obg (vlax-ename->vla-object new_line))
      (setq new_end_pt_xyz (vlax-safearray->list (vlax-variant-value (vla-get-endpoint new_line_obg))))
      (setq new_end_pt_x (car new_end_pt_xyz))
      (setq new_end_pt_y (cadr new_end_pt_xyz))
      (setq new_end_pt_xy (strcat (rtos new_end_pt_x 2 8) "," (rtos new_end_pt_y 2 8)))
      
      ;dlc part ไม่เอาก็ comment ไว้ก่อน
      (setq _pi  3.14)
      (setq new_angle (LM:round (*(/ (vla-get-angle new_line_obg) _pi) 180)))
      
      
      
        (command "insert" "A$C510cc02f" new_end_pt_xy 1 new_angle) 
        (vla-delete new_line_obg)
        ; (command "explode" "L")
  
             

      
        (setq line1_tenthData_i (+ line1_tenthData_i 1))
        (setq line2_tenthData_ii (+ line2_tenthData_ii 1))
        
      
      
      ;dlc part ไม่เอาก็ comment ไว้ก่อน
    )
  ;
  (setvar "osmode" 1215)
)

; (setq po1 (getpoint))
; (setq po2 (getorient))
; (setq po3 (getangle))
; (command "rotate" )