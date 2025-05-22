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
(defun c:z54_ins_blk_to_vertex_pline ()
  (setq REF_1st_LWPOLYLINE nil)
  (while (not REF_1st_LWPOLYLINE)
    (setq REF_1st_LWPOLYLINE (entsel "\nSelect a LWPOLYLINE "))
    (if 
      (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
        (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
        (progn
          (setq REF_1st_LWPOLYLINE nil)
          (alert "Please select a LWPOLYLINE.")
        )
    )
  )
  (if
    (/= REF_1st_LWPOLYLINE nil)
    (progn
       (setq tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE))) ;for pline

      ; ใช้ mapcar เพื่อดึงข้อมูลออกมา
      (setq zdataList (mapcar 'cdr tenthData))
      
      ; นำข้อมูลที่ได้มาใช้ต่อ
      (foreach data zdataList
        ; ทำสิ่งที่คุณต้องการกับข้อมูลที่ดึงออกมาได้ที่ตัวแปร data
        (princ data) ; ตัวอย่าง: พิมพ์ข้อมูลที่ได้
      )

      (setq coord_data_ zdataList)
      (setq coord_data_total (length coord_data_))
      (setq coord_data_i 0)
    )
    (princ "\n")
  )
  
  ;user_input_data
    (initget "1 2")
    (setq mode-v nil)
    ; (setq mode-v (cond ( (getint (strcat "\nSpecify object \nmode 1 = insert_by_point \nmode 2 = insert_by_block \n<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : " ) ) ) (mode-v) ) )
    (setq mode-v (cond ( (getkword (strcat "\nSpecify object \nmode 1 = insert_by_point \nmode 2 = insert_by_block \n<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : " ) ) ) (mode-v) ) )
    (if
      (= mode-v "2")
      (progn 
        (setq ref_efname nil)
        (while (not ref_efname) 
          (setq ref_efname (entsel "\nSelect a insert "))
          (if 
            (and ref_efname 
                 (= (cdr (assoc 0 (entget (car ref_efname)))) "INSERT")
            )
            (setq ref_efname (car ref_efname))
            (progn 
              (setq ref_efname nil)
              (alert "Please select a insert.")
            )
          )
        )
      )
      (princ "\n")
    )
    (if
      (= mode-v "2")
      (progn 
        (setq ref_efname_obj (vlax-ename->vla-object ref_efname))
        (setq ref_efname_efname (LM:effectivename ref_efname_obj))
        (setq get_sc_x (vla-get-xscalefactor ref_efname_obj ))
        (setq get_sc_y (vla-get-yscalefactor ref_efname_obj ))
        (setq get_sc_z (vla-get-zscalefactor ref_efname_obj ))
        (setq get_sc_z (vla-get-zscalefactor ref_efname_obj ))
        (setq get_sc_zzz (vla-get-blockscaling ref_efname_obj ))
        

      )
      (princ "\n")
    )
    (setq old_osmode (getvar "osmode"))
    (setvar "osmode" 0)
  ;
  
  
  (while
    (< coord_data_i coord_data_total)
    (setq coord_data_list (nth coord_data_i coord_data_))
    (cond
      (
        (and 
          (= mode-v "2")     
        ) 
        (progn 
          ; (command "insert" ref_efname_efname coord_data_list get_sc_x 0)
          (command "insert" ref_efname_efname coord_data_list get_sc_x get_sc_y get_sc_z 0)
          ; (command "insert" ref_efname_efname coord_data_list get_sc_x 0)
        )
      )
      (
        (and 
          (= mode-v "1")     
        ) 
        (progn 
          (command "point" coord_data_list)
        )
      )
    )
    (setq coord_data_i (+ coord_data_i 1))
  )
  (setvar "osmode" old_osmode)
)




