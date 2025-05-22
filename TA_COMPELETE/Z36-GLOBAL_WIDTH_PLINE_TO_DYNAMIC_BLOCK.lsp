
(setq main (entget (car (entsel "\n OK GO"))))
  (setq main_ename (cdr (assoc -1 main )))
  (setq main_obj (vlax-ename->vla-object main_ename ))
  (vla-get-length main_obj)
  (vla-get-constantwidth main_obj)



  (setq associate_10th (vl-remove-if-not '(lambda (x) (= 10 (car x))) main)) ;for pline

  (setq front_set (car associate_10th))
  (setq back_set (cadr associate_10th))


  (setq front_set_ins_x (cadr front_set))
  (setq front_set_ins_y (caddr front_set))

  (setq back_set_ins_x (cadr back_set))
  (setq back_set_ins_y (caddr back_set))


  (setq main_glo_width (car (assoc 40 main)))
  (setq main_ins_xy (strcat (rtos front_set_ins_x 2 8) "," (rtos front_set_ins_y 2 8)))

  ; (command "insert" "001 - DYNAMINC ALU. TUBE V REV01." main_ins_xy 1 0)

  (command "insert" "001 - DYNAMINC ALU. TUBE V REV01" main_ins_xy 1 0)

  
(setq ss_widPLINE (ssget 
                    (list 
                      (cons 0 "LWPOLYLINE") ;type of object
                      ; (cons 8 "000 - GRID")   ;kind of layer
                      ; (cons 2 "SSSS")       ;kind of nameblock
                      ; (cons 62 1)           ;kind of color call sign with color code index
                    )
                  )
)
(setq ss_widPLINE_total (sslength ss_widPLINE))
(setq ss_widPLINE_i 0)
(while
  (< ss_widPLINE_i ss_widPLINE_total)
  (setq ss_widPLINE_ename (ssname ss_widPLINE ss_widPLINE_i))
  (setq ss_widPLINE_obj (vlax-ename->vla-object ss_widPLINE_ename))
  (setq ss_widPLINE_obj_globalwidth (vla-get-ConstantWidth ss_widPLINE_obj))
  
)

        