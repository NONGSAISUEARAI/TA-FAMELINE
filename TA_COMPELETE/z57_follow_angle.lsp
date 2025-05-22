;sub_func
  (defun deg2rad (deg)
    (* pi (/ deg 180.0))
  )
  (defun LM:effectivename ( obj )
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
  )

;
; (setq REF_1st_LINE nil)
;   (while (not REF_1st_LINE)
;     (setq REF_1st_LINE (entsel "\nSelect a First LINE "))
;     (if 
;       (and REF_1st_LINE (= (cdr (assoc 0 (entget (car REF_1st_LINE)))) "LINE"))
;         (setq REF_1st_LINE (car REF_1st_LINE))
        
;         (progn
;           (setq REF_1st_LINE nil)
;           (alert "Please select a LINE")
;         )
;     )
;     (if
;       (/= REF_1st_LINE nil)
;       (progn 
;         (setq REF_1st_LINE_obj (vlax-ename->vla-object REF_1st_LINE))
;         (setq REF_1st_LINE_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-endpoint REF_1st_LINE_obj))))
;         (setq ref_angle_ (atof (angtos (vla-get-angle REF_1st_LINE_obj))))
;         (setq ref_angle_1  (deg2rad ref_angle_))
;         (setq ref_angle_2 (deg2rad 90))
;       )
;       (princ "/n")
;     )
;   )
; (setq REF_1st_BLK nil)
;   (while (not REF_1st_BLK)
    
;     (setq REF_1st_BLK (entsel "\nSelect a First INSERT "))
;     (if 
;       (and REF_1st_BLK (= (cdr (assoc 0 (entget (car REF_1st_BLK)))) "INSERT"))
;         (setq REF_1st_BLK (car REF_1st_BLK))
        
;         (progn
;           (setq REF_1st_BLK nil)
;           (alert "Please select a INSERT")
;         )
;     )
;     (if
;       (/= REF_1st_BLK nil)
;       (progn 
;         (setq REF_1st_BLK_obj (vlax-ename->vla-object REF_1st_BLK))
;         (setq set_angle_ (atof 
;                            (angtos 
;                              ((vla-put-rotation REF_1st_BLK_obj 
;                                                 (- ref_angle_1 ref_angle_2)
;                               ) 
;                              )
;                            )
;                          )
;         )
;         (princ "/n")
;       )
;     )
;   )
(defun c:z57_follow_angle ()
  (setq my-selection-set_ (ssget 
                            (list 
                              (cons 0 "LINE") ;type of object
                              ;  (cons 8 "000 - GRID") ;kind of layer
                              ; (cons 2 "SSSS")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
  )
  (setq my-selection-set_total (sslength my-selection-set_ ))
  (setq my-selection-set_i 0)

  (while
  (setq my-selection-set_ename (ssname my-selection-set_ my-selection-set_i))
    (setq my-selection-set_obj (vlax-ename->vla-object my-selection-set_ename))
    (setq my-selection-set_ins_start (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my-selection-set_obj))))
    (setq my-selection-set_ex_angle (atof (angtos (vla-get-angle my-selection-set_obj))))
    (setq deg90_to_rad (deg2rad 90))
    (setq real_angle (+ my-selection-set_ex_angle 90))
    
    ;user_input
    (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
    (setq select_name_obj (vlax-ename->vla-object select_name))
    (setq efname_blk (LM:effectivename select_name_obj))
    ;
    
    
    (command "insert" efname_blk my-selection-set_ins_start 1 real_angle)
    (setq my-selection-set_i (+ my-selection-set_i 1 ))
    
  )
)

(defun c:z57m_match_angle ()
  (setq REF_1st_LINE nil)
    (while (not REF_1st_LINE)
      (setq REF_1st_LINE (entsel "\nSelect a First LINE "))
      (if 
        (and REF_1st_LINE (= (cdr (assoc 0 (entget (car REF_1st_LINE)))) "LINE"))
          (setq REF_1st_LINE (car REF_1st_LINE))
          
          (progn
            (setq REF_1st_LINE nil)
            (alert "Please select a LINE")
          )
      )
      (if
        (/= REF_1st_LINE nil)
        (progn 
          (setq REF_1st_LINE_obj (vlax-ename->vla-object REF_1st_LINE))
          (setq REF_1st_LINE_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-endpoint REF_1st_LINE_obj))))
          (setq ref_angle_ (atof (angtos (vla-get-angle REF_1st_LINE_obj))))
          (setq ref_angle_1  (deg2rad ref_angle_))
          (setq ref_angle_2 (deg2rad 90))
        )
        (princ "/n")
      )
    )
  (setq REF_block_ nil)
   (while (not REF_block_)
      (setq REF_block_ (entsel "\nSelect a First BLOCK "))
      (if 
        (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
          (setq REF_block_ (car REF_block_))
          
          (progn
            (setq REF_block_ nil)
            (alert "Please select a BLOCK")
          )
      )
      (if
        (/= REF_block_ nil)
        (progn 
          (setq REF_block_obj (vlax-ename->vla-object REF_block_))
          
          (setq ref_angle_blk (vla-put-rotation REF_block_obj ref_angle_1))
          (setq ref_angle_1  (deg2rad ref_angle_))
        )
        (princ "/n")
      )
    ) ;::::method 2
    ; (setq select_name_obj (vlax-ename->vla-object select_name))
    ; (setq efname_blk (LM:effectivename select_name_obj))
  
)
