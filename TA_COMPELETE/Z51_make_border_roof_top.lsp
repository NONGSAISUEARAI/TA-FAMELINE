(defun c:Z51_make_border_roof_top ()
    (setq REF_1st_LWPOLYLINE nil)
    (while (not REF_1st_LWPOLYLINE)
      (setq REF_1st_LWPOLYLINE (entsel "\nSelect a polyline "))
      (if 
        (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
          (progn
            (setq REF_1st_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )
    ; (setq REF_1st_LWPOLYLINE_as_ins-set(vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE))) ;varaint test
    ; (setq xx (mapcar 'cdr REF_1st_LWPOLYLINE_as_ins-set)) ;varaint test
    (setq REF_1st_LWPOLYLINE_ins-set (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE))))

    (setq ref_point (append (car REF_1st_LWPOLYLINE_ins-set) (list 0) ))
    (setq ref_point1 (car REF_1st_LWPOLYLINE_ins-set))

    
    ; ; (command "copy" REF_1st_LWPOLYLINE "" (vlax-safearray->list (vlax-variant-value (vlax-3d-point ref_point))) ) 
    ; (command "copy" REF_1st_LWPOLYLINE "" (vlax-safearray->list (vlax-variant-value (vlax-3d-point ref_point1)))) 
    
    (setq REF_2nd_LWPOLYLINE nil)
    (while (not REF_2nd_LWPOLYLINE)
      (setq REF_2nd_LWPOLYLINE (entsel "\nSelect a polyline "))
      (if 
        (and REF_2nd_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_2nd_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_2nd_LWPOLYLINE (car REF_2nd_LWPOLYLINE))
          (progn
            (setq REF_2nd_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )
    
    (setq REF_2nd_LWPOLYLINE_get (entget REF_2nd_LWPOLYLINE))
    (setq REF_2nd_LWPOLYLINE_obj (vlax-ename->vla-object REF_2nd_LWPOLYLINE))
    
    ; (command "pselect" REF_2nd_LWPOLYLINE "")

    ; (setq REF_2nd_LWPOLYLINE_as_ins-set (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_2nd_LWPOLYLINE))) ;varaint test
    ; (setq xx (mapcar 'cdr REF_2nd_LWPOLYLINE_as_ins-set)) ;varaint test
    (setq REF_2nd_LWPOLYLINE_ins-set (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) REF_2nd_LWPOLYLINE_get)))

    (setq insset_i 0)
    (setq insset_ii 0)
    (setq REF_1st_LWPOLYLINE_ins-set_total (length REF_1st_LWPOLYLINE_ins-set))
    (setq REF_2nd_LWPOLYLINE_ins-set_total (length REF_2nd_LWPOLYLINE_ins-set))

    (while
      (and
        (< insset_i REF_1st_LWPOLYLINE_ins-set_total)
        (< insset_ii REF_2nd_LWPOLYLINE_ins-set_total)
      )
      (setq s1stset (nth insset_i REF_1st_LWPOLYLINE_ins-set))
      (setq s2ndset (nth insset_ii REF_2nd_LWPOLYLINE_ins-set))
      
      (command "line" s1stset s2ndset "")
      (command "CHPROP" "L" "" "LT" "DOTX2" "")
      (setq insset_i (+ insset_i 1))
      (setq insset_ii (+ insset_ii 1))
    )
    ; (vl-remove REF_2nd_LWPOLYLINE_obj)
)

(defun c:Z51ar_make_area_border_roof_top ()
    (setq REF_1st_LWPOLYLINE nil)
    (while (not REF_1st_LWPOLYLINE)
      (setq REF_1st_LWPOLYLINE (entsel "\nSelect a polyline "))
      (if 
        (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
          (progn
            (setq REF_1st_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )


    (setq REF_1st_LWPOLYLINE_obj (vlax-ename->vla-object REF_1st_LWPOLYLINE))
    (setq area_pline (/ (vla-get-area REF_1st_LWPOLYLINE_obj) 1000000))
    (setq area_text (strcat (rtos area_pline 2 4) " sq.m."))
    (setq ins_xyz (getpoint "specify_ins"))
    (setq text_height (cond ( (getreal (strcat "\nSpecify text_height <" (rtos (setq text_height (cond (text_height) (1.0) ) ) ) "> : " ) ) ) (text_height) ) )
    (command "text" ins_xyz text_height 0 area_text)
)
(defun c:Z51ar1_main_minus_sub ()
    (setq REF_1st_LWPOLYLINE nil)
    (while (not REF_1st_LWPOLYLINE)
      (setq REF_1st_LWPOLYLINE (entsel "\nSelect a polyline "))
      (if 
        (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
          (progn
            (setq REF_1st_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )
   
    (setq REF_2nd_LWPOLYLINE nil)
      (while 
        (= REF_2nd_LWPOLYLINE nil)
        (setq REF_2nd_LWPOLYLINE (entsel "\nSelect a Second LWPOLYLINE "))
        (if 
          (and REF_2nd_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_2nd_LWPOLYLINE)))) "LWPOLYLINE"))
            (setq REF_2nd_LWPOLYLINE (car REF_2nd_LWPOLYLINE))
            
            (progn
              (setq REF_2nd_LWPOLYLINE nil)
              (alert "Please select a LWPOLYLINE")
            )
        )
        (if
          (= 
            (setq REF_1st_LWPOLYLINE_handle (vlax-get-property (vlax-ename->vla-object REF_1st_LWPOLYLINE) "handle"))
            (setq REF_2nd_LWPOLYLINE_handle (vlax-get-property (vlax-ename->vla-object REF_2nd_LWPOLYLINE) "handle"))
          )
          (progn
              (setq REF_2nd_LWPOLYLINE nil)
              (setq REF_1st_LWPOLYLINE_handle nil)
              (setq REF_2nd_LWPOLYLINE_handle nil)
              (alert "This line has been choose \nPlease select a LWPOLYLINE \nนะจ๊ะ\nนะจ๊ะ\nนะจ๊ะ")
            )
          (princ "\n")
        )
      )
    (if
      (and
       (/= REF_1st_LWPOLYLINE nil)
       (/= REF_2nd_LWPOLYLINE nil)
      )
        (progn
          (setq REF_1st_LWPOLYLINE_obj (vlax-ename->vla-object REF_1st_LWPOLYLINE))
          (setq 1st_area_pline (/ (vla-get-area REF_1st_LWPOLYLINE_obj) 1000000))
          (setq REF_2nd_LWPOLYLINE_obj (vlax-ename->vla-object REF_2nd_LWPOLYLINE))
          (setq 2nd_area_pline (/ (vla-get-area REF_2nd_LWPOLYLINE_obj) 1000000))
          
          ;calculator_area
            (setq area_ (- 1st_area_pline 2nd_area_pline))
            (setq area_text (strcat (rtos area_ 2 4) " sq.m."))
          ;
            (setq ins_xyz (getpoint "specify_ins"))
            (setq text_height (cond ( (getreal (strcat "\nSpecify text_height <" (rtos (setq text_height (cond (text_height) (1.0) ) ) ) "> : " ) ) ) (text_height) ) )
            (command "text" ins_xyz text_height 0 area_text)
        ) 
    )

  
 )
(defun c:sum-area_text ()
  (setq my-text-set_ (ssget  '((0 . "text"))))
  (setq my-text-set_total (sslength my-text-set_))
  (setq my-text-set_i 0)
  
  (setq set_textstring_list_ '())
  (while
    (setq my-text-set_ename (ssname my-text-set_ my-text-set_i))
    (setq my-text-set_obj (vlax-ename->vla-object my-text-set_ename))
    (setq my-text-set_textstring (substr (vla-get-textstring my-text-set_obj) 1 7))
    (setq set_textstring (list (atof my-text-set_textstring)))
    
    (setq set_textstring_list_ (cons set_textstring set_textstring_list_ ))
    
    (setq my-text-set_i (+ my-text-set_i 1))
  )
 (setq sumlist (apply '+ (mapcar 'car set_textstring_list_ )))
)


