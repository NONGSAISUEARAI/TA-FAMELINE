(defun c:z55_test ()
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
  )
  
 (setq REF_2nd_LINE nil)
  (while (not REF_2nd_LINE)
    (setq REF_2nd_LINE (entsel "\nSelect a Second LINE "))
    (if 
      (and REF_2nd_LINE (= (cdr (assoc 0 (entget (car REF_2nd_LINE)))) "LINE"))
        (setq REF_2nd_LINE (car REF_2nd_LINE))
        
        (progn
          (setq REF_2nd_LINE nil)
          (alert "Please select a LINE")
        )
    )
    (if
      (= 
        (setq REF_1st_LINE_handle (vlax-get-property (vlax-ename->vla-object REF_1st_LINE) "handle"))
        (setq REF_2nd_LINE_handle (vlax-get-property (vlax-ename->vla-object REF_2nd_LINE) "handle"))
      )
      (progn
          (setq REF_2nd_LINE nil)
          (setq REF_1st_LINE_handle nil)
          (setq REF_2nd_LINE_handle nil)
          (alert "This line has been choose \nPlease select a LINE \nนะจ๊ะ\nนะจ๊ะ\nนะจ๊ะ")
        )
      (princ "\n")
    )
  )


  (if
    (and 
      (/= REF_1st_LINE nil)
      (/= REF_2nd_LINE nil)
    )
    (progn
      (setq 1st_L_obj_ (vlax-ename->vla-object REF_1st_LINE))
      (setq 2nd_L_obj_ (vlax-ename->vla-object REF_2nd_LINE))
      ;standard_variant
        (setq 1st_L_obj_ins_str (vlax-safearray->list (vlax-variant-value (vla-get-startpoint 1st_L_obj_))))
        (setq 1st_L_obj_ins_end (vlax-safearray->list (vlax-variant-value (vla-get-endpoint 1st_L_obj_))))
      
        (setq 2nd_L_obj_ins_str (vlax-safearray->list (vlax-variant-value (vla-get-startpoint 2nd_L_obj_))))
        (setq 2nd_L_obj_ins_end (vlax-safearray->list (vlax-variant-value (vla-get-endpoint 2nd_L_obj_))))
        ; (setq ssq (list 1665.30128448 -610.36489984 0) )
        ; (command "pselect" ssq )
      ;
      ;finding_slope_formula_set1
        ; ((ins_Yend - ins_Ystr) / (ins_Xend - ins_Xstr)) = slope
        (setq 1st_frac_top
          (- (cadr 1st_L_obj_ins_str) (cadr 1st_L_obj_ins_end)) 
        )
        (setq 1st_frac_bot
          (- (car 1st_L_obj_ins_str) (car 1st_L_obj_ins_end)) 
        )
        (setq 1st_line_slope (/ 1st_frac_top 1st_frac_bot))
      ; 
      ;find_cline1
        ;                               ins_Ystr = (slope * ins_Xstr) + c
        ;if slope < 0 (ติดลบ)    ให้      ins_Ystr + slope = c 
        ;if slope > 0 (ไม่ิติดลบ)  ให้      ins_Ystr - slope = c 
        (setq 1st_Mslope (* 1st_line_slope (car 1st_L_obj_ins_end)))
        (setq Cline1_logic
          (cond
            (
              (and
                (<= 1st_Mslope 0)
              )
              (progn
                (setq abs_1st_Mslope (abs 1st_Mslope))    
                (setq cline1 (+ (cadr 1st_L_obj_ins_end) abs_1st_Mslope))
                (princ 
                  (strcat "Y = " (rtos 1st_line_slope 2 2) "x "  
                    (if
                      (< 1st_Mslope 0)
                      (progn
                      "+ "
                      )
                      "-"
                    )
                    (rtos cline1 2 2)
                    
                  )       
                )
              )
            )
            (
              (and
                (> 1st_Mslope 0)
              )
              (progn
                (setq abs_1st_Mslope (abs 1st_Mslope))    
                (setq cline1 (- (cadr 1st_L_obj_ins_end) abs_1st_Mslope))
                (princ 
                  (strcat "Y = " (rtos 1st_line_slope 2 2) "x "  
                    (if
                      (< 1st_Mslope 0)
                      (progn
                      "- "
                      )
                      "+ "
                    )
                    (rtos cline1 2 2)
                   
                  )       
                )
              )
            )
            (princ "\n")
          )
        )
      ;
      ;finding_slope_formula_set2
        ; ((ins_Yend - ins_Ystr) / (ins_Xend - ins_Xstr)) = slope
        (setq 2nd_frac_top
          (- (cadr 2nd_L_obj_ins_str) (cadr 2nd_L_obj_ins_end)) 
        )
        (setq 2nd_frac_bot
          (- (car 2nd_L_obj_ins_str) (car 2nd_L_obj_ins_end)) 
        )
        (setq 2nd_line_slope (/ 2nd_frac_top 2nd_frac_bot))
      ;
      ;find_cline2
        ;                               ins_Ystr = (slope * ins_Xstr) + c
        ;if slope < 0 (ติดลบ)    ให้      ins_Ystr + slope = c 
        ;if slope > 0 (ไม่ิติดลบ)  ให้      ins_Ystr - slope = c 
        (setq 2nd_Mslope (* 2nd_line_slope (car 2nd_L_obj_ins_end)))
        (setq Cline2_logic
          (cond
            (
              (and
                (< 2nd_Mslope 0)
              )
              (progn
                (setq abs_2nd_Mslope (abs 2nd_Mslope))    
                (setq cline2 (+ (cadr 2nd_L_obj_ins_end) abs_2nd_Mslope))
                (princ 
                  (strcat "Y = " (rtos 2nd_line_slope 2 2) "x "  
                    (if
                      (< 2nd_Mslope 0)
                      (progn
                      "+ "
                      )
                      ""
                    )
                    (rtos cline2 2 2)
                    
                  )       
                )
              )
            )
            (
              (and
                (>= 2nd_Mslope 0)
              )
              (progn
                (setq abs_2nd_Mslope (abs 2nd_Mslope))    
                (setq cline2 (- (cadr 2nd_L_obj_ins_end) abs_2nd_Mslope))
                (princ 
                  (strcat "Y = " (rtos 2nd_line_slope 2 2) "x "  
                    (if
                      (< 2nd_Mslope 0)
                      (progn
                      "+ "
                      )
                      ""
                    )
                    (rtos cline2 2 2)
                    
                  )       
                )
              )
            )
            (princ "\n")
          )
        )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;
      ;finding_X_value_step_1
        ;summary_slope_value
          (cond 
            (
              (and
                (< 2nd_line_slope 0)
              )
              (progn
                (setq abs_2nd_line_slope (abs 2nd_line_slope))
                (setq summary_slope_value (+ 1st_line_slope abs_2nd_line_slope))
              )
            )
            (
              (and
                (>= 2nd_line_slope 0)
              )
              (progn
                (setq summary_slope_value (- 1st_line_slope 2nd_line_slope))
              )
            ) 
          )
        ;
        ;summmary_cline_value (move equation cline2 to cline)
          (cond
            (
              (and
                (< cline2 0)
              )
              (progn
                (setq minus_cline1 (- cline1))
                (setq summmary_cline_value (+ cline2 minus_cline1))
              )
            )
              (
              (and
                (>= cline2 0)
              )
              (progn
                (setq summmary_cline_value (- cline2 cline1))
              )
            )
          )
        
        (if
          (and
            (/= summary_slope_value nil)
            (/= summmary_cline_value nil)
          )
          (progn
            (setq insertion_point_x (/ summmary_cline_value summary_slope_value))
            (setq insertion_point_y (+ (* 1st_line_slope insertion_point_x) cline1))
          )
          (princ "/n")
        )
          
          
          (princ (setq insertion_point_xy (list insertion_point_x insertion_point_y)))
          (command "point" insertion_point_xy)
        
      ;
    )
  )
)

    
