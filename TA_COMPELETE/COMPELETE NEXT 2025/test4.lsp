(defun c:try1 ()
  ;preloop_and_while
    (setq new_sum_att_list_main_set_ '("FRONT VIEW" "SIDE VIEW") )
    (setq sum_att_list_sub_set_ '(("FRONT VIEW" ("6")) ("FRONT VIEW" ("1")) ("FRONT VIEW" ("2")) ("FRONT VIEW" ("3")) ("SIDE VIEW" ("5")) ("SIDE VIEW" ("4"))) )
    ; (setq sum_att_list_sub_set_ '(("FRONT VIEW" 6 ) ("FRONT VIEW" 1) ("FRONT VIEW" 2) ("FRONT VIEW" 3) ("SIDE VIEW" 5) ("SIDE VIEW" 4)) )
    (setq new_sum_att_list_main_set_i 0)
    (setq text_data_lv1 ())
    (setq text_data_lv2 ())
    (setq neww ())
    (setq temp_val_q13 ())
    (setq temp_val_q14 ())

    (setq new_sum_att_list_main_set_i 0)
    (setq sum_att_list_sub_set_i 0)
    (while (< new_sum_att_list_main_set_i (length new_sum_att_list_main_set_))
    ; (while (< new_sum_att_list_main_set_i 1)
      (setq new_sum_att_list_main_set_val_ (nth  new_sum_att_list_main_set_i new_sum_att_list_main_set_))
      ;preloop_and_while
        (setq sum_att_list_sub_set_i 0)
        (setq temp_val_q13 ())
        (while (< sum_att_list_sub_set_i (length sum_att_list_sub_set_))
          (setq sum_att_list_sub_set_ename_test (nth  sum_att_list_sub_set_i sum_att_list_sub_set_)) 
          (cond
            (
              (and
                (eq (type (car sum_att_list_sub_set_ename_test) ) 'STR)
                (eq (type new_sum_att_list_main_set_val_ ) 'STR)
                (eq new_sum_att_list_main_set_val_ (car sum_att_list_sub_set_ename_test) )
              )
              (progn
                (setq temp_val_q13 (cons (atoi (car (cadr sum_att_list_sub_set_ename_test))) temp_val_q13))
                ;TA:stanndard_lambda_sorting
                  (setq temp_val_q13 (vl-sort temp_val_q13  ;bigest open indent list
                                                            (function 
                                                              (lambda (a b) 
                                                                (< a b)
                                                              )
                                                            )
                                        ) ;bigest close indent list
                  )
                ;
                

              )
            )
          )
          (setq sum_att_list_sub_set_i (+ sum_att_list_sub_set_i 1))
        )
      ;
      (setq temp_val_q13 (append (list new_sum_att_list_main_set_val_) temp_val_q13  ))
      (setq temp_val_q14 (cons temp_val_q13 temp_val_q14) )
      (setq new_sum_att_list_main_set_i (+ new_sum_att_list_main_set_i 1))
    )
  ;
  ; (princ temp_val_q14 )
  (setq temp_val_q14 temp_val_q14)
)
(c:try) 