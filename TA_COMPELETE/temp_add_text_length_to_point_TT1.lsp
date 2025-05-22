
    (defun TA:Get_Pline_vertext_ins_point_ (ename_) ;must be (car (entsel))
      (setq Get_Pline_ (entget ename_))
      (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
      (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
      (setq ename+Get_Pline_vtx_pt_ (list
                                      ename_
                                      Get_Pline_vtx_pt_
                                    )
      
      )
    )
    (defun TA:get_vertex_len_ (ename_)
      ; (setq ref_line (car (entsel)))
      (setq ss (vlax-ename->vla-object ename_))
      (setq vertex_total (length (cadr (TA:Get_Pline_vertext_ins_point_ ename_))))
      (setq sum_ ())

      (setq vertex_i 0)
      (setq vertex_ii 1)

      (while (< vertex_i vertex_total)
        (cond
          (;_case_1
            (and
                (/= vertex_i (- vertex_total 1))
            )
            (progn
              ;get_data_len
                (setq len_i (vlax-curve-getDistatParam ss vertex_i) )
                (setq len_ii (vlax-curve-getDistatParam ss vertex_ii))
                ;
                ;sum
                  (setq sum (list (- len_ii len_i)))
                  (setq sum_ (append sum sum_ ))
                ;
              (princ "_case_1\n")
            )
          )
          (;_case_2
            (and
              (= vertex_i (- vertex_total 1))    
            )
            (progn
              ;ไม่มี
              (princ "_case_2\n")
            )
          )

        )
        
        
        (setq vertex_i (+ vertex_i 1))
        (setq vertex_ii (+ vertex_ii 1))
      )
      ;summary_reverse
        (setq sum_len (reverse sum_))
      ;
    )
    (defun c:temp_add_text_length_to_point_TT1 ()
      
    
      
      ;userinput_
        (setq newline_ (car (entsel)))
        (setq text_height (cond ( (getreal (strcat "\nspecify text height<" (rtos (setq text_height (cond (text_height) (1.0) ) ) ) "> : " ) ) ) (text_height) ) )
      ;
      ;getdata_
        (setq newline_pt_ (cadr (TA:Get_Pline_vertext_ins_point_ newline_)))
        (setq text_height_ 2)
        (setq newline_length_ (TA:get_vertex_len_ newline_))
      ;
      
      
      ;preloop_and_while
        (setq text_loop_i 1)
        (while (< text_loop_i (length newline_pt_))
          (setq text_loop_pt_ (nth  text_loop_i newline_pt_))
          (setq text_loop_L_ (nth  (- text_loop_i 1) newline_length_))
          
            ; (create-text-vla 
              ;   (setq ins_pt (vlax-3d-point 
              ;                  (list 
              ;                    (car text_loop_pt_)
              ;                    (cadr text_loop_pt_)
              ;                    0
              ;                  )
              ;                )
              ;   ) 
              ;   (rtos text_loop_L_ 2 2) 
              ;   text_height_  
              ;   24 
            ; )
          (command "text" 
                    (list 
                      (car text_loop_pt_)
                      (cadr text_loop_pt_)
                      0
                    )
                    1
                    0
                    (rtos text_loop_L_ 2 2)
          )
          (setq text_loop_i (+ text_loop_i 1))
        )
      ;
    )
