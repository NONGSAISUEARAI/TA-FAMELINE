;PURPLE_LINE_
  (defun c:insertion_AL_Ceiling_300SK_skins_ ()
    
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "lwpolyline") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget 
                                            (list
                                              (cons 0 "lwpolyline") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (setq ss_pre_filter_set_xx_ii 1)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        
          ;insertion_by_center_recline
          (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
          (setq ss_pre_filter_set_xx_ename_centroid_ (LM:PolyCentroid ss_pre_filter_set_xx_ename_ ))
          (setq Bo_line_ (TA:Find_boundary_line_ ss_pre_filter_set_xx_ename_ ))
          (setq dist_x
                (distance 
                  (car Bo_line_)
                  (list (car (cadr bo_line_)) (cadr (car bo_line_)))
                )
          )
          (setq dist_y
                (distance 
                  
                  (list (car (cadr bo_line_)) (cadr (car bo_line_)))
                  (cadr Bo_line_)
                )
          )
          ; (TA:set-block-blockscaling "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW" acUniform)
          (TA:vla-insertblock "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"  ss_pre_filter_set_xx_ename_centroid_ 1 0)
          
          
          (setq new_dynamic_blk_ (entlast))
          (setq new_dynamic_blk_obj_ (vlax-ename->vla-object new_dynamic_blk_))
          (LM:setdynpropvalue new_dynamic_blk_obj_ "ALL_X" dist_x )
          (LM:setdynpropvalue new_dynamic_blk_obj_ "ALL_Y" dist_y)

          (vla-delete ss_pre_filter_set_xx_obj_ ) 

        ;screen process
          (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
          ; (princ 
          ;   (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n")
          ; ) 
          (cond
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 1)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 2)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 3)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 4)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 5)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 6)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 7)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 8)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 9)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 10)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
            
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 20)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "] \n" )
                )
                (setq ss_pre_filter_set_xx_ii 1)
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 19)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 18)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 17)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 16)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 15)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 14)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 13)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 12)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]] \n " )
                )
              )
            )
            (;_case_1
              (and
                (= ss_pre_filter_set_xx_ii 11)
              )
              (progn
                (princ 
                  (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0) " (" (rtos percent 2 0) "%" ")" "]]]]]]]]]] \n " )
                  
                )
              )
            )
            
          )
          
          (textscr)
        ; 
        
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        (setq ss_pre_filter_set_xx_ii (+ ss_pre_filter_set_xx_ii 1))
      )
    ;
  )
  (defun c:temp_ppline_start-end_insk1 ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (+ (cadr user_input_pt_) 75)
                        )
    )

    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_center_insk2 ()
    (setq user_input_pt_ (getpoint "specify point "))
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_" user_input_pt_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_mk300sk ()
    ; (setq line_ename_ (car (entsel "specify Object")))
    ; (setq line_obj_ (vlax-ename->vla-object line_ename_))
    ; (setq line_obj_length (vla-get-length line_obj_))
    
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_1 (list 
                          (+ (car user_input_pt_) 150)
                          (cadr user_input_pt_)
                          (caddr user_input_pt_)
                        )
    )
    ; (setq H (- (* 2 (getdist user_input_pt_ )) 150))
    (setq H (- (* 2 (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_" user_input_pt_1 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
    
    
    
  )
  (defun c:temp_ppline_mk400sk ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (+ (cadr user_input_pt_) 75)
                        )
    )
    ; (setq H (- (getdist user_input_pt_new_ ) 100))
    (setq H (- (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_mk500sk ()
    (setq user_input_pt_ (getpoint "specify point "))
    (setq user_input_pt_new_ (List 
                          (+ (car user_input_pt_) 150)
                          (- (cadr user_input_pt_) 75)
                        )
    )
    ; (setq H (- (getdist user_input_pt_new_ ) 100))
    (setq H (- (distance user_input_pt_ (list (car user_input_pt_) (cadr (getpoint "specify point length")) ) ) 150 ) )
    (setq array_length_ (getdist user_input_pt_new_ ))
    (setvar "osmode" 0)
    (command "insert" "000TYP-LINE_AND_ARRAY_@400mm._TYPE1_" user_input_pt_new_ 1 0 )
    (setq new_ename_ (entlast))
    (setq new_obj_ (vlax-ename->vla-object new_ename_))
    (LM:setdynpropvalue new_obj_ "lookup1" "AL-C300SK")
    (LM:setdynpropvalue new_obj_ "Flip state1" 1)
    (LM:setdynpropvalue new_obj_ "h" H   )
    (LM:setdynpropvalue new_obj_ "array_width" (+ 400 array_length_) )
    (setvar "osmode" 1215)
    
    (vla-put-color new_obj_ 3)
    
  )
  (defun c:temp_ppline_convert_dynamic_side_condyn ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget 
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
      (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"))
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq get_dyn_all_x (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_x"))
        (setq get_dyn_all_y (rtos (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_y") 2 0))
        ;input back_
          (if (= get_dyn_all_y "300")
            (progn
              (vla-put-rotation ss_pre_filter_set_xx_obj_ (deg-to-rad 90))
              (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "all_y" get_dyn_all_x)
              (LM:setdynpropvalue ss_pre_filter_set_xx_obj_ "all_x" get_dyn_all_y)
            )
          )
          
        ;
        ;screen process
        (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
        (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
        (textscr)
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
  )
  (defun c:temp_ppline_getalllength_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget 
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      ;
        (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000 - FAMELINE_AL.CELING_300SK_TOP_VIEW"))
      ;
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (setq sum_ ())
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        
        (setq get_dyn_all_y (rtos (LM:getdynpropvalue ss_pre_filter_set_xx_obj_ "all_y") 2 2))
        (setq sum_ (cons get_dyn_all_y sum_))
        ;screen process
          (setq percent (fix (* (/ (float ss_pre_filter_set_xx_i) (sslength ss_pre_filter_set_xx_)) 100)))  
          (princ (strcat "tranfering " (rtos ss_pre_filter_set_xx_i 2 0) "/" (rtos (sslength ss_pre_filter_set_xx_) 2 0)  " (" (rtos percent 2 0) "%" ")" "\n"))
          (textscr)
        ; 
          ; (princ)
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;)
    ;TA:stanndard_lambda_sorting
      (setq sum_sorted_ (vl-sort sum_  ;bigest open indent list
                                (function 
                                  (lambda (a b) 
                                    (< a b)
                                  )
                                )
                        ) ;bigest close indent list
      )
    ;
    
    (princ (TA:Count_item_in_list_ sum_sorted_ ))
    (princ (setq sum_length_all_ (LM:CountItems sum_sorted_ )))
      (if (= vla-obj_app nil)
      (progn
        (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
        
      )
    )
    (setq Excel_obj_ (TA:New_Excel_File_NEX_ vla-obj_app ))
    ;preloop_and_while
      (setq sum_length_all_i 0)
      (setq row_i 3)
      (while (< sum_length_all_i (length sum_length_all_))
        (setq sum_length_all_main_ (car (nth  sum_length_all_i sum_length_all_)))
        (setq sum_length_all_sub_ (cdr (nth  sum_length_all_i sum_length_all_)))

        (setq Excel_entlast_ (caddr (car (nth 3 (car (reverse (TA:Excel_Assembly_ALL-obj_list_ Excel_obj_ )))))))
        
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 1 "AL.300SK" )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 2 sum_length_all_main_ )
        (TA:Excel_input_data_R1C1 Excel_entlast_ row_i 3 sum_length_all_sub_ )
        
        (setq row_i (+ row_i 1))
        (setq sum_length_all_i (+ sum_length_all_i 1))
      )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 1 "Matt." )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 2 "legnth" )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 2 3 "qty." )
      (TA:Excel_input_data_R1C1 Excel_entlast_ 1 1 (getvar "dwgname") )
        
    ;
    
  )
  (defun c:c+ ()
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          ;selection_set_for_fillter_blk_name
            (if  ;pre_select_ssget_or_post_select_ssget
              (=
                (setq ss_pre_filter_set_xx_ (ssget "i"
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
                nil
              )
              (progn
                (setq ss_pre_filter_set_xx_ (ssget 
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
              )
              (sslength ss_pre_filter_set_xx_)
            )
          ;
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq get_l (LM:getdynpropvalue sss_obj_ "array_width" ) )
                  (LM:setdynpropvalue sss_obj_ "array_width" (+ get_l 250) )
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_)) 
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ) )
            (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq get_l (LM:getdynpropvalue sss_obj_ "array_width"))
            (LM:setdynpropvalue sss_obj_ "array_width" (+ get_l 250))
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )
        )
        
      )
    ;

  )
  (defun c:c- ()
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          ;selection_set_for_fillter_blk_name
            (if  ;pre_select_ssget_or_post_select_ssget
              (=
                (setq ss_pre_filter_set_xx_ (ssget "i"
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
                nil
              )
              (progn
                (setq ss_pre_filter_set_xx_ (ssget 
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
              )
              (sslength ss_pre_filter_set_xx_)
            )
          ;
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq get_l (LM:getdynpropvalue sss_obj_ "array_width" ) )
                  (LM:setdynpropvalue sss_obj_ "array_width" (- get_l 250) )
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_)) 
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ) )
            (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq get_l (LM:getdynpropvalue sss_obj_ "array_width"))
            (LM:setdynpropvalue sss_obj_ "array_width" (- get_l 250))
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )
        )
        
      )
    ;

  )
  (defun c:y+ ()
    ;for moving support frame Fameline celing SK300 (0.025 / 1 enter)
    ;set paremeter for moving
      (setq move_length_ 25)
    ;
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          ;selection_set_for_fillter_blk_name
            (if  ;pre_select_ssget_or_post_select_ssget
              (=
                (setq ss_pre_filter_set_xx_ (ssget "i"
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
                nil
              )
              (progn
                (setq ss_pre_filter_set_xx_ (ssget 
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
              )
              (sslength ss_pre_filter_set_xx_)
            )
          ;
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (+ (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (+ (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;

  )
  (defun c:y- ()
    ;for moving support frame Fameline celing SK300 (0.025 / 1 enter)
    ;set paremeter for moving
      (setq move_length_ 25)
    ;
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          ;selection_set_for_fillter_blk_name
            (if  ;pre_select_ssget_or_post_select_ssget
              (=
                (setq ss_pre_filter_set_xx_ (ssget "i"
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
                nil
              )
              (progn
                (setq ss_pre_filter_set_xx_ (ssget 
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
              )
              (sslength ss_pre_filter_set_xx_)
            )
          ;
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (- (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
                  (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
                  (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
                  (setq sss_obj_putins_ (list
                                          (car sss_obj_ins_)
                                          (- (cadr sss_obj_ins_) move_length_) 
                                          (caddr sss_obj_ins_)
                                        ) 
                  )
                  (vla-put-insertionpoint sss_obj_ (vlax-3d-point sss_obj_putins_))
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;

  )
  (defun c:TEMP:copy_from_basepoint_cfb_ () 
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                    (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                  )
                tar_result_      (list (car tar_copy_obj_ins) 
                                        (cadr tar_copy_obj_ins)
                                  )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
              tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
              tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
              tar_result_ (list (car tar_copy_obj_ins)(cadr tar_copy_obj_ins))
        )
        
        
        (command "_copy" "m" tar_copy_obj_ins   )
        
        
      )
      (princ "\n")
    )
  )
  (defun c:TEMP:copy_from_basepoint_cfs_ () 
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 75)
                                      0
                                )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 75)
                                      0
                                )
          )
        
        
        
        ; (command "_copy" "m" tar_copy_ "" tar_copy_obj_ins  )
        (command "_copy" "m" tar_copy_obj_ins   )
        
      )
      (princ "\n")
    )
  )
  (defun c:TEMP:copy_from_basepoint_cfs1_ () 
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq tar_copy_        (car (entsel))
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 0)
                                      0
                                )
          )
          (command "_copy" tar_copy_ "" "m" tar_copy_obj_ins)
        )
        (princ "\n")
      )
    ;
    (if (/= ss_pre_filter_set_xx nil)
      (progn 
        (setq tar_copy_        (ssname ss_pre_filter_set_xx 0)
                tar_copy_obj_    (vlax-ename->vla-object tar_copy_)
                tar_copy_obj_ins (vlax-safearray->list 
                                  (vlax-variant-value (vla-get-insertionpoint tar_copy_obj_))
                                )
                tar_copy_obj_ins (list (+ (car tar_copy_obj_ins) 150) 
                                      (- (cadr tar_copy_obj_ins) 0)
                                      0
                                )
          )
        
        
        
        ; (command "_copy" "m" tar_copy_ "" tar_copy_obj_ins  )
        (command "_copy" "m" tar_copy_obj_ins   )
        
      )
      (princ "\n")
    )
  )
  (defun c:Resupport_spacing_DP1_ ()
    (setq insert_int_spcaing_dist_ (getint "insert_int_spcaing_dist_" ))
    ;user_input_
      (initget "Yes No")
      (setq ss_ ;Detect the variable's result in case of a function error
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (setq DELETE_OBJ_ (not (eq "No" (getkword "\nKeep Existing point with object? [Yes/No] <Y>: "))))
            )
          )
        )
      )
      (if (or (= ss_ nil) (= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          ;selection_set_for_fillter_blk_name
            (if  ;pre_select_ssget_or_post_select_ssget
              (=
                (setq ss_pre_filter_set_xx_ (ssget "i"
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
                nil
              )
              (progn
                (setq ss_pre_filter_set_xx_ (ssget 
                                                  (list
                                                    (cons 0 "INSERT") ;type of object
                                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                                    ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                                  )
                                            )
                )
              )
              (sslength ss_pre_filter_set_xx_)
            )
          ;
          ;
            ; (setq ss_pre_filter_set_xx_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx_ "000TYP-LINE_AND_ARRAY_@400mm._TYPE2_"))
          ;
          ;preloop_and_while
            (setq ss_pre_filter_set_xx_i 0)
            (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
              (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            
              (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
              (setq sss_obj_rotation_ (rad-to-deg (vla-get-rotation sss_obj_ )))
              
              ;get-dyn-blk_process
                (setq H (LM:getdynpropvalue sss_obj_ "H"))
                (setq array_width (LM:getdynpropvalue sss_obj_ "array_width"))
              ;
              (setq insert_blk_name_ (strcat "000TYP-LINE_AND_ARRAY_@" 
                                            (rtos insert_int_spcaing_dist_ 2 0)
                                            "mm._start"
                                    )
              )
              ;insertion-new_object_
                (command "insert" insert_blk_name_ sss_obj_ins_ 1 sss_obj_rotation_ )
                (setq new_ename_ (entlast))
                (setq new_obj_ (vlax-ename->vla-object new_ename_))
              
                (LM:setdynpropvalue new_obj_ "H" H)
                (LM:setdynpropvalue new_obj_ "array_width" array_width)
              ;
              ;delete main_obj_
                (vla-delete sss_obj_ )
              ;

              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
          ;
        
        )
      )
      (if (and (/= ss_ nil) (/= (vl-prin1-to-string ss_) "#<%catch-all-apply-error%>")  )
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
              (if (= (setq sss_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_)) nil)
                (progn
                  (setq sss_obj_ (vlax-ename->vla-object (entlast)))
                )
              )
              
            
              (setq sss_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sss_obj_))))
              (setq sss_obj_rotation_ (rad-to-deg (vla-get-rotation sss_obj_ )))
              
              ;get-dyn-blk_process
                (setq H (LM:getdynpropvalue sss_obj_ "H"))
                (setq array_width (LM:getdynpropvalue sss_obj_ "array_width"))
              ;
              (setq insert_blk_name_ (strcat "000TYP-LINE_AND_ARRAY_@" 
                                            (rtos insert_int_spcaing_dist_ 2 0)
                                            "mm._start"
                                    )
              )
              ;insertion-new_object_
                (command "insert" insert_blk_name_ sss_obj_ins_ 1 sss_obj_rotation_ )
                (setq new_ename_ (entlast))
                (setq new_obj_ (vlax-ename->vla-object new_ename_))
              
                (LM:setdynpropvalue new_obj_ "H" H)
                (LM:setdynpropvalue new_obj_ "array_width" array_width)
              ;
              ;delete main_obj_
                (vla-delete sss_obj_ )
              ;
              (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
            )
        )
        
      )
    ;
  )
  (defun c:Assembly_blcok_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget 
                                            (list
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    
  
  
  )
  ;000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET
  (defun c:PPLINE_300hk_ins300hk ()
    (setq line_ename_ (car (entsel "specify Object")))
    (setq line_obj_ (vlax-ename->vla-object line_ename_))
    ;main_idea_coding_code
      (if (= (vla-get-objectname line_obj_) "AcDbLine")
        (progn
          ;standard properties line
          (setq line_obj_start_end_pt_ (list
                                          (setq line_obj_startpt_ (vlax-safearray->list
                                                                     (vlax-variant-value ( vla-get-startpoint line_obj_ ))
                                                                   )
                                          )
                                          (setq line_obj_endpt_ (vlax-safearray->list
                                                                    (vlax-variant-value ( vla-get-endpoint line_obj_ ))
                                                                 )
                                          )
                                        )
          )
          (setq line_obj_midpt_ (TA:midpoint line_obj_startpt_ line_obj_endpt_))
          (setq line_obj_angle (rad-to-deg (vla-get-angle line_obj_ )))
          (setq new_line_obj_angle (TA:ModifyRotation line_obj_angle -90))
          (setq line_obj_length (vla-get-length line_obj_))
          ;
          ;insertion process
            (TA:vla-insertblock "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" line_obj_midpt_ 1 (deg-to-rad new_line_obj_angle) )
            ;new_blk_entlast object
              (setq new300HK_ename (entlast))
              (setq new300HK_obj_ (vlax-ename->vla-object new300HK_ename))
              ;input data
                (cond
                  (;total_length_process_case_1
                     (and
                        ; (> line_obj_length 3000)
                        (= (LM:effectivename new300HK_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" )
                        (= "this dynamic block was create by me" "this dynamic block was create by me")
                     )
                     (progn
                       (LM:setdynpropvalue new300HK_obj_ "view" "2_top_view")
                       (LM:setdynpropvalue new300HK_obj_ "array_panel_length" line_obj_length)
                       (LM:setdynpropvalue new300HK_obj_ "panel_width" (getint "spectify panel_width "))
                       (command "draworder" new300HK_ename "" "F" )
                       
                       (princ "total_length_process_case_1")
                     )
                  )
                  (;total_length_process_case_2
                     (and
                        (> line_obj_length 3000)
                        (= (LM:effectivename new300HK_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET" )
                        (/= "this dynamic block was create by me" "this dynamic block was create by me")
                     )
                     (progn
                       (princ "total_length_process_case_2")
                     )
                  )
                )    
              ;
            ;
          ;
        )
      )
    ;
  )
  ;
  ;000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02AA_COMBINE_VIEW
  (defun c:PPLINE_300HK_insHR_ ()
    (setq 300HK_ename_ (car (entsel "specify Object")))
    (setq 300HK_obj_ (vlax-ename->vla-object 300HK_ename_))
    ;main_idea_coding_code
      (if (= (LM:Effectivename 300hk_obj_) "000 - DYNAMIC_ARRAY+COMBINE+VIEW_ASSCESSORIES_FAMELINE_AL.CELL_CELING_PANEL_300HK_COMBINE_SET")
        (progn
          ;get_ex-insertion_pt_300HK
            (list 
              (setq HR-02A_top_point_x (LM:getdynpropvalue 300hk_obj_ "HR-02A_top_point x" ) )
              (setq HR-02A_top_point_y (LM:getdynpropvalue 300hk_obj_ "HR-02A_top_point y" ) )
            )
            (list 
              (setq HR-02B_top_point_x (LM:getdynpropvalue 300hk_obj_ "HR-02B_top_point x" ) ) 
              (setq HR-02B_top_point_y (LM:getdynpropvalue 300hk_obj_ "HR-02B_top_point Y" ) )
            )
            (setq 300hk_obj_ins_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint 300hk_obj_))))
            (setq 300hk_obj_rotation_ (TA:ModifyRotation (rad-to-deg (vla-get-rotation 300hk_obj_ ) ) 180))
          ;
          ;insertion_panel_insertion_point_
            (setq HR-02A_top_point_preins_ (list 
                                              (+ ( car 300hk_obj_ins_ ) HR-02A_top_point_x  )
                                              (+ ( cadr 300hk_obj_ins_ ) 0  )
                                            )
            )
            (setq HR-02B_top_point_preins_ (list 
                                              (+ ( car 300hk_obj_ins_ ) HR-02B_top_point_x  )
                                              (+ ( cadr 300hk_obj_ins_ ) 0  )
                                            )
            )
          ;
          ;get_dynamic_data_
            (setq array_panel_length (LM:getdynpropvalue 300hk_obj_ "array_panel_length"))
          ; 
          ;insertion_HR_side_A
            (TA:vla-insertblock "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02AA_COMBINE_VIEW" HR-02A_top_point_preins_ 1 (deg-to-rad 90)) 
            (setq new_ename_ (entlast))
            (setq new_obj_ (vlax-ename->vla-object new_ename_)) 
            (command "rotate" new_ename_ "" 300hk_obj_ins_ (rad-to-deg (vla-get-rotation 300hk_obj_ ) ))         
            (LM:setdynpropvalue new_obj_ "view" "2_top_view")
            (setq array_length (LM:setdynpropvalue new_obj_ "array_length" (* (fix (/ array_panel_length 2400)) 2400 ) ))
            (cond ;visibility&length condition
              ( ;visibility condition
                (or 
                  (> array_panel_length 3000)
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "2_top_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_left"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_right"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                )
              )
              ( ;visibility condition
                (and 
                  (< array_panel_length 3000)
                  
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "7_dynamic_length_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_middle"   
                    (- array_panel_length 100)
                  )
                )
              )
            )
          ;
          ;
          ;insertion_HR_side_B
            (TA:vla-insertblock "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_Z-PROFILE_HR-02B_COMBINE_VIEW" HR-02B_top_point_preins_ 1 (deg-to-rad 90))
            (setq new_ename_ (entlast))
            (setq new_obj_ (vlax-ename->vla-object new_ename_))   
            (command "rotate" new_ename_ "" 300hk_obj_ins_ (rad-to-deg (vla-get-rotation 300hk_obj_ ) ))
            (LM:setdynpropvalue new_obj_ "view" "2_top_view")
            (setq array_length (LM:setdynpropvalue new_obj_ "array_length" (* (fix (/ array_panel_length 2400)) 2400 ) ))
            (cond ;visibility&length condition
              ( ;visibility condition
                (or 
                  (> array_panel_length 2400)
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "2_top_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_left"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_right"   
                    (if (= (- array_panel_length array_length) 0) ;condition for input fragment left/right value
                      (progn
                        0
                      )
                      (- (/ (- array_panel_length array_length ) 2) 50)
                    )
                  )
                )
              )
              ( ;visibility condition
                (and 
                  (< array_panel_length 2400)
                  
                )
                (progn
                  (LM:setdynpropvalue new_obj_ "view" "7_dynamic_length_view")
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_middle"   
                    (- array_panel_length 100)
                  )
                  (LM:setdynpropvalue 
                    new_obj_ 
                    "fragment_array_length"   
                    array_panel_length
                  )
                )
              )
            )
            
        )
      )
    ;
  )
  ;
  ;000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET
  (defun c:PPLINE_300HK_insHanging_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      ;get-rotaton_object_
        (setq angle_ename_ (car (entsel "specify Object")))
        (setq angle_obj_ (vlax-ename->vla-object angle_ename_))
      ;
    
      (foreach x (LM:intersectionsinset ss_pre_filter_set_xx_)
        (TA:vla-insertblock 
          "000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET"
          x
          1
          (cond ;get rotate condition
            (;Line PLine condition 
              (or
                (= "AcDbLine" (vla-get-objectname angle_obj_))
                (= "AcDbPolyline" (vla-get-objectname angle_obj_ ))   
              )
              (progn
                (vla-get-angle angle_obj_)
              )
            )
            (;BLK condition 
              (and
                (= "AcDbBlockReference" (vla-get-objectname angle_obj_))
                (= "AcDbBlockReference" (vla-get-objectname angle_obj_ ))   
              )
              (progn
                (vla-get-rotation angle_obj_)
              )
            )
          )
        )
          ;new_blk_entlast object
          (setq new_hanging_ename (entlast))
          (setq new_hanging_obj_ (vlax-ename->vla-object new_hanging_ename))
          (LM:setdynpropvalue new_hanging_obj_ (LM:getvisibilityparametername new_hanging_obj_) "2_top_view" )
          ;
        
        
      )
    ;
  )
  (defun c:PPLINE_300HK_insCHanging_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "LWPOLYLINE,LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
      ;user_input_
        (setq distance_measure_ 1200)
      ;
      ;preloop_and_while
        (setq ss_pre_filter_set_xx_i 0)
        (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
          ;main Line PLine Properties
            (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
            (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
            (setq ss_pre_filter_set_xx_obj_angle  (vla-get-angle ss_pre_filter_set_xx_obj_))
            (setq ss_pre_filter_set_xx_obj_length (vla-get-length ss_pre_filter_set_xx_obj_))
            (setq distance_measure_ (if (< ss_pre_filter_set_xx_obj_length 1200) (progn (fix (- ss_pre_filter_set_xx_obj_length 100 ))) distance_measure_)) 
            (setq ss_pre_filter_set_xx_obj_fragment_length_ (/ (- ss_pre_filter_set_xx_obj_length (* (fix (/ ss_pre_filter_set_xx_obj_length distance_measure_)) distance_measure_) ) 2))
            (setq ss_pre_filter_set_xx_obj_time_full_length_ (fix (/ ss_pre_filter_set_xx_obj_length distance_measure_)))
            (if (= ss_pre_filter_set_xx_obj_fragment_length_ 0) ;incase for number spcaing distance
              (progn
                (setq ss_pre_filter_set_xx_obj_time_full_length_ ss_pre_filter_set_xx_obj_time_full_length_ )
              )
              (setq ss_pre_filter_set_xx_obj_time_full_length_ (+ 2 ss_pre_filter_set_xx_obj_time_full_length_) )
            )
            ;preloop_and_while measure_spcaing_distance
              (setq measure_length_ ())
              (setq time_i 0)
              (while (< time_i ss_pre_filter_set_xx_obj_time_full_length_)
                (if 
                  (or (= time_i 0) (= time_i (- ss_pre_filter_set_xx_obj_time_full_length_ 1)) )
                  (progn
                    (setq measure_length_ (cons ss_pre_filter_set_xx_obj_fragment_length_ measure_length_))
                  )
                  (setq measure_length_ (cons distance_measure_ measure_length_))
                )
                (setq time_i (+ time_i 1))
              )       
            ;
            ;preloop_and_while measure_val_distance
              (setq measure_length_i 0)
              (setq finish_measure_length_ 0)
              (setq finish_measure_length_result ())
              (while (< measure_length_i (length measure_length_))
                (setq measure_length_ename_ (nth  measure_length_i measure_length_))
                (setq finish_measure_length_ (+ finish_measure_length_ measure_length_ename_)) 
                (setq finish_measure_length_result (cons finish_measure_length_ finish_measure_length_result))
                (setq measure_length_i (+ measure_length_i 1))
              )
            ;
          ;
          ;insertion_process
            (foreach x (vl-remove (nth (- (length finish_measure_length_result) 1) (reverse finish_measure_length_result) ) (reverse finish_measure_length_result) )
              (TA:vla-insertblock 
                "000 - DYNAMIC_LENGTH+COMBINE+VIEW_ASSCESSORIES_FAMELINE_BK-08_HANGING_SYSTEM_300HK_COMBINE_SET"
                (vlax-curve-getpointatdist ss_pre_filter_set_xx_obj_ x)
                1
                ss_pre_filter_set_xx_obj_angle
              )
              ;new_blk_entlast object
                (setq newblk_ename (entlast))
                (setq newblk_obj (vlax-ename->vla-object newblk_ename))
                (LM:setdynpropvalue newblk_obj (LM:getvisibilityparametername new_hanging_obj_) "2_top_view" )
              ;
            )
          ;
          (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
        )
      ;
    ;
  )

  ;
  ;000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW
  ; (defun c:PPLINE_300HK_inscc01_ ()
  ;   ;selection_set_for_fillter_blk_name
  ;     (if  ;pre_select_ssget_or_post_select_ssget
  ;       (=
  ;         (setq ss_pre_filter_set_xx_ (ssget "i"
  ;                                           (list
  ;                                             (cons 0 "LINE") ;type of object
  ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
  ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
  ;                                           )
  ;                                     )
  ;         )
  ;         nil
  ;       )
  ;       (progn
  ;         (setq ss_pre_filter_set_xx_ (ssget
  ;                                           (list
  ;                                             (cons 0 "LINE") ;type of object
  ;                                             ; (cons 8 "000 - GRID")   ;kind of layer
  ;                                             ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
  ;                                             ; (cons 62 1)           ;kind of color call sign with color code index
  ;                                           )
  ;                                     )
  ;         )
  ;       )
  ;       (sslength ss_pre_filter_set_xx_)
  ;     )
  ;   ;
  ;   ;
  ;   ;preloop_and_while
  ;     (setq ss_pre_filter_set_xx_i 0)
  ;     (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
  ;       (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
  ;       (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
  ;       (setq obj_ ss_pre_filter_set_xx_obj_)
  ;       ;get-propertie_data
  ;         (setq ss_pre_filter_set_xx_obj_angle_ (vla-get-angle ss_pre_filter_set_xx_obj_))  
  ;         (setq ss_pre_filter_set_xx_obj_length_ (vla-get-length ss_pre_filter_set_xx_obj_))
  ;         (setq ss_pre_filter_set_xx_obj_start_end_pt_ (list
  ;                                         (setq ss_pre_filter_set_xx_obj_startpt_ (vlax-safearray->list
  ;                                                                    (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_obj_ ))
  ;                                                                  )
  ;                                         )
  ;                                         (setq ss_pre_filter_set_xx_obj_endpt_ (vlax-safearray->list
  ;                                                                   (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_obj_ ))
  ;                                                                )
  ;                                         )
  ;                                       )
  ;         )
        
  ;       ;
  ;       (TA:vla-insertblock
  ;         "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW" 
  ;         (TA:midpoint ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_obj_endpt_ )
  ;         1
  ;         ss_pre_filter_set_xx_obj_angle_
  ;       )
  ;       ;new_blk_entlast object
  ;         (setq newinsert_ename (entlast))
  ;         (setq newinsert_obj_ (vlax-ename->vla-object newinsert_ename))
  ;         (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "2_top_view" )
  ;         (LM:setdynpropvalue newinsert_obj_ "array_length" (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) )
  ;         (LM:setdynpropvalue newinsert_obj_ "fragment_left" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
  ;         (LM:setdynpropvalue newinsert_obj_ "fragment_right" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
  ;       ;
  ;       (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
  ;     )
  ;   ;
    
  ;   ;
  ; )
  (defun c:PPLINE_300HK_inscc01_ ()
    ;selection_set_for_fillter_blk_name
      (if  ;pre_select_ssget_or_post_select_ssget
        (=
          (setq ss_pre_filter_set_xx_ (ssget "i"
                                            (list
                                              (cons 0 "LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn
          (setq ss_pre_filter_set_xx_ (ssget
                                            (list
                                              (cons 0 "LINE") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx_)
      )
    ;
    ;while_select_block_on_condition_
        (setq direction_ename_ nil)
        (while (= direction_ename_ nil)
          (setq direction_ename_ (car (entsel "specify direction_ename Object")))
          (if
            (and ;conditional_rule_for_select_object
              (/= direction_ename_ nil)
              (setq direction_ename_obj_ (vlax-ename->vla-object direction_ename_))
              (= (vla-get-objectname direction_ename_obj_ ) "AcDbLine")
            )
            (progn
              (setq direction_ename_obj_angle_ (vla-get-angle direction_ename_obj_) )
            )
            (alert "Object invalid Please try again")
          )
        )
    ;
    ;preloop_and_while
      (setq ss_pre_filter_set_xx_i 0)
      (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
        (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i))
        (setq ss_pre_filter_set_xx_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_))
        (setq obj_ ss_pre_filter_set_xx_obj_)
        ;get-propertie_data
          (setq ss_pre_filter_set_xx_obj_angle_ (vla-get-angle ss_pre_filter_set_xx_obj_))  
          (setq ss_pre_filter_set_xx_obj_length_ (vla-get-length ss_pre_filter_set_xx_obj_))
          (setq ss_pre_filter_set_xx_obj_start_end_pt_ (list
                                          (setq ss_pre_filter_set_xx_obj_startpt_ (vlax-safearray->list
                                                                     (vlax-variant-value ( vla-get-startpoint ss_pre_filter_set_xx_obj_ ))
                                                                   )
                                          )
                                          (setq ss_pre_filter_set_xx_obj_endpt_ (vlax-safearray->list
                                                                    (vlax-variant-value ( vla-get-endpoint ss_pre_filter_set_xx_obj_ ))
                                                                 )
                                          )
                                        )
          )
        
        ;
        (TA:vla-insertblock
          "000 - DYNAMIC_COMBINE+VIEW_ASSCESSORIES_FAMELINE_GALV.BRACKET_FITTING_U-PROFILE_CC-01_COMBINE_VIEW" 
          (TA:midpoint ss_pre_filter_set_xx_obj_startpt_ ss_pre_filter_set_xx_obj_endpt_ )
          1
          ss_pre_filter_set_xx_obj_angle_
        )
        ;new_blk_entlast object
          (setq newinsert_ename (entlast))
          (setq newinsert_obj_ (vlax-ename->vla-object newinsert_ename))
          (cond ;visibility&length condition
            ( (> ss_pre_filter_set_xx_obj_length_ 3000 ) ;visibility&length condition
              (progn
                (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "2_top_view" )
                (LM:setdynpropvalue newinsert_obj_ "array_length" (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) )
                (LM:setdynpropvalue newinsert_obj_ "fragment_left" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
                (LM:setdynpropvalue newinsert_obj_ "fragment_right" (/ (- ss_pre_filter_set_xx_obj_length_ (* 3000 (fix (/ ss_pre_filter_set_xx_obj_length_ 3000))) ) 2 ) )
              )
            )
            ( (<= ss_pre_filter_set_xx_obj_length_ 3000 ) ;visibility&length condition
              (progn
                (LM:setdynpropvalue newinsert_obj_ (LM:getvisibilityparametername newinsert_obj_) "7_dynamic_length_view" )
                (LM:setdynpropvalue newinsert_obj_ "fragment_middle" ss_pre_filter_set_xx_obj_length_ )
              )
            )
          )
          
        ;
        (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
      )
    ;
    
    ;
  )
  ;
;
