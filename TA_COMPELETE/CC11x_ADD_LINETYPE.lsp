;TEMP_SUB_FUNC
  (defun LM:str->lst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
  )
  (defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
  )
  (defun TA:FIND_DUPLICATE_LIST (target_val_ target_list_)
    ;example_user
      ; (setq target_list_ (list "TA_HL_000.00010" "TA_HL_002.00" ) )
      ; (setq target_val_ "TA_HL_000.00010" )
      ; (TA:FIND_DUPLICATE_LIST target_val_ target_list_)
    ;
    ;preloop_and_while
      (setq target_list_i 0)
      (setq check_list_val "N")
      (while (and (< target_list_i (length target_list_)) (= check_list_val "N"))
        (setq target_list_ename_ (nth  target_list_i target_list_))
          (if (= target_val_ target_list_ename_ )
            (progn
              (setq check_list_val "Y")
            )
            (setq check_list_val "N")
          )
        (setq target_list_i (+ target_list_i 1))
      )
    ;
    ;summary_checking_list_
    (if (= check_list_val "Y")
      (progn
        (princ (strcat "target_val_ has been duplicate in " (rtos target_list_i 2 0) "\n" ))
        (setq check_list_val check_list_val)
      )
      (princ (strcat "target_val_ has not duplicate in list\n" ))
    )

    ;
  )
;

(defun c:writetextfile_v1 ( / des txt ) ;; Define function, declare local variables
   (if (setq txt (getfiled "Create Text File" "" "lin" 1 )) ;; Prompt user for filename/filepath
       (if (setq des (open txt "w")) ;; Attempt to create text file with given filename/filepath
           (progn ;; Evaluate the enclose expressions as the 'then' expression for the IF statement
               (write-line "This is a test." des) ;; Write line of text to text file
               (close des) ;; Close file descriptor
           ) ;; end PROGN
           (princ "\nUnable to create text file.") ;; Else the text file could not be created
       ) ;; end IF
       (princ "\n*Cancel*") ;; Else the user pressed Cancel
   ) ;; end IF
   (princ) ;; Suppress the return of the last evaluated expression
)
(defun c:writetextfile_v2 (/ file lines line)
  (setq file (strcat "C:\\vba-files\\" (getstring "\nEnter file name (without extension): ") ".lin"))
  (setq lines '("Line 1" "Line 2" "Line 3")) ; เปลี่ยนเป็นข้อมูลที่คุณต้องการจะบันทึก
  (if file
    (progn
      (setq line 1)
      (setq outfile (open file "w"))
      (while lines
        (write-line (car lines) outfile)
        (setq lines (cdr lines))
        (setq line (1+ line))
      )
      (close outfile)
      (princ (strcat "Successfully exported " (itoa line) " lines to " file))
    )
  )
  (princ)
)
(defun c:writetextfile_v3 ( / des txt ) ;; Define function, declare local variables
   (if 
     (setq txt (getfiled "Create Text File eeee" "texttesttest" "lin" 1)) ;; Prompt user for filename/filepath
       (if (setq des (open txt "w")) ;; Attempt to create text file with given filename/filepath
           (progn  ;; Evaluate the enclose expressions as the 'then' expression for the IF statement

                  (write-line "*TA_G_001.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,1.1,-.25,.25,-.25,1.1" des) ;; Write line of text to text file

                  (write-line "*TA_G_002.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,2,-.5,.5,-.5,2" des) ;; Write line of text to text file

                  (write-line "*TA_G_005.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,5,-1.25,1.25,-1.25,5" des) ;; Write line of text to text file

                  (write-line "*TA_G_010.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,15,-2.75,2.75,-2.75,15" des) ;; Write line of text to text file

                  (write-line "*TA_G_015.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,15,-3.75,3.75,-3.75,15" des) ;; Write line of text to text file

                  (write-line "*TA_G_020.00_GG_TA_L_INE,++" des) ;; Write line of text to text file
                  (write-line "A,20,-5,5,-5,20" des) ;; Write line of text to text file

                  (write-line "*TA_G_025.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,25,-6.25,6.25,-6.25,25" des) ;; Write line of text to text file

                  (write-line "*TA_G_030.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,30,-7.5,7.5,-7.5,30" des) ;; Write line of text to text file

                  (write-line "*TA_G_035.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,35,-8.75,8.75,-8.75,35" des) ;; Write line of text to text file

                  (write-line "*TA_G_050.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,50,-12.5,12.5,-12.5,50" des) ;; Write line of text to text file

                  (write-line "*TA_G_075.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,75,-18.75,18.75,-18.75,75" des) ;; Write line of text to text file

                  (write-line "*TA_G_100.00_GG_TA_L_INE.LIN,++" des) ;; Write line of text to text file
                  (write-line "A,100,-25,25,-25,100" des) ;; Write line of text to text file

                  (write-line "*TA_L_000.10.LIN," des) ;; Write line of text to text file
                  (write-line "A,.355,-.295" des) ;; Write line of text to text file

                  (write-line "*TA_L_000.25.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,.875,-.75" des) ;; Write line of text to text file

                  (write-line "*TA_L_000.50.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,1.775,-1.475" des) ;; Write line of text to text file

                  (write-line "*TA_L_001.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,3.55,-2.95" des) ;; Write line of text to text file

                  (write-line "*TA_L_002.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,7.1,-5.9" des) ;; Write line of text to text file

                  (write-line "*TA_L_003.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,10.65,-8.85" des) ;; Write line of text to text file

                  (write-line "*TA_L_005.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,17.75,-14.75" des) ;; Write line of text to text file

                  (write-line "*TA_L_025.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,88.75,-73.75" des) ;; Write line of text to text file

                  (write-line "*TA_L_050.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,177.5,-147.5" des) ;; Write line of text to text file

                  (write-line "*TA_L_100.00.LIN,+" des) ;; Write line of text to text file
                  (write-line "A,355,-295" des) ;; Write line of text to text file


                  (close des) ;; Close file descriptor
           ) ;; end PROGN
           (princ "\nUnable to create text file.") ;; Else the text file could not be created
       ) ;; end IF
       (princ "\n*Cancel*") ;; Else the user pressed Cancel
   ) ;; end IF
   (princ) ;; Suppress the return of the last evaluated expression
)

(defun c:CC111_add_gridline() ;; Define function, declare local variables
  ;set_file_name_and_location_path
    (setq location_file_ (getvar "dwgprefix"))
    (setq filename_ (substr  (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))
    (setq extension_file_ ".lin")
    (setq full_name (strcat location_file_ filename_ extension_file_))
    
  ;
  ;set_val
    (setq gridline_prefix_name "TA_GRID_LINE_") 
    (setq description_ "GRID LINE")
    ; (setq gridline_val_scale_ (list 0.10 0.25 0.50 0.75 1.00 2.00 5.00 10.00 15.00 20.00 25.00 30.00 40.00 50.00 60.00 70.00 80.00 90.00 100.00))
    (setq gridline_val_scale_ (list 0.00010 0.00025 0.00030 0.00040 0.00050 0.00060 0.00075 0.00080 
                                      0.00100 0.00200 0.00500 
                                      0.01000 0.01500 0.02000 0.02500 0.03000 0.04000 0.05000 0.06000 0.07000 0.08000 0.09000 
                                      0.10000 0.15000 0.20000 0.25000 0.30000 0.40000 0.50000 0.60000 0.70000 0.80000 0.90000 
                                      1.00000 3.00000 5.00000 7.00000 9.00000 11.00000 13.00000 15.00000 17.00000 19.00000 21.00000 23.00000 25.00000 27.00000 29.00000 31.00000 33.00000 35.00000 37.00000 39.00000 41.00000 43.00000 45.00000 47.00000 49.00000 51.00000 53.00000 55.00000 57.00000 59.00000 61.00000 63.00000 65.00000 67.00000 69.00000 71.00000 73.00000 75.00000 77.00000 79.00000 81.00000 83.00000 85.00000 87.00000 89.00000 91.00000 93.00000 95.00000 97.00000 99.00000 100.00000
                                )
    )
    (setq gridline_val_scale_i 0)
    (setq gridline_list_ ())
  ;
  ;preloop_and_while get_linetype_name_and_value
    (while (< gridline_val_scale_i (length gridline_val_scale_))
      (setq gridline_sc (nth gridline_val_scale_i gridline_val_scale_))
      ;get_data_
        (setq gridline_name (strcat
                              "*"                            
                              (setq gridline_prefix_name "TA_GL_")
                              (cond 
                                ((and 
                                    (<= gridline_sc 9)
                                  )
                                  (progn 
                                    (strcat "00" (rtos gridline_sc 2 2) "," description_)
                                  )
                                )
                                ((and 
                                    (>= gridline_sc 10)
                                    (<= gridline_sc 99)
                                  )
                                  (progn 
                                    (strcat "0" (rtos gridline_sc 2 2) "," description_)
                                  )
                                )
                                ((and 
                                    (>= gridline_sc 100)
                                  )
                                  (progn 
                                    (strcat "" (rtos gridline_sc 2 2) "," description_)
                                  )
                                )
                              )
                            )
        )
        (setq gridline_val (strcat 
                                (setq gridline_val_line_Code  "A")
                                ","
                                (rtos (* (setq gridline_val_line_set1   1.00) gridline_sc) 2 2)
                                ","
                                (rtos (* (setq gridline_val_line_set2  -0.25) gridline_sc) 2 2)
                                ","
                                (rtos (* (setq gridline_val_line_set3   0.25) gridline_sc) 2 2)
                                ","
                                (rtos (* (setq gridline_val_line_set4  -0.25) gridline_sc) 2 2)
                                ","
                                (rtos  (* (setq gridline_val_line_set5  1.00) gridline_sc) 2 2)
                              )
        )
      ;
      ;summary_data
        (setq sum (list 
                    gridline_name
                    gridline_val
                  )
        )
        (setq gridline_list_ (cons sum gridline_list_))
      ;
      
      (setq gridline_val_scale_i (+ gridline_val_scale_i 1))
    )
    (length gridline_list_)
  ;
  ;preloop_and_while
    (setq gridline_list_i 0)
      (setq content_ (open full_name "w"))
        (while (< gridline_list_i (length gridline_list_))
          (setq gridline_list_name (car (nth gridline_list_i (reverse gridline_list_))))
          (setq gridline_list_val (cadr (nth gridline_list_i (reverse gridline_list_))))
          
          (write-line gridline_list_name content_)
          (write-line gridline_list_val content_)
          (write-line "" content_)
          
          (setq gridline_list_i (+ gridline_list_i 1))
          
        )
      (close content_)
  ;
  ;get_total_linetype_GL_in_file
    ;sub_func
      (setq linetype_list_ (TA:get_name_linetype_in_drawing))
      (setq linetype_list_i 0)
    ;
    ;linetype_fillter 
      (setq sum_linetype_ ())
      
      (while (< linetype_list_i (length linetype_list_))
        (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_GL" )
          (progn
            (setq mk_list_HL (list linetype_list_name))
            (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
          )
        )
        (setq linetype_list_i (+ linetype_list_i 1))
      )
    ;
    ;reverse_list_
      (setq sum_linetype_ (reverse sum_linetype_))
      (setq sum_grid_linetype_target_list_ (reverse sum_linetype_))
    ;
  ;
  
  ;preloop_and_while
    (setq gridline_list_ii 0)
    (while (< gridline_list_ii (length gridline_list_))
      (setq gridline_list_name (substr (car (nth gridline_list_ii (reverse gridline_list_))) 2 (- (strlen (car (nth gridline_list_ii (reverse gridline_list_)))) 11)))
      (setq gridline_list_name_string_checker_val_1 (car (LM:str->lst gridline_list_name ",")))
      

      (if (= (setq check_list_val_ (TA:FIND_DUPLICATE_LIST gridline_list_name_string_checker_val_1 sum_grid_linetype_target_list_ )) "Y")
        (progn
          (command "_.linetype" "_load" gridline_list_name full_name "Y" "" )
        )
        (command "_.linetype" "_load" gridline_list_name full_name "" )
      )
      (setq gridline_list_ii (+ gridline_list_ii 1))
    
    )
  ;
  
)
(defun c:CC112_add_hiddenline () ;; Define function, declare local variables
  ;note 20250425
    (setvar "dimzin" 0)
  ;
  ;set_file_name_and_location_path
    (setq location_file_ (getvar "dwgprefix"))
    (setq filename_ (substr  (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))
    (setq extension_file_ ".lin")
    (setq full_name (strcat location_file_ filename_ extension_file_))
    
  ;
  ;set_val
    (setq hiddenline_prefix_name "TA_HIDDEN_LINE_") 
    (setq description_ "HIDDEN LINE")
    (setq hiddenline_val_scale_ (list 0.10 0.25 0.50 0.75 1.00 2.00 5.00 10.00 15.00 20.00 25.00 30.00 40.00 50.00 60.00 70.00 80.00 90.00 100.00))
    ; (setq hiddenline_val_scale_ (list 0.00010 0.00025 0.00030 0.00035 0.00040 0.00045 
    ;                                   0.00050 0.00055 0.00060 0.00065 0.00070 0.00075 
    ;                                   0.00080 0.00085 0.00090 0.00095 0.00100 0.00050 
    ;                                   0.00100 0.00150 0.00200 0.00250 0.00300 0.00350 
    ;                                   0.00400 0.00450 0.00500 0.00550 0.00600 0.00650 
    ;                                   0.00700 0.00750 0.00800 0.00850 0.00900 0.00950 
    ;                                   0.01000 0.00500 0.01000 0.01500 0.02000 0.02500 
    ;                                   0.03000 0.03500 0.04000 0.04500 0.05000 0.05500 
    ;                                   0.06000 0.06500 0.07000 0.07500 0.08000 0.08500 
    ;                                   0.09000 0.09500 0.10000 0.05000 0.10000 0.15000 
    ;                                   0.20000 0.25000 0.30000 0.35000 0.40000 0.45000 
    ;                                   0.50000 0.55000 0.60000 0.65000 0.70000 0.75000 
    ;                                   0.80000 0.85000 0.90000 0.95000 1.00000 2.00000 
    ;                                   3.00000 4.00000 5.00000 10.00000 15.00000 20.00000 
    ;                                   25.00000 30.00000 35.00000 40.00000 45.00000 50.00000 
    ;                                   55.00000 60.00000 65.00000 70.00000 75.00000 80.00000 
    ;                                   85.00000 90.00000 95.00000 100.00000
    ;                             )
    ; )
    (setq hiddenline_val_scale_i 0)
    (setq hiddenline_list_ ())
  ;
  ;preloop_and_while get_linetype_name_and_value
    (while (< hiddenline_val_scale_i (length hiddenline_val_scale_))
      (setq hiddenline_sc (nth hiddenline_val_scale_i hiddenline_val_scale_))
      ;get_data_
        (setq hiddenline_name (strcat
                              "*"                            
                              (setq hiddenline_prefix_name "TA_HL_")
                              (cond 
                                ((and 
                                    (<= hiddenline_sc 9)
                                  )
                                  (progn 
                                    (strcat "00" (rtos hiddenline_sc 2 2) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 10)
                                    (<= hiddenline_sc 99)
                                  )
                                  (progn 
                                    (strcat "0" (rtos hiddenline_sc 2 2) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 100)
                                  )
                                  (progn 
                                    (strcat "" (rtos hiddenline_sc 2 2) "," description_)
                                  )
                                )
                              )
                            )
        )
        (setq hiddenline_val (strcat 
                                (setq hiddenline_val_line_Code  "A")
                                ","
                                (rtos (* (setq hiddenline_val_line_set1   3.55) hiddenline_sc) 2 2)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  -2.95) hiddenline_sc) 2 2)
                              )
        )
      ;
      ;summary_data
        (setq sum (list 
                    hiddenline_name
                    hiddenline_val
                  )
        )
        (setq hiddenline_list_ (cons sum hiddenline_list_))
      ;
      
      (setq hiddenline_val_scale_i (+ hiddenline_val_scale_i 1))
    )
    (length hiddenline_list_)
  ;
  ;preloop_and_while
    (setq hiddenline_list_i 0)
      (setq content_ (open full_name "w"))
        (while (< hiddenline_list_i (length hiddenline_list_))
          (setq hiddenline_list_name (car (nth hiddenline_list_i (reverse hiddenline_list_))))
          (setq hiddenline_list_val (cadr (nth hiddenline_list_i (reverse hiddenline_list_))))
          
          (write-line hiddenline_list_name content_)
          (write-line hiddenline_list_val content_)
          (write-line "" content_)
          
          (setq hiddenline_list_i (+ hiddenline_list_i 1))
          
        )
      (close content_)
  ;
  ;get_total_linetype_HL_in_file
    ;sub_func
      (setq linetype_list_ (TA:get_name_linetype_in_drawing))
      (setq linetype_list_i 0)
    ;
    ;linetype_fillter 
      (setq sum_linetype_ ())
      (while (< linetype_list_i (length linetype_list_))
        (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_HL" )
          (progn
            (setq mk_list_HL (list linetype_list_name))
            (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
          )
        )
        (setq linetype_list_i (+ linetype_list_i 1))
      )
    ;
    ;reverse_list_
      (setq sum_linetype_ (reverse sum_linetype_))
      (setq sum_linetype_target_list_ (reverse sum_linetype_))
    ;
  ;
  ;preloop_and_while
    (setq hiddenline_list_ii 0)
    (while (< hiddenline_list_ii (length hiddenline_list_))
      (setq hiddenline_list_name (substr (car (nth hiddenline_list_ii (reverse hiddenline_list_))) 2 (- (strlen (car (nth hiddenline_list_ii (reverse hiddenline_list_)))) 11)))
      (setq hiddenline_list_name_string_checker_val_1 (car (LM:str->lst hiddenline_list_name ",")))
      

      (if (= (setq check_list_val_ (TA:FIND_DUPLICATE_LIST hiddenline_list_name_string_checker_val_1 sum_linetype_target_list_ )) "Y")
        (progn
          (command "_.linetype" "_load" hiddenline_list_name full_name "Y" "" )
        )
        (command "_.linetype" "_load" hiddenline_list_name full_name "" )
      )
      
      
      
      
    
      (setq hiddenline_list_ii (+ hiddenline_list_ii 1))
    
    )
  ;
  
)
(defun c:CM112_add_hiddenline () ;; Define function, declare local variables
  ;note 20250425
    (setvar "dimzin" 0)
  ;
  ;set_file_name_and_location_path
    (setq location_file_ (getvar "dwgprefix"))
    (setq filename_ (substr  (strcat (getvar "dwgname") "_GL_LINE") 1 (- (strlen (strcat (getvar "dwgname") "_HL_LINE")) 4)))
    (setq extension_file_ ".lin")
    (setq full_name (strcat location_file_ filename_ extension_file_))
    
  ;
  ;set_val
    (setq hiddenline_prefix_name "TA_HIDDEN_LINE_") 
    (setq description_ "HIDDEN LINE")
    ; (setq hiddenline_val_scale_ (list 0.00010 0.00025 0.00030 0.00040 0.00050 0.00060 0.00075 0.00080 
    ;                                   0.00100 0.00200 0.00500 
    ;                                   0.01000 0.01500 0.02000 0.02500 0.03000 0.04000 0.05000 0.06000 0.07000 0.08000 0.09000 
    ;                                   0.10000 0.15000 0.20000 0.25000 0.30000 0.40000 0.50000 0.60000 0.70000 0.80000 0.90000 
    ;                                   1.00000 2.00000 3.00000 
    ;                             )
    ; )
    (setq hiddenline_val_scale_ (list 0.00010 0.00020 0.00030 0.00040 0.00050 0.00060 0.00070 0.00080 0.00090
                                      0.00100 0.00200 0.00300 0.00400 0.00500 0.00600 0.00700 0.00800 0.00900
                                      0.01000 0.02000 0.03000 0.04000 0.05000 0.06000 0.07000 0.08000 0.09000
                                      0.10000 0.20000 0.30000 0.40000 0.50000 0.60000 0.70000 0.80000 0.90000 
                                      1.00000 2.00000 3.00000 4.00000 5.00000 6.00000 7.00000 8.00000 9.00000 10.00000
                                      11.00000 12.00000 13.00000 14.00000 15.00000 16.00000 17.00000 18.00000 19.00000 20.00000
                                      21.00000 22.00000 23.00000 24.00000 25.00000 26.00000 27.00000 28.00000 29.00000 30.00000
                                      31.00000 32.00000 33.00000 34.00000 35.00000 36.00000 37.00000 38.00000 39.00000 40.00000
                                      41.00000 42.00000 43.00000 44.00000 45.00000 46.00000 47.00000 48.00000 49.00000 50.00000
                                      51.00000 52.00000 53.00000 54.00000 55.00000 56.00000 57.00000 58.00000 59.00000 60.00000
                                      61.00000 62.00000 63.00000 64.00000 65.00000 66.00000 67.00000 68.00000 69.00000 70.00000
                                      71.00000 72.00000 73.00000 74.00000 75.00000 76.00000 77.00000 78.00000 79.00000 80.00000
                                      81.00000 82.00000 83.00000 84.00000 85.00000 86.00000 87.00000 88.00000 89.00000 90.00000
                                      91.00000 92.00000 93.00000 94.00000 95.00000 96.00000 97.00000 98.00000 99.00000 100.00000
                                )
    )
    ; (setq hiddenline_val_scale_ (list 0.00010 
    ;                                   0.00025 
    ;                                   0.00030 
    ;                                   0.00035 
    ;                                   0.00040 
    ;                                   0.00045 
    ;                                   ; 0.00050 
    ;                                   ; 0.00055 
    ;                                   0.00060 
    ;                                   0.00065 
    ;                                   0.00070 
    ;                                   0.00075 
    ;                                   0.00080 
    ;                                   0.00085 
    ;                                   0.00090 
    ;                                   0.00095 
    ;                                   0.00100
    ;                                   0.00050 
    ;                                   0.00100 
    ;                                   0.00150 
    ;                                   0.00200 
    ;                                   0.00250 
    ;                                   0.00300 
    ;                                   0.00350 
    ;                                   0.00400 
    ;                                   0.00450 
    ;                                   ; 0.00500 
    ;                                   ; 0.00550 
    ;                                   0.00600 
    ;                                   0.00650 
    ;                                   0.00700 
    ;                                   0.00750 
    ;                                   0.00800 
    ;                                   0.00850 
    ;                                   0.00900 
    ;                                   0.00950 
    ;                                   0.01000 
    ;                                   0.05000
    ;                                   0.01000 
    ;                                   0.01500 
    ;                                   0.02000 
    ;                                   0.02500 
    ;                                   0.03000 
    ;                                   0.03500 
    ;                                   0.04000 
    ;                                   0.04500 
    ;                                   ; 0.05000 
    ;                                   ; 0.05500 
    ;                                   0.06000 
    ;                                   0.06500 
    ;                                   0.07000 
    ;                                   0.07500 
    ;                                   0.08000 
    ;                                   0.08500 
    ;                                   0.09000 
    ;                                   0.09500 
    ;                                   0.10000                                   
    ;                                   0.15000 
    ;                                   0.20000 
    ;                                   0.25000 
    ;                                   0.30000 
    ;                                   0.35000 
    ;                                   0.40000 
    ;                                   0.45000 
    ;                                   ; 0.50000 
    ;                                   ; 0.55000 
    ;                                   0.60000 
    ;                                   0.65000 
    ;                                   0.70000 
    ;                                   0.75000 
    ;                                   0.80000 
    ;                                   0.85000 
    ;                                   0.90000 
    ;                                   0.95000 
    ;                                   1.00000 
    ;                                   2.00000 
    ;                                   3.00000 
    ;                                   4.00000 
    ;                                   5.00000
    ;                                   10.00000 
    ;                                   15.00000 
    ;                                   20.00000 
    ;                                   25.00000 
    ;                                   30.00000 
    ;                                   35.00000 
    ;                                   40.00000 
    ;                                   45.00000
    ;                                   ; 50.00000 
    ;                                   ; 55.00000
    ;                                   60.00000 
    ;                                   65.00000 
    ;                                   70.00000 
    ;                                   75.00000 
    ;                                   80.00000 
    ;                                   85.00000 
    ;                                   90.00000 
    ;                                   95.00000 
    ;                                   100.00000
    ;                             )
    ; )
    (setq hiddenline_val_scale_i 0)
    (setq hiddenline_list_ ())
  ;
  ;preloop_and_while get_linetype_name_and_value
    (while (< hiddenline_val_scale_i (length hiddenline_val_scale_))
      (setq hiddenline_sc (nth hiddenline_val_scale_i hiddenline_val_scale_))
      ;get_data_
        (setq hiddenline_name (strcat
                              "*"                            
                              (setq hiddenline_prefix_name "TA_HL_")
                              (cond 
                                ((and 
                                    (<= hiddenline_sc 9)
                                  )
                                  (progn 
                                    (strcat "00" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 10)
                                    (<= hiddenline_sc 99)
                                  )
                                  (progn 
                                    (strcat "0" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 100)
                                  )
                                  (progn 
                                    (strcat "" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                              )
                            )
        )
        (setq hiddenline_val (strcat 
                                (setq hiddenline_val_line_Code  "A")
                                ","
                                (rtos (* (setq hiddenline_val_line_set1   3.55) hiddenline_sc) 2 5)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  -2.95) hiddenline_sc) 2 5)
                              )
        )
      ;
      ;summary_data
        (setq sum (list 
                    hiddenline_name
                    hiddenline_val
                  )
        )
        (setq hiddenline_list_ (cons sum hiddenline_list_))
      ;
      
      (setq hiddenline_val_scale_i (+ hiddenline_val_scale_i 1))
    )
    (length hiddenline_list_)
  ;
  ;preloop_and_while
    (setq hiddenline_list_i 0)
      (setq content_ (open full_name "w"))
        (while (< hiddenline_list_i (length hiddenline_list_))
          (setq hiddenline_list_name (car (nth hiddenline_list_i (reverse hiddenline_list_))))
          (setq hiddenline_list_val (cadr (nth hiddenline_list_i (reverse hiddenline_list_))))
          
          (write-line hiddenline_list_name content_)
          (write-line hiddenline_list_val content_)
          (write-line "" content_)
          
          (setq hiddenline_list_i (+ hiddenline_list_i 1))
          
        )
      (close content_)
  ;
  ;get_total_linetype_HL_in_file
    ;sub_func
      (setq linetype_list_ (TA:get_name_linetype_in_drawing))
      (setq linetype_list_i 0)
    ;
    ;linetype_fillter 
      (setq sum_linetype_ ())
      (while (< linetype_list_i (length linetype_list_))
        (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_HL" )
          (progn
            (setq mk_list_HL (list linetype_list_name))
            (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
          )
        )
        (setq linetype_list_i (+ linetype_list_i 1))
      )
    ;
    ;reverse_list_
      (setq sum_linetype_ (reverse sum_linetype_))
      (setq sum_linetype_target_list_ (reverse sum_linetype_))
    ;
  ;
  ;preloop_and_while
    (setq hiddenline_list_ii 0)
    (while (< hiddenline_list_ii (length hiddenline_list_))
      (setq hiddenline_list_name (substr (car (nth hiddenline_list_ii (reverse hiddenline_list_))) 2 (- (strlen (car (nth hiddenline_list_ii (reverse hiddenline_list_)))) 11)))
      (setq hiddenline_list_name_string_checker_val_1 (car (LM:str->lst hiddenline_list_name ",")))
      

      (if (= (setq check_list_val_ (TA:FIND_DUPLICATE_LIST hiddenline_list_name_string_checker_val_1 sum_linetype_target_list_ )) "Y")
        (progn
          (command "_.linetype" "_load" hiddenline_list_name full_name "Y" "")
        )
        (command "_.linetype" "_load" hiddenline_list_name full_name "" )
        
      )
      (princ "\n")
      (princ "aaaa\n")
      (PRINC hiddenline_list_ii)
      (princ "\n")
      (princ hiddenline_list_name)
      
      
    
      (setq hiddenline_list_ii (+ hiddenline_list_ii 1))
      ; (setq hiddenline_list_ii (- hiddenline_list_ii 1))
    
    )
  ;
  
)
(defun c:CM113_add_gridline () ;; Define function, declare local variables
  ;note 20250425
    (setvar "dimzin" 0)
  ;
  ;set_file_name_and_location_path
    (setq location_file_ (getvar "dwgprefix"))
    (setq filename_ (substr  (strcat (getvar "dwgname") "_HL_LINE") 1 (- (strlen (strcat (getvar "dwgname") "_HL_LINE")) 4)))
    (setq extension_file_ ".lin")
    (setq full_name (strcat location_file_ filename_ extension_file_))
    
  ;
  ;set_val
    (setq hiddenline_prefix_name "TA_GRID_LINE_") 
    (setq description_ "GRID LINE")
    ; (setq hiddenline_val_scale_ (list 0.00010 0.00025 0.00030 0.00040 0.00050 0.00060 0.00075 0.00080 
    ;                                   0.00100 0.00200 0.00500 
    ;                                   0.01000 0.01500 0.02000 0.02500 0.03000 0.04000 0.05000 0.06000 0.07000 0.08000 0.09000 
    ;                                   0.10000 0.15000 0.20000 0.25000 0.30000 0.40000 0.50000 0.60000 0.70000 0.80000 0.90000 
    ;                                   1.00000 2.00000 3.00000 
    ;                             )
    ; )
    (setq hiddenline_val_scale_ (list 0.00010 0.00020 0.00030 0.00040 0.00050 0.00060 0.00070 0.00080 0.00090
                                      0.00100 0.00200 0.00300 0.00400 0.00500 0.00600 0.00700 0.00800 0.00900
                                      0.01000 0.02000 0.03000 0.04000 0.05000 0.06000 0.07000 0.08000 0.09000
                                      0.10000 0.20000 0.30000 0.40000 0.50000 0.60000 0.70000 0.80000 0.90000 
                                      1.00000 2.00000 3.00000 4.00000 5.00000 6.00000 7.00000 8.00000 9.00000 10.00000
                                      11.00000 12.00000 13.00000 14.00000 15.00000 16.00000 17.00000 18.00000 19.00000 20.00000
                                      21.00000 22.00000 23.00000 24.00000 25.00000 26.00000 27.00000 28.00000 29.00000 30.00000
                                      31.00000 32.00000 33.00000 34.00000 35.00000 36.00000 37.00000 38.00000 39.00000 40.00000
                                      41.00000 42.00000 43.00000 44.00000 45.00000 46.00000 47.00000 48.00000 49.00000 50.00000
                                      51.00000 52.00000 53.00000 54.00000 55.00000 56.00000 57.00000 58.00000 59.00000 60.00000
                                      61.00000 62.00000 63.00000 64.00000 65.00000 66.00000 67.00000 68.00000 69.00000 70.00000
                                      71.00000 72.00000 73.00000 74.00000 75.00000 76.00000 77.00000 78.00000 79.00000 80.00000
                                      81.00000 82.00000 83.00000 84.00000 85.00000 86.00000 87.00000 88.00000 89.00000 90.00000
                                      91.00000 92.00000 93.00000 94.00000 95.00000 96.00000 97.00000 98.00000 99.00000 100.00000
                                )
    )
    ; (setq hiddenline_val_scale_ (list 0.00010 
    ;                                   0.00025 
    ;                                   0.00030 
    ;                                   0.00035 
    ;                                   0.00040 
    ;                                   0.00045 
    ;                                   ; 0.00050 
    ;                                   ; 0.00055 
    ;                                   0.00060 
    ;                                   0.00065 
    ;                                   0.00070 
    ;                                   0.00075 
    ;                                   0.00080 
    ;                                   0.00085 
    ;                                   0.00090 
    ;                                   0.00095 
    ;                                   0.00100
    ;                                   0.00050 
    ;                                   0.00100 
    ;                                   0.00150 
    ;                                   0.00200 
    ;                                   0.00250 
    ;                                   0.00300 
    ;                                   0.00350 
    ;                                   0.00400 
    ;                                   0.00450 
    ;                                   ; 0.00500 
    ;                                   ; 0.00550 
    ;                                   0.00600 
    ;                                   0.00650 
    ;                                   0.00700 
    ;                                   0.00750 
    ;                                   0.00800 
    ;                                   0.00850 
    ;                                   0.00900 
    ;                                   0.00950 
    ;                                   0.01000 
    ;                                   0.05000
    ;                                   0.01000 
    ;                                   0.01500 
    ;                                   0.02000 
    ;                                   0.02500 
    ;                                   0.03000 
    ;                                   0.03500 
    ;                                   0.04000 
    ;                                   0.04500 
    ;                                   ; 0.05000 
    ;                                   ; 0.05500 
    ;                                   0.06000 
    ;                                   0.06500 
    ;                                   0.07000 
    ;                                   0.07500 
    ;                                   0.08000 
    ;                                   0.08500 
    ;                                   0.09000 
    ;                                   0.09500 
    ;                                   0.10000                                   
    ;                                   0.15000 
    ;                                   0.20000 
    ;                                   0.25000 
    ;                                   0.30000 
    ;                                   0.35000 
    ;                                   0.40000 
    ;                                   0.45000 
    ;                                   ; 0.50000 
    ;                                   ; 0.55000 
    ;                                   0.60000 
    ;                                   0.65000 
    ;                                   0.70000 
    ;                                   0.75000 
    ;                                   0.80000 
    ;                                   0.85000 
    ;                                   0.90000 
    ;                                   0.95000 
    ;                                   1.00000 
    ;                                   2.00000 
    ;                                   3.00000 
    ;                                   4.00000 
    ;                                   5.00000
    ;                                   10.00000 
    ;                                   15.00000 
    ;                                   20.00000 
    ;                                   25.00000 
    ;                                   30.00000 
    ;                                   35.00000 
    ;                                   40.00000 
    ;                                   45.00000
    ;                                   ; 50.00000 
    ;                                   ; 55.00000
    ;                                   60.00000 
    ;                                   65.00000 
    ;                                   70.00000 
    ;                                   75.00000 
    ;                                   80.00000 
    ;                                   85.00000 
    ;                                   90.00000 
    ;                                   95.00000 
    ;                                   100.00000
    ;                             )
    ; )
    (setq hiddenline_val_scale_i 0)
    (setq hiddenline_list_ ())
  ;
  ;preloop_and_while get_linetype_name_and_value
    (while (< hiddenline_val_scale_i (length hiddenline_val_scale_))
      (setq hiddenline_sc (nth hiddenline_val_scale_i hiddenline_val_scale_))
      ;get_data_
        (setq hiddenline_name (strcat
                              "*"                            
                              (setq hiddenline_prefix_name "TA_GL_")
                              (cond 
                                ((and 
                                    (<= hiddenline_sc 9)
                                  )
                                  (progn 
                                    (strcat "00" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 10)
                                    (<= hiddenline_sc 99)
                                  )
                                  (progn 
                                    (strcat "0" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                                ((and 
                                    (>= hiddenline_sc 100)
                                  )
                                  (progn 
                                    (strcat "" (rtos hiddenline_sc 2 5) "," description_)
                                  )
                                )
                              )
                            )
        )
        (setq hiddenline_val (strcat 
                                (setq hiddenline_val_line_Code  "A")
                                ","
                                (rtos (* (setq hiddenline_val_line_set1   1.00) hiddenline_sc) 2 8)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  -0.25) hiddenline_sc) 2 8)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  0.25) hiddenline_sc) 2 8)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  -0.25) hiddenline_sc) 2 8)
                                ","
                                (rtos (* (setq hiddenline_val_line_set2  1.00) hiddenline_sc) 2 8)
                              )
        )
      ;
      ;summary_data
        (setq sum (list 
                    hiddenline_name
                    hiddenline_val
                  )
        )
        (setq hiddenline_list_ (cons sum hiddenline_list_))
      ;
      
      (setq hiddenline_val_scale_i (+ hiddenline_val_scale_i 1))
    )
    (length hiddenline_list_)
  ;
  ;preloop_and_while
    (setq hiddenline_list_i 0)
      (setq content_ (open full_name "w"))
        (while (< hiddenline_list_i (length hiddenline_list_))
          (setq hiddenline_list_name (car (nth hiddenline_list_i (reverse hiddenline_list_))))
          (setq hiddenline_list_val (cadr (nth hiddenline_list_i (reverse hiddenline_list_))))
          
          (write-line hiddenline_list_name content_)
          (write-line hiddenline_list_val content_)
          (write-line "" content_)
          
          (setq hiddenline_list_i (+ hiddenline_list_i 1))
          
        )
      (close content_)
  ;
  ;get_total_linetype_HL_in_file
    ;sub_func
      (setq linetype_list_ (TA:get_name_linetype_in_drawing))
      (setq linetype_list_i 0)
    ;
    ;linetype_fillter 
      (setq sum_linetype_ ())
      (while (< linetype_list_i (length linetype_list_))
        (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_GL" )
          (progn
            (setq mk_list_HL (list linetype_list_name))
            (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
          )
        )
        (setq linetype_list_i (+ linetype_list_i 1))
      )
    ;
    ;reverse_list_
      (setq sum_linetype_ (reverse sum_linetype_))
      (setq sum_linetype_target_list_ (reverse sum_linetype_))
    ;
  ;
  ;preloop_and_while
    (setq hiddenline_list_ii 0)
    (while (< hiddenline_list_ii (length hiddenline_list_))
      (setq hiddenline_list_name (substr (car (nth hiddenline_list_ii (reverse hiddenline_list_))) 2 (- (strlen (car (nth hiddenline_list_ii (reverse hiddenline_list_)))) 11)))
      (setq hiddenline_list_name_string_checker_val_1 (car (LM:str->lst hiddenline_list_name ",")))
      

      (if (= (setq check_list_val_ (TA:FIND_DUPLICATE_LIST hiddenline_list_name_string_checker_val_1 sum_linetype_target_list_ )) "Y")
        (progn
          (command "_.linetype" "_load" hiddenline_list_name full_name "Y" "")
        )
        (command "_.linetype" "_load" hiddenline_list_name full_name "" )
        
      )
      (princ "\n")
      (princ "aaaa\n")
      (PRINC hiddenline_list_ii)
      (princ "\n")
      (princ hiddenline_list_name)
      
      
    
      (setq hiddenline_list_ii (+ hiddenline_list_ii 1))
      ; (setq hiddenline_list_ii (- hiddenline_list_ii 1))
    
    )
  ;
  
)




(defun TA:sequence_list (val_ member_list_)
  ; (setq val_ 5)
  ; (setq member_list_ (list 1 5 8 6 2 8 2 96 8 1 2 6 4521 5 ) )
  
  (setq member_list_i 0 )
  (setq sequence_val 0 )
  (setq sequence_list_val 0 )
  (setq temp_ ())
  (setq result nil)
  
;preloop_and_while
  (while (and (< member_list_i (length member_list_)) (/= result T))
    (if (= val_ (setq member_list_name (nth member_list_i member_list_)))
      (progn
        ; (setq result T)
        (setq sequence_val (list member_list_i))
        (setq temp_(vl-sort (append sequence_val temp_) '<))
      )
      (setq result nil)    
    )
    (setq member_list_i (+ member_list_i 1 ))
  )
;
;summary_result
  (setq sequence_list_val (vl-sort temp_ '<) )
  
;
)
(setq member_list_1 (list 1 5 8 6 2 8 2 96 8 1 2 6 4521 5 ))
(setq member_list_2 "55")
(TA:sequence_list member_list_2 member_list_1)


  (defun TA:get_name_linetype_in_drawing ()
    (setq linetypes_list_ ())
    (setq linetypes_obj (vla-get-linetypes (vla-get-ActiveDocument (vlax-get-acad-object))))
    ;method_1_get_linetypes_name
      (vlax-for lay linetypes_obj
        (setq linetypes_list (append linetypes_list (list (vla-get-Name lay))))
      )
    ;
    ;method_2_get_linetypes_name
      (setq linetypes_i 0)
      (while (< linetypes_i (vla-get-count linetypes_obj))
        (setq linetypes_ (vla-item linetypes_obj linetypes_i))
        (setq linetypes_list_ (append linetypes_list_ (list (vla-get-name linetypes_))))
        (setq linetypes_i (1+ linetypes_i))
      )
    ;
    ;summary_layer_data
      (setq linetypes_list_total (length linetypes_list_))
      (princ "\n                  |=====================|")
      (princ "\n                  | TOTAL linetypes IN FILE |")
      (princ (strcat "\n                  |     = " (itoa linetypes_list_total) " llinetype     |"))
      (princ "\n                  |          set        |")
      (princ "\n                  |---------------------|\n")
      (setq linetypes_list_ linetypes_list_)
    ;
  )
(defun c:HJ ()
  ;sub_func
    (setq linetype_list_ (TA:get_name_linetype_in_drawing))
    (setq linetype_list_i 0)
  ;
  ;linetype_fillter 
    (setq sum_linetype_ ())
    (while (< linetype_list_i (length linetype_list_))
      (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_HL" )
        (progn
          (setq mk_list_HL (list linetype_list_name))
          (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
        )
      )
      (setq linetype_list_i (+ linetype_list_i 1))
    )
  ;
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx (ssget "I" 
                                          (list 
                                            ; (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
        )
        nil
      )
      (progn 
        (setq ss_pre_filter_set_xx (ssget 
                                    (list 
                                      ; (cons 0 "INSERT") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                      ; (cons 62 1)           ;kind of color call sign with color code index
                                    )
                                  )
        )
      )
      (sslength ss_pre_filter_set_xx)
    )
  ;
  ;preloop_and_while_main_loop
    (setq x nil)
    (setq symbol_ nil)
    (while (and (< 0 (+ (length sum_linetype_) 100)) (/= x "CANCEL"))
      ;user-input for specify value and *CANCEL* method 
        (setq result (vl-catch-all-apply 
                       (function 
                         (lambda () 
                           (setq symbol_ (cond 
                                           ((getint 
                                              (strcat "\n next = 1 \nno previous = 0  \n<" 
                                                      (rtos 
                                                        (setq symbol_ (cond 
                                                                        (symbol_)
                                                                        (0.0)
                                                                      )
                                                        )
                                                      )
                                                      "> : "
                                              )
                                            )
                                           )
                                           (symbol_)
                                         )
                           )
                         )
                       ) ; ใช้ lambda เพื่อจับคำสั่ง getint
                     )
        )
        (if (vl-catch-all-error-p result)
          (if (wcmatch (strcase (vl-catch-all-error-message result)) "*CANCEL*")
            (setq x "CANCEL")  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
          )
            
        )
      ; 
      ;preloop_and_while_for_input_data
        (if
          (and 
            (/= x "CANCEL")
          )
          (progn
            (if 
              (or 
                (= symbol_ 0)
                (= symbol_ 1)      
              )
              (progn
                
                (if (/= ss_pre_filter_set_xx nil)
                  
                  (progn
                    (setq ss_pre_filter_set_xx_i 0)
                    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx))
                      (setq target_obj_ (vlax-ename->vla-object (ssname ss_pre_filter_set_xx ss_pre_filter_set_xx_i)))
                      (setq target_linetype_ (vla-get-linetype target_obj_))
                      (if (= (TA:sequence_list target_linetype_ sum_linetype_) nil)
                        (progn
                          (vla-put-linetype target_obj_ (nth 0 sum_linetype_))
                        )
                        (princ "nn")
                      )
                      (cond
                        ((and
                            (/= target_linetype_ (TA:sequence_list target_linetype_ sum_linetype_) nil)
                            (= symbol_ 0)
                          )
                          (progn
                            (setq target_linetype_ (vla-get-linetype target_obj_))
                            (vla-put-linetype target_obj_ (nth 
                                                            (if (< (- (car (TA:sequence_list target_linetype_ sum_linetype_)) 1) 0)
                                                                (progn
                                                                  (-(length sum_linetype_) 1)
                                                                )
                                                                (- (car (TA:sequence_list target_linetype_ sum_linetype_)) 1)
                                                            ) 
                                                            sum_linetype_ 
                                                          ) 
                            )
                            (princ (vla-get-linetype target_obj_))
                          )
                        )
                        ((and
                            (/= target_linetype_ (TA:sequence_list target_linetype_ sum_linetype_) nil)
                            (= symbol_ 1)
                          )
                          (progn
                            (setq target_linetype_ (vla-get-linetype target_obj_))
                            (vla-put-linetype target_obj_ (nth 
                                                            (if (> (+ (car (TA:sequence_list target_linetype_ sum_linetype_)) 1) (- (length sum_linetype_) 1))
                                                                (progn
                                                                  0
                                                                )
                                                                (+ (car (TA:sequence_list target_linetype_ sum_linetype_)) 1)
                                                            ) 
                                                            sum_linetype_ 
                                                          ) 
                            )
                            (princ (vla-get-linetype target_obj_))
                          )
                        )
                      )
                      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
                    )
                  )
                )
              )
              (princ "ERRORRRRRRRRR")
            )
          )
          (princ "COMMAND CANCEL")
        )
      ;
      ; (setq sum_linetype_i (+ sum_linetype_i 1))
    )
  ;
)
(defun c:GJ ()
  ;sub_func
    (setq linetype_list_ (TA:get_name_linetype_in_drawing))
    (setq linetype_list_i 0)
  ;
  ;linetype_fillter 
    (setq sum_linetype_ ())
    
    (while (< linetype_list_i (length linetype_list_))
      (if (= (substr (setq linetype_list_name (nth linetype_list_i linetype_list_)) 1 5) "TA_GL" )
        (progn
          (setq mk_list_HL (list linetype_list_name))
          (setq sum_linetype_ (append sum_linetype_ mk_list_HL ))
        )
      )
      (setq linetype_list_i (+ linetype_list_i 1))
    )
  ;
  ;selection_set
    (if  ;pre_select_ssget_or_post_select_ssget
      (= 
        (setq ss_pre_filter_set_xx (ssget "I" 
                                          (list 
                                            ; (cons 0 "INSERT") ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
        )
        nil
      )
      (progn 
        (setq ss_pre_filter_set_xx (ssget 
                                    (list 
                                      ; (cons 0 "INSERT") ;type of object
                                      ; (cons 8 "000 - GRID")   ;kind of layer
                                      ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                      ; (cons 62 1)           ;kind of color call sign with color code index
                                    )
                                  )
        )
      )
      (sslength ss_pre_filter_set_xx)
    )
  ;
  ;preloop_and_while_main_loop
    (setq x nil)
    (setq symbol_ nil)
    (while (and (< 0 (+ (length sum_linetype_) 100)) (/= x "CANCEL"))
      ;user-input for specify value and *CANCEL* method 
        (setq result (vl-catch-all-apply 
                      (function 
                        (lambda () 
                          (setq symbol_ (cond 
                                          ((getint 
                                              (strcat "\n next = 1 \nno previous = 0  \n<" 
                                                      (rtos 
                                                        (setq symbol_ (cond 
                                                                        (symbol_)
                                                                        (0.0)
                                                                      )
                                                        )
                                                      )
                                                      "> : "
                                              )
                                            )
                                          )
                                          (symbol_)
                                        )
                          )
                        )
                      ) ; ใช้ lambda เพื่อจับคำสั่ง getint
                    )
        )
        (if (vl-catch-all-error-p result)
          (if (wcmatch (strcase (vl-catch-all-error-message result)) "*CANCEL*")
            (setq x "CANCEL")  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
          )
            
        )
      ; 
      ;preloop_and_while_for_input_data
        (if
          (and 
            (/= x "CANCEL")
          )
          (progn
            (if 
              (or 
                (= symbol_ 0)
                (= symbol_ 1)      
              )
              (progn
                
                (if (/= ss_pre_filter_set_xx nil)
                  
                  (progn
                    (setq ss_pre_filter_set_xx_i 0)
                    (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx))
                      (setq target_obj_ (vlax-ename->vla-object (ssname ss_pre_filter_set_xx ss_pre_filter_set_xx_i)))
                      (setq target_linetype_ (vla-get-linetype target_obj_))
                      (if (= (TA:sequence_list target_linetype_ sum_linetype_) nil)
                        (progn
                          (vla-put-linetype target_obj_ (nth 0 sum_linetype_))
                        )
                        (princ "nn")
                      )
                      (cond
                        ((and
                            (/= target_linetype_ (TA:sequence_list target_linetype_ sum_linetype_) nil)
                            (= symbol_ 0)
                          )
                          (progn
                            (setq target_linetype_ (vla-get-linetype target_obj_))
                            (vla-put-linetype target_obj_ (nth 
                                                            (if (< (- (car (TA:sequence_list target_linetype_ sum_linetype_)) 1) 0)
                                                                (progn
                                                                  (-(length sum_linetype_) 1)
                                                                )
                                                                (- (car (TA:sequence_list target_linetype_ sum_linetype_)) 1)
                                                            ) 
                                                            sum_linetype_ 
                                                          ) 
                            )
                          )
                        )
                        ((and
                            (/= target_linetype_ (TA:sequence_list target_linetype_ sum_linetype_) nil)
                            (= symbol_ 1)
                          )
                          (progn
                            (setq target_linetype_ (vla-get-linetype target_obj_))
                            (vla-put-linetype target_obj_ (nth 
                                                            (if (> (+ (car (TA:sequence_list target_linetype_ sum_linetype_)) 1) (- (length sum_linetype_) 1))
                                                                (progn
                                                                  0
                                                                )
                                                                (+ (car (TA:sequence_list target_linetype_ sum_linetype_)) 1)
                                                            ) 
                                                            sum_linetype_ 
                                                          ) 
                            )
                          )
                        )
                      )
                      (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
                    )
                  )
                )
              )
              (princ "ERRORRRRRRRRR")
            )
          )
          (princ "COMMAND CANCEL")
        )
      ;
      ; (setq sum_linetype_i (+ sum_linetype_i 1))
    )
)

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)
(defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)
 
;; End Undo  -  Lee Mac
;; Closes an Undo Group.
 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)



      
      


(defun C:Y2 ()
  (setq x 0)  ; กำหนดค่าเริ่มต้นของ x
  (setq symbol_ 0)  ; กำหนดค่าเริ่มต้นของ symbol_
  (setq prompt (strcat "\n next = 1 \nno previous = 0  \n<" (rtos symbol_) "> : "))
  
  (setq result (vl-catch-all-apply 
                 (function (lambda () (getint prompt))) ; ใช้ lambda เพื่อจับคำสั่ง getint
               )
  )
  
  ; ตรวจสอบว่าผลลัพธ์เป็นข้อผิดพลาดหรือไม่
  (if (vl-catch-all-error-p result)
    (if (wcmatch (strcase (vl-catch-all-error-message result)) "*CANCEL*")
      (setq x 1)  ; ถ้าเป็นข้อความ *cancel* ให้ x = 1
    )
    (setq symbol_ (vl-catch-all-error-value result))  ; ถ้าไม่มีการยกเลิก ให้ใช้ค่าที่ได้รับ
  )
  
  (princ (strcat "\nSymbol value: " (rtos symbol_)))  ; แสดงค่า symbol_
  (princ (strcat "\nX value: " (rtos x)))  ; แสดงค่า x
  (princ)
)




  (setq result (vl-catch-all-apply
                (function
                  (lambda ()
                    (setqq sd wwww) ; เรียกใช้งานฟังก์ชันที่ส่งเข้ามา setqq ที่ผืดพลาด ให้ result = #<%catch-all-apply-error%>
                  )
                )
              )
  )
  (setq final-result 
         (wcmatch (strcase 
                    (vl-catch-all-error-message 
                      result
                    )
                  ) 
                  "NO FUNCTION*,*ERROR*"
         )
  ) ;final-result ให้ผล = "no function definition: SETQQ" 
  



(defun gt4 (F)
  (setq result (vl-catch-all-apply
                (function
                  (lambda ()
                    (setq ss "1") ; เรียกใช้งานฟังก์ชันที่ส่งเข้ามา
                  )
                )
              )
  )
)



; (setvar "LUNITS" 3)  ; Decimal
; (setvar "LUPREC" 5)  ; 5 ตำแหน่งทศนิยม

; (rtos 100 2 5) ; => "100.00000"