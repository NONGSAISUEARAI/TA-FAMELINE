(defun c:writetextfile ( / des txt ) ;; Define function, declare local variables
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
(defun c:tt1 () ;; Define function, declare local variables
  ;set_file_name_and_location_path
    (setq location_file_ (getvar "dwgprefix"))
    (setq filename_ (substr  (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))
    (setq extension_file_ ".lin")
    (setq full_name (strcat location_file_ filename_ extension_file_))
    
  ;
  ;set_val
    (setq gridline_prefix_name "TA_GRID_LINE_") 
    (setq description_ "GRID LINE")
    (setq gridline_val_scale_ (list 0.10 0.25 0.50 0.75 1.00 2.00 5.00 10.00 15.00 20.00 25.00 30.00 40.00 50.00 60.00 70.00 80.00 90.00 100.00))
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
  ;preloop_and_while
    (setq gridline_list_ii 0)
    (while (< gridline_list_ii (length gridline_list_))
      (setq gridline_list_name (substr (car (nth gridline_list_ii (reverse gridline_list_))) 2 (- (strlen (car (nth gridline_list_ii (reverse gridline_list_)))) 11)))
      (command "_.linetype" "_load" gridline_list_name full_name "" )
    
      (setq gridline_list_ii (+ gridline_list_ii 1))
    
    )
  ;
  
)


(defun C:LAL (); = Load All Linetypes
  (command "_.linetype" "_load" "*" "linetype test.lin"); leaves in command
  (command "_.linetype" "_load" ); leaves in command
  (command "_.linetype" "_load" "xx" ); leaves in command
  (while (> (getvar 'cmdactive) 0) (command "")); Enters until finished
); 
  
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


(getvar "dwgname")
(getvar "dwgprefix")

(setq location_file_ (getvar "dwgprefix"))
(setq filename_ (substr  (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))
(setq extension_file_ ".lin")
(setq full_name (strcat location_file_ filename_ extension_file_))

(defun C:tt4 (/ file lines line)
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

(setq str "linetype test.dwg")
(setq new-str (substr str 1 (vl-string-position 32 str)))

(setq str "linetype test.dwg")
(setq new-str (substr str 1 (- (strlen str) 2)))

