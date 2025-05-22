(defun c:xxxxxxxxxx ()
  (setq ss_ename_ (car (entsel "specify Object")))
  
  (setq newWorkbook (vlax-invoke-method workbooks 'Add))
  
  
  (setq excelApp (vlax-create-object "Excel.Application"))
  (setq excelApp (vlax-get-object "Excel.Application"))
  (setq xls-app (vlax-get-object "Excel.Application"))
  (vlax-put-property excelApp 'Visible :vlax-true)
  
  
  
  (setq excelApp (vlax-get-object "Excel.Application"))
  
  
  
  (vl-load-com) 
  (setq excel-app (vlax-get-object "Excel.Application"))
  
  (setq excel-app (vlax-get-or-create-object "excel.application"))
  (vlax-put-property excel-app 'Visible :vlax-true)
  (setq workbooks (vlax-get-property excel-app 'Workbooks))
  (setq newWorkbook (vlax-invoke-method workbooks 'Add))
  
  
  (vlax-dump-object excel-app)
  
  
  
  
  (vlax-dump-object ss)
  
  (findfile "gridline test.xlsm")
  (findfile "SITE PLAN.pdf")
  
  
  (vl-load-com) ; โหลด ActiveX/COM
  (setq excel (vlax-get-object "Excel.Application")) ; ดึงอินสแตนซ์ Excel ที่เปิดอยู่
  

  (setq initialPath nil
        filePath nil
        ExcelApp nil
        Excel_Workbook_obj_ nil
        _Worksheet nil
        sheets_test nil
        
  )
  ;class_object_
    ;VLA-OBJECT _Application
      ;VLA-OBJECT _Workbooks
        ;VLA-OBJECT _Workbook
          ;VLA-OBJECT Sheets
            ;VLA-OBJECT _Worksheet 
  ;
  ;method Open Excel Ex-file
    (setq initialPath "C:/TA/TEMP EST FILES") ;filepath
    (setq filePath (getfiled "Select an Excel file"  "C:/TA/TEMP EST FILES/" "" 8)) ;open Ex-file
    (wcmatch filePath "*xlsx*")
    (setq vla-obj_app (vlax-get-or-create-object "Excel.Application")) ;VLA-OBJECT _Application
        
      (setq _Workbook_obj_ (vlax-invoke-method (vlax-get-property vla-obj_app 'WorkBooks) 'Open filePath)) ;VLA-OBJECT _Workbook
        (setq sheets_obj_ (vlax-get-property _Workbook_obj_ 'Sheets)) ;VLA-OBJECT Sheets
          (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count )) ;total_worksheet_in_file_
        (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item 1)) ;VLA-OBJECT _Worksheet
          (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name)) ;VLA-OBJECT _Worksheet 
          (vlax-get (vlax-get-property _worksheet_obj_ 'Range "AA1") "value2")  ;get_value_from_range_cell
          (vlax-put (vlax-get-property _worksheet_obj_ 'Range "AA1") 'Value "XXX") ;put_value_from_range_cell
          (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value) ;get_value_from_range_cell
          (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value "TESTTESTTESTTEST") ;put_value_from_range_cell
  
    (vlax-put-property vla-obj_app 'Visible :vlax-true) ;Open Excel Window
  
    (setq AC_Worksheet_obj_ (vlax-get-property _Workbook_obj_ 'ActiveSheet)) ;VLA-OBJECT _Worksheet [_Workbook_obj_ to (current _workshseet_obj_)]
      (setq AC_Worksheet_obj_name (vlax-get-property AC_Worksheet_obj_ 'name))
      (setq AC_Worksheet_obj_test (vlax-get-property _Workbook_obj_ 'item "ใบเบิกเงินสดย่อย_ตัวอย่าง"))
;==================================================================================================================================
;==================================================================================================================================
;==================================================================================================================================
;==================================================================================================================================
;==================================================================================================================================
    
    (defun c:Open_Excel_File_OPEX_ ()
      (if (= (vlax-dump-object vla-obj_app) nil)
        (progn
          (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
          
        )
      )
      (setq filePath (getfiled "Select an Excel file"  "C:/TA/TEMP EST FILES/" "" 8)) ;open Ex-file
      (if 
        (and
          (if
            (/= filePath nil)
            (progn
              (or 
                (wcmatch (strcase filePath) "*XLSX")
                (wcmatch (strcase filePath) "*XLSM")
              )
            )
            nil          
          )
        )
        (progn
              ; (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
                (setq _Workbook_obj_ (vlax-invoke-method (vlax-get-property vla-obj_app 'WorkBooks) 'Open filePath)) ;VLA-OBJECT _Workbook
                  (setq sheets_obj_ (vlax-get-property _Workbook_obj_ 'Sheets)) ;VLA-OBJECT Sheets
                    (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count )) ;total_worksheet_in_file_
                  (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item 1)) ;VLA-OBJECT _Worksheet
                  (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
            (vlax-put-property vla-obj_app 'Visible :vlax-true)
        )
        (alert "invalid file please try agaian")
      )
    )
    (defun TA:New_Excel_File_NEX_ ( new_vla-obj_app )
      ; (setq new_vla-obj_app (vlax-create-object "Excel.Application"))
      (setq new_Workbooks_ (vlax-get-property new_vla-obj_app 'Workbooks))
      (setq new_workbook_ (vlax-invoke-method new_Workbooks_ 'Add))
      (setq Vla_workbook_list_ (TA:Excel_Assembly_worksheet-book_obj_list_ new_Workbooks_ ))
      (vlax-put-property new_vla-obj_app 'Visible :vlax-true)
      (setq Vla_workbook_list_ Vla_workbook_list_)
    )
  
    (TA:Excel_Assembly_worksheet-book_obj_list_ test_ttest_ )
    (setq test_ttest_ (vlax-get-property (caddr (nth 1 Vla_workbook_list_)) 'sheets))
    (setq sheets_obj_ (caddr (nth 1 Vla_workbook_list_)))
  
  
    (defun TA:Excel_Ac ()
      (setq vla-obj_app (vlax-get-or-create-object "Excel.Application"))
      (setq workbooks (vlax-get-property vla-obj_app 'Workbooks))
    )
    (defun TA:Excel_Assembly_worksheet-book_obj_list_ (sheets_obj_)
      ;Note By Code_Developer
      ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
      ;
      ;sheets_obj_ = #<VLA-OBJECT Sheets >
      ;preloop_and_while
        (if 
          (or
            (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*WORKBOOKS*")
            (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*SHEETS*")
          )
          (progn
            (setq _worksheet_Vla-obj_list_ ())
            (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count ))
            (setq sheets_obj_i 1)
            (while (<= sheets_obj_i sheets_obj_length)
              (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item sheets_obj_i))
                (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
                (setq _worksheet_Vla-obj_sum_ (list sheets_obj_i _worksheet_obj_name _worksheet_obj_ ))
              (setq _worksheet_Vla-obj_list_ (cons _worksheet_Vla-obj_sum_ _worksheet_Vla-obj_list_))
              (setq sheets_obj_i (+ sheets_obj_i 1))
            )
            (setq _worksheet_Vla-obj_list_ (reverse _worksheet_Vla-obj_list_ ))
          )
        )
      ;preloop
    )
    (defun TA:Excel_Assembly_worksheet_obj_list_ (sheets_obj_)
      ;Note By Code_Developer
      ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
      ;
      ;sheets_obj_ = #<VLA-OBJECT Sheets >
      ;preloop_and_while
        (if 
          (or
            (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*WORKBOOKS*")
            (wcmatch (strcase (vl-prin1-to-string sheets_obj_)) "*SHEETS*")
          )
          (progn
            (setq _worksheet_Vla-obj_list_ ())
            (setq sheets_obj_length (vlax-get-property sheets_obj_ 'count ))
            (setq sheets_obj_i 1)
            (while (<= sheets_obj_i sheets_obj_length)
              (setq _worksheet_obj_ (vlax-get-property sheets_obj_ 'item sheets_obj_i))
                (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
                (setq _worksheet_Vla-obj_sum_ (list sheets_obj_i _worksheet_obj_name _worksheet_obj_ ))
              (setq _worksheet_Vla-obj_list_ (cons _worksheet_Vla-obj_sum_ _worksheet_Vla-obj_list_))
              (setq sheets_obj_i (+ sheets_obj_i 1))
            )
            (setq _worksheet_Vla-obj_list_ (reverse _worksheet_Vla-obj_list_ ))
          )
        )
      ;preloop
    )
    (defun TA:Excel_get_data_Rangecell (_worksheet_obj_ Rangecell_name_)
      ;Note By Code_Developer
      ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
      ;
      ;argument
        ;_worksheet_obj_ = <VLA-OBJECT _Worksheet >
        ;Rangecell_name_ = "Rangecell name in Excel App"
      ;
      ;Example 
        ;(vlax-get (vlax-get-property _worksheet_obj_ 'Range "AA1") "value2")  ;get_value_from_range_cell
      ;
      (vlax-get (vlax-get-property _worksheet_obj_ 'Range Rangecell_name_) "value2")
    )
    (defun TA:Excel_put_data_Rangecell (_worksheet_obj_ Rangecell_name_ Rangecell_value_)
      ;Note By Code_Developer
      ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
      ;
      ;argument
        ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
        ;Rangecell_name_  = "Rangecell name in Excel App"
        ;Rangecell_value_ = "Rangecell Value in Excel App"
      ;
      ;Example 
        ;(vlax-put (vlax-get-property _worksheet_obj_ 'Range "AA1") 'Value "XXX") ;put_value_from_range_cell
      ;
      (vlax-put (vlax-get-property _worksheet_obj_ 'Range Rangecell_name_ ) 'Value Rangecell_value_)
    )
    (defun TA:Excel_get_data_R1C1 (_worksheet_obj_  Rangecell_row_ Rangecell_column_)
      ;Note By Code_Developer
      ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
      ;
      ;argument
        ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
        ;Rangecell_row_  = "Row cell in Excel App"
        ;Rangecell_column_  = "Column cell in Excel App"
        ;Rangecell_value_ = "get Rangecell Value in Excel App"
      ;
      ;Example 
        ; (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value) ;get_value_from_range_cell
      ;
      (vlax-get (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'Value) ;get_value_from_range_cell
    )
    (defun TA:Excel_input_data_R1C1 (_worksheet_obj_ Rangecell_row_ Rangecell_column_ Rangecell_value_ )
      ;Note By Code_Developer
      ;pripicle of code for get data from rangecell in Excel App by specify <VLA-OBJECT _Worksheet
      ;
      ;argument
        ;_worksheet_obj_  = <VLA-OBJECT _Worksheet >
        ;Rangecell_row_  = "Row cell in Excel App"
        ;Rangecell_column_  = "Column cell in Excel App"
        ;Rangecell_value_ = "put Rangecell Value in Excel App"
        ;
      ;
      ;Example 
        ; (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item 1 27)) 'Value "TESTTESTTESTTEST") ;put_value_from_range_cell
  
      ;
      (vlax-put (vlax-variant-value (vlax-get-property (vlax-get _worksheet_obj_ 'Cells) 'Item Rangecell_row_ Rangecell_column_)) 'Value Rangecell_value_) ;p
    )
  
     
    (defun TA:Excel_Assembly_ALL-obj_list_ (books_obj_)
      ;Note By Code_Developer
      ;pripicle of code for get total _worksheet sequnce name vla-obj _sheet by sorting via sequnce  
      ;
      ;books_obj_ = #<VLA-OBJECT Sheets >
      ;preloop_and_while
        (if 
          (or
            (wcmatch (strcase (vl-prin1-to-string books_obj_)) "*WORKBOOKS*")
            ; (wcmatch (strcase (vl-prin1-to-string books_obj_)) "*SHEETS*")
          )
          (progn
            (setq _workbook_Vla-obj_list_ ())
            (setq _worksheet_vla-obj_name_list_ ())
            
            (setq books_obj_length (vlax-get-property books_obj_ 'count )) ;total file open in 1 app
            (setq books_obj_i 1)
            (while (<= books_obj_i books_obj_length)
              (setq _workbook_obj_ (vlax-get-property books_obj_ 'item books_obj_i))
                (setq _sheets_obj_ (vlax-get-property _workbook_obj_ 'sheets))
                  ;preloop_and_while
                    (setq _sheets_obj_length (vlax-get-property _sheets_obj_ 'count))
                    (setq _sheets_obj_i 1 )
                    (setq _worksheet_vla-obj_name_list_ ())
                      (while (<= _sheets_obj_i _sheets_obj_length)
                        (setq _worksheet_obj_ (vlax-get-property _sheets_obj_ 'item _sheets_obj_i))
                          (setq _worksheet_obj_name (vlax-get-property _worksheet_obj_ 'name))
                          (setq _worksheet_vla-obj_name_list_sum (list _sheets_obj_i _worksheet_obj_name _worksheet_obj_))
                        (setq _worksheet_vla-obj_name_list_ (cons _worksheet_vla-obj_name_list_sum _worksheet_vla-obj_name_list_))
                        (setq _sheets_obj_i (+ _sheets_obj_i 1))
                        ; (setq _worksheet_vla-obj_name_list_ (reverse _worksheet_vla-obj_name_list_))
                        (setq _worksheet_vla-obj_name_list_ (vl-sort _worksheet_vla-obj_name_list_  ;bigest open indent list
                                                        (function 
                                                          (lambda (a b) 
                                                            (< (nth 0 a) (nth 0 b))
                                                          )
                                                        )
                                                ) ;bigest close indent list
                        )
                      )
                    ;
              (setq _workbook_obj_name (vlax-get-property _workbook_obj_ 'name))
              (setq _worksheet_Vla-obj_list_sum_ (list books_obj_i _workbook_obj_name _workbook_obj_ _worksheet_vla-obj_name_list_))
              
              (setq _workbook_Vla-obj_list_ (cons _worksheet_Vla-obj_list_sum_ _workbook_Vla-obj_list_))
              (setq books_obj_i (+ books_obj_i 1))
            )
            ; (setq _workbook_Vla-obj_list_ (reverse _workbook_Vla-obj_list_ ))
            (setq _workbook_Vla-obj_list_ (vl-sort _workbook_Vla-obj_list_  ;bigest open indent list
                                            (function 
                                              (lambda (a b) 
                                                (< (nth 0 a) (nth 0 b))
                                              )
                                            )
                                    ) ;bigest close indent list
            )
          )
        )
      ;preloop
    )  
    
    (setq books_obj_ (vlax-get-property vla-obj_app 'WorkBooks))
    (TA:Excel_Assembly_ALL-obj_list_ (vlax-get-property vla-obj_app 'WorkBooks) )
  
    (vlax-get-property vla-obj_app 'WorkBooks)
    (setq workbook (vlax-get-property vla-obj_app 'ActiveWorkbook))
    (setq targetSheet (vlax-get-property workbook 'count))
  

  
  

    (vlax-invoke-method _worksheet_obj_ 'Activate)
  
  

  

  
  
  
  
    
    
    (vlax-invoke-method (caddr (nth 3 worksheet_obj_list_)) 'Activate) ;go to sheet by  #<VLA-OBJECT _Worksheet
    (vlax-get-property _Workbook_obj_ 'ActiveSheet)) ;VLA-OBJECT _Worksheet [_Workbook_obj_ to (current _workshseet_obj_)]

    (vlax-get-property sheets_obj_ '_Default   )
    (vlax-get-property vla-obj_Workbook_ 'name  )
    (vlax-get-property sheets_test 'count  )
    
  
    (setq name_1 (vlax-get-property _Workbook 'item 1))
    (setq name_1 (vlax-get-property vla-obj_sheets 'item 1))
    (setq sheet_test (vlax-get-property vla-obj_Workbook_ 'Caption ))
  
  
    (vlax-get-property sheets_test 'count  )
    (vlax-dump-object new_vla-obj_app )
    (vlax-dump-object (vlax-get-property (caddr (nth 3 worksheet_obj_list_)) 'Range "AA1") )
    (vlax-dump-object (vlax-get-property (caddr (nth 3 worksheet_obj_list_)) 'Range "AA2") )
    (vlax-dump-object new_vla-obj_app )
  
    (vlax-get-property sheets 'count  )
    (vlax-get-property vla-obj_sheets 'item 1)
  

    
    (vlax-for _Worksheet (vlax-get-property vla-obj_app "Sheets")
      ; (if (= (vlax-get-property Worksheet "Name") SheetName$)
      ;   (vlax-invoke-method Worksheet "Activate")
      ; );if
    );vlax-for
  
  
    (vlax-get-object "Excel.Application")
  
  
  
    (setq sheet (vlax-get-property Excel_Workbook_obj_ 'ActiveSheet)) ;VLA-OBJECT _Worksheet

    (vlax-invoke-method Excel_Workbook_obj_ 'Activate )
    (setq worksheets_ (vlax-get-property ExcelApp "Sheets"))
  
    (vlax-put (vlax-get-property sheet 'Range "AB1") 'Value "XXX")
  
  
  
  
  
  
  
  
  
    (setq test (vlax-put-property sheet 'name "SSS" ))
    (setq test (vlax-get-property sheet 'name "SSS" ))
    (vlax-dump-object (vlax-get-property sheet 'Range "a2"))
  
    (vlax-dump-object _Worksheet)
  
    (setq sheet (vlax-get-property newWorkbook 'ActiveSheet))  

  
  ;
  
    (vlax-get-object "Excel.Application")
  
  
  
  
  
)





(defun c:ListExcelInstances ()
  (vl-load-com)
  (setq excelInstances nil) ; เตรียมรายการเก็บ instances

  ;; ใช้ PowerShell ดึง process ที่เกี่ยวข้องกับ Excel
  (setq command "powershell -command \"Get-Process excel | ForEach-Object { $_.Id }\"")
  (setq processIDs (vl-string->list (vlax-invoke-method (vlax-get-object "WScript.Shell") 'Exec command)))

  (foreach processID processIDs
    (progn
      ;; พยายามเชื่อมต่อกับ Excel instance
      (setq excelObj (vl-catch-all-apply 'vlax-get-object (list "Excel.Application" processID)))
      (if (not (vl-catch-all-error-p excelObj))
        (setq excelInstances (cons excelObj excelInstances))
      )
    )
  )

  ;; แสดงรายการ instance ที่พบ
  (if excelInstances
    (foreach excelInstance excelInstances
      (princ (strcat "\nFound Excel instance: " (vlax-get excelInstance 'Name)))
    )
    (princ "\nNo Excel instances found.")
  )
  (princ)
)
