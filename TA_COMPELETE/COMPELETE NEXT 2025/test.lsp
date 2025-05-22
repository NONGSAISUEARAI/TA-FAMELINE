(defun c:ExportToExcel ()
  ;; Function to export data from AutoCAD to Excel
  (vl-load-com)
  (setq acad-app (vlax-get-acad-object)) ; Get AutoCAD application object
  (setq doc (vla-get-ActiveDocument acad-app)) ; Get active document
  
  ;; Create Excel application object
  (setq excel-app (vlax-create-object "Excel.Application"))
  (vlax-put-property excel-app 'Visible :vlax-true) ; Show Excel application
  (setq workbook (vlax-invoke-method excel-app 'Workbooks 'Add)) ; Create a new workbook
  (setq sheet (vlax-get-property workbook 'ActiveSheet)) ; Get active sheet

  ;; Example: Write headers
  (vlax-put-property (vlax-invoke-method sheet 'Cells 1 1) 'Value "Object Type")
  (vlax-put-property (vlax-invoke-method sheet 'Cells 1 2) 'Value "Layer")
  (vlax-put-property (vlax-invoke-method sheet 'Cells 1 3) 'Value "Handle")

  ;; Get all selected objects
  (setq ss (ssget))
  (if ss
    (progn
      (setq count 1) ; Row counter
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i))) ; Get object
        (setq count (1+ count)) ; Move to next row
        ;; Write object type
        (vlax-put-property (vlax-invoke-method sheet 'Cells count 1) 'Value (vlax-get-property obj 'ObjectName))
        ;; Write layer name
        (vlax-put-property (vlax-invoke-method sheet 'Cells count 2) 'Value (vlax-get-property obj 'Layer))
        ;; Write handle
        (vlax-put-property (vlax-invoke-method sheet 'Cells count 3) 'Value (vla-get-Handle obj))
        (setq i (1+ i))
      )
    )
    (alert "No objects selected!")
  )

  ;; Save or keep Excel open for review
  (alert "Data has been exported to Excel.")
  (vlax-release-object sheet)
  (vlax-release-object workbook)
  (vlax-release-object excel-app)
  (princ)
)


; (setq xls (vlax-create-object "excel.application"))
; (vlax-put-property xls 'visible :vlax-true)
; (vlax-put-property xls 'visible :vlax-false)
; (vlax-dump-object xls)
;  (setq wb (vlax-invoke-method xls 'Workbooks 'Add))

; (startapp (vl-registry-read (strcat "HKEY_CLASSES_ROOT\\" (vl-registry-read "HKEY_CLASSES_ROOT\\.xls") "\\shell\\new\\command")))



; (or
; (and
;   FilePath
;   (= (type FilePath) 'STR)
;   (findfile FilePath)
;  )
; (setq FilePath (getfiled "Select File:" (getvar 'dwgprefix) "xls;csv" 2))
; )
; (setq wb-collection (vlax-get excel-app "workbooks")
; (setq excel-app (vlax-get-or-create-object "excel.application")
;      wb-collection (vlax-get excel-app "workbooks")
;      arq    (vlax-invoke-method wb-collection  "Open" FilePath)
;      sheets (vlax-get arq "sheets")
;      sheet1 (vlax-get-property sheets "item" 1)
; )
       
       
  ;      (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  ; (if ExcelFile$
  ;   (if (findfile ExcelFile$)
  ;     (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
  ;     (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
  ;   );if
  ;   (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
  ; )
       
       
       
       
(defun c:NewExcelSheet () 
  ;; สร้างวัตถุ Excel Application
  (setq xlss (vlax-create-object "Excel.Application"))
  (vlax-put-property xlss 'Visible :vlax-true) ; ทำให้ Excel แสดงผล

  (vlax-get-property xlss 'Workbooks)

  ;; เพิ่ม Workbook ใหม่
  (setq wb (vlax-invoke-method xlss 'Workbooks 'Add))

  ;; เพิ่ม Sheet ใหม่
  (setq sheets (vlax-get-property wb 'Sheets)) ; เข้าถึงคอลเลกชัน Sheets
  (setq new-sheet (vlax-invoke-method sheets 'Add)) ; เพิ่ม Sheet ใหม่

  ;; ตั้งชื่อ Sheet
  (vlax-put-property new-sheet 'Name "MyNewSheet")

  ;; เขียนค่าลงใน Sheet ใหม่
  (vlax-put-property (vlax-invoke-method new-sheet 'Cells 1 1) 
                     'Value
                     "Hello, New Sheet!"
  )

  ;; ทำความสะอาดหน่วยความจำ
  (vlax-release-object new-sheet)
  (vlax-release-object sheets)
  (vlax-release-object wb)
  (vlax-release-object xls)

  (princ "\nNew sheet has been created!")
  (princ)
)
(defun c:c1cCreateExcelWorkbook ()
  ;; สร้าง Object Excel
  (setq excelApp (vlax-create-object "Excel.Application"))
  
  ;; ทำให้ Excel มองเห็นได้ (optional)
  (vlax-put-property excelApp 'Visible :vlax-true)
  
  ;; สร้าง Workbook ใหม่
  (setq workbooks (vlax-get-property excelApp 'Workbooks))
  (setq newWorkbook (vlax-invoke-method workbooks 'Add))
  
  
    (vlax-dump-object newWorkbook)
    
    (vlax-dump-object sheets)
    (vlax-dump-object firstSheet)
    
    (vlax-dump-object newSheet)
    (vlax-dump-object activeSheet)
  
  ;; เข้าถึง Sheet ที่ต้องการ
  (setq sheets (vlax-get-property newWorkbook 'Sheets)) ;Sheet คือ file
  (setq activeSheet (vlax-get-property newWorkbook 'ActiveSheet)) ;ดึง VLA-OBJECT ปัจจุบัน
  (vlax-put-property activeSheet 'name "ssss" )
  
  ;; เพิ่ม worksheet ที่ต้องการ
  (setq newSheet (vlax-invoke-method sheets 'Add))
  (setq test (vlax-put-property newSheet 'name "SSS" )) ;เปลี่ยนชื่อ worksheet
 
  
  
  (setq firstSheet (vlax-get-property sheets 'Item 1))
  (setq firstSheet (vlax-invoke-method sheets 'Item 1))
  (setq cell (vlax-get-property firstSheet 'Cells 1 1))

  ;; เขียนข้อความลงเซลล์ (A1)
  (setq cell (vlax-invoke-method newSheet 'Cells 1 1))
  (vlax-put-property cell 'Value "Hello from AutoLISP!")
  
  ;; ปิด Workbook (optional)
  ;; (vlax-invoke-method newWorkbook 'SaveAs "C:\\Temp\\MyWorkbook.xlsx")
  ;; (vlax-invoke-method newWorkbook 'Close)
  
  ;; จบการทำงาน (optional)
  ;; (vlax-invoke-method excelApp 'Quit)
  ;; (vlax-release-object excelApp)
  
  ;; แจ้งเตือน
  (princ "\nWorkbook created successfully!")
)




(defun c:create-excel ()
  (vl-load-com) ; โหลดการใช้งาน ActiveX
  
  ;; สร้าง Excel Application
  (setq excelApp (vlax-create-object "Excel.Application"))
  
  ;; ทำให้ Excel มองเห็นได้ (optional)
  (vlax-put-property excelApp 'Visible :vlax-true) ;VLA-OBJECT _Application 
  
  ;; สร้าง Workbook ใหม่
  (setq workbooks (vlax-get-property excelApp 'Workbooks)) ;VLA-OBJECT Workbooks
  (setq newWorkbook (vlax-invoke-method workbooks 'Add)) ;VLA-OBJECT _Workbook
  
  ;; เข้าถึงแผ่นงานแรก
  (setq sheet (vlax-get-property newWorkbook 'ActiveSheet)) ;VLA-OBJECT _Worksheet
  
  (vlax-dump-object sheet)
  (vlax-dump-object (vlax-get-property sheet 'Range "B1")) ;VLA-OBJECT Range
  
  ;; เขียนคำว่า "XXX" ลงในเซลล์ A1
  (vlax-put (vlax-get-property sheet 'Range "B1") 'Value "XXX")
  (vlax-put (vlax-variant-value (vlax-get-property (vlax-get sheet 'Cells) 'Item 1 5 )) 'Value "XXX")
  
  (vlax-get-property sheet 'row )
  
  (princ)
)




(setq excel-app (vlax-get-or-create-object "excel.application"))
(vlax-put-property excel-app 'Visible :vlax-true)