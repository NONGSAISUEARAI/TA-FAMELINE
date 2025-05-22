(setq abx (entget (car (entsel "\n OK GO"))))

(setq entData (entget abx)) ; เปลี่ยน someEntity เป็นรหัสของวัตถุที่คุณต้องการ

; ใช้ vl-remove-if-not เพื่อกรองรายการที่มีคีย์เป็น 11
(setq eleventhData (vl-remove-if-not '(lambda (x) (= 11 (car x))) abx))

; ใช้ mapcar เพื่อดึงข้อมูลออกมา
(setq dataList (mapcar 'cdr eleventhData))

; นำข้อมูลที่ได้มาใช้ต่อ
(foreach data dataList
  ; ทำสิ่งที่คุณต้องการกับข้อมูลที่ดึงออกมาได้ที่ตัวแปร data
  (princ data) ; ตัวอย่าง: พิมพ์ข้อมูลที่ได้
)