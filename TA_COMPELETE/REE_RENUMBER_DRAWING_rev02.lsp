; (defun selection-set-to-list (selection-set)
  ;   (setq my-list '()) ; สร้างลิสต์ว่าง

  ;   (setq num-entities (sslength selection-set)) ; หาความยาวของรายการ

  ;   (setq i 0) ; ตั้งค่าตัวแปร i เป็น 0
  ;   (while (< i num-entities)
  ;     (setq entity (ssname selection-set i)) ; ดึง ename ของสมาชิกที่ i
  ;     (setq my-list (cons entity my-list)) ; เพิ่ม ename ลงในลิสต์
  ;     (setq i (+ i 1)) ; เพิ่มค่า i อีก 1 หลังจากที่ดึงข้อมูลแล้ว
  ;   )

  ;   (reverse my-list) ; กลับด้านลิสต์เพื่อให้ลำดับถูกต้อง)
  ; )

  ; (setq my-selection-set (ssget '((0 . "INSERT")))) ; ตัวอย่างการใช้ ssget

  ; (setq my-list (selection-set-to-list my-selection-set))

; ; ผลลัพธ์คือ my-list ที่มีรายการของ ename ของเส้นทั้งหมดในรายการ
(defun c:REE_RENUMBER_DRAWING ()
  (defun LM:vl-setattributevalue ( blk tag val )
      (setq tag (strcase tag))
      (vl-some
        '(lambda ( att )
              (if (= tag (strcase (vla-get-tagstring att)))
                  (progn (vla-put-textstring att val) val)
              )
          )
          (vlax-invoke blk 'getattributes)
      )
  )

  (defun get-x-coordinate (entity)
    (cadr (assoc 10 (entget entity)))
  )

  (defun sort-entities-by-x (selection-set)
    (setq entity-list '())

    (setq num-entities (sslength selection-set))

    (setq i 0)
    (while (< i num-entities)
      (setq entity (ssname selection-set i))
      (setq x-coord (get-x-coordinate entity))
      (setq entity-list (cons (list entity x-coord) entity-list))
      (setq i (+ i 1))
    )

    (setq sorted-entity-list (vl-sort entity-list 
                                    '(lambda (a b) 
                                      (if (and (cdr a) (cdr b))
                                        (< (cadr a) (cadr b))
                                        t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                    )
                          )
    )

    (setq sorted-ename-list '())
    (foreach entity-entity sorted-entity-list
      (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
    )

    (reverse sorted-ename-list)
  )

  (setq my-selection-set (ssget  '((0 . "INSERT"))))

  (setq sorted-enames (sort-entities-by-x my-selection-set))
  (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 101) ; เริ่มต้น ii ที่ 101
  (setq iii 1) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              (setq NAME_VIEWPORT_NEW (strcat "M" (if (< ii 10) "0" "") (itoa ii))) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              (setq SHEET_NO_NEW (strcat (itoa iii) " of " (itoa total-entities))) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              (setq TOTAL_SHEET_NO_NEW (strcat (itoa total-entities) " PAGE")) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
            
              (LM:vl-setattributevalue o "DRAWING_NO2" NAME_VIEWPORT_NEW)
              (LM:vl-setattributevalue o "SHEET_NO." SHEET_NO_NEW)
              (LM:vl-setattributevalue o "TOTAL_SHEET_NO." TOTAL_SHEET_NO_NEW)
            
              (LM:vl-setattributevalue o "NAME_VIEWPORT" NAME_VIEWPORT_NEW)
              


              (setq i (+ i 1))
              (setq ii (+ ii 1)) ; เพิ่ม ii ทีละ 1   
              (setq iii (+ iii 1)) ; เพิ่ม iii ทีละ 1   
            
          )
    
; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
