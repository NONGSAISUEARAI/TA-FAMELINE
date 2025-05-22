(defun c:z50_test ()
  (setq abx (entget (car (entsel "\n OK GO"))))
  ; (setq entData (entget abx)) ; เปลี่ยน someEntity เป็นรหัสของวัตถุที่คุณต้องการ

  ; ใช้ vl-remove-if-not เพื่อกรองรายการที่มีคีย์เป็น 11
  ; (setq eleventhData (vl-remove-if-not '(lambda (x) (= 11 (car x))) abx)) ;for mline
    (setq tenthData (vl-remove-if-not '(lambda (x) (= 10 (car x))) abx)) ;for pline
    (setq total_point (length tenthData))
    (setq total_point1 (- total_point 1))
    
    (setq 1st_total_point_x (cadr (nth 0 tenthData)))
    (setq 1st_total_point_y (caddr (nth 0 tenthData)))
    (setq 1st_total_point_xy (strcat (rtos 1st_total_point_x 2 8) "," (rtos 1st_total_point_y 2 8)))
    
    (setq mid_total_point_x (cadr (nth (/ total_point 2 ) tenthData)))
    (setq mid_total_point_y (caddr (nth (/ total_point 2 ) tenthData)))
    (setq mid_total_point_xy (strcat (rtos mid_total_point_x 2 8) "," (rtos mid_total_point_y 2 8)))
    
    (setq last_total_point_x (cadr(nth total_point1 tenthData)))
    (setq last_total_point_y (caddr(nth total_point1 tenthData)))
    (setq last_total_point_xy (strcat (rtos last_total_point_x 2 8) "," (rtos last_total_point_y 2 8)))
I(command "arc" 1st_total_point_xy mid_total_point_xy last_total_point_xy ) 
 
)