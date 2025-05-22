
(defun c:z53_make_xline_atpint ()
  
  
  ;SUB_FUNC
     (defun sort_by_X (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
      )
      (defun sort_by_y (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
      (defun sort_by_number (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadddr a) (cadddr b))))))
      )
  ;
  (setq ss_point_ (ssget 
                   (list 
                     (cons 0 "point") ;type of object
                     ; (cons 8 "000 - GRID")   ;kind of layer
                     ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                     ; (cons 62 1)           ;kind of color call sign with color code index
                   )
                 )
  )
  ;user_input_mode
    (initget "H V A")

    ; (setq direction_value (getstring "specify direction value"))
    (setq direction_mode (getkword "\nspecify direction value [H/V/A] <H>: "))
    (cond
      (
        (= direction_mode "H")
        (setq direction_mode "H")
      )
      (
        (= direction_mode "V")
        (setq direction_mode "V")
      )
      (
        (= direction_mode "A")
        (setq direction_mode "A")
        (setq direction_angle (getreal "\nspecify angle"))
        (setq direction_angle_string (rtos direction_angle 2 8))
      )
    )
  ;
  (setq ss_point_total (sslength ss_point_))
  (setq ss_point_i 0)
  (while
    (< ss_point_i ss_point_total)
    ;get_data_part
      (setq ss_point_ename (ssname ss_point_ ss_point_i))
      (setq ss_point_obj (vlax-ename->vla-object ss_point_ename))
      ; (setq ss_point_dump (vlax-dump-object ss_point_obj))
      (setq ss_point_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-coordinates  ss_point_obj))))
    ;
    ;command_part
      (if
        (= direction_mode "A")
        (progn
        (command "xline" direction_mode direction_angle ss_point_ins_xyz "")
        )
        (command "xline" direction_mode ss_point_ins_xyz "")
      )
      (setq ss_point_i (+ ss_point_i 1))
    ;
  )
)

