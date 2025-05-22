(defun c:CC100_DOF_DOUBLE_OFFSET_ ()
;suib_func
  
  (defun TA:Offset_line (obj num_ )
    ; (setq 1-PL_en_ (car (entsel)))
    (vla-Offset obj num_)
  )
  (defun TA:Offset_lines (selection_set num_ )
    (setq num+ num_)
    (setq num- (* num_ -1))
    (setq ss_line_i 0)
    (while (< ss_line_i (sslength selection_set))
      (setq ss_line_obj (vlax-ename->vla-object (ssname selection_set ss_line_i)))
      (TA:Offset_line ss_line_obj num-)
      (TA:Offset_line ss_line_obj num+)

      (vla-erase ss_line_obj)
      
      (setq ss_line_i (+ ss_line_i 1))
    )
  )
;
;selection_
  (if ;(= (setq ssfillter_set (ssget "I" ) nil)
    (= (setq ss_fillter_set (ssget "I" 
                                      (list 
                                        (cons 0 "XLINE,LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                              )
        )
      nil
    )
    (progn
      (setq ss_fillter_set (ssget 
                          (list 
                            (cons 0 "XLINE,LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE") ;type of object
                            ; (cons 8 "000 - GRID")   ;kind of layer
                            ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                            ; (cons 62 1)           ;kind of color call sign with color code index
                          )
                        )
      )
    )
    (princ ss_fillter_set)
  )
;
;user_input
  (setq offset_L (cond ( (getdist (strcat "\nSpecify offset length <" (rtos (setq offset_L (cond (offset_L) (1.0) ) ) ) "> : " ) ) ) (offset_L) ) )
;
;main_command 
  (TA:Offset_lines ss_fillter_set offset_L)
;
)




