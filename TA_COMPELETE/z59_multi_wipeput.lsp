(defun c:z59w_multi_wipeout ()
 (setq ss_pline_ (ssget 
                (list 
                  (cons 0 "lwpolyline")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (setq ss_pline_total (sslength ss_pline_))
  (setq ss_pline_i 0)
  (setq ename-list '())
  (while
    (< ss_pline_i ss_pline_total)
    (setq ss_pline_ename (ssname ss_pline_ ss_pline_i))
    (setq ss_pline_obj (vlax-ename->vla-object ss_pline_ename))
    (setq ss_pline_obj_closed_line (vla-get-closed ss_pline_obj ))
    (if
      (= ss_pline_obj_closed_line :vlax-true)
      (progn
        (setq ename-list (cons ss_pline_ename ename-list))
      )
      (princ "\n")
    )
    (setq ss_pline_i (+ ss_pline_i 1))
  )
  (setq filter_ename (ssadd))
  (foreach ename ename-list
    (ssadd ename filter_ename)
  )
  (setq filter_ename_total (sslength filter_ename))
  (setq filter_ename_i 0)
  (while
    (< filter_ename_i filter_ename_total)
    (setq filter_ename_ename (ssname filter_ename filter_ename_i))
    (command "wipeout" "" filter_ename_ename "n" )
    (command "draworder"  "L" "" "b")
    (setq filter_ename_i (+ filter_ename_i 1))
  
  )
)
