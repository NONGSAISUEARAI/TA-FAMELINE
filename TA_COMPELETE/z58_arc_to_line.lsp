(defun c:z58_arc_to_line ()
 (setq my-arc-set_ (ssget 
                            (list 
                              (cons 0 "arc") ;type of object
                              ;  (cons 8 "000 - GRID") ;kind of layer
                              ; (cons 2 "SSSS")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
  )
  (setq  my-arc-set_total (sslength my-arc-set_))
  (setq my-arc-set_i 0)

  (while
    (< my-arc-set_i my-arc-set_total)
    (setq my-arc-set_ename (ssname my-arc-set_ my-arc-set_i))
    (setq my-arc-set_obj (vlax-ename->vla-object my-arc-set_ename))
    
    (setq my-arc-set_ins_start (vlax-safearray->list (vlax-variant-value(vla-get-startpoint my-arc-set_obj ))))
    (setq my-arc-set_ins_end (vlax-safearray->list (vlax-variant-value(vla-get-endpoint my-arc-set_obj ))))
    (if
      (< (car my-arc-set_ins_start) (car my-arc-set_ins_end))
      (progn
        (command "line" my-arc-set_ins_start my-arc-set_ins_end "" )
      )
      (command "line"  my-arc-set_ins_end  my-arc-set_ins_start"" )
      
      
    )
    (setq my-arc-set_i (+ my-arc-set_i 1))
    (vla-delete my-arc-set_obj)
  
  )
 )