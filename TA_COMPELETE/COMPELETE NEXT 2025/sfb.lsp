(defun c:SFB_FINDING_SPECFILY_NAME_BLOCK ()
  ;selection_set
    (if 
      (= (setq my-selection-set (ssget "_I" 
                                        (list 
                                          (cons 0 "INSERT") ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                 )
           )
        nil
      )
      (progn
        (setq my-selection-set (ssget 
                            (list 
                              (cons 0 "INSERT") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
        )
      )
      (princ my-selection-set)
     )
  ;

  ; (setq ef_name "000-GRID_LINE_DYN")
  ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
  
  (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
  (setq select_name_obj (vlax-ename->vla-object select_name))
  (setq ef_name (LM:effectivename select_name_obj))

  (setq total_ssget (sslength my-selection-set))
  (setq ie 0)
  (setq ename-list '())

  (while 
    (< ie total_ssget)
    (setq blk_obj (ssname my-selection-set ie))
    (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
    (setq ss (LM:effectivename blk_obj_Set))

      (if (= ef_name ss)
        (progn
          (setq ename-list (cons blk_obj ename-list)) 
        )
        (princ "\n")

      )
    (setq ie (+ ie 1))
  )
  (princ "\n")
  (princ (setq total_ename-list (length ename-list)))
  (setq ff (ssadd))
    (foreach ename ename-list
      (ssadd ename ff)
    )
  (command "_pselect" ff "")
  (command "regen")
)