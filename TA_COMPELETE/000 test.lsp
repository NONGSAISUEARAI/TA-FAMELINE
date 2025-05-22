; (defun c:z11 ()
 

; ) 
  

 (setq e_ename_ (car (entsel "specify Object")))
  (setq e_obj_ (vlax-ename->vla-object e_ename_))
  (setq a (getstring "sss"))
  (setq s (getint "aaa"))

  (setq eee 1)