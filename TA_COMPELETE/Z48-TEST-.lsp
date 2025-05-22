
(setq k1 (vla-get-startpoint '()))

(setq lll (car (entsel)))
(setq lll_obj (vlax-ename->vla-object lll))
(kjj lll_obj)

(setq k1k1 (funcall k2))



(setq k1k1 (get-startpoint-list))





(defun sub_startpt_func_XY (vla-object)
  (strcat 
    (rtos (car(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
    ","
    (rtos (cadr(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
  )
)
(defun sub_entpt_func_XY (vla-object)
  (strcat 
    (rtos (car(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
    ","
    (rtos (cadr(vlax-safearray->list (vlax-variant-value (vla-get-startpoint vla-object)))) 2 8)
  )
)

(setq xx (kjj lll_obj))


(command "insert" "A$C4bd085c9" xx 1 0)


