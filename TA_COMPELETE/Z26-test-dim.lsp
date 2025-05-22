(defun c:ec () ;this is a checkingfor test finding ename or total entget
  (setq ec (car (entsel)))                          ;1st way for ename only
  (setq abx (entget (car (entsel "\n OK GO"))))     ;2nd way for total asssociate dxf code 
)

(defun c:z26 ()
; start sub function for sorting  
    (defun get-x-coordinate (entity)
      (caddr (assoc 10 (entget entity))) 
      ;cadr is func to indicate coord x 
      ;caddr is func to indicate coord y
    )

    (defun sort-entities-by-coord (selection-set)
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
                                          (> (cadr a) (cadr b)) ; > = max to min, < = min to max 
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
  ; start finding sssget for sorting coord x or y 
    (setq my-selection-set (ssget 
                  (list 
                    (cons 0 "INSERT")
                    (cons 8 "000 - GRID")
                    ;  (cons 62 1)
                  )
                )
    )
  ; end finding sssget for sorting coord x or y 
    
    (princ (setq sorted-enames (sort-entities-by-coord my-selection-set)))
    (setq total-entities (length sorted-enames))
    
    (princ (strcat "total =" (rtos total-entities 2 0)))
; end sub function for sorting   
; ที่เหลือก็คือใส่อะไรใส่เลย
)
