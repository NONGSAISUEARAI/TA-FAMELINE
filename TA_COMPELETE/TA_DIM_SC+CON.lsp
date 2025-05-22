(defun c:DHH_HONLI_MAIN ()
  (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )

  (setq sc (* scl 10)) ; 5 = 50 2 = 20
  (setq of_point 3); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq OF_DIM (* (* scl 1) of_point))
  (setq HI_DIM (* (* scl 1) hi_point))
  (princ "\n OFFSET FROM pt2 = ")(princ OF_DIM)
  (setq pt1 (getpoint "\n PT1"))
  (princ "\n X=") (princ (car pt1))(princ "\n Y=") (princ (cadr pt1))

  (setq pt2 (getpoint "\n PT2"))
  (princ "\n X=") (princ (car pt2))(princ "\n Y=") (princ (cadr pt2))

  (setq pt3 (list (car pt1)(+ (cadr pt2)OF_DIM)) )
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))

  (setq pt4 (list (car pt1)(+ (cadr pt2)HI_DIM)) )
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))
  
  (command "dimstyle" "r" "DIM E-E")
  (setvar "DIMSCALE" sc)


  (command "dimlinear" pt1 pt2 pt3)



  (command "DIMCONTINUE")
  ; (command "dimlinear" pt1 pt2 pt4)
  
  (princ)
)
(defun c:DH_HONLI_MAIN ()
  (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )

  (setq sc (* scl 10)) ; 5 = 50 2 = 20
  (setq of_point 6); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq OF_DIM (* (* scl 1) of_point))
  (setq HI_DIM (* (* scl 1) hi_point))
  (princ "\n OFFSET FROM pt2 = ")(princ OF_DIM)
  (setq pt1 (getpoint "\n PT1"))
  (princ "\n X=") (princ (car pt1))(princ "\n Y=") (princ (cadr pt1))

  (setq pt2 (getpoint "\n PT2"))
  (princ "\n X=") (princ (car pt2))(princ "\n Y=") (princ (cadr pt2))

  (setq pt3 (list (car pt1)(+ (cadr pt2)OF_DIM)) )
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))

  (setq pt4 (list (car pt1)(+ (cadr pt2)HI_DIM)) )
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))
  
  (command "dimstyle" "r" "DIM E-E")
  (setvar "DIMSCALE" sc)


  (command "dimlinear" pt1 pt2 pt3)



  (command "DIMCONTINUE")
  ; (command "dimlinear" pt1 pt2 pt4)
  
  (princ)
)
(defun c:DVV_VERTI_MAIN ()
  (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )

  (setq sc (* scl 10)) ; 5 = 50 2 = 20
  (setq of_point 3); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq OF_DIM (* (* scl 1) of_point))
  (princ "\n OFFSET FROM pt2 = ")(princ OF_DIM)
  (setq pt1 (getpoint "\n PT1"))
  (princ "\n X=") (princ (car pt1))(princ "\n Y=") (princ (cadr pt1))

  (setq pt2 (getpoint "\n PT2"))
  (princ "\n X=") (princ (car pt2))(princ "\n Y=") (princ (cadr pt2))

  ; (setq pt3 (list (car pt2)(+ (cadr pt1)OF_DIM)) )
  (setq pt3 (list (- (car pt1)OF_DIM) (cadr pt2)))
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))

  (command "dimstyle" "r" "DIM E-E")
  (setvar "DIMSCALE" sc)


  (command "dimlinear" pt1 pt2 pt3)
  


  (command "DIMCONTINUE")
  
  (princ)
) 
(defun c:DV_VERTI_MAIN ()
  (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )

  (setq sc (* scl 10)) ; 5 = 50 2 = 20
  (setq of_point 6); OF_DIM OFFSET FROM pt2 = 6 mm.
  (setq OF_DIM (* (* scl 1) of_point))
  (princ "\n OFFSET FROM pt2 = ")(princ OF_DIM)
  (setq pt1 (getpoint "\n PT1"))
  (princ "\n X=") (princ (car pt1))(princ "\n Y=") (princ (cadr pt1))

  (setq pt2 (getpoint "\n PT2"))
  (princ "\n X=") (princ (car pt2))(princ "\n Y=") (princ (cadr pt2))

  ; (setq pt3 (list (car pt2)(+ (cadr pt1)OF_DIM)) )
  (setq pt3 (list (- (car pt1)OF_DIM) (cadr pt2)))
  (princ "\n X=") (princ (car pt3))(princ "\n Y=") (princ (cadr pt3))

  (command "dimstyle" "r" "DIM E-E")
  (setvar "DIMSCALE" sc)


  (command "dimlinear" pt1 pt2 pt3)
  


  (command "DIMCONTINUE")
  
  (princ)
) 


(defun c:DSD_DIMEDITd ()
  (setq DDS (ssget '((0 . "*DIMENSION"))))

    (setq bp (ssname DDS 0))
    (setq bp_en (entget bp))
    (setq bp_coor_set (assoc 13 bp_en))
  ; (command "_PSELECT" DDS "")
  
    (princ "\n STAR_X =")
    (princ (setq bn_INSERT_X (cadr bp_coor_set)))
    
    (princ "\n STAR_Y =")
    (princ (setq bn_INSERT_Y (caddr bp_coor_set)))  

    (setq bn_INSERT_XY (strcat
                          (rtos bn_INSERT_X 2 5)
                          ","
                          (rtos bn_INSERT_Y 2 5)
                        )
    )
  
  (command "DIMEDIT" "OB" DDS ) 
)

(defun LM:getdynpropvalue (blk prp) 
  (setq prp (strcase prp))
  (vl-some 
    '(lambda (x) 
       (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)
(defun LM:vl-getattributevalue (blk tag) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
     )
    (vlax-invoke blk 'getattributes)
  )
)
(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
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
(vl-load-com)
(princ)

(defun c:DCCV ()
  (setq myset (ssget 
                (list 
                  ;  (cons 0 "inset")
                  ;  (cons 8 "000 - GRID")
                  ;  (cons 62 1)
                ) 
              ) 
  )
  (vl-load-com)
  (setq entityCount (sslength mySet))
  (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq entity1 (ssname mySet i))
      (setq s0 (vlax-ename->vla-object entity1))
      (setq s0_dimst (vla-get-stylename s0))
      (setq s0_dimst (vla-get-scalefactor s0))
      

      (setq new_s0_dimst (vla-put-stylename s0 "DIM C-C"))
      (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
      (setq new_s0_dimst (vla-get-TextMovement s0 ))
      (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
      
    (setq i (1+ i))
  )  
)
(defun c:DECV ()
  
  (setq myset (ssget (list 
                       
                      ;  (cons 0 "inset") 
                      ;  (cons 8 "000 - GRID") 
                      ;  (cons 62 1) 
                       
                     ) 
              ) 
  )
  (vl-load-com)
  (setq entityCount (sslength mySet))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq entity1 (ssname mySet i))
      (setq s0 (vlax-ename->vla-object entity1))
      (setq s0_dimst (vla-get-stylename s0))
      (setq s0_dimst (vla-get-scalefactor s0))
      
      (setq new_s0_dimst (vla-put-stylename s0 "DIM E-C"))
      (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
    (setq i (1+ i))
  )  
)
(defun c:DCEV ()
  
  (setq myset (ssget (list 
                       
                      ;  (cons 0 "inset") 
                      ;  (cons 8 "000 - GRID") 
                      ;  (cons 62 1) 
                       
                     ) 
              ) 
  )
  (vl-load-com)
  (setq entityCount (sslength mySet))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq entity1 (ssname mySet i))
      (setq s0 (vlax-ename->vla-object entity1))
      (setq s0_dimst (vla-get-stylename s0))
      (setq s0_dimst (vla-get-scalefactor s0))
      
      (setq new_s0_dimst (vla-put-stylename s0 "DIM C-E"))
      (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
    (setq i (1+ i))
  )  
)
(defun c:DEEV ()
  
  (setq myset (ssget (list 
                       
                      ;  (cons 0 "inset") 
                      ;  (cons 8 "000 - GRID") 
                      ;  (cons 62 1) 
                       
                     ) 
              ) 
  )
  (vl-load-com)
  (setq entityCount (sslength mySet))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
    (setq entity1 (ssname mySet i))
      (setq s0 (vlax-ename->vla-object entity1))
      (setq s0_dimst (vla-get-stylename s0))
      (setq s0_dimst (vla-get-scalefactor s0))
      
      (setq new_s0_dimst (vla-put-stylename s0 "DIM E-E"))
      (setq new_s0_dimst (vla-put-scalefactor s0 s0_dimst))
    (setq i (1+ i))
  )  
)
(defun c:R1s_ins_grid ()
  (setq ins_xy (getpoint "take my horse to the old town road"))
  (setq L (getdist ins_xy "\nSpecify BOX_LENGTH : ")) 
  
  (command "insert" "000-SS_GRID" ins_xy 1 0 )
    (setq mai (entlast))
    (setq mai_en (entget mai))
    (setq mai_o (vlax-ename->vla-object mai))
  
    (setq mai_o_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mai_o))))
  (command "rotate" mai "" mai_o_ins_xy)


   
  (LM:setdynpropvalue mai_o "L" (rtos L 2 2))
  ; (setq of_l (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq of_l (cond (of_l) (1.0) ) ) ) "> : " ) ) ) (of_l) ) )
  ; (LM:setdynpropvalue mai_o "of_l" (rtos of_l 2 2))
  
)