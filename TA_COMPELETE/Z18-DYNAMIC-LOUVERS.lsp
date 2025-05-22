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
(defun LM:getvisibilitystate ( blk / vis )
    (if (setq vis (LM:getvisibilityparametername blk))
        (LM:getdynpropvalue blk vis)
    )
)
(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)
(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
(vl-load-com)
(princ)

(defun c:Z18 () 
  
  (setq myset (ssget 
                (list 

                  (cons 0 "INSERT")
                  ;  (cons 8 "0")
                  ;  (cons 62 1)
                )
              )
  )
  (setvar "osmode" 0)
  (vl-load-com)

  (setq entityCount (sslength mySet))

  (setq i 0)
  (while (< i entityCount)  ; เมื่อลำดับยังไม่ถึง entityCount
    (setq entity1 (ssname mySet i))

    (setq s1 (vlax-ename->vla-object entity1))
    (setq rotation_value (vla-get-rotation s1))
    (setq rotation_value_REAL (rtos (/ rotation_value 0.0174533 ) 2 0))
    (setq s1_inspoint(vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint s1))))
      (setq s1_inspoint_X (car s1_inspoint ))
      (setq s1_inspoint_Y (cadr s1_inspoint ))
    (setq s1_inspoint_X_Y (strcat (rtos s1_inspoint_X 2 8) "," (rtos s1_inspoint_Y 2 8) ))
    
    (setq S1_H (LM:getdynpropvalue (vlax-ename->vla-object entity1) "H"))
    (setq S1_H2 (/ S1_H 2))
    (setq S1_W (LM:getdynpropvalue (vlax-ename->vla-object entity1) "W"))
    (setq S1_W2 (/ S1_W 2))
  
    (setq s1_inspoint_NEW_PT_X (+ s1_inspoint_X S1_W2))
    (setq s1_inspoint_NEW_PT_XX (rtos s1_inspoint_NEW_PT_X 2 8))
    
    (setq s1_inspoint_NEW_PT_Y (+ s1_inspoint_Y S1_H2))
    (setq s1_inspoint_NEW_PT_YY (rtos s1_inspoint_NEW_PT_Y 2 8))
    
    (setq s1_inspoint_XX_YY (strcat (rtos s1_inspoint_NEW_PT_X 2 8) "," (rtos s1_inspoint_NEW_PT_Y 2 8) ))
    
    (command "insert" "000TYP-LINE_AND_ARRAY_@60mm." s1_inspoint_XX_YY 1 0)


      (setq o (entlast)) ; àÅ×Í¡ dynamic block ·ÕèµéÍ§¡ÒÃ
        (setq Hnew (LM:getdynpropvalue (vlax-ename->vla-object o) "H"))
        (setq Warraynew (LM:getdynpropvalue (vlax-ename->vla-object o) "W_array"))  
        (if
          (and o
              (setq o (vlax-ename->vla-object o))
              (if (not (eq (vla-get-objectname o) "AcDbBlockReference"))
                (alert "Please select block object.")
                t
              )
              (if (not (eq (vla-get-isdynamicblock o) :vlax-true))
                (alert "Please select dynamic block.")
                t
              )
              ;  (setq VIS (getstring "\nEnter new CONTENT_LINE : ")) ; ãªé GETSTRING à¾×èÍÃÑº¤èÒ visibility state ¨Ò¡¼Ùéãªé
              (setq VIS (rtos 50 2 0)) ; ãªé GETSTRING à¾×èÍÃÑº¤èÒ visibility state ¨Ò¡¼Ùéãªé

          )
          (progn
              (LM:setdynpropvalue o "DEPT. LINE" VIS)
              (LM:setdynpropvalue o "H" S1_H)
              (LM:setdynpropvalue o "W_array" S1_W)
            
          )
        )

      (command "ROTATE" "L" "" s1_inspoint_X_Y rotation_value_REAL)
        
        
    
    
    (setq i (1+ i))
    ; (setq ss_l (vla-get- s1))
  )
  (setvar "osmode" 1215)
)


; (setq block_ename (car (entsel "เลือกบล็อก: ")))

; (setq block_vla (vlax-ename->vla-object block_ename))
; (setq rotation_in_degrees1 (vla-get-insertionpoint block_vla))
; (setq rotation_in_degrees2 (vla-get-rotation block_vla))
; (setq rotation_in_degrees3 (vla-get-propertyname block_vla Rotation))



