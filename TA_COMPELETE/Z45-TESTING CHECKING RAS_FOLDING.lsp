;Dynamic Funcion
  ;; Get Dynamic Block Property Value  -  Lee Mac
    ;; Returns the value of a Dynamic Block property (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)

    (defun LM:getdynpropvalue ( blk prp )
        (setq prp (strcase prp))
        (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Set Dynamic Block Property Value  -  Lee Mac
    ;; Modifies the value of a Dynamic Block property (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)
    ;; val - [any] New value for property
    ;; Returns: [any] New value if successful, else nil

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
  ;; Get Dynamic Block Properties  -  Lee Mac
    ;; Returns an association list of Dynamic Block properties & values.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [lst] Association list of ((<prop> . <value>) ... )

    (defun LM:getdynprops ( blk )
        (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Set Dynamic Block Properties  -  Lee Mac
    ;; Modifies values of Dynamic Block properties using a supplied association list.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; lst - [lst] Association list of ((<Property> . <Value>) ... )
    ;; Returns: nil

    (defun LM:setdynprops ( blk lst / itm )
        (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
        (foreach x (vlax-invoke blk 'getdynamicblockproperties)
            (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
                (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
            )
        )
    )
  ;; Get Dynamic Block Property Allowed Values  -  Lee Mac
    ;; Returns the allowed values for a specific Dynamic Block property.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; prp - [str] Dynamic Block property name (case-insensitive)
    ;; Returns: [lst] List of allowed values for property, else nil if no restrictions

    (defun LM:getdynpropallowedvalues ( blk prp )
        (setq prp (strcase prp))
        (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Toggle Dynamic Block Flip State  -  Lee Mac
    ;; Toggles the Flip parameter if present in a supplied Dynamic Block.
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Return: [int] New Flip Parameter value

    (defun LM:toggleflipstate ( blk )
        (vl-some
          '(lambda ( prp / rtn )
                (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                    (progn
                        (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                        rtn
                    )
                )
            )
            (vlax-invoke blk 'getdynamicblockproperties)
        )
    )
  ;; Get Visibility Parameter Name  -  Lee Mac
    ;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [str] Name of Visibility Parameter, else nil

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
  ;; Get Dynamic Block Visibility State  -  Lee Mac
    ;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; Returns: [str] Value of Visibility Parameter, else nil

    (defun LM:getvisibilitystate ( blk / vis )
        (if (setq vis (LM:getvisibilityparametername blk))
            (LM:getdynpropvalue blk vis)
        )
    )
  ;; Set Dynamic Block Visibility State  -  Lee Mac
    ;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
    ;; blk - [vla] VLA Dynamic Block Reference object
    ;; val - [str] Visibility State Parameter value
    ;; Returns: [str] New value of Visibility Parameter, else nil

    (defun LM:SetVisibilityState ( blk val / vis )
        (if
            (and
                (setq vis (LM:getvisibilityparametername blk))
                (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
            )
            (LM:setdynpropvalue blk vis val)
        )
    )
;
;Effective Block Name Funcition
  ;; Effective Block Name  -  Lee Mac
    ;; obj - [vla] VLA Block Reference object
        (defun LM:effectivename ( obj )
            (vlax-get-property obj
                (if (vlax-property-available-p obj 'effectivename)
                    'effectivename
                    'name
                )
            )
        )
  ;; Effective Block Name  -  Lee Mac
    ;; ent - [ent] Block Reference entity

    (defun LM:al-effectivename ( ent / blk rep )
        (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
            (if
                (and
                    (setq rep
                        (cdadr
                            (assoc -3
                                (entget
                                    (cdr
                                        (assoc 330
                                            (entget
                                                (tblobjname "block" blk)
                                            )
                                        )
                                    )
                                  '("AcDbBlockRepBTag")
                                )
                            )
                        )
                    )
                    (setq rep (handent (cdr (assoc 1005 rep))))
                )
                (setq blk (cdr (assoc 2 (entget rep))))
            )
        )
        blk
    )
;
;Attribute Function
  ;; Get Attribute Value  -  Lee Mac
    ;; Returns the value held by the specified tag within the supplied block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; Returns: [str] Attribute value, else nil if tag is not found.

    (defun LM:vl-getattributevalue ( blk tag )
        (setq tag (strcase tag))
        (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Value  -  Lee Mac
    ;; Sets the value of the first attribute with the given tag found within the block, if present.
    ;; blk - [vla] VLA Block Reference Object
    ;; tag - [str] Attribute TagString
    ;; val - [str] Attribute Value
    ;; Returns: [str] Attribute value if successful, else nil.

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
  ;; Get Attribute Values  -  Lee Mac
    ;; Returns an association list of attributes present in the supplied block.
    ;; blk - [vla] VLA Block Reference Object
    ;; Returns: [lst] Association list of ((<tag> . <value>) ... )

    (defun LM:vl-getattributevalues ( blk )
        (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
    )
  ;; Set Attribute Values  -  Lee Mac
    ;; Sets attributes with tags found in the association list to their associated values.
    ;; blk - [vla] VLA Block Reference Object
    ;; lst - [lst] Association list of ((<tag> . <value>) ... )
    ;; Returns: nil

    (defun LM:vl-setattributevalues ( blk lst / itm )
        (foreach att (vlax-invoke blk 'getattributes)
            (if (setq itm (assoc (vla-get-tagstring att) lst))
                (vla-put-textstring att (cdr itm))
            )
        )
    )
;
;sub-function
  (defun LM:round ( n )
      (fix (+ n (if (minusp n) -0.5 0.5)))
  )
  
;







(defun c:z45 ()
  ;dye_pin part
    (setq dye_pin nil)
    (while (not dye_pin)
      (setq dye_pin (entsel "\nPLEASE Select a 000-DYE_PIN_PROTOTYPE"))
      (if 
        (and 
          (= (cdr (assoc 0 (entget (car dye_pin)))) "INSERT")
          (= "000-dye_pin_prototype" (LM:effectivename (vlax-ename->vla-object (cdr (assoc -1 (entget (car dye_pin)))))))
          ; (= (vla-get-isdynamicblock (setq dye_pin_OBJ (vlax-ename->vla-object (car dye_pin)))))
        )
        (progn 
            (setq dye_ename (cdr (assoc -1 (entget (car dye_pin)))))
          
            (setq dye_ins_xyz (cdr (assoc 10 (entget (car dye_pin)))))
            (princ "\n dye_ins_x = ")
            (princ (setq dye_ins_x (cadr (assoc 10 (entget (car dye_pin))))))
            (princ "\n dye_ins_y = ")
            (princ (setq dye_ins_y (caddr (assoc 10 (entget (car dye_pin))))))
            (setq dye_ins_xy (strcat (rtos dye_ins_x 2 8) "," (rtos dye_ins_y 2 8)))
        )
        (setq dye_pin nil) ; เมื่อเงื่อนไขไม่ถูกต้องให้กำหนดค่า dye_pin เป็น nil และทำการ entsel ใหม่
      )
    )
  ;
  ;ref_sheet part
    (setq ref_SHEET nil)
    (while (not ref_SHEET)
      (setq ref_SHEET (entsel "\nPLEASE Select a ref_sheet"))
      (if 
        (and 
          (= (cdr (assoc 0 (entget (car ref_SHEET)))) "INSERT")
          (= "000-RASTER_DYNAMIC_SHEET_8seg" (LM:effectivename (vlax-ename->vla-object (cdr (assoc -1 (entget (car ref_SHEET)))))))
          (= (vla-get-isdynamicblock (setq ref_SHEET_OBJ (vlax-ename->vla-object (car ref_SHEET)))))
          (= (vla-get-isdynamicblock (setq ref_SHEET_OBJ (vlax-ename->vla-object (car ref_SHEET)))))
          
        )
        (progn 
          (princ "\nxx")
            (setq ref_ename (cdr (assoc -1 (entget (car ref_SHEET)))))
          
            (setq ref_ins_xy (cdr (assoc 10 (entget (car ref_SHEET)))))
            (princ "\n ref_ins_x = ")
            (princ (setq ref_ins_x (cadr (assoc 10 (entget (car ref_SHEET))))))
            (princ "\n ref_ins_y = ")
            (princ (setq ref_ins_x (caddr (assoc 10 (entget (car ref_SHEET))))))
            (setq pi_value 3.1428571428571428571428571428571)
            (setq std_ang 180)         
          
            ;dynamic_data part
              (setq ref_Folding_number (LM:getvisibilitystate (vlax-ename->vla-object (car ref_SHEET))))
              (setq ref_line_1 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE1"))
              (setq ref_line_2 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE2"))
              (setq ref_line_3 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE3"))
              (setq ref_line_4 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE4"))
              (setq ref_line_5 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE5"))
              (setq ref_line_6 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE6"))
              (setq ref_line_7 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE7"))
              (setq ref_line_8 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "LINE8"))
          
              (setq Position1_X (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "Position1 X"))
              (setq Position1_Y (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET)) "Position1 Y"))
          
              (LM:round (setq ref_get_angle1 (* (/ (setq get_angle1 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle1")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle2 (* (/ (setq get_angle2 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle2")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle3 (* (/ (setq get_angle3 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle3")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle4 (* (/ (setq GET_ANGLE4 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle4")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle5 (* (/ (setq get_angle5 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle5")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle6 (* (/ (setq get_angle6 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle6")) pi_value) std_ang)))
              (LM:round (setq ref_get_angle7 (* (/ (setq get_angle7 (LM:getdynpropvalue (vlax-ename->vla-object (car ref_SHEET) ) "angle7")) pi_value) std_ang)))


        )
        (setq ref_SHEET nil) ; เมื่อเงื่อนไขไม่ถูกต้องให้กำหนดค่า ref_SHEET เป็น nil และทำการ entsel ใหม่
      )
    )

  ;
  ;target_sheet part
    
    (setq target_SHEET nil)
    (while (not target_SHEET)
      (command "insert" "000-RASTER_DYNAMIC_SHEET_8seg" dye_ins_xy 1 0)
      (setq hx (entlast))
      (setq hxx (list hx hx))
      (setq hxxx (car hxx))
            
      
      (setq target_SHEET hxx)
      (if 
        (and 
          (= (cdr (assoc 0 (entget (car target_SHEET)))) "INSERT")
          (= "000-RASTER_DYNAMIC_SHEET_8seg" (LM:effectivename (vlax-ename->vla-object (cdr (assoc -1 (entget (car target_SHEET)))))))
          (= (vla-get-isdynamicblock (setq target_SHEET_OBJ (vlax-ename->vla-object (car target_SHEET)))))
          (= (setq get_folding_number (LM:getvisibilitystate (vlax-ename->vla-object (car target_SHEET)))))
          
          (setq pi_value 3.1428571428571428571428571428571)
          (setq std_ang 180)
          (= (LM:round (setq target_angle1 (* (/ (setq target_angle1 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle1")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle2 (* (/ (setq target_angle2 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle2")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle3 (* (/ (setq target_angle3 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle3")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle4 (* (/ (setq target_angle4 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle4")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle5 (* (/ (setq target_angle5 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle5")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle6 (* (/ (setq target_angle6 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle6")) pi_value) std_ang))) 180)
          (= (LM:round (setq target_angle7 (* (/ (setq target_angle7 (LM:getdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "angle7")) pi_value) std_ang))) 180)
        )
        (progn 
          (princ "\nxx")
            (setq target_ename (cdr (assoc -1 (entget (car target_SHEET)))))
            (setq target_obj (vlax-ename->vla-object target_ename))
          
            (setq target_ins_xy (cdr (assoc 10 (entget (car target_SHEET)))))
            (princ "\n target_ins_x = ")
            (princ (setq target_ins_x (cadr (assoc 10 (entget (car target_SHEET))))))
            (princ "\n target_ins_y = ")
            (princ (setq target_ins_x (caddr (assoc 10 (entget (car target_SHEET))))))
          
            (setq insertionPoint (vlax-3d-point dye_ins_x dye_ins_y 0))
            
            (vla-put-insertionpoint target_obj insertionPoint)
          
            ;get_ref_data
              (LM:SetVisibilityState (vlax-ename->vla-object (car target_SHEET)) ref_Folding_number)
              (setq target_line_1 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE1" ref_line_1))
              (setq target_line_2 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE2" ref_line_2))
              (setq target_line_3 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE3" ref_line_3))
              (setq target_line_4 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE4" ref_line_4))
              (setq target_line_5 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE5" ref_line_5))
              (setq target_line_6 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE6" ref_line_6))
              (setq target_line_7 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE7" ref_line_7))
              (setq target_line_8 (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET) ) "LINE8" ref_line_8))
            ;

            

        )
        (setq target_SHEET nil) ; เมื่อเงื่อนไขไม่ถูกต้องให้กำหนดค่า target_SHEET เป็น nil และทำการ entsel ใหม่
      )
    )

    (setq ilast (atoi ref_Folding_number))
    (setq i (- ilast 1))
    
      (while 
        (and (> i 0))
        (setq angleXX (strcat "Angle" (rtos i 2 0)))
        (setq get_angleXX (eval (read (strcat "GET_ANGLE" (rtos i 2 0)))))
        (setq positionXX (strcat "Position" (rtos i 2 0) " X"))
        
        
        (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET)) positionXX 0)
        (LM:setdynpropvalue (vlax-ename->vla-object (car target_SHEET)) angleXX get_angleXX)
        (setq kk (getstring "ddddddd"))
      

        ; ทำมาถึง ขั้น while เพื่อ ส่งค่าต่างๆ ไป ที่การพับองศา
        
        (setq i (* (- 1 i) -1))
      )

  ;
  
)
