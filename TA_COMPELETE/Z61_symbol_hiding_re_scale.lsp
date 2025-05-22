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
;Polygon Centroid  -  Lee Mac
  ;; Returns the WCS Centroid of an LWPolyline Polygon Entity
    (defun LM:PolyCentroid ( e / l )
        (foreach x (setq e (entget e))
            (if (= 10 (car x)) (setq l (cons (cdr x) l)))
        )
        (
            (lambda ( a )
                (if (not (equal 0.0 a 1e-8))
                    (trans
                        (mapcar '/
                            (apply 'mapcar
                                (cons '+
                                    (mapcar
                                        (function
                                            (lambda ( a b )
                                                (
                                                    (lambda ( m )
                                                        (mapcar
                                                            (function
                                                                (lambda ( c d ) (* (+ c d) m))
                                                            )
                                                            a b
                                                        )
                                                    )
                                                    (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                                                )
                                            )
                                        )
                                        l (cons (last l) l)
                                    )
                                )
                            )
                            (list a a)
                        )
                        (cdr (assoc 210 e)) 0
                    )
                )
            )
            (* 3.0
                (apply '+
                    (mapcar
                        (function
                            (lambda ( a b )
                                (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                            )
                        )
                        l (cons (last l) l)
                    )
                )
            )
        )
    )
  ;;
;






(defun c:ss1_set1 () ;run ทีบรรทัด เท่านั้น copy ทำงานแบบ entlast
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "INSERT")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (setq TOTAL_SSGET (sslength my-selection-set))
  (setq tl 80)
  (setq bkk 0)
  (setq blank_x 0)
  (setq blank_y 0)
  (setq blank_xy (strcat (rtos blank_x 2 8) "," (rtos blank_y 2 8)))
  (setq new_x 0)
  (setq new_y -45) 
  (while 
      (setvar "osmode" 0)
      (< bkk 80)
      (setq new_xy (strcat (rtos new_x 2 8) "," (rtos new_y 2 8)))
      (setq blank_xy_set (strcat blank_xy "," (rtos 0 2 8)))
      (setq new_xy_set (strcat new_xy "," (rtos 0 2 8)))
      
      (command "copy" "L" ""  blank_xy_set new_xy_set)
      ; (command "draworder" "L" "" "b")
      (setq bkk (+ bkk 1))
      (setq new_y (- new_y 5))
  )
)
(defun c:ss2_set2 ()  ;run ทีบรรทัด เท่านั้น copy ทำงานแบบ entlast
  (setq my-selection-set (ssget 
                (list 
                  (cons 0 "INSERT")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (setq TOTAL_SSGET (sslength my-selection-set)) 
  (setq bk 0)
  (setq blank_x 0)
  (setq blank_y 0)
  (setq blank_xy (strcat (rtos blank_x 2 8) "," (rtos blank_y 2 8)))
  (setq new_x 0)
  (setq new_y -47.5)
  (while 
    (setvar "osmode" 0)
    (< bk 80)
    (setq new_xy (strcat (rtos new_x 2 8) "," (rtos new_y 2 8)))
    (setq blank_xy_set (strcat blank_xy "," (rtos 0 2 8)))
    (setq new_xy_set (strcat new_xy "," (rtos 0 2 8)))
    
    (command "copy" "L" ""  blank_xy_set new_xy_set)
    ; (command "draworder" "L" "" "b")
    (setq bk (+ bk 1))
    (setq new_y (- new_y 5))
  )
)
;
  ; (setq my-selection-set (ssget 
  ;               (list 
  ;                 (cons 0 "INSERT")       ;type of object
  ;                 ; (cons 8 "000 - GRID")   ;kind of layer 
  ;                 ; (cons 2 "SSSS")       ;kind of nameblock
  ;                 ; (cons 62 1)           ;kind of color call sign with color code index
  ;               )
  ;             )
  ; )
  ; (setq TOTAL_SSGET (sslength my-selection-set))

  ; (setq tl 80)
  ; (setq bkk 0)
  ; (setq blank_x 0)
  ; (setq blank_y 0)
  ; (setq blank_xy (strcat (rtos blank_x 2 8) "," (rtos blank_y 2 8)))
  ; (setq new_x 0)
  ; (setq new_y -45)
  
  ; (while 
  ;   (setvar "osmode" 0)
  ;   (< bkk 80)
  ;   (setq new_xy (strcat (rtos new_x 2 8) "," (rtos new_y 2 8)))
  ;   (setq blank_xy_set (strcat blank_xy "," (rtos 0 2 8)))
  ;   (setq new_xy_set (strcat new_xy "," (rtos 0 2 8)))
    
  ;   (command "copy" "L" ""  blank_xy_set new_xy_set)
  ;   ; (command "draworder" "L" "" "b")
  ;   (setq bkk (+ bkk 1))
  ;   (setq new_y (- new_y 5))
  ; )


  ; (setq bk 0)
  ; (setq blank_x 0)
  ; (setq blank_y 0)
  ; (setq blank_xy (strcat (rtos blank_x 2 8) "," (rtos blank_y 2 8)))
  ; (setq new_x 0)
  ; (setq new_y -47.5)
  
  ; (while 
  ;   (setvar "osmode" 0)
  ;   (< bk 80)
  ;   (setq new_xy (strcat (rtos new_x 2 8) "," (rtos new_y 2 8)))
  ;   (setq blank_xy_set (strcat blank_xy "," (rtos 0 2 8)))
  ;   (setq new_xy_set (strcat new_xy "," (rtos 0 2 8)))
    
  ;   (command "copy" "L" ""  blank_xy_set new_xy_set)
  ;   ; (command "draworder" "L" "" "b")
  ;   (setq bk (+ bk 1))
  ;   (setq new_y (- new_y 5))
  ; )
;

(defun c:xxx ()
  (setq c-green 3) 
  (setq para_name (entsel "specify_parameter"))

  (setq circle_1st (entsel "select_circle_1st"))
  (setq circle_2nd (entsel "select_circle_2nd"))

  (command "CHPROP" circle_1st "" "C" c-green "")
  (command "CHPROP" circle_2nd "" "C" c-green "")

  (command "_bactiontool" "m" para_name "s" circle_1st "")
  (command "_bactiontool" "m" para_name "s" circle_2nd "")
)


; (defun selection-set-to-list (selection-set)
  ;   (setq my-list '()) ; สร้างลิสต์ว่าง

  ;   (setq num-entities (sslength selection-set)) ; หาความยาวของรายการ

  ;   (setq i 0) ; ตั้งค่าตัวแปร i เป็น 0
  ;   (while (< i num-entities)
  ;     (setq entity (ssname selection-set i)) ; ดึง ename ของสมาชิกที่ i
  ;     (setq my-list (cons entity my-list)) ; เพิ่ม ename ลงในลิสต์
  ;     (setq i (+ i 1)) ; เพิ่มค่า i อีก 1 หลังจากที่ดึงข้อมูลแล้ว
  ;   )

  ;   (reverse my-list) ; กลับด้านลิสต์เพื่อให้ลำดับถูกต้อง)
  ; )

  ; (setq my-selection-set (ssget '((0 . "INSERT")))) ; ตัวอย่างการใช้ ssget

  ; (setq my-list (selection-set-to-list my-selection-set))

; ; ผลลัพธ์คือ my-list ที่มีรายการของ ename ของเส้นทั้งหมดในรายการ
(defun c:REECC_RENUMBER_TEMP ()
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

  (defun get-x-coordinate (entity)
    (caddr (assoc 10 (entget entity)))
  )

  (defun sort-entities-by-x (selection-set)
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
                                        (> (cadr a) (cadr b))
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

  (setq my-selection-set (ssget  '((0 . "INSERT"))))

  (setq sorted-enames (sort-entities-by-x my-selection-set))
  (setq total-entities (length sorted-enames))
  (setq i 40)
  (setq ii 1) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              (setq NAME_VIEWPORT_NEW ii) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              
            
              (LM:vl-setattributevalue o "LINE" NAME_VIEWPORT_NEW)
        


              (setq i (+ i 1))
              (setq ii (+ ii 1))

          )
    
; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
(defun c:REECC1_RENUMBER_TEMP ()
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

  (defun get-x-coordinate (entity)
    (caddr (assoc 10 (entget entity)))
  )

  (defun sort-entities-by-x (selection-set)
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
                                        (> (cadr a) (cadr b))
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

  (setq my-selection-set (ssget  '((0 . "INSERT"))))

  (setq sorted-enames (sort-entities-by-x my-selection-set))
  (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 40) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              (setq NAME_VIEWPORT_NEW ii) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              
            
              (LM:vl-setattributevalue o "LINE" NAME_VIEWPORT_NEW)
        


              (setq i (+ i 1))
              (setq ii (+ ii 2.5))

          )
    
; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)


(defun c:r1r_read1_ce_att_perforated ()
  

  (setq my_perforated_set (ssget 
                (list 
                  (cons 0 "INSERT")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  ; find_code_ce
    (setq my_perforated_set_total (sslength my_perforated_set)) 
    (setq my_perforated_set_ief 0)
    (setq ssefname "_ce")
    (setq _ce_ename-list '())
    (while
      (< my_perforated_set_ief my_perforated_set_total)
      (setq my_perforated_set_ename (ssname my_perforated_set my_perforated_set_ief))
      (setq my_perforated_set_obj (vlax-ename->vla-object my_perforated_set_ename))
      (setq my_perforated_set_efname (LM:effectivename my_perforated_set_obj))
      
      (if 
        (= my_perforated_set_efname ssefname) 
        (progn 
          (setq _ce_ename-list (cons my_perforated_set_ename _ce_ename-list))
        )
        (princ "\n")
      )
      (setq my_perforated_set_ief (+ my_perforated_set_ief 1))
    )
    (princ (setq total_ce_ename-list (length _ce_ename-list)))
    (setq final_ce_ename (ssadd))
      (foreach _ce_ename _ce_ename-list 
        (ssadd _ce_ename final_ce_ename)
      )
  ;
  ; re_distance_dynamic
    (setq final_ce_ename_i 0)
    (setq final_ce_ename_total (sslength final_ce_ename))
    (while
      (setq final_ce_ename_ename (ssname final_ce_ename final_ce_ename_i))
      (setq final_ce_ename_obj (vlax-ename->vla-object final_ce_ename_ename))
      (setq line (LM:vl-getattributevalue final_ce_ename_obj "LINE"))
   
      (setq d1 (LM:setdynpropvalue final_ce_ename_obj "distance1" line))
      (setq d2 (LM:setdynpropvalue final_ce_ename_obj "distance2" line))
      (setq d3 (LM:setdynpropvalue final_ce_ename_obj "distance3" line))
      (setq d4 (LM:setdynpropvalue final_ce_ename_obj "distance4" line))
      (setq d5 (LM:setdynpropvalue final_ce_ename_obj "distance5" line))
      (setq d6 (LM:setdynpropvalue final_ce_ename_obj "distance6" line))
      (setq d7 (LM:setdynpropvalue final_ce_ename_obj "distance7" line))
      (setq d8 (LM:setdynpropvalue final_ce_ename_obj "distance8" line))
      (setq d9 (LM:setdynpropvalue final_ce_ename_obj "distance9" line))
      (setq d10 (LM:setdynpropvalue final_ce_ename_obj "distance10" line))
      (setq d11 (LM:setdynpropvalue final_ce_ename_obj "distance11" line))
      (setq d12 (LM:setdynpropvalue final_ce_ename_obj "distance12" line))
      (setq d13 (LM:setdynpropvalue final_ce_ename_obj "distance13" line))
      (setq d14 (LM:setdynpropvalue final_ce_ename_obj "distance14" line))
      (setq final_ce_ename_i (+ final_ce_ename_i 1))
    )
   
  
  ;
)
(defun c:r2r_read2_le_att_perforated ()
  

  (setq my_perforated_set (ssget 
                (list 
                  (cons 0 "INSERT")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  ; find_code_le
    (setq my_perforated_set_total (sslength my_perforated_set)) 
    (setq my_perforated_set_ief 0)
    (setq ssefname "_le")
    (setq _le_ename-list '())
    (while
      (< my_perforated_set_ief my_perforated_set_total)
      (setq my_perforated_set_ename (ssname my_perforated_set my_perforated_set_ief))
      (setq my_perforated_set_obj (vlax-ename->vla-object my_perforated_set_ename))
      (setq my_perforated_set_efname (LM:effectivename my_perforated_set_obj))
      
      (if 
        (= my_perforated_set_efname ssefname) 
        (progn 
          (setq _le_ename-list (cons my_perforated_set_ename _le_ename-list))
        )
        (princ "\n")
      )
      (setq my_perforated_set_ief (+ my_perforated_set_ief 1))
    )
    (princ (setq total_le_ename-list (length _le_ename-list)))
    (setq final_le_ename (ssadd))
      (foreach _le_ename _le_ename-list 
        (ssadd _le_ename final_le_ename)
      )
  ;
  ; re_distance_dynamic
    (setq final_le_ename_i 0)
    (setq final_le_ename_total (sslength final_le_ename))
    (setq Dist1_le 62.50)
    (setq Dist0_le 20.00)
    (while
      (setq final_le_ename_ename (ssname final_le_ename final_le_ename_i))
      (setq final_le_ename_obj (vlax-ename->vla-object final_le_ename_ename))
      (setq line (LM:vl-getattributevalue final_le_ename_obj "LINE"))
      (setq ref_d1 (LM:getdynpropvalue final_le_ename_obj "distance1"))
   
      (setq d0 (LM:setdynpropvalue final_le_ename_obj "distance0" Dist0_le))
      (setq d1 (LM:setdynpropvalue final_le_ename_obj "distance1" Dist1_le))
      (setq d2 (LM:setdynpropvalue final_le_ename_obj "distance2" line))
      (setq d3 (LM:setdynpropvalue final_le_ename_obj "distance3" line))
      (setq d4 (LM:setdynpropvalue final_le_ename_obj "distance4" line))
      (setq d5 (LM:setdynpropvalue final_le_ename_obj "distance5" line))
      (setq d6 (LM:setdynpropvalue final_le_ename_obj "distance6" line))
      (setq d7 (LM:setdynpropvalue final_le_ename_obj "distance7" line))
      (setq d8 (LM:setdynpropvalue final_le_ename_obj "distance8" line))
      (setq d9 (LM:setdynpropvalue final_le_ename_obj "distance9" line))
      (setq d10 (LM:setdynpropvalue final_le_ename_obj "distance10" line))
      (setq d11 (LM:setdynpropvalue final_le_ename_obj "distance11" line))
      (setq d12 (LM:setdynpropvalue final_le_ename_obj "distance12" line))
      (setq d13 (LM:setdynpropvalue final_le_ename_obj "distance13" line))
      (setq d14 (LM:setdynpropvalue final_le_ename_obj "distance14" line))
      (setq final_le_ename_i (+ final_le_ename_i 1))
      (setq Dist1_le (+ Dist1_le 7.5))
      (setq Dist0_le (+ Dist0_le 2.5))
    )
   
  
  ;
)

(defun c:ff2oo ()
    (setq REF_1st_LWPOLYLINE nil)
    (while (not REF_1st_LWPOLYLINE)
      (setq REF_1st_LWPOLYLINE (entsel "\nSelect a circle "))
      (if 
        (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
          (progn
            (setq REF_1st_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )
    ; (setq REF_1st_LWPOLYLINE_as_ins-set(vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE))) ;varaint test
    ; (setq xx (mapcar 'cdr REF_1st_LWPOLYLINE_as_ins-set)) ;varaint test
    (setq REF_1st_LWPOLYLINE_ins-set (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE))))

    (setq ref_point (append (car REF_1st_LWPOLYLINE_ins-set) (list 0) ))
    (setq ref_point1 (car REF_1st_LWPOLYLINE_ins-set))

    
    ; ; (command "copy" REF_1st_LWPOLYLINE "" (vlax-safearray->list (vlax-variant-value (vlax-3d-point ref_point))) ) 
    ; (command "copy" REF_1st_LWPOLYLINE "" (vlax-safearray->list (vlax-variant-value (vlax-3d-point ref_point1)))) 
    
    (setq REF_2nd_LWPOLYLINE nil)
    (while (not REF_2nd_LWPOLYLINE)
      (setq REF_2nd_LWPOLYLINE (entsel "\nSelect a circle "))
      (if 
        (and REF_2nd_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_2nd_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_2nd_LWPOLYLINE (car REF_2nd_LWPOLYLINE))
          (progn
            (setq REF_2nd_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE.")
          )
      )
    )
    
    (setq REF_2nd_LWPOLYLINE_get (entget REF_2nd_LWPOLYLINE))
    (setq REF_2nd_LWPOLYLINE_obj (vlax-ename->vla-object REF_2nd_LWPOLYLINE))
    
    ; (command "pselect" REF_2nd_LWPOLYLINE "")

    ; (setq REF_2nd_LWPOLYLINE_as_ins-set (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_2nd_LWPOLYLINE))) ;varaint test
    ; (setq xx (mapcar 'cdr REF_2nd_LWPOLYLINE_as_ins-set)) ;varaint test
    (setq REF_2nd_LWPOLYLINE_ins-set (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) REF_2nd_LWPOLYLINE_get)))

    (setq insset_i 0)
    (setq insset_ii 0)
    (setq REF_1st_LWPOLYLINE_ins-set_total (length REF_1st_LWPOLYLINE_ins-set))
    (setq REF_2nd_LWPOLYLINE_ins-set_total (length REF_2nd_LWPOLYLINE_ins-set))

    (while
      (and
        (< insset_i REF_1st_LWPOLYLINE_ins-set_total)
        (< insset_ii REF_2nd_LWPOLYLINE_ins-set_total)
      )
      (setq s1stset (nth insset_i REF_1st_LWPOLYLINE_ins-set))
      (setq s2ndset (nth insset_ii REF_2nd_LWPOLYLINE_ins-set))
      
      (command "line" s1stset s2ndset "")
      (command "CHPROP" "L" "" "LT" "DOTX2" "")
      (setq insset_i (+ insset_i 1))
      (setq insset_ii (+ insset_ii 1))
    )
    (vl-remove REF_2nd_LWPOLYLINE_obj)
)
  
    (defun c:mkhi_make_symbol_hiding ()
      
      (setq REF_2nd_LWPOLYLINE nil)
      (while (not REF_2nd_LWPOLYLINE)
        (setq REF_2nd_LWPOLYLINE (entsel "\nSelect a LWPOLYLINE "))
        (if 
          (and REF_2nd_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_2nd_LWPOLYLINE)))) "LWPOLYLINE"))
            (setq REF_2nd_LWPOLYLINE (car REF_2nd_LWPOLYLINE))
            (progn
              (setq REF_2nd_LWPOLYLINE nil)
              (alert "Please select a LWPOLYLINE.")
            )
        )
      )
      (setvar "osmode" 0)
      (setq ins_cen (LM:PolyCentroid REF_2nd_LWPOLYLINE ))
      (setq sc_x_val (cond ( (getreal (strcat "\nSpecify \nScale \nXYZ <" (rtos (setq sc_x_val (cond (sc_x_val) (1.0) ) ) ) "> : " ) ) ) (sc_x_val) ) )
      (setq angle_val (cond ( (getreal (strcat "\nSpecify \nScale \nXYZ <" (rtos (setq angle_val (cond (angle_val) (1.0) ) ) ) "> : " ) ) ) (angle_val) ) )
      (command "insert" "000 - symbol hiding V5" ins_cen sc_x_val angle_val)
      ; (setq main_ins_ (entlast))
      ; (setq main_ins_obj (vlax-ename->vla-object main_ins_))
      (setvar "osmode" 1215)
    )
    (defun c:Z61_symbol_hiding_re_scale ()
      (setq REF_symbol_hiding nil)
        (while (not REF_symbol_hiding)
          (setq REF_symbol_hiding (car (entsel "\nSelect a symbol_hiding ")))
          (if 
            (and REF_symbol_hiding 
                (= (LM:Effectivename (vlax-ename->vla-object REF_symbol_hiding)) 
                    "000 - symbol hiding V5"
                )
            )
              (setq REF_symbol_hiding REF_symbol_hiding)
              (progn
                (setq REF_symbol_hiding nil)
                (alert "Please select blk name : symbol_hiding")
              )
          )
        )
      
      (setq sc_x (vla-get-xscalefactor (vlax-ename->vla-object REF_symbol_hiding)))

      (setq distance2 (LM:getdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "distance2"))
      (setq ระยะบังแบบใน (LM:getdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "ระยะบังแบบใน"))
      (setq ระยะบังแบบนอก (LM:getdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "ระยะบังแบบนอก"))
      (setq filp_val_s1 (LM:getdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "flip state1"))
      (setq filp_val (LM:toggleflipstate (vlax-ename->vla-object REF_symbol_hiding)))
      (setq vis_val (LM:getvisibilitystate (vlax-ename->vla-object REF_symbol_hiding)))

      (setq sc_x_val (cond ( (getdist (strcat "\nSpecify \nScale \nXYZ <" (rtos (setq sc_x_val (cond (sc_x_val) (1.0) ) ) ) "> : " ) ) ) (sc_x_val) ) )
      (vla-put-xscalefactor (vlax-ename->vla-object REF_symbol_hiding) sc_x_val)
      (LM:SetVisibilityState (vlax-ename->vla-object REF_symbol_hiding) vis_val)
      (LM:setdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "flip state1" filp_val_s1)
      (LM:setdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "distance2" distance2)
      (LM:setdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "ระยะบังแบบใน" ระยะบังแบบใน)
      (LM:setdynpropvalue (vlax-ename->vla-object REF_symbol_hiding) "ระยะบังแบบนอก" ระยะบังแบบนอก)
    )
 



