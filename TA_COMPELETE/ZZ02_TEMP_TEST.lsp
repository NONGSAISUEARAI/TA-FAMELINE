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

(defun c:REECC1_RENUMBER_TEMP1 ()
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
              (setq LL (LM:vl-getattributevalue o "LINE"))
              
              (LM:setdynpropvalue o "distance1" LL)
              (LM:setdynpropvalue o "distance2" LL)
              (LM:setdynpropvalue o "distance3" LL)
              (LM:setdynpropvalue o "distance4" LL)
              (LM:setdynpropvalue o "distance5" LL)
              (LM:setdynpropvalue o "distance6" LL)
              (LM:setdynpropvalue o "distance7" LL)
              (LM:setdynpropvalue o "distance8" LL)
              (LM:setdynpropvalue o "distance9" LL)
              (LM:setdynpropvalue o "distance10" LL)
              (LM:setdynpropvalue o "distance11" LL)
              (LM:setdynpropvalue o "distance12" LL)
              (LM:setdynpropvalue o "distance13" LL)
              (LM:setdynpropvalue o "distance14" LL)
              
        


              (setq i (+ i 1))
              (setq ii (+ ii 5))

          )
    
  ; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
(defun c:REECC2_RENUMBER_TEMP1 ()
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
              (LM:vl-setattributevalue o "BUBBLE_NAME" NAME_VIEWPORT_NEW)
              (setq LL (LM:vl-getattributevalue o "BUBBLE_NAME"))
 
              (setq i (+ i 1))
              (setq ii (+ ii 2.5))

          )
    
  ; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
(defun c:REECC3_RENUMBER_TEMP1 ()
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
  (setq iii 62.5) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              ; (setq NAME_VIEWPORT_NEW ii) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              ; (LM:vl-setattributevalue o "LINE" NAME_VIEWPORT_NEW)
              (setq LL (LM:vl-getattributevalue o "LINE"))
              
              
              (LM:setdynpropvalue o "distance1" iii)
              (LM:setdynpropvalue o "distance2" LL)
              (LM:setdynpropvalue o "distance3" LL)
              (LM:setdynpropvalue o "distance4" LL)
              (LM:setdynpropvalue o "distance5" LL)
              (LM:setdynpropvalue o "distance6" LL)
              (LM:setdynpropvalue o "distance7" LL)
              (LM:setdynpropvalue o "distance8" LL)
              (LM:setdynpropvalue o "distance9" LL)
              (LM:setdynpropvalue o "distance10" LL)
              (LM:setdynpropvalue o "distance11" LL)
              (LM:setdynpropvalue o "distance12" LL)
              (LM:setdynpropvalue o "distance13" LL)
              (LM:setdynpropvalue o "distance14" LL)
              
        


              (setq i (+ i 1))
              (setq ii (+ ii 5))
              (setq iii (+ iii 7.5))

          )
    
  ; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
(defun c:REECC4_RENUMBER_TEMP1 ()
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
  (setq iii 60) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              ; (setq NAME_VIEWPORT_NEW ii) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              ; (LM:vl-setattributevalue o "LINE" NAME_VIEWPORT_NEW)
              (setq LL (LM:vl-getattributevalue o "LINE"))
              
              
              (LM:setdynpropvalue o "distance1" LL)
              (LM:setdynpropvalue o "distance2" LL)
              (LM:setdynpropvalue o "distance3" LL)
              (LM:setdynpropvalue o "distance4" LL)
              (LM:setdynpropvalue o "distance5" LL)
              (LM:setdynpropvalue o "distance6" LL)
              (LM:setdynpropvalue o "distance7" LL)
              (LM:setdynpropvalue o "distance8" LL)
              (LM:setdynpropvalue o "distance9" LL)
              (LM:setdynpropvalue o "distance10" LL)
              (LM:setdynpropvalue o "distance11" LL)
              (LM:setdynpropvalue o "distance12" LL)
              (LM:setdynpropvalue o "distance13" LL)
              (LM:setdynpropvalue o "distance14" LL)
              
        


              (setq i (+ i 1))
              (setq ii (+ ii 5))
              (setq iii (+ ii 7.5))

          )
    
  ; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)
(defun c:REECC5_RENUMBER_TEMP1 ()
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
  (setq iii 51.25) ; เริ่มต้น ii ที่ 101

          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
            
              ; (setq NAME_VIEWPORT_NEW ii) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              ; (LM:vl-setattributevalue o "LINE" NAME_VIEWPORT_NEW)
              ; (setq LL (LM:vl-getattributevalue o "LINE"))
              
              
              (LM:setdynpropvalue o "distance1" iii)
              ; (LM:setdynpropvalue o "distance2" LL)
              ; (LM:setdynpropvalue o "distance3" LL)
              ; (LM:setdynpropvalue o "distance4" LL)
              ; (LM:setdynpropvalue o "distance5" LL)
              ; (LM:setdynpropvalue o "distance6" LL)
              ; (LM:setdynpropvalue o "distance7" LL)
              ; (LM:setdynpropvalue o "distance8" LL)
              ; (LM:setdynpropvalue o "distance9" LL)
              ; (LM:setdynpropvalue o "distance10" LL)
              ; (LM:setdynpropvalue o "distance11" LL)
              ; (LM:setdynpropvalue o "distance12" LL)
              ; (LM:setdynpropvalue o "distance13" LL)
              ; (LM:setdynpropvalue o "distance14" LL)
              
        


              (setq i (+ i 1))
              ; (setq ii (+ ii 5))
              (setq iii (+ iii 2.5))

          )
    
  ; ผลลัพธ์คือ sorted-enames ที่เป็นรายการของ ename ที่ถูกจัดเรียงตามแกน x จากน้อยไปมาก

)




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


; (setq my-selection-set (ssget  '((0 . "line"))))
; (setq ent_total_line (sslength my-selection-set))

; (setq i 0)

; (while 
;   (< i ent_total_line)
;   (setq ent_line (ssname my-selection-set i))
;   (setq ent_line_get (entget ent_line))

;   (setq ent_line_obj (vlax-ename->vla-object ent_line))
;   (setq ent_line_st_x (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint ent_line_obj)))))
;   (setq ent_line_st_y (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint ent_line_obj)))))
;   (setq ent_line_length (vla-get-length ent_line_obj))
;   (setq ent_line_mid_pt_x (- ent_line_st_x (/ (vla-get-length ent_line_obj) 2)))

;   (setq ent_line_mid_pt_xy (strcat (rtos ent_line_mid_pt_x 2 8) "," (rtos ent_line_st_y 2 8) ))
;   (command "circle" ent_line_mid_pt_xy 10 "")
;   (setq i (+ i 1))

; )

