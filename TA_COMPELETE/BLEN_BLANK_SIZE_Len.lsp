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
(defun c:BLEN_BLANK_SIZE_Len (/ i ss ent j str gghh)
  (defun LM:getdynpropallowedvalues ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
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
  (defun LM:getdynpropvalue (blk prp) 
    (setq prp (strcase prp))
    (vl-some 
      '(lambda (x) 
        (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
      )
      (vlax-invoke blk 'getdynamicblockproperties)
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
  (defun c:SS20-1_SET_LA_CO_FOR_BLANKSIZE()
    (setq newLayer "A11_ALU.SOLID SHEET") ; ????????? Layer ?????????????????
    (setq newColor 1) ; ???????? Color ?????????????????
    (setq newlinetype "TA_LL0.50") ; ???????? Color ?????????????????

    (command "CLAYER" newLayer)
    (command "COLOR" newColor)
    (command "LINETYPE" "s" newlinetype "") 
  )
  (defun c:SS21_SET_LA_CO_FOR_GRID()
    (setq newLayer "000 - GRID") ; ????????? Layer ?????????????????
    (setq newColor "bylayer") ; ???????? Color ?????????????????
    (setq newlinetype "005_GG_TA_LINE") ; ???????? Color ?????????????????

    (command "CLAYER" newLayer)
    (command "COLOR" newColor)
    (command "LINETYPE" "s"  newlinetype "")
  )
  (defun c:SS25_MAKE_BO_DYNAMIC ()
    (setq newColor1 3)
    (setq newlinetype1 "TA_L2.LIN")
    (command "CHPROP" "L" "" "C" newColor1 "")
    (command "CHPROP" "L" "" "LT" newlinetype1 "")
  )
  (defun c:SS26_MAKE_BO_DYNAMIC ()
    (setq newColor1 3)
    (setq newlinetype1 "bylayer")
    (command "CHPROP" "L" "" "C" newColor1 "")
    (command "CHPROP" "L" "" "LT" newlinetype1 "")
  )
  
  (vl-load-com)
  (setq gghh '()) ; สร้างตัวแปรที่มีชื่อว่า gghh และกำหนดให้เป็นรายการว่าง

  (cond 
    (
     (and (setq i -1 ss (ssget '((0 . "LWPOLYLINE"))))t)
        (setvar "osmode" 0)
        (while (setq ent (ssname ss (setq i (1+ i))))
          (setq polyline (vlax-ename->vla-object ent))
          (setq length_total (vla-get-length polyline))
          
          (setq j (1- (vlax-curve-getStartParam ent)) str "0")

          (while (<= (setq j (1+ j)) (vlax-curve-getEndParam ent))
            (setq str
              (strcat str
                "_"
                (rtos (- (vlax-curve-getDistatParam ent j)
                           (if (zerop j) 0
                             (vlax-curve-getDistatParam ent (1- j)))) 2 2)
                
                ; (chr 9) มีไว้ทำไมไม่รู้
              )
            )
          )

          ; (setq gghh (cons (strcat str (rtos (vlax-curve-getDistatParam ent
          ;                           (vlax-curve-getEndParam ent)) 2 2) ) gghh))
        )
    )

    ; แสดงความยาวทั้งหมดในตัวแปร gghh
    (princ (reverse gghh))
    (setq ssf str)

    (princ)
  )
  
  (defun LM:str->lst (str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
  )
  
  (setq ssf str)
    (setq delimiter "_")
    (setq ss-list (LM:str->lst ssf delimiter))
  (setq length_total_all length_total)
  (setq H (getreal "hieght"))
  (setq OFFSET 8)

  ; PART สร้าง
    (setq total_ss-list (vl-list-length ss-list))
      (setq faa_x (atof (nth 2 ss-list)))
      (setq faa_x3 (atof (nth 3 ss-list)))
      (setq y 0)  
      (setq fv1 (strcat (rtos faa_x 2 8) "," (rtos y 2 8) "," (rtos y 2 8)))
      (setq no2 "2.NON WIPEOUT")
  
      (c:SS20-1_SET_LA_CO_FOR_BLANKSIZE) 
  
      (command "insert" "001 - DYNAMIC BLANK SIZE REC&SSQ SHAPE SOLID" "0,0" 1 0)
      (setq BLANK_ALL (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_ALL) "W" length_total_all)
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_ALL) "H" H)
      (LM:SetVisibilityState (vlax-ename->vla-object BLANK_ALL) no2)
      (c:SS26_MAKE_BO_DYNAMIC)
      
  
      (command "insert" "001 - DYNAMIC BLANK SIZE REC&SSQ SHAPE SOLID" "0,0" 1 0)
      (setq BLANK_1 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1) "W" faa_x)
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1) "H" H)
      
      (command "copy" BLANK_1 "" "Displacement" fv1 "")
      (setq BLANK_1-1 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1-1) "W" faa_x3)
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_1-1) "H" H)
    (setq pl 3)
    (setq p 4)
    (setq pp 5)
  
    (while 
      ; (< p (- total_ss-list 1))
      (< p total_ss-list)
        (setq faa_p_x_p2 (atof (nth pl ss-list)))
        (setq faa_p_x (atof (nth p ss-list)))
        ; (setq faa_p_x_pp (atof (nth pp ss-list)))
        (setq faa_p_xy (strcat (rtos faa_p_x 2 8) "," (rtos 0 2 8)))
      
      
      (setq fv (strcat (rtos faa_p_x_p2 2 8) "," (rtos y 2 8) "," (rtos y 2 8)))
      (command "copy" "L" "" "Displacement" fv "")
      (setq BLANK_2 (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object BLANK_2) "W" faa_p_x)

      (setq pl (+ pl 1))
      (setq p (+ p 1))
      (setq pp (+ pp 1))
    )
    (setq bk 2)
    (c:SS21_SET_LA_CO_FOR_GRID)
      (setq bb_blank_top_size_zero (strcat (rtos y 2 8) "," (rtos y 2 8)))
      (command "insert" "000-GRID_LINE_DYN" bb_blank_top_size_zero 1 90)
      (setq bb_hon_blank_size (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object bb_hon_blank_size) "H" length_total_all)
      (LM:setdynpropvalue (vlax-ename->vla-object bb_hon_blank_size) "OFFSET" OFFSET)
      (command "draworder" bb_hon_blank_size "" "b")

      (setq move_to_bot (strcat (rtos y 2 8) "," "-" (rtos h 2 8) "," (rtos y 2 8)))
      (command "copy" "L" "" "Displacement" move_to_bot "")
      (command "draworder" "L" "" "b")
  
      (command "insert" "000-GRID_LINE_DYN" bb_blank_top_size_zero 1 0)
      (setq bb_ver_blank_size (entlast))
      (LM:setdynpropvalue (vlax-ename->vla-object bb_ver_blank_size) "H" H)
      (LM:setdynpropvalue (vlax-ename->vla-object bb_ver_blank_size) "OFFSET" OFFSET)
      (command "draworder" bb_ver_blank_size "" "b")
  
    (while 
      (< bk total_ss-list)
      (setq blank_xy (atof (nth bk ss-list)))
      (setq blank_xy_set (strcat (rtos blank_xy 2 8) "," (rtos 0 2 8)))
      
      (command "copy" "L" "" "Displacement" blank_xy_set "")
      (command "draworder" "L" "" "b")
      (setq bk (+ bk 1))
    )
  
  (command "draworder" BLANK_ALL "" "F")
  

  
  
  (setvar "osmode" 1215)
)
  (defun TA:Get_Pline_vertext_ins_point_ (ename_) ;must be (car (entsel))
    (setq Get_Pline_ (entget ename_))
    (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
    (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
    ; (setq ename+Get_Pline_vtx_pt_ (list
    ;                                 ; ename_
    ;                                 Get_Pline_vtx_pt_
    ;                               )
    
    ; )
  )
(defun TA:get_vertex_len_ (ename_)
  ; (setq ref_line (car (entsel)))
  (setq vertex_total (length (TA:Get_Pline_vertext_ins_point_ ename_)))
  (setq sum_ ())

  (setq vertex_i 0)
  (setq vertex_ii 1)

  (while (< vertex_i vertex_total)
    ;get_data_len
    (setq len_i (vlax-curve-getDistatParam ss vertex_i) )
    (setq len_ii (vlax-curve-getDistatParam ss vertex_ii))
    ;
    ;sum
      (setq sum (list (- len_ii len_i)))
      (setq sum_ (append sum sum_ ))
    ;
    (setq vertex_i (+ vertex_i 1))
    (setq vertex_ii (+ vertex_ii 1))
  )
  ;summary_reverse
    (setq sum_len (reverse sum_))
  ;
)
(setq sg (car (entsel)))
(TA:get_vertex_len_ sg)