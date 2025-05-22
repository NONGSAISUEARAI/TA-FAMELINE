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
;Sub_Func.
  ;; Polygon Centroid  -  Lee Mac
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
  ;; Polygon Centroid  -  Lee Mac
;
;grid line group
  (defun c:G001()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-001_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G002()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-002_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G005()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-005_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G010()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-010_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G015()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-015_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G020()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-020_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G025()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-025_GG_TA_LINE "")
      (command "pselect" select_group "")    
    ;
  )
  (defun c:G030()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-030_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G035()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-035_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G050()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-050_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G075()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-075_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
  (defun c:G100()
    ; selection part 
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
      (command "CHPROP" select_group "" "LA" LY-000-GRID "")
      (command "CHPROP" select_group "" "C" c-grey08 "")
      (command "CHPROP" select_group "" "LT" li-100_GG_TA_LINE "") 
      (command "pselect" select_group "")
    ;
  )
;
;hidden line group
  (defun c:L00010 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_000-10 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L00025 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_000-25 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L00050 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_000-50 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L001 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_001-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L002 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_002-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L003 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_003-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L005 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_005-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L025 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_025-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L050 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_050-00 "")
    (command "pselect" select_group "")
    ;
  )
  (defun c:L100 () 
    ; selection part
      (setq select_group (ssget 
                                ;  (list
                                ;    (cons 0 "INSERT") ;type of object
                                ;    (cons 8 "000 - GRID") ;kind of layer
                                ;    ; (cons 2 "SSSS")       ;kind of nameblock
                                ;    ; (cons 62 1)           ;kind of color call sign with color code index
                                ;  )
                        )
      )
    ;
    ; command part
    (command "CHPROP" select_group "" "LA" LY-000-HIDDEN "")
    (command "CHPROP" select_group "" "C" c-bylayer "")
    (command "CHPROP" select_group "" "LT" li-TA_L_100-00 "")
    (command "pselect" select_group "")
    ;
  )
;
(defun c:dde1_decruve_to_line ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "arc")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (setq my_cruve_set_start_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                     )
      )
      (setq my_cruve_set_end_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                     )
      )
      
      (command "line" my_cruve_set_start_insxy my_cruve_set_end_insxy "")
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
      (vla-delete my_cruve_set_obj)
      
      
      
    )
)
(defun c:dde2_delete_arc_l110 ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "arc")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (setq my_cruve_set_start_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                     )
      )
      (setq my_cruve_set_end_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                     )
      )
      (setq my_cruve_set_obj_arc_length (vla-get-arclength my_cruve_set_obj))
      
      (if
        (< my_cruve_set_obj_arc_length 110)
        (progn
          (command "erase" my_cruve_set_ename "")
        )
        (princ "\n")
      )
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
    )
)

(defun c:dde33_inssolissheet_arc_l110 ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (setq my_cruve_set_start_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                     )
      )
      (setq my_cruve_set_end_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                     )
      )
      (setq L_value (vla-get-length my_cruve_set_obj ))
      (setq angle_value (angtos (vla-get-angle my_cruve_set_obj )))
      
      
      (command "insert" "A$C86b6a5bd" my_cruve_set_start_insxy 1 0)
      (setq dyn_solid_ (entlast))
      (setq dyn_solid_obj (vlax-ename->vla-object dyn_solid_))
      (LM:setdynpropvalue dyn_solid_obj "distance1" L_value )
      (vla-put-rotation dyn_solid_obj (vla-get-angle my_cruve_set_obj ))
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
    )
)
(defun c:dde31_inssolissheet_arc_l110 ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (setq my_cruve_set_start_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
                                     )
      )
      (setq my_cruve_set_end_insxy (list
                                       (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                       (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
                                     )
      )
      (setq L_value (vla-get-length my_cruve_set_obj ))
      (setq angle_value (angtos (vla-get-angle my_cruve_set_obj )))
      
      
      (command "insert" "PF_HALLWAY" my_cruve_set_start_insxy 1 0)
      (setq dyn_solid_ (entlast))
      (setq dyn_solid_obj (vlax-ename->vla-object dyn_solid_))
      (LM:setdynpropvalue dyn_solid_obj "distance1" L_value )
      (vla-put-rotation dyn_solid_obj (vla-get-angle my_cruve_set_obj ))
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
    )
)
(defun c:dde4_delete_arc_l110 ()
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
                                            (< (cadr a) (cadr b))
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
      (setq my_cruve_set_ ff )
      (setq sorted (sort-entities-by-x my_cruve_set_))
      (setq sorted_total (length sorted))
  (setq my_cruve_set_ (ssget 
                (list 
                  ; (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)
  (setq ele_I 0)
   

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      ; (setq my_cruve_set_start_insxy (list
      ;                                  (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
      ;                                  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
      ;                                )
      ; )
      ; (setq my_cruve_set_end_insxy (list
      ;                                  (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
      ;                                  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
      ;                                )
      ; )
      (setq ele_value (vla-put-elevation my_cruve_set_obj ele_I ))
      
      

      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
      (setq ele_I (+ ele_I 166.6666))
    )
)

  ; find




;CODING STARTS HERE
;This application will perform an Offset on multiple entities.
;Kenny Ramage - April 2000
;afralisp@mweb.com.na
;http://www.afralisp.com

(prompt "\nType MULTOFF to run....")

(defun c:multoff ( / off sel1 side n lng)

(setq off (getdist "\nOffset Distance : ")
sel1 (ssget)
side (getpoint "\nSelect Side to Offset: ")
n 0
lng (sslength sel1))

(repeat lng

(setq ent (ssname sel1 n))
(command "OFFSET" off ent side "")
(setq n (1+ n))

);repeat

(princ)

);defun

(princ)

;CODING ENDS HERE



(defun c:dde55_offset_pline ()
  (setq my_cruve_set_ (ssget 
                (list 
                  ; (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)
  (setq ele_I 0)
  (setq offset_dist_ (getreal ))

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      ; (setq my_cruve_set_start_insxy (list
      ;                                  (car (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
      ;                                  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-startpoint my_cruve_set_obj))))
      ;                                )
      ; )
      ; (setq my_cruve_set_end_insxy (list
      ;                                  (car (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
      ;                                  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-endpoint my_cruve_set_obj))))
      ;                                )
      ; )
      (setq xxx (list 
                  (car (LM:PolyCentroid my_cruve_set_ename))
                  (cadr (LM:PolyCentroid my_cruve_set_ename))
                )
      )
      
      (command "offset" offset_dist_ my_cruve_set_ename xxx "")
      (vla-delete my_cruve_set_obj)
      
      

      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
      (setq ele_I (+ ele_I 166.6666))
    )
)

; (setq sss_ (car (entsel )))
; (setq sss_obj (vlax-ename->vla-object sss_))
; (command "offset" 50 sss_ (LM:PolyCentroid sss_) "")


(defun c:dde7_delete_arc_l110 ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "line")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  (cons 62 250)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (setq my_cruve_set_length (vla-get-length my_cruve_set_obj ))
      (setq Dist1st  (vlax-curve-getDistAtPoint my_cruve_set_obj (vlax-curve-getStartPoint my_cruve_set_obj)))
      (setq Dist2nd  (vlax-curve-getDistAtPoint my_cruve_set_obj (vlax-curve-getEndPoint my_cruve_set_obj)))
      (setq lineLen (- Dist2nd Dist1st))
      (setq PointMid (vlax-curve-getPointAtDist my_cruve_set_obj (+ Dist1st (* 0.5 lineLen))))
      
      
      (setq angle_value (angtos (vla-get-angle my_cruve_set_obj )))
      
      
      (command "insert" "A$Cca6cb894" PointMid 1 0)
      (setq dyn_solid_ (entlast))
      (setq dyn_solid_obj (vlax-ename->vla-object dyn_solid_))
      (LM:setdynpropvalue dyn_solid_obj "distance1" (/ my_cruve_set_length 2))
      (vla-put-rotation dyn_solid_obj (vla-get-angle my_cruve_set_obj ))
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
    )
)




(defun c:MidPL( / PlinesSet thePline Dist1st Dist2nd PlineLen PointMid OldOsmode )
(setq OldOsmode (getvar "OSMODE"))
(setvar "OSMODE" 0)

(prompt "\nSelect polylines to mark middle point:")
(if (and (setq PlinesSet (ssget '((0 . "LWPOLYLINE"))))   ;select polylines
         (> (sslength PlinesSet) 0))
 (while (> (sslength PlinesSet) 0)
  (setq thePline (ssname PlinesSet 0))

  (setq Dist1st  (vlax-curve-getDistAtPoint thePline (vlax-curve-getStartPoint thePline))
        Dist2nd  (vlax-curve-getDistAtPoint thePline (vlax-curve-getEndPoint   thePline))
        PlineLen (- Dist2nd Dist1st)
        PointMid (vlax-curve-getPointAtDist thePline (+ Dist1st (* 1 PlineLen))))

  
  (entmake (list (cons '0  "POINT")                       ;add point entity
                 (cons '10 PointMid)))

  (setq PlinesSet (ssdel thePline PlinesSet))             ;remove processed polyline
 )
)

(setvar "OSMODE" OldOsmode)
(princ)
)

(setq sss_ (car (entsel )))
(setq sss_obj (vlax-ename->vla-object sss_))
; (command "offset" 50 sss_ (LM:PolyCentroid sss_) "")
(vlax-curve-getstartpoint sss_obj)
(setq Dist1st  (vlax-curve-getDistAtPoint sss_obj (vlax-curve-getStartPoint sss_obj)))
(setq Dist2nd  (vlax-curve-getDistAtPoint sss_obj (vlax-curve-getEndPoint sss_obj)))
(setq lineLen (- Dist2nd Dist1st))
(setq PointMid (vlax-curve-getPointAtDist sss_obj (+ Dist1st (* 0.5 lineLen))))
(setq PointMid2 (vlax-curve-getPointAtDist sss_obj (- Dist2nd 600)))
(setq PointMid3 (vlax-curve-getPointAtDist sss_obj (- Dist2nd 1800)))
(setq PointMid4 (vlax-curve-getPointAtDist sss_obj (- Dist2nd 3000)))

(command "insert" "x1" PointMid2 1 0)
(command "insert" "x1" PointMid3 1 0)
(command "insert" "x1" PointMid4 1 0)


(defun c:dde8_delete_circle_l110 ()
  (setq my_cruve_set_ (ssget 
                (list 
                  (cons 0 "circle")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 250)           ;kind of color call sign with color code index
                )
              )
  )

  (setq my_cruve_set_total (sslength my_cruve_set_))
  (setq my_cruve_set_I 0)

    (while
      (< my_cruve_set_I my_cruve_set_total)
      (setq my_cruve_set_ename (ssname my_cruve_set_ my_cruve_set_I))
      (setq my_cruve_set_obj (vlax-ename->vla-object my_cruve_set_ename))
      (vla-delete my_cruve_set_obj)
      
      
    
      
      (setq my_cruve_set_I (+ my_cruve_set_I 1))
    )
)