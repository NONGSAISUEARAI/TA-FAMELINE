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
;sub_function_for_coding
  (defun c:ucs_world()
    (command "ucs" "world")
  )
  (defun c:ucs_previ ()
    (command "ucs" "p")
  )
  (defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )
  (defun LM:round_add_1 ( n )
    (fix (+ n (if (minusp n) -1 1)))
  )
;
;Basic Function and Setting Layer
  (defun c:ucs_world()
    (command "ucs" "world")
  )
  (defun c:ucs_previ()
    (command "ucs" "p")
  )
  (defun c:S000_reset_layer()
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints") 
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-main-color "byblock") 
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 
      (setq li-TA_L_000.10.LIN "TA_L_000.10.LIN") 
      (setq li-TA_L_000.25.LIN "TA_L_000.25.LIN") 
      (setq li-TA_L_000.50.LIN "TA_L_000.50.LIN") 
      (setq li-TA_L_001.00.LIN "TA_L_001.00.LIN") 
      (setq li-TA_L_002.00.LIN "TA_L_002.00.LIN") 
      (setq li-TA_L_003.00.LIN "TA_L_003.00.LIN ") 
      (setq li-TA_L_005.00.LIN "TA_L_005.00.LIN") 
      (setq li-TA_L_025.00.LIN "TA_L_025.00.LIN") 
      (setq li-TA_L_050.00.LIN "TA_L_050.00.LIN") 
      (setq li-TA_L_100.00.LIN "TA_L_100.00.LIN") 
    ; command part
      (command "CLAYER" 0)
      (command "COLOR" c-bylayer)
      (command "LINETYPE" "s" li-bylayer "") 
  )
  (defun c:S100_set_gridline()
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints") 
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-main-color "byblock") 
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 
      (setq li-TA_L_000.10.LIN "TA_L_000.10.LIN") 
      (setq li-TA_L_000.25.LIN "TA_L_000.25.LIN") 
      (setq li-TA_L_000.50.LIN "TA_L_000.50.LIN") 
      (setq li-TA_L_001.00.LIN "TA_L_001.00.LIN") 
      (setq li-TA_L_002.00.LIN "TA_L_002.00.LIN") 
      (setq li-TA_L_003.00.LIN "TA_L_003.00.LIN ") 
      (setq li-TA_L_005.00.LIN "TA_L_005.00.LIN") 
      (setq li-TA_L_025.00.LIN "TA_L_025.00.LIN") 
      (setq li-TA_L_050.00.LIN "TA_L_050.00.LIN") 
      (setq li-TA_L_100.00.LIN "TA_L_100.00.LIN") 
    ; command part
      (command "CLAYER" LY-000-GRID)
      (command "COLOR" c-grey08)
      (command "LINETYPE" "s" li-002_GG_TA_LINE "") 
  )
  (defun c:S200_set_AL_SHEET()
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints") 
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-main-color "byblock") 
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 
      (setq li-TA_L_000.10.LIN "TA_L_000.10.LIN") 
      (setq li-TA_L_000.25.LIN "TA_L_000.25.LIN") 
      (setq li-TA_L_000.50.LIN "TA_L_000.50.LIN") 
      (setq li-TA_L_001.00.LIN "TA_L_001.00.LIN") 
      (setq li-TA_L_002.00.LIN "TA_L_002.00.LIN") 
      (setq li-TA_L_003.00.LIN "TA_L_003.00.LIN ") 
      (setq li-TA_L_005.00.LIN "TA_L_005.00.LIN") 
      (setq li-TA_L_025.00.LIN "TA_L_025.00.LIN") 
      (setq li-TA_L_050.00.LIN "TA_L_050.00.LIN") 
      (setq li-TA_L_100.00.LIN "TA_L_100.00.LIN") 
    ; command part
      (command "CLAYER" LY-A11_ALU.SOLIDSHEET)
      (command "COLOR" c-green)
      (command "LINETYPE" "s" li-bylayer "") 
  )
  (defun c:S300_set_AL_PF()
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints") 
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-main-color "byblock") 
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 
      (setq li-TA_L_000.10.LIN "TA_L_000.10.LIN") 
      (setq li-TA_L_000.25.LIN "TA_L_000.25.LIN") 
      (setq li-TA_L_000.50.LIN "TA_L_000.50.LIN") 
      (setq li-TA_L_001.00.LIN "TA_L_001.00.LIN") 
      (setq li-TA_L_002.00.LIN "TA_L_002.00.LIN") 
      (setq li-TA_L_003.00.LIN "TA_L_003.00.LIN ") 
      (setq li-TA_L_005.00.LIN "TA_L_005.00.LIN") 
      (setq li-TA_L_025.00.LIN "TA_L_025.00.LIN") 
      (setq li-TA_L_050.00.LIN "TA_L_050.00.LIN") 
      (setq li-TA_L_100.00.LIN "TA_L_100.00.LIN") 
    ; command part
      (command "CLAYER" LY-A12_PERFORATED)
      (command "COLOR" c-red)
      (command "LINETYPE" "s" li-bylayer "") 
  )
  (defun c:CH300_set_gridline()
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DIM "000 - D I M") 
      (setq LY-000-DIM0.25 "000 - D I M 0.25") 
      (setq LY-000-GRID "000 - GRID") 
      (setq LY-000-GROUND "000 - GROUND") 
      (setq LY-000-HATCH "000 - H A T C H") 
      (setq LY-000-HIDDEN "000 - H I D D E N") 
      (setq LY-000-LINE "000 - L I N E") 
      (setq LY-000-LINE0.1 "000 - L I N E 0.1") 
      (setq LY-000-SYMBOLCON "000 - SYMBOL CON") 
      (setq LY-000-TEXT "000 - T E X T") 
      (setq LY-000-TEMPLAYER "000 - TEMP LAYER") 
      (setq LY-000-TITLEBLOCK "000 - TITLEBLOCK") 
      (setq LY-000-WIPEOUT "000 - WIPEOUT") 
      (setq LY-001-ASCESSORIES "001 - ASCESSORIES") 
      (setq LY-001-DOOR "001 - DOOR") 
      (setq LY-001-WINDOWS "001 - WINDOWS") 
      (setq LY-002-STEELLRIP "002 - STEEL LRIP") 
      (setq LY-002-STEELTUBE "002 - STEEL TUBE") 
      (setq LY-002-STRUCTURAL "002 - STRUCTURAL") 
      (setq LY-004-CONCRETE "004 - CONCRETE") 
      (setq LY-004-DOORWINDOW "004 - DOOR WINDOW") 
      (setq LY-004-FLOOR "004 - FLOOR") 
      (setq LY-004-VIVABOARD "004 - VIVA BOARD") 
      (setq LY-004-WALL "004 - WALL") 
      (setq LY-005-WATERCLOSE "005 - WATER CLOSE") 
      (setq LY-006-ROOF "006 - ROOF") 
      (setq LY-008-LIGHTING "008 - LIGHTING") 
      (setq LY-A01_ACP "A01_ACP") 
      (setq LY-A02_LITEWOOD "A02_LITEWOOD") 
      (setq LY-A03_SUNLOUVER "A03_SUN LOUVER") 
      (setq LY-A04_TUBESERIES "A04_TUBE SERIES") 
      (setq LY-A05_AHP. "A05_AHP.") 
      (setq LY-A06_PERFORMANCEL "A06_PERFORMANCE LOUVER") 
      (setq LY-A07_PRANKCAD "A07_PRANKCAD") 
      (setq LY-A08_AEROLITE "A08_AEROLITE") 
      (setq LY-A09_BIFOLDING "A09_BIFOLDING") 
      (setq LY-A10_ENTRANCEMATT "A10_ENTRANCE MATT") 
      (setq LY-A11_ALU.SOLIDSHEET "A11_ALU.SOLID SHEET") 
      (setq LY-A12_PERFORATED "A12_PERFORATED") 
      (setq LY-A13_EXPANDED "A13_EXPANDED") 
      (setq LY-Defpoints "Defpoints") 
    ; color part
      (setq c-red 1) 
      (setq c-yellow 2) 
      (setq c-green 3) 
      (setq c-cyan 4) 
      (setq c-blue 5) 
      (setq c-magenta 6) 
      (setq c-grey08 8) 
      (setq c-black 250) 
      (setq c-bylayer "bylayer") 
      (setq c-main-color "byblock") 
    ; line part
      (setq li-bylayer "bylayer") 
      (setq li-byblock "byblock") 
      (setq li-001_GG_TA_LINE "TA_G_001.00_GG_TA_L_INE") 
      (setq li-002_GG_TA_LINE "TA_G_002.00_GG_TA_L_INE") 
      (setq li-005_GG_TA_LINE "TA_G_005.00_GG_TA_L_INE") 
      (setq li-010_GG_TA_LINE "TA_G_010.00_GG_TA_L_INE") 
      (setq li-015_GG_TA_LINE "TA_G_015.00_GG_TA_L_INE") 
      (setq li-020_GG_TA_LINE "TA_G_020.00_GG_TA_L_INE") 
      (setq li-025_GG_TA_LINE "TA_G_025.00_GG_TA_L_INE.LIN") 
      (setq li-030_GG_TA_LINE "TA_G_030.00_GG_TA_L_INE.LIN") 
      (setq li-035_GG_TA_LINE "TA_G_035.00_GG_TA_L_INE.LIN") 
      (setq li-050_GG_TA_LINE "TA_G_050.00_GG_TA_L_INE.LIN") 
      (setq li-075_GG_TA_LINE "TA_G_075.00_GG_TA_L_INE.LIN") 
      (setq li-100_GG_TA_LINE "TA_G_100.00_GG_TA_L_INE.LIN") 
      (setq li-TA_L_000.10.LIN "TA_L_000.10.LIN") 
      (setq li-TA_L_000.25.LIN "TA_L_000.25.LIN") 
      (setq li-TA_L_000.50.LIN "TA_L_000.50.LIN") 
      (setq li-TA_L_001.00.LIN "TA_L_001.00.LIN") 
      (setq li-TA_L_002.00.LIN "TA_L_002.00.LIN") 
      (setq li-TA_L_003.00.LIN "TA_L_003.00.LIN ") 
      (setq li-TA_L_005.00.LIN "TA_L_005.00.LIN") 
      (setq li-TA_L_025.00.LIN "TA_L_025.00.LIN") 
      (setq li-TA_L_050.00.LIN "TA_L_050.00.LIN") 
      (setq li-TA_L_100.00.LIN "TA_L_100.00.LIN") 
    ; command part
      (command "CHPROP" "L" "" "LA" LY-000-GRID "")
      (command "CHPROP" "L" "" "C" c-grey08 "")
      (command "CHPROP" "L" "" "LT" li-005_GG_TA_LINE "")
  )
  





;
;change all text_
  (defun c:styfn (/ Oldtstyle Sttxt Userfont error)
    (defun error (s)
      (setvar "textstyle" oldtstyle)
    )
    (setq oldtstyle (getvar "textstyle"))
    (setq userfont "cordiaupc") ;"arial.ttf" <<<<change this for your textfont
    (setvar "textstyle" (cdr (assoc 2 (tblnext "style" t))))
    (command "._Style" "" userfont 2 1 0 "N" "N")
    (while
      (setq sttxt (cdr (assoc 2 (tblnext "style"))))
      (setvar "textstyle" sttxt)
      (command "._Style" "" userfont 2 1 0 "N" "N")
    )
    (setvar "textstyle" oldtstyle)
    (princ)
  )
;
  (defun c:cac ()
    (vl-load-com)
    ;user_input_to_specify_color
      (setq c-main-color nil) 
      (while (not c-main-color)
        (setq c-main-color (getint "Enter a number \nbetween 1 and 256 : \n256 = bylayer \n0 = byblock")) 
        (if (or (< c-main-color 0) (> c-main-color 256) ) 
            (progn
              (princ "\nPlease enter a number between 0 and 255.") 
              (setq c-main-color nil) 
            )
        )
      )
      (cond ;condition_for_color_to_object
        ((and 
           (= c-main-color 0)
         ) 
          (progn 
            (setq c-main-color "byblock")
            (setq dim-main-color 0)
          )
        )
        ((and 
           (= c-main-color 256)
         ) 
          (progn 
            (setq c-main-color "bylayer")
            (setq dim-main-color 256)
          )
        )
      )
    ;
    
      (setq block_list ())
      (setq multi_blocks (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
      (vlax-for block multi_blocks
      (setq block_list (append block_list (list (vla-get-name block))))
      )
      (setq new_sort_block_list (acad_strlsort block_list))
      
      (setq total_block_list (length block_list))
      (setq i_block 0)
      (setq block_list_x '())
      ; (setq c-main-color (getreal))
      (while
        (< i_block total_block_list)
        (setq block_list_nth (nth i_block new_sort_block_list))
        
        (if (/= (substr block_list_nth 1 1) "*") ; ตรวจสอบว่าชื่อเริ่มต้นด้วย *
          (progn
            (setq block_list_x (cons block_list_nth block_list_x))
          )
        )
        (setq i_block (+ i_block 1))
      )
    
      
      (setq block_list_x_total (length block_list_x))
      (princ "\n         |=====================|")
      (princ "\n         | TOTAL BLOCK IN FILE |")
      (princ (strcat "\n         |     = " (itoa block_list_x_total) " block     |"))
      (princ "\n         |          set        |")
      (princ "\n         |---------------------|")
      
      (setq ii_block_list_x 0)
      ; in-block-loop
      
        (while
          (< ii_block_list_x block_list_x_total)
          (setq block_list_x_efname (nth ii_block_list_x block_list_x))
          (command "-bedit" block_list_x_efname)
          
          (setq all_obj_in_block (ssget "x" 
                                            (list 
                                              ; (cons 0 "insert")       ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "SSSS")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                    )
          )
          (command "chprop" all_obj_in_block "" "c" c-main-color "")
            
          (command "_attsync" "n" block_list_x_efname)
          (setq ii_block_list_x (+ ii_block_list_x 1))
        )
      ;
      ; all_obj_out-block-loop
        ; all_obj_mode
          (setq all_obj_out_block (ssget "x" 
                                        (list 
                                          ; (cons 0 "insert")       ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "SSSS")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                )
          )
          (command "chprop" all_obj_out_block "" "c" c-main-color "")
        ;
        
        ;dim_obj_mode
          ; (setq ss (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'objectname)) ;logic for testing
          ; (setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel))) 'objectname))  ;logic for testing
          (setq all_dim_out_block (ssget "x" 
                                          (list 
                                            (cons 0 "dimension")       ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "SSSS")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
          )
          ; (command "pselect" all_dim_out_block "")
          (setq all_dim_out_block_total (sslength all_dim_out_block))
          
          (setq all_dim_out_block_iii 0)
          (while
            (< all_dim_out_block_iii all_dim_out_block_total)
            (setq all_dim_out_block_ename (ssname all_dim_out_block all_dim_out_block_iii))
            (setq all_dim_out_block_obj (vlax-ename->vla-object all_dim_out_block_ename))
            ; (vlax-dump-object all_dim_out_block_obj)
              (cond 
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbAlignedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                  (princ "\n")
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDb2LineAngularDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbRotatedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbArcDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbDiametricDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
              )
            ; (vla-put-textcolor all_dim_out_block_obj dim-main-color)
            ; (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
            ; ; (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
            (setq all_dim_out_block_iii (+ all_dim_out_block_iii 1))
          )
        ;
        ;leader_obj_mode
          (setq all_leader_out_block (ssget "x" 
                                        (list 
                                          (cons 0 "leader")       ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "SSSS")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                )
          )
          (setq all_leader_out_block_total (sslength all_leader_out_block))
          (setq all_leader_out_block_iii 0)
          
          (while
            (< all_leader_out_block_iii all_leader_out_block_total)
            (setq all_leader_out_block_ename (ssname all_leader_out_block all_leader_out_block_iii))
            (setq all_leader_out_block_obj (vlax-ename->vla-object all_leader_out_block_ename))
            (vla-put-dimensionlinecolor all_leader_out_block_obj dim-main-color)
            (setq all_leader_out_block_iii (+ all_leader_out_block_iii 1))
          ) 
        ;
      ;
      
      

  )







    (defun TA:get_name_block_in_drawing_to_list ()
      ;get_obj_file_
        (setq multi_blocks_obj (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
        (setq block_list_ ())
      ;
      ;method_for_get_name_block_in_drawing
        ;method_1_get_name_
          (vlax-for block_ multi_blocks_obj 
            (setq block_list_ (append block_list_ (list (vla-get-name block_))))
          )
        ;
        ; ;method_2_get_name_
        ;   (setq block_i 0)
        ;   (while (< block_i (vla-get-count multi_blocks_obj))
        ;     (setq block_ (vla-item multi_blocks_obj block_i))
        ;     (setq block_list_ (append block_list_ (list (vla-get-name block_))))
        ;     (setq block_i (1+ block_i))
        ;   )
        ; ;
      ;
      ;sorting_name_block
        (setq sorted_block_list_ (acad_strlsort block_list_))
      ;
      ;preloop_and_while filtter_
        (setq sorted_block_list_i 0)
        (setq filter_sorted_block_ ())

        (while (< sorted_block_list_i (length sorted_block_list_))
          (setq filter_sorted_block_effectivename_ (nth sorted_block_list_i sorted_block_list_))
          
          (if (/= (substr filter_sorted_block_effectivename_ 1 1) "*") ; ตรวจสอบว่าชื่อเริ่มต้นด้วย *
            (progn
              (setq filter_sorted_block_ (cons filter_sorted_block_effectivename_ filter_sorted_block_))
            )
          )
          (setq sorted_block_list_i (+ sorted_block_list_i 1))
        )
      ;
      ;summary_total_block
        (setq filter_sorted_block_total (length filter_sorted_block_))
        (princ "\n                  |=====================|")
        (princ "\n                  | TOTAL BLOCK IN FILE |")
        (princ (strcat "\n                  |     = " (itoa filter_sorted_block_total) " block         |"))
        (princ "\n                  |          set        |")
        (princ "\n                  |---------------------|")
        (setq filter_sorted_block_ (acad_strlsort filter_sorted_block_))
      ;
    )

(defun c:RESET_COLOR_ ()
  (vl-load-com)
  (setq file_block_list_ (TA:get_name_block_in_drawing_to_list))
  ;user_input_to_specify_color
    (setq c-main-color nil) 
    (while (not c-main-color)
      (setq c-main-color (getint "Enter a number \nbetween 1 and 256 : \n256 = bylayer \n0 = byblock")) 
      (if (or (< c-main-color 0) (> c-main-color 256) ) 
          (progn
            (princ "\nPlease enter a number between 0 and 255.") 
            (setq c-main-color nil) 
          )
      )
    )
    (cond ;condition_for_color_to_object
      ((and 
          (= c-main-color 0)
        ) 
        (progn 
          (setq c-main-color "byblock")
          (setq dim-main-color 0)
        )
      )
      ((and 
          (= c-main-color 256)
        ) 
        (progn 
          (setq c-main-color "bylayer")
          (setq dim-main-color 256)
        )
      )
      ((and
          (/= c-main-color 0)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
      ((and
          (/= c-main-color 256)
        ) 
        (progn 
          (setq dim-main-color c-main-color)
        )
      )
    )
  ;
  ;preloop_and_while for_block_editor
    (setq file_block_list_i 0)
    (while  (< file_block_list_i (length file_block_list_))
      (setq file_block_list_blkname_ (nth file_block_list_i file_block_list_))
      (command "-bedit" file_block_list_blkname_)
      (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
      (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
      (command "_bclose" "s")
      (command "_attsync" "n" file_block_list_blkname_)
      (setq file_block_list_i (+ file_block_list_i 1))
      
    )
  ;
  ;sub_func_for_model_page
    (TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ c-main-color)
    (TA:RE_COLOR_ALL_DIM+LEADER_OBJ dim-main-color)
  ;
)

  (defun TA:RE_COLOR_ALL_NORMAL_OBJECT_CHANGE_ (c-main-color)
    (setq all_obj_in_block (ssget "x" 
                                  (list 
                                    ; (cons 0 "insert")       ;type of object
                                    ; (cons 8 "000 - GRID")   ;kind of layer
                                    ; (cons 2 "SSSS")       ;kind of nameblock
                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                  )
                           )
    )
    (command "chprop" all_obj_in_block "" "c" c-main-color "")
  )
  (defun TA:RE_COLOR_ALL_DIM+LEADER_OBJ (dim-main-color)
    ;dim_obj_mode
      ; (setq ss (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'objectname)) ;logic for testing
      ; (setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel))) 'objectname))  ;logic for testing
      ;user_input_ selection_dim_object
        (setq all_dim_out_block (ssget "x" 
                                        (list 
                                          (cons 0 "dimension")       ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "SSSS")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                )
        )
      ;
      ; (command "pselect" all_dim_out_block "")
      (if (/= all_dim_out_block nil)
        (progn
          (setq all_dim_out_block_total (sslength all_dim_out_block))
          (setq all_dim_out_block_iii 0)
          (while
            (< all_dim_out_block_iii all_dim_out_block_total)
            (setq all_dim_out_block_ename (ssname all_dim_out_block all_dim_out_block_iii))
            (setq all_dim_out_block_obj (vlax-ename->vla-object all_dim_out_block_ename))
            ; (vlax-dump-object all_dim_out_block_obj)
              (cond 
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbAlignedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                  (princ "\n")
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDb2LineAngularDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbRotatedDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbArcDimension"
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                    (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbDiametricDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbOrdinateDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-ExtensionLineColor  all_dim_out_block_obj dim-main-color)
                  )
                )
                ((and 
                    (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                        "AcDbRadialDimension" 
                    )
                  )
                  (progn 
                    (vla-put-textcolor all_dim_out_block_obj dim-main-color)
                    (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
                  )
                )
              )
            ; (vla-put-textcolor all_dim_out_block_obj dim-main-color)
            ; (vla-put-dimensionlinecolor all_dim_out_block_obj dim-main-color)
            ; ; (vla-put-extensionlinecolor all_dim_out_block_obj dim-main-color)
            (setq all_dim_out_block_iii (+ all_dim_out_block_iii 1))
          )
        )
        (princ "\n")
      )
    ;
    ;leader_obj_mode
      ;user_input_ selection_leader_object    
        (setq all_leader_out_block (ssget "x" 
                                      (list 
                                        (cons 0 "leader")       ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "SSSS")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                              )
        )
      ;
        (if (/= all_leader_out_block nil)
          (progn
            (setq all_leader_out_block_total (sslength all_leader_out_block))
            (setq all_leader_out_block_iii 0)
            (while
              (< all_leader_out_block_iii all_leader_out_block_total)
              (setq all_leader_out_block_ename (ssname all_leader_out_block all_leader_out_block_iii))
              (setq all_leader_out_block_obj (vlax-ename->vla-object all_leader_out_block_ename))
              (vla-put-dimensionlinecolor all_leader_out_block_obj dim-main-color)
              (setq all_leader_out_block_iii (+ all_leader_out_block_iii 1))
            )
          )
          (princ "\n")
        )
    ;
  )

(setq ssss (vla-get-objectname (vlax-ename->vla-object (car (entsel)))))






(setq all_obj_in_block (ssget "x" 
                              (list 
                                (cons -4 "<NOT")       ; เริ่มต้นกลุ่มของคุณลักษณะที่ไม่ต้องการ
                                  (cons 0 "HATCH")       ; ประเภทของวัตถุที่ไม่ต้องการ
                                (cons -4 "NOT>")      ; สิ้นสุดกลุ่มของคุณลักษณะที่ไม่ต้องการ
                                (cons -4 "<NOT")       ; เริ่มต้นกลุ่มของคุณลักษณะที่ไม่ต้องการ
                                  (cons 62 1)       ; ประเภทของวัตถุที่ไม่ต้องการ
                                (cons -4 "NOT>")      ; สิ้นสุดกลุ่มของคุณลักษณะที่ไม่ต้องการ
                                (cons -4 "<NOT")       ; เริ่มต้นกลุ่มของคุณลักษณะที่ไม่ต้องการ
                                  (cons 62 2)       ; ประเภทของวัตถุที่ไม่ต้องการ
                                (cons -4 "NOT>")      ; สิ้นสุดกลุ่มของคุณลักษณะที่ไม่ต้องการ
                              )
                     )
)




(command "pselect" all_obj_in_block "")