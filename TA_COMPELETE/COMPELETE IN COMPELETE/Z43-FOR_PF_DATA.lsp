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
      (setq c-byblock "byblock") 
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
      (setq c-byblock "byblock") 
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
      (setq c-byblock "byblock") 
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
      (setq c-byblock "byblock") 
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
      (setq c-byblock "byblock") 
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
;Custom DATA Funcition
  (defun c:PF_PART () 
    ; (setq att (car (entsel))) ;1st method
    (setq att (entlast)) ;2nd method
    (setq att_get (entget att))
    (setq att_get1 (cdr (assoc 0 att_get)))
    (setq att_obj (vlax-ename->vla-object att))
    (setq att_dyn (vla-get-isdynamicblock att_obj))
  
    (if 
      (and  (= att_get1 "INSERT") 
            (= att_dyn :vlax-true)
            (= (LM:al-effectivename att) "001 - PF_PATTERN DATA CUSTOM1")
            (= (LM:effectivename att_obj) "001 - PF_PATTERN DATA CUSTOM1")
      )
      (progn 
        (princ "XX")
        ;visibilitystate part
          ; (LM:getvisibilitystate att_obj)
          ; (LM:SetVisibilityState att_obj "STRAIGHT")
        ;Attribute part
          (setq PATTERN_DNCE (getstring "specify PATTERN_DNCE"))
          (LM:vl-setattributevalue att_obj "PATTERN_DNCE" PATTERN_DNCE)

          (setq HOLE_DIAMETER_VALUE (getstring "specify HOLE_DIAMETER_VALUE"))
          (LM:vl-setattributevalue att_obj "HOLE_DIAMETER_VALUE" HOLE_DIAMETER_VALUE)

          (setq SQUARE/REC_PITCH_VALUE (getstring "specify SQUARE/REC_PITCH_VALUE"))
          (LM:vl-setattributevalue att_obj "SQUARE/REC_PITCH_VALUE" SQUARE/REC_PITCH_VALUE)

          (setq DIGONAL_PITCH_VALUE (getstring "specify DIGONAL_PITCH_VALUE"))
          (LM:vl-setattributevalue att_obj "DIGONAL_PITCH_VALUE" DIGONAL_PITCH_VALUE)

          (setq OPEN_AREA_VALUE (getstring "specify OPEN_AREA_VALUE"))
          (LM:vl-setattributevalue att_obj "OPEN_AREA_VALUE" OPEN_AREA_VALUE)

          (setq MATTERIAL_VALUE (getstring "specify MATTERIAL_VALUE"))
          (LM:vl-setattributevalue att_obj "MATTERIAL_VALUE" MATTERIAL_VALUE)

          (setq MATTERIAL_VALUE2 (getstring "specify MATTERIAL_VALUE2"))
          (LM:vl-setattributevalue att_obj "MATTERIAL_VALUE2" MATTERIAL_VALUE2)

          (setq SIZING_VALUE (getstring "specify SIZING_VALUE"))
          (LM:vl-setattributevalue att_obj "SIZING_VALUE" SIZING_VALUE)

          (setq COLOR_VALUE (getstring "specify COLOR_VALUE"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE" COLOR_VALUE)
        
          (setq TOTAL_HOLE_VALUE (getstring "specify TOTAL_HOLE_VALUE"))
          (LM:vl-setattributevalue att_obj "TOTAL_HOLE_VALUE" TOTAL_HOLE_VALUE)

          (setq TOTAL_VALUE (getstring "specify TOTAL_VALUE"))
          (LM:vl-setattributevalue att_obj "TOTAL_VALUE" TOTAL_VALUE)

          (setq BLK_NAME (getstring "specify BLK_NAME"))
          (LM:vl-setattributevalue att_obj "BLK_NAME" BLK_NAME)
      )
      (alert "Please select dynamic block object.")
    )
  )
  (defun c:LV_PART () 
    ; (setq att (car (entsel))) ;1st method
    (setq att (entlast)) ;2nd method
    (setq att_get (entget att))
    (setq att_get1 (cdr (assoc 0 att_get)))
    (setq att_obj (vlax-ename->vla-object att))
    (setq att_dyn (vla-get-isdynamicblock att_obj))
  
    (if 
      (and  (= att_get1 "INSERT") 
            (= att_dyn :vlax-true)
            (= (LM:al-effectivename att) "001 - LV_PATTERN DATA CUSTOM6")
            (= (LM:effectivename att_obj) "001 - LV_PATTERN DATA CUSTOM6")
      )
      (progn 
        (princ "XX")
        ;visibilitystate part
          ; (LM:getvisibilitystate att_obj)
          ; (LM:SetVisibilityState att_obj "STRAIGHT")
        ;Attribute part
          (setq MOCKUP_# (getstring "specify MOCKUP_#"))
          (LM:vl-setattributevalue att_obj "MOCKUP_#" MOCKUP_#)

          (setq MATTERIAL_VALUE (getstring "specify MATTERIAL_VALUE"))
          (LM:vl-setattributevalue att_obj "MATTERIAL_VALUE" MATTERIAL_VALUE)

          (setq SIZING_VALUE (getstring "specify SIZING_VALUE"))
          (LM:vl-setattributevalue att_obj "SIZING_VALUE" SIZING_VALUE)

          (setq DISTANCE_LOUVERS_VALUE (getstring "specify DISTANCE_LOUVERS_VALUE"))
          (LM:vl-setattributevalue att_obj "DISTANCE_LOUVERS_VALUE" DISTANCE_LOUVERS_VALUE)

          (setq TOTAL_LOUVERS_VALUE1 (getstring "specify TOTAL_LOUVERS_VALUE1"))
          (LM:vl-setattributevalue att_obj "TOTAL_LOUVERS_VALUE1" TOTAL_LOUVERS_VALUE1)

          (setq ACESSORIES_LINE_LOOKUP (getstring "specify ACESSORIES_LINE_LOOKUP"))
          (LM:setdynpropvalue att_obj "ACESSORIES_LINE_LOOKUP" ACESSORIES_LINE_LOOKUP)
        
          (setq ACESSORIES_VALUE1 (getstring "specify ACESSORIES_VALUE1"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE1" ACESSORIES_VALUE1)

          (setq ACESSORIES_VALUE2 (getstring "specify ACESSORIES_VALUE2"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE2" ACESSORIES_VALUE2)
        
          (setq ACESSORIES_VALUE3 (getstring "specify ACESSORIES_VALUE3"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE3" ACESSORIES_VALUE3)
        
          (setq ACESSORIES_VALUE4 (getstring "specify ACESSORIES_VALUE4"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE4" ACESSORIES_VALUE4)
        
          (setq ACESSORIES_VALUE5 (getstring "specify ACESSORIES_VALUE5"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE5" ACESSORIES_VALUE5)
        
          (setq ACESSORIES_VALUE6 (getstring "specify ACESSORIES_VALUE6"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE6" ACESSORIES_VALUE6)
        
          (setq ACESSORIES_VALUE7 (getstring "specify ACESSORIES_VALUE7"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE7" ACESSORIES_VALUE7)
        
          (setq ACESSORIES_VALUE8 (getstring "specify ACESSORIES_VALUE8"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE8" ACESSORIES_VALUE8)
        
          (setq ACESSORIES_VALUE9 (getstring "specify ACESSORIES_VALUE9"))
          (LM:vl-setattributevalue att_obj "ACESSORIES_VALUE9" ACESSORIES_VALUE9)

          (setq COLOR_VALUE (getstring "specify COLOR_VALUE"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE" COLOR_VALUE)

          (setq COLOR_VALUE2 (getstring "specify COLOR_VALUE2"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE2" COLOR_VALUE2)

          (setq TOTAL_SET_VALUE (getstring "specify TOTAL_SET_VALUE"))
          (LM:vl-setattributevalue att_obj "TOTAL_SET_VALUE" TOTAL_SET_VALUE)

          (setq BLK_NAME (getstring "specify BLK_NAME"))
          (LM:vl-setattributevalue att_obj "BLK_NAME" BLK_NAME)
      )
      (alert "Please select dynamic block object.")
    )
  )
  (defun c:MAIN_PART () 
    ; (setq att (car (entsel))) ;1st method
    (setq att (entlast)) ;2nd method
    (setq att_get (entget att))
    (setq att_get1 (cdr (assoc 0 att_get)))
    (setq att_obj (vlax-ename->vla-object att))
    (setq att_dyn (vla-get-isdynamicblock att_obj))
  
    (if 
      (and  (= att_get1 "INSERT") 
            (= att_dyn :vlax-true)
            (= (LM:al-effectivename att) "001 - PART DATA CUSTOM 2023_REV01")
            (= (LM:effectivename att_obj) "001 - PART DATA CUSTOM 2023_REV01")
      )
      (progn 
        (princ "XX")
        ;visibilitystate part
          (LM:getvisibilitystate att_obj)
          (LM:SetVisibilityState att_obj "SQ")
        ;Attribute part
          (setq CODE_VALUE (getstring "specify CODE_VALUE"))
          (LM:vl-setattributevalue att_obj "CODE_VALUE" CODE_VALUE)

          (setq PART_NAME_VALUE_1 (getstring "specify PART_NAME_VALUE_1"))
          (LM:vl-setattributevalue att_obj "PART_NAME_VALUE_1" PART_NAME_VALUE_1)

          (setq PART_NAME_VALUE_2 (getstring "specify PART_NAME_VALUE_2"))
          (LM:vl-setattributevalue att_obj "PART_NAME_VALUE_2" PART_NAME_VALUE_2)

          (setq PART_NAME_VALUE_3 (getstring "specify PART_NAME_VALUE_3"))
          (LM:vl-setattributevalue att_obj "PART_NAME_VALUE_3" PART_NAME_VALUE_3)

          (setq MATTERIAL_VALUE (getstring "specify MATTERIAL_VALUE"))
          (LM:vl-setattributevalue att_obj "MATTERIAL_VALUE" MATTERIAL_VALUE)

          (setq SPEC_VALUE (getstring "specify SPEC_VALUE"))
          (LM:vl-setattributevalue att_obj "SPEC_VALUE" SPEC_VALUE)

          (setq BLANK_SIZE_VALUE (getstring "specify BLANK_SIZE_VALUE"))
          (LM:vl-setattributevalue att_obj "BLANK_SIZE_VALUE" BLANK_SIZE_VALUE)

          (setq THICKNESS_VALUE (getstring "specify THICKNESS_VALUE"))
          (LM:vl-setattributevalue att_obj "THICKNESS_VALUE" THICKNESS_VALUE)

          (setq COLOR_VALUE (getstring "specify COATING_VALUE"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE" COLOR_VALUE)

          (setq COLOR_VALUE2 (getstring "specify COATING_VALUE2"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE2" COLOR_VALUE2)

          (setq LENGTH_VALUE (getstring "specify LENGTH_VALUE"))
          (LM:vl-setattributevalue att_obj "LENGTH_VALUE" LENGTH_VALUE)
        
          (setq LENGTH_VALUE_2 (getstring "specify LENGTH_VALUE_2"))
          (LM:vl-setattributevalue att_obj "LENGTH_VALUE_2" LENGTH_VALUE_2)

          (setq TOTAL_VALUE (getstring "specify TOTAL_VALUE"))
          (LM:vl-setattributevalue att_obj "TOTAL_VALUE" TOTAL_VALUE)

          (setq BLK_NAME (getstring "specify BLK_NAME"))
          (LM:vl-setattributevalue att_obj "BLK_NAME" BLK_NAME)
      )
      (alert "Please select dynamic block object.")
    )
  )
  (defun c:TBP_TITTLE_BLOCK_PART () 
    ; (setq att (car (entsel))) ;1st method
    (setq att (entlast)) ;2nd method
    (setq att_get (entget att))
    (setq att_get1 (cdr (assoc 0 att_get)))
    (setq att_obj (vlax-ename->vla-object att))
    (setq att_dyn (vla-get-isdynamicblock att_obj))
  
    (if 
      (and  (= att_get1 "INSERT") 
            (= att_dyn :vlax-true)
            (= (LM:al-effectivename att) "LNAD - A4 TITLE BLOCK PART REV01")
            (= (LM:effectivename att_obj) "LNAD - A4 TITLE BLOCK PART REV01")
      )
      (progn 
        (princ "XX")
        ;visibilitystate part
          (LM:getvisibilitystate att_obj)
          (LM:SetVisibilityState att_obj "SQ")
        ;Attribute part
          (setq NAME_JOB_1 (getstring "specify NAME_JOB_1"))
          (LM:vl-setattributevalue att_obj "NAME_JOB_1" NAME_JOB_1)

          (setq NAME_JOB_2 (getstring "specify NAME_JOB_2"))
          (LM:vl-setattributevalue att_obj "NAME_JOB_2" NAME_JOB_2)

          (setq NAME_PROJECT_1 (getstring "specify NAME_PROJECT_1"))
          (LM:vl-setattributevalue att_obj "NAME_PROJECT_1" NAME_PROJECT_1)

          (setq NAME_PROJECT_2 (getstring "specify NAME_PROJECT_2"))
          (LM:vl-setattributevalue att_obj "NAME_PROJECT_2" NAME_PROJECT_2)

          (setq NAME_PROJECT_3 (getstring "specify NAME_PROJECT_3"))
          (LM:vl-setattributevalue att_obj "NAME_PROJECT_3" NAME_PROJECT_3)

          (setq NAME_PART_CODE1 (getstring "specify NAME_PART_CODE1"))
          (LM:vl-setattributevalue att_obj "NAME_PART_CODE1" NAME_PART_CODE1)

          (setq NAME_PART_NAME1 (getstring "specify NAME_PART_NAME1"))
          (LM:vl-setattributevalue att_obj "NAME_PART_NAME1" NAME_PART_NAME1)

          (setq NAME_PART_NAME2 (getstring "specify NAME_PART_NAME2"))
          (LM:vl-setattributevalue att_obj "NAME_PART_NAME2" NAME_PART_NAME2)

          (setq NAME_PART_NAME3 (getstring "specify NAME_PART_NAME3"))
          (LM:vl-setattributevalue att_obj "NAME_PART_NAME3" NAME_PART_NAME3)

          (setq NAME_MAT (getstring "specify NAME_MAT"))
          (LM:vl-setattributevalue att_obj "NAME_MAT" NAME_MAT)

          (setq NAME_SPEC (getstring "specify NAME_SPEC"))
          (LM:vl-setattributevalue att_obj "NAME_SPEC" NAME_SPEC)

          (setq NAME_THK (getstring "specify NAME_THK"))
          (LM:vl-setattributevalue att_obj "NAME_THK" NAME_THK)

          (setq COLOR_VALUE (getstring "specify COLOR_VALUE"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE" COLOR_VALUE)

          (setq COLOR_VALUE2 (getstring "specify COLOR_VALUE"))
          (LM:vl-setattributevalue att_obj "COLOR_VALUE2" COLOR_VALUE2)

          (setq NAME_DAFT (getstring "specify NAME_DAFT"))
          (LM:vl-setattributevalue att_obj "NAME_DAFT" NAME_DAFT)

          (setq NAME_CHECKED (getstring "specify NAME_CHECKED"))
          (LM:vl-setattributevalue att_obj "NAME_CHECKED" NAME_CHECKED)

          (setq NAME_DESIGNER (getstring "specify NAME_DESIGNER"))
          (LM:vl-setattributevalue att_obj "NAME_DESIGNER" NAME_DESIGNER)

          (setq NAME_DESIGN_MGR (getstring "specify NAME_DESIGN_MGR"))
          (LM:vl-setattributevalue att_obj "NAME_DESIGN_MGR" NAME_DESIGN_MGR)

          (setq NAME_TITLE_1 (getstring "specify NAME_TITLE_1"))
          (LM:vl-setattributevalue att_obj "NAME_TITLE_1" NAME_TITLE_1)

          (setq NAME_TITLE_2 (getstring "specify NAME_TITLE_2"))
          (LM:vl-setattributevalue att_obj "NAME_TITLE_2" NAME_TITLE_2)

          (setq NAME_TITLE_3 (getstring "specify NAME_TITLE_3"))
          (LM:vl-setattributevalue att_obj "NAME_TITLE_3" NAME_TITLE_3)

          (setq DRAWING_NO1 (getstring "specify DRAWING_NO1"))
          (LM:vl-setattributevalue att_obj "DRAWING_NO1" DRAWING_NO1)

          (setq SUPERLINK (getstring "specify SUPERLINK"))
          (LM:vl-setattributevalue att_obj "SUPERLINK" SUPERLINK)

          (setq NAMEFILE (getstring "specify NAMEFILE"))
          (LM:vl-setattributevalue att_obj "NAMEFILE" NAMEFILE)

      )
      (alert "Please select dynamic block object.")
    )
  )
;
;Dimension _function
  (defun c:z37-MK_V_DIM ()
    ; (c:ucs_world)
    ;tittle_block_ref_for_cpolygon
      (setq tittle_blk (entlast))
        (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
        (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
        (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
        (if (and (= ef_tillte_blk ef_blk) )
          (progn
            (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
            (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

            (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
            (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
            (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
            
            (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
            (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
            (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
            ; ทำคำสั่งอื่น ๆ ที่ต้องการ
          )
          (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
        )
      
      (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
      (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
    ;tittle_block_ref_for_cpolygon
    
    ; finding_grid_line part
      (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))

      ; (setq my-selection-set (ssget "C" 
      ;                           '(100 100) ;for insert x
      ;                           '(200 200) ;for insert y
      ;                           '(
      ;                             (0 . "INSERT") 
                                  
      ;                             )
      ;                   )
      ; )
      (setq total_ssget (sslength my-selection-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-selection-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (vla-get-rotation blk_obj_Set)
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= (vla-get-rotation blk_obj_Set) 0))
              (progn
                (setq ename-list (cons blk_obj ename-list))
                ; ทำคำสั่งอื่น ๆ ที่ต้องการ
              )
              (princ "\n")
          )
          (setq ie (+ ie 1))
        )
      (princ "\n")
      (princ (setq total_ename-list (length ename-list)))
      (setq ff (ssadd))
        (foreach ename ename-list
          (ssadd ename ff)
        )
      
      
      ; (command "pselect" ff )
    ; finding_grid_line part
    
    ; sub_function part
      ;basic_func
        (defun c:ucs_world()
          (command "ucs" "world")
        )
        (defun c:ucs_previ ()
          (command "ucs" "p")
        )
      ;basic_func

      ;custom_func_for_grid_line
          (defun ddxc (/ scl)
            ; (c:ucs_world)
            (defun LM:getdynpropvalue (blk prp) 
              (setq prp (strcase prp))
              (vl-some 
                '(lambda (x) 
                  (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
                )
                (vlax-invoke blk 'getdynamicblockproperties)
              )
            )
            (setvar "osmode" 0)
            
            ; เริ่ม part คำนวณ SCALE
              ; (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
            
              (setq scl sc_tt_blk ) ; changing this Variable for dynamic data Excel Programing
              
              (setq sc (* scl 10)) ; 5 = 50 2 = 20
              (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
              (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
              (setq OF_DIM (* (* scl 1) of_point))
              (setq HI_DIM (* (* scl 1) hi_point))
            ; จบ part คำนวณ SCALE
            (defun get-x-coordinate (entity)
              (cadr (assoc 10 (entget entity)))
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
            
            (setq my-selection-set ff) ; changing this Variable for dynamic data Excel Programing
            
              (setq sorted-enames (sort-entities-by-x my-selection-set))
              (setq total-entities (length sorted-enames))
            (setq i 0)
            (setq ii 1)
            
                    (setq total-entities (length sorted-enames))
                    (while (and (< i total-entities) (< ii total-entities))
                      ; (while (< i total-entities)
                        ; part i
                      (setq entity_i (nth i sorted-enames))
                        (setq o_i (vlax-ename->vla-object entity_i))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                        (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                        (setq o_i_ins_x (car o_i_ins_xy))
                        (setq o_i_ins_y (cadr o_i_ins_xy))
                        (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
                      (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
                      
                      ; part ii
                      (setq entity_ii (nth ii sorted-enames))
                        (setq o_ii (vlax-ename->vla-object entity_ii))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                        (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                        (setq o_ii_ins_x (car o_ii_ins_xy))
                        (setq o_ii_ins_y (cadr o_ii_ins_xy))
                        (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
                      (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
                      
                      ; part i+ii offset dim
                      (setq o_ii3_ins_y (+ o_ii_ins_y_off OF_DIM))
                      (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

                      (command "dimstyle" "r" "DIM E-E")
                      (setvar "DIMSCALE" sc)
                      (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
                      
                      (setq i (+ i 1))
                      (setq ii (+ i 1))
                              
                    )
          
                    ; part i
                      (setq iii (- total-entities 1))
                      (setq entity_1st (nth 0 sorted-enames))
                        (setq o_1st (vlax-ename->vla-object entity_1st))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_1st) "OFFSET"))
                        (setq o_1st_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_1st))))
                        (setq o_1st_ins_x (car o_1st_ins_xy))
                        (setq o_1st_ins_y (cadr o_1st_ins_xy))
                        (setq o_1st_ins_y_off (+ (cadr o_1st_ins_xy) OFFSET))
                      (setq o_1st_ins_xy_new (strcat (rtos o_1st_ins_x 2 8) "," (rtos o_1st_ins_y_off 2 8) ))
                    ; part last
                      (setq entity_last (nth iii sorted-enames))
                        (setq o_last (vlax-ename->vla-object entity_last))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_last) "OFFSET"))
                        (setq o_last_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_last))))
                        (setq o_last_ins_x (car o_last_ins_xy))
                        (setq o_last_ins_y (cadr o_last_ins_xy))
                        (setq o_last_ins_y_off (+ (cadr o_last_ins_xy) OFFSET))
                      (setq o_last_ins_xy_new (strcat (rtos o_last_ins_x 2 8) "," (rtos o_last_ins_y_off 2 8) ))   

                    ; part i+iii offset dim
                    (setq o_iii3_ins_y (+ o_last_ins_y_off HI_DIM))
                    (setq o_iii3_ins_xy_new (strcat (rtos o_last_ins_x 2 8) "," (rtos o_iii3_ins_y 2 8) ))          
                  
                    (command "dimlinear" o_1st_ins_xy_new o_last_ins_xy_new o_iii3_ins_xy_new)
            
            (setvar "osmode" 1215)
            ; (c:ucs_previ)
          )


      ;custom_func_for_grid_line
    
        (ddxc) ;sub func for send to command line 

    
    ; sub_function part
  )
  (defun c:z37-MK_H_DIM ()
    ; (c:ucs_world)
    ;tittle_block_ref_for_cpolygon  
      ; (setq tittle_blk (entlast))
        (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
        (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
        (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
        (if (and (= ef_tillte_blk ef_blk) )
          (progn
            (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
            (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

            (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
            (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
            (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
            
            (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
            (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
            (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
            ; ทำคำสั่งอื่น ๆ ที่ต้องการ
          )
          (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
        )
      
      (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
      (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
    ;tittle_block_ref_for_cpolygon
    
    ; finding_grid_line part
      
      (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))
      ; (vla-delete tittle_blk_obj)

      ; (setq my-selection-set (ssget "C" 
      ;                           '(100 100) ;for insert x
      ;                           '(200 200) ;for insert y
      ;                           '(
      ;                             (0 . "INSERT") 
                                  
      ;                             )
      ;                   )
      ; )
      (setq total_ssget (sslength my-selection-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-selection-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation (rtos (vla-get-rotation blk_obj_Set) 2 8))
          (setq rotation_set "1.57079633")
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (not (= (vla-get-rotation blk_obj_Set) 0)))
              (progn
                (setq ename-list (cons blk_obj ename-list))
                ; ทำคำสั่งอื่น ๆ ที่ต้องการ
              )
              (princ "\n")
          )
          (setq ie (+ ie 1))
        )
      (princ "\n")
      (princ (setq total_ename-list (length ename-list)))
      (setq ff (ssadd))
        (foreach ename ename-list
          (ssadd ename ff)
        )
      
      
      ;  (command "pselect" ff )
    ; finding_grid_line part
    
    ; sub_function part
      ;basic_func
        (defun c:ucs_world()
          (command "ucs" "world")
        )
        (defun c:ucs_previ ()
          (command "ucs" "p")
        )
      ;basic_func

      ;custom_func_for_grid_line
          (defun ddyc (/ scl)
            ; (c:ucs_world)
            (defun LM:getdynpropvalue (blk prp) 
              (setq prp (strcase prp))
              (vl-some 
                '(lambda (x) 
                  (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
                )
                (vlax-invoke blk 'getdynamicblockproperties)
              )
            )
            (setvar "osmode" 0)
            
            ; เริ่ม part คำนวณ SCALE
              ; (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
            
              (setq scl sc_tt_blk ) ; changing this Variable for dynamic data Excel Programing
              
              (setq sc (* scl 10)) ; 5 = 50 2 = 20
              (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
              (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
              (setq OF_DIM (* (* scl 1) of_point))
              (setq HI_DIM (* (* scl 1) hi_point))
            ; จบ part คำนวณ SCALE
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
                                                '(lambda (b a) 
                                                  (if (and (cdr b) (cdr a))
                                                    (< (cadr b) (cadr a))
                                                    t) ; เมื่อไม่มีข้อมูล x ให้ถือว่าต่างกัน
                                                )
                                      )
                )

                (setq sorted-ename-list '())
                (foreach entity-entity sorted-entity-list
                  (setq sorted-ename-list (cons (car entity-entity) sorted-ename-list))
                )

                ; (reverse sorted-ename-list)
              )
            
            (setq my-selection-set ff) ; changing this Variable for dynamic data Excel Programing
            
              (setq sorted-enames (sort-entities-by-x my-selection-set))
              (setq total-entities (length sorted-enames))
            (setq i 0)
            (setq ii 1)
            
                    (setq total-entities (length sorted-enames))
                    (while (and (< i total-entities) (< ii total-entities))
                      ; (while (< i total-entities)
                        ; part i
                      (setq entity_i (nth i sorted-enames))
                        (setq o_i (vlax-ename->vla-object entity_i))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                        (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                        (setq o_i_ins_x (car o_i_ins_xy))
                        (setq o_i_ins_y (cadr o_i_ins_xy))
                        (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
                      (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
                      
                      ; part ii
                      (setq entity_ii (nth ii sorted-enames))
                        (setq o_ii (vlax-ename->vla-object entity_ii))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                        (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                        (setq o_ii_ins_x (car o_ii_ins_xy))
                        (setq o_ii_ins_y (cadr o_ii_ins_xy))
                        (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
                      (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
                      
                      ; part i+ii offset dim
                      (setq o_ii3_ins_x (- o_ii_ins_x_off OF_DIM))
                      (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
                      
                      (command "dimstyle" "r" "DIM E-E")
                      (setvar "DIMSCALE" sc)
                      (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
                      
                      (setq i (+ i 1))
                      (setq ii (+ i 1))
                              
                    )
          
                    ; part i
                      (setq iii (- total-entities 1))
                      (setq entity_1st (nth 0 sorted-enames))
                        (setq o_1st (vlax-ename->vla-object entity_1st))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_1st) "OFFSET"))
                        (setq o_1st_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_1st))))
                        (setq o_1st_ins_x (car o_1st_ins_xy))
                        (setq o_1st_ins_y (cadr o_1st_ins_xy))
                        (setq o_1st_ins_x_off (- (car o_ii_ins_xy) OFFSET))
                      (setq o_1st_ins_xy_new (strcat (rtos o_1st_ins_x_off 2 8) "," (rtos o_1st_ins_y 2 8) ))
                    ; part last
                      (setq entity_last (nth iii sorted-enames))
                        (setq o_last (vlax-ename->vla-object entity_last))
                        (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_last) "OFFSET"))
                        (setq o_last_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_last))))
                        (setq o_last_ins_x (car o_last_ins_xy))
                        (setq o_last_ins_y (cadr o_last_ins_xy))
                        (setq o_last_ins_x_off (- (car o_ii_ins_xy) OFFSET))
                      (setq o_last_ins_xy_new (strcat (rtos o_last_ins_x_off 2 8) "," (rtos o_last_ins_y 2 8) ))   

                    ; part i+iii offset dim
                    (setq o_iii3_ins_x (- o_last_ins_x_off HI_DIM))
                    (setq o_iii3_ins_xy_new (strcat (rtos o_iii3_ins_x 2 8) "," (rtos o_last_ins_y 2 8) ))          
                  
                    (command "dimlinear" o_1st_ins_xy_new o_last_ins_xy_new o_iii3_ins_xy_new)
            
            (setvar "osmode" 1215)
            ; (c:ucs_previ)
          )


      ;custom_func_for_grid_line
    
        (ddyc) ;sub func for send to command line 

    
    ; sub_function part
  )
  (defun c:pre_main_func()
    (setvar "osmode" 0)
    (setq h (getint "specify scale"))
    (setq ins_x -25)
    (setq ins_y -30)
    (setq sc_ins_x (* ins_x h))
    (setq sc_ins_y (* ins_y h))
    (setq sc_ins_xy (list sc_ins_x sc_ins_y))   
    (command "_-INSERT""LNAD - A4 TITLE BLOCK PART REV01" sc_ins_xy h 0)
    (setq ti_blk (entlast))
    (setq ti_blk_obj (vlax-ename->vla-object ti_blk ))
    (c:ucs_world)
    (c:z37-MK_V_DIM)
    (c:z37-MK_H_DIM)
    (command "ucs" "p")
    (setvar "osmode" 1215)
    (vla-delete ti_blk_obj)
  )
  (defun c:pre_main_func_lv()
    (setvar "osmode" 0)
    (setq h (getint "specify scale"))
    (setq ins_x -25)
    (setq ins_y -30)
    (setq sc_ins_x (* ins_x h))
    (setq sc_ins_y (* ins_y h))
    (setq sc_ins_xy (list sc_ins_x sc_ins_y))   
    (command "_-INSERT""LNAD - A4 TITLE BLOCK PART REV01" sc_ins_xy h 0)
    (setq ti_blk (entlast))
    (setq ti_blk_obj (vlax-ename->vla-object ti_blk ))
    (c:ucs_world)
    (c:inssubf_insertion_louvers)
    (c:inslv_insertion_louvers)
    (command "ucs" "p")
    (setvar "osmode" 1215)
    (vla-delete ti_blk_obj)
  )
  (defun c:z37-DYN_GRIDLINE ()
    (c:CH300_set_gridline)
    (setq grid_ins (entlast))
      (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
      (setq grid_ins_ef_name (LM:effectivename grid_ins_obj))
      (setq dyn_grid_blk "000-GRID_LINE_DYN")
        (if (and (= grid_ins_ef_name dyn_grid_blk))
          (progn
            (setq H (getreal "specify H"))
            (setq OFS (getreal "specify OFS"))
            (setq LOCATION (getstring "specify LOCATION TEXT"))
            (setq grid_ins_obj (vlax-ename->vla-object grid_ins))
            
            (setq grid_ins_H (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "H" (rtos H 2 8)))
            (setq grid_ins_OFS (LM:setdynpropvalue (vlax-ename->vla-object grid_ins) "OFFSET" (rtos OFS 2 8)))
            (setq grid_ins_LOCATION (LM:vl-setattributevalue (vlax-ename->vla-object grid_ins) "LOCATION" LOCATION))
            ; ทำคำสั่งอื่น ๆ ที่ต้องการ
          )
          (princ "\n")
        )
    (c:S000_reset_layer)
  )
;
;Lv Heading _function
  (defun c:LV_HEADING () 
    ; (setq att (car (entsel))) ;1st method
    (setq att (entlast)) ;2nd method
    (setq att_get (entget att))
    (setq att_get1 (cdr (assoc 0 att_get)))
    (setq att_obj (vlax-ename->vla-object att))
    (setq att_dyn (vla-get-isdynamicblock att_obj))
  
    (if 
      (and  (= att_get1 "INSERT") 
            (= att_dyn :vlax-true)
            (= (LM:al-effectivename att) "CONTENT NAME3 LEFT - BUBBLE v6")
            (= (LM:effectivename att_obj) "CONTENT NAME3 LEFT - BUBBLE v6")
      )
      (progn 
        (princ "XX")
        ;visibilitystate part : NOTTHING DATA FOR ADJUSTMENT
          ; (LM:getvisibilitystate att_obj)
          ; (LM:SetVisibilityState att_obj "SQ")
        ;Attribute part
          (setq CONTENT_LINE (getstring "specify CONTENT_LINE"))
          (LM:setdynpropvalue att_obj "CONTENT_LINE" CONTENT_LINE)
        
          (setq 1_ARROW_FLAT_L (getstring "specify 1_ARROW_FLAT_L"))
          (LM:setdynpropvalue att_obj "1_ARROW_FLAT_L" 1_ARROW_FLAT_L)
        
          (setq 1_ARROW_FLAT_OBQ (getstring "specify 1_ARROW_FLAT_OBQ"))
          (LM:setdynpropvalue att_obj "1_ARROW_FLAT_OBQ" 1_ARROW_FLAT_OBQ)
        
          ; (setq 2_ARROW_FLAT_L (getstring "specify 2_ARROW_FLAT_L"))
          (setq 2_ARROW_FLAT_L (rtos 0 2 8))
          (LM:setdynpropvalue att_obj "2_ARROW_FLAT_L" 2_ARROW_FLAT_L)
        
          ; (setq 2_ARROW_FLAT_OBQ (getstring "specify 2_ARROW_FLAT_OBQ"))
          (setq 2_ARROW_FLAT_OBQ 1_ARROW_FLAT_OBQ)
          (LM:setdynpropvalue att_obj "2_ARROW_FLAT_OBQ" 2_ARROW_FLAT_OBQ)

          (setq NO.CODE (getstring "specify NO.CODE"))
          (LM:vl-setattributevalue att_obj "NO.CODE" NO.CODE)

          (setq NAME1 (getstring "specify NAME1"))
          (LM:vl-setattributevalue att_obj "NAME1" NAME1)

          (setq NAME2 (getstring "specify NAME2"))
          (LM:vl-setattributevalue att_obj "NAME2" NAME2)

          (setq NAME3 (getstring "specify NAME3"))
          (LM:vl-setattributevalue att_obj "NAME3" NAME3)

          (setq NAME4 (getstring "specify NAME4"))
          (LM:vl-setattributevalue att_obj "NAME4" NAME4)

          (setq NAME5 (getstring "specify NAME5"))
          (LM:vl-setattributevalue att_obj "NAME5" NAME5)

          (setq NAME6 (getstring "specify NAME6"))
          (LM:vl-setattributevalue att_obj "NAME6" NAME6)

          (setq NAME7 (getstring "specify NAME7"))
          (LM:vl-setattributevalue att_obj "NAME7" NAME7)

          (setq NAME8 (getstring "specify NAME8"))
          (LM:vl-setattributevalue att_obj "NAME8" NAME8)

          (setq NAME9 (getstring "specify NAME9"))
          (LM:vl-setattributevalue att_obj "NAME9" NAME9)
      )
      (alert "Please select dynamic block object.")
    )
  )
;
;Insertion_Point_Louvers
  (defun c:inslv_insertion_louvers  ()
    (setq Lv_width (getreal "Specify_Louver_Width"))
    ;tittle_block_ref_for_cpolygon  
      (setq tittle_blk (entlast))
        (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
        (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
        (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
        (if (and (= ef_tillte_blk ef_blk) )
          (progn
            (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
            (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

            (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
            (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
            (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
            
            (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
            (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
            (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
            ; ทำคำสั่งอื่น ๆ ที่ต้องการ
          )
          (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
        )
      
      (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
      (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
    ;
    
    ; finding_grid_line part
      
      (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))
      ; (vla-delete tittle_blk_obj)

      ; (setq my-selection-set (ssget "C" 
      ;                           '(100 100) ;for insert x
      ;                           '(200 200) ;for insert y
      ;                           '(
      ;                             (0 . "INSERT") 
                                  
      ;                             )
      ;                   )
      ; )
      (setq total_ssget (sslength my-selection-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-selection-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation (rtos (vla-get-rotation blk_obj_Set) 2 8))
          (setq rotation_set "1.57079633") ;
          (setq ss (LM:effectivename blk_obj_Set))
          (setq location_text (LM:vl-getattributevalue blk_obj_Set "LOCATION"))
          

          (if 
            (and 
              (= ef_name ss)
              (= (vla-get-rotation blk_obj_Set) 0)
              (= "ELEVATION_LV" location_text)
            ) 
            (progn 
              (setq ename-list (cons blk_obj ename-list))
              ; ทำคำสั่งอื่น ๆ ที่ต้องการ
            )
            (princ "\n")
          )
          (setq ie (+ ie 1))
        )
      (princ "\n")
      (princ (setq total_ename-list (length ename-list)))
      (setq ff (ssadd))
        (foreach ename ename-list
          (ssadd ename ff)
        )
      
      
      ;  (command "pselect" ff ) ;just tesing
    ; 
    
    ;sub_func_for_sorting_part
      (defun get-x-coordinate (entity)
        (cadr (assoc 10 (entget entity)))
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
    ;sub_func_for_sorting_part
    
      (setq my-selection-set ff )
      (setq sorted (sort-entities-by-x my-selection-set))
      (setq sorted_total (length sorted))


    ;Setting_Louvers_part
      (setq i_LV 1)
      (setq sorted_total (length sorted))
      (while
        (< i_LV (- sorted_total 1))
        (setq sorted-ename (nth i_LV sorted))
        (setq sorted-ename_obj (vlax-ename->vla-object sorted-ename))
        (setq H (LM:getdynpropvalue sorted-ename_obj "H"))
        (setq sorted-ename_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj))))
        (setq sorted-ename_ins_xy 
               (list 
                 (car (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj))))
                 (- (cadr (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj)))) H)
               )
        )
        
        (command "insert" "001 - DYNAMIC LV" sorted-ename_ins_xy 1 0)
        
        (setq fv_lv (entlast))
          (setq fv_lv_obj (vlax-ename->vla-object fv_lv))
          (LM:setdynpropvalue fv_lv_obj "H" H)
          (LM:setdynpropvalue fv_lv_obj "W" Lv_width)
        
        (setq i_LV (+ 1 i_LV))
      )
    ;
  )
  (defun c:inssubf_insertion_louvers  ()
    ; (setq subf_Width (getreal "Specify_Louver_Width"))
    (setq subf_Height (getreal "Specify_subf_Height"))
    ;tittle_block_ref_for_cpolygon  
      (setq tittle_blk (entlast))
        (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
        (setq ef_tillte_blk (LM:effectivename tittle_blk_obj))
        (setq ef_blk "LNAD - A4 TITLE BLOCK PART REV01")
        (if (and (= ef_tillte_blk ef_blk) )
          (progn
            (setq tittle_blk_obj (vlax-ename->vla-object tittle_blk))
            (setq example multi dynamic (LM:getdynprops (vlax-ename->vla-object tittle_blk))) ; LM:getdynprops for example multi dynamic

            (setq top_r_x (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER X")) ; LM:getdynprops for example multi dynamic
            (setq top_r_y (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "TOP_R_CONNER Y")) ; LM:getdynprops for example multi dynamic
            (setq sc_tt_blk (LM:getdynpropvalue (vlax-ename->vla-object tittle_blk) "SC")) ; LM:getdynprops for example multi dynamic
            
            (setq tittle_blk_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint tittle_blk_obj))))
            (setq tittle_blk_ins_x (rtos (nth 0 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_y (rtos (nth 1 tittle_blk_ins_xy) 2 8))
            (setq tittle_blk_ins_x_real (atof tittle_blk_ins_x))
            (setq tittle_blk_ins_y_real (atof tittle_blk_ins_y))
            ; ทำคำสั่งอื่น ๆ ที่ต้องการ
          )
          (princ "\n TITTLE_BLOCK_NOT_ENTLAST")
        )
      
      (setq tx3 (list tittle_blk_ins_x_real tittle_blk_ins_y_real))
      (setq tx4 (list (+ top_r_x tittle_blk_ins_x_real) (+ top_r_y tittle_blk_ins_y_real)))
    ;
    
    ; finding_grid_line part
      
      (setq my-selection-set (ssget "C" tx3 tx4 (list (cons 0 "insert"))))
      ; (vla-delete tittle_blk_obj)

      ; (setq my-selection-set (ssget "C" 
      ;                           '(100 100) ;for insert x
      ;                           '(200 200) ;for insert y
      ;                           '(
      ;                             (0 . "INSERT") 
                                  
      ;                             )
      ;                   )
      ; )
      
    
      (setq total_ssget (sslength my-selection-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-selection-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_get (angtos (vla-get-rotation blk_obj_Set) 0 8))
          (setq rotaion_honli (rtos 90 2 8))
          (setq rotation_set "1.57079633") ;=90 angle
          
          (setq ss (LM:effectivename blk_obj_Set))
          (setq location_text (LM:vl-getattributevalue blk_obj_Set "LOCATION"))
          

          (if 
            (and 
              (= ef_name ss)
              (= rotation_get rotaion_honli)
              (= "ELEVATION_SUBF" location_text)
            ) 
            (progn 
              (setq ename-list (cons blk_obj ename-list))
              ; ทำคำสั่งอื่น ๆ ที่ต้องการ
            )
            (princ "\n")
          )
          (setq ie (+ ie 1))
        )
      (princ "\n")
      (princ (setq total_ename-list (length ename-list)))
      (setq ff (ssadd))
        (foreach ename ename-list
          (ssadd ename ff)
        )
      
      
      ;  (command "pselect" ff ) ;just tesing
    ; 
    
    ;sub_func_for_sorting_part
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
    ;sub_func_for_sorting_part
    
      (setq my-selection-set ff )
      (setq sorted (sort-entities-by-x my-selection-set))
      (setq sorted_total (length sorted))


    ;Setting_Louvers_part
      (setq i_subf 1)
      (setq sorted_total (length sorted))
      (while
        (< i_subf (- sorted_total 1))
        (setq sorted-ename (nth i_subf sorted))
        (setq sorted-ename_obj (vlax-ename->vla-object sorted-ename))
        (setq H (LM:getdynpropvalue sorted-ename_obj "H"))
        (setq sorted-ename_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj))))
       
        (setq sorted-ename_ins_xy 
               (list 
                 (+ (car (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj)))) (/ H 2))
                 (cadr (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sorted-ename_obj))))
               )
        )
        
        (command "insert" "001 - DYNAMIC subfame" sorted-ename_ins_xy 1 0)
        
        (setq fv_lv (entlast))
          (setq fv_lv_obj (vlax-ename->vla-object fv_lv))
          (LM:setdynpropvalue fv_lv_obj "H" subf_Height)
          (LM:setdynpropvalue fv_lv_obj "W" H)
        
        (setq i_subf (+ 1 i_subf))
      )
    ;
  )
;
;Block_Command&FUNC._
  (defun c:circle_to_block_cccb_ (  )
    ;Sub_FUNC._
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
      (defun TA:ename+vla-getboundingbox (obj) ;ประกอบ entity_name กับ vla-getboundingbox ให้เป็น list (LINE OR POLYLINE ONLY)
        ; (setq obj (vla-get-objectname (vlax-ename->vla-object (car (entsel))))) ;For Check objectname
        (if (or
              (= (vla-get-objectname obj) "AcDbLine")
              (= (vla-get-objectname obj) "AcDbPolyline")
              (= (vla-get-objectname obj) "AcDbBlockReference")
            )
          (progn
            ;get_data
              (setq ename_ (vlax-vla-object->ename obj)) 
              
              (setq _inspt (vla-getboundingbox obj 'min_ 'max_))
              (setq min_ (vlax-safearray->list min_))
              (setq max_ (vlax-safearray->list max_))
            ;
            ;sum_data_to_list
              (setq sum_list (list 
                              ename_
                              min_
                              max_
                              ;add more
                            )
              )
            ;
          )
          (setq error "OBJECT IS NOT LINE/POLYLINE")
        )
      )
      (defun TA:set-block-blockscaling (block-name parameter)
        ;example_using
          ;;parameter = acAny or acUniform
          ; (set-block-explodable "A$C5b7816c3" acAny)
          ; (set-block-explodable "A$C5b7816c3" acUniform)
        ;
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
        (setq block-def (vla-Item (vla-get-Blocks doc) block-name))
        (if block-def
          (vla-put-blockscaling block-def parameter)
          (princ (strcat "\nBlock " block-name " not found."))
        )
      )
      (defun TA:find_center (main_border_) -
        (setq main_border_obj_ (vlax-ename->vla-object main_border_))
        (setq obj (vla-get-objectname main_border_obj_)) ;For Check objectname
        (if (= obj "AcDbPolyline") 
          (progn 
            (setq main_border_obj_boline_ (TA:ename+vla-getboundingbox main_border_obj_))
            (setq min_crood (cadr main_border_obj_boline_))
            (setq max_crood (caddr main_border_obj_boline_))
            (setq Mx (/ (+ (car min_crood) (car max_crood)) 2))
            (setq My (/ (+ (cadr min_crood) (cadr max_crood)) 2))
            (setq Mz (/ (+ (caddr min_crood) (caddr max_crood)) 2))
            (setq center_point_ (list Mx My Mz))
          )
          (alert "TA:find_center : Object is not PolyLine")
        )
      )
    ;
    ;pre_var_setting_
      (setvar "osmode" 0)
      (command "ucs" "world" "")
    ;
    ;user_input
      ;block_data_for insertion
        (setq blk_ename_ (car (entsel "specify_block_")))
        (setq blk_ename_obj_ (vlax-ename->vla-object blk_ename_))
        (if (= (setq type_obj_ (vla-get-objectname (vlax-ename->vla-object blk_ename_))) "AcDbBlockReference") ;For Check objectname
          (progn
            (setq blk_ename_obj_inspt_ (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint blk_ename_obj_))))
            (setq blk_ename_obj_efname_ (LM:effectivename blk_ename_obj_ ))
          )
          (alert "Object is not block.")
        )
      ;
      ;selection_set
        (if  ;pre_select_ssget_or_post_select_ssget
          (= 
            (setq ss_pre_filter_set_xx_ (ssget "I" 
                                              (list 
                                                (cons 0 "POLYLINE,LWPOLYLINE") ;type of object 
                                                (cons 8 "A12_PERFORATED")   ;kind of layer
                                                ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                                ; (cons 62 1)           ;kind of color call sign with color code index
                                              )
                                        )
            )
            nil
          )
          (progn 
            (setq ss_pre_filter_set_xx_ (ssget 
                                          (list 
                                            (cons 0 "POLYLINE,LWPOLYLINE") ;type of object
                                            (cons 8 "A12_PERFORATED")   ;kind of layer
                                            ; (cons 2 "000-GRIDLINE_DYN")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                        )
            )
          )
          (sslength ss_pre_filter_set_xx_)
        )
      ;
    ;
    ;preloop_and_while
      (if (/= ss_pre_filter_set_xx_ nil) ;for start logic 
        (progn
          (setq ss_pre_filter_set_xx_i 0)
          (while (< ss_pre_filter_set_xx_i (sslength ss_pre_filter_set_xx_))
            ;get_data_
              
              (setq ss_pre_filter_set_xx_ename_ (ssname ss_pre_filter_set_xx_ ss_pre_filter_set_xx_i ))
              (setq ss_pre_filter_set_xx_ename_obj_ (vlax-ename->vla-object ss_pre_filter_set_xx_ename_ ))
              (setq ss_pre_filter_set_xx_ename_obj_centerpt_ (TA:find_center ss_pre_filter_set_xx_ename_) ) ;for polyline_obj_
              ; (setq ss_pre_filter_set_xx_ename_obj_centerpt_ (vlax-safearray->list (vlax-variant-value (vla-get-center ss_pre_filter_set_xx_ename_obj_)))) ;for_circle_obj_
            ;
            ;main_idea_code_
              
              (TA:set-block-blockscaling blk_ename_obj_efname_ acUniform)
              (command "point" ss_pre_filter_set_xx_ename_obj_centerpt_)
              (command "_insert"  blk_ename_obj_efname_ ss_pre_filter_set_xx_ename_obj_centerpt_ 1 0)
              (vla-delete ss_pre_filter_set_xx_ename_obj_)
            ;
            (setq ss_pre_filter_set_xx_i (+ ss_pre_filter_set_xx_i 1))
          )
        )
        (alert "ss_pre_filter_set_xx_ = nil !!")
      )
    ;
    ;post_var_setting_
      (command "ucs" "p" "")
      (setvar "osmode" 1215)
    ;
  )
;

;temp_main_command_
   (defun c:rectangle_to_block ()
    
    ;sub_func
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
    ;selection_set_
      (setq ssset_ (ssget))
    ;
    ;preloop_and_while_ 
      (setq ssset_i 0)
      (while (< ssset_i (sslength ssset_)) 
        (setq ssset_obj (vlax-ename->vla-object (ssname ssset_ ssset_i) ))
        (setq ssset_get_center_ (LM:PolyCentroid (ssname ssset_ ssset_i)))
        (vla-erase ssset_obj)
        (setq ef_name "OBL_5.2x20mm.")
        (command "insert" ef_name ssset_get_center_ 1 0)
        (setq ssset_i (+ ssset_i 1))
      )
    ;
  )
;





















;Based on code by Tharwat, http://www.autolisp.com/forum/threads/774-linetype-change?p=2696&viewfull=1#post2696
  (vl-load-com)
  (defun c:LtMerge (/ *error* lto ltn fnd ss i sn name blocks) ;โค้ดอะไร ของใครจำไม่ได้

  (defun *error* (errmsg)
      (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
        (princ (strcat "\nError: " errmsg)))
      (vla-endundomark acdoc)
      (princ))
    
    (if (not acdoc) (setq acdoc (vla-get-activedocument (vlax-get-acad-object))))
    (vla-startundomark acdoc)
    
    (if (and (not (eq (setq lto (strcase (getstring "\nSpecify OLD lineType: "))) ""))
      (setq fnd (tblsearch "LTYPE" lto))
      (not (eq (setq ltn (strcase (getstring "\nSpecify NEW lineType: "))) ""))
      (setq fnd (tblsearch "LTYPE" ltn))
      (setq ss (ssget "_X"))
      (if (eq (getvar 'CELTYPE) ltn)
        (setvar 'CELTYPE ltn)
        T)
        )
      (repeat (setq i (sslength ss))
        (setq sn (ssname ss (setq i (1- i))))
        (if (eq (cdr (assoc 6 (entget sn))) lto)
    (vla-put-linetype (vlax-ename->vla-object sn) ltn))
        (if
    (and (eq (cdr (assoc 0 (entget sn))) "INSERT")
        (not (member (setq name (cdr (assoc 2 (entget sn)))) blocks))
    )
    (progn
      (setq blocks (cons name blocks))
      (vlax-for each (vla-item (vla-get-blocks acdoc) name)
        (if (eq (vla-get-linetype each) lto)
          (vla-put-linetype each ltn)))
    )
        )
      )
    )
    (command "_.-PURGE" "_lt" lto "_n")
      
    (cond	((not ltn) (princ "\n LineType is nil "))
    ((not fnd)
    (princ "\n Couldn't find the Linetype or not loaded "))
    )

    (vla-regen acdoc AcallViewports)
    (vla-endundomark acdoc)
    (princ)
  )
;
