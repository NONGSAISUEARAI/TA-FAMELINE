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
;sub_func [some_sub_func_depend_on_project]
  (defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )
;
(defunc c:rr1_reset_rotation_line ()
  (setq my_line_set (ssget  '((0 . "LINE"))))
  (setq total_my_line_set (sslength my_line_set))

    (setq i_line_ro 0)
    (while ;reset_rotation_line
      (setq line_ename (ssname my_line_set i_line_ro))
      (setq line_ename_obj (vlax-ename->vla-object line_ename))

      (setq line_ename_obj_start_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-startpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_end_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-endpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_start_x (car line_ename_obj_start_xyz))
      (setq line_ename_obj_start_y (cadr line_ename_obj_start_xyz))
      (setq line_ename_obj_end_y (cadr line_ename_obj_end_xyz))
      (setq line_ename_obj_length (vla-get-length line_ename_obj))
      (setq line_ename_obj_length_Divide (/ line_ename_obj_length 2))


        
        (if 
          (or 
            (> line_ename_obj_start_y line_ename_obj_end_y)
            ; (= (vla-get-rotate line_ename_obj) 90.0)
          )
          (progn
            (setq new_line_ename_obj_start_y (- line_ename_obj_start_y line_ename_obj_length_Divide))
            (setq new_line_ename_obj_start_xy (strcat (rtos line_ename_obj_start_x 2 8) "," (rtos new_line_ename_obj_start_y 2 8) ))
            (command "rotate" line_ename "" new_line_ename_obj_start_xy 180)
          )
        )
        
        (setq i_line_ro (+ i_line_ro 1))
    )
    (setq i_line_ins 0)
    (while ;ins_dynamic_block
      (setq line_ename (ssname my_line_set i_line_ins))
      (setq line_ename_obj (vlax-ename->vla-object line_ename))
      (setq existing_rotate 
        (LM:round 
          (* (/ (vla-get-angle line_ename_obj) 3.14) 180)
        )
      )

      (setq line_ename_obj_start_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-startpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_end_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-endpoint line_ename_obj)
                                )
                              )
      )
      (setq line_ename_obj_start_x (car line_ename_obj_start_xyz))
      (setq line_ename_obj_start_y (cadr line_ename_obj_start_xyz))
      (setq line_ename_obj_end_y (cadr line_ename_obj_end_xyz))
      (setq line_ename_obj_length (vla-get-length line_ename_obj))
      (setq line_ename_obj_length_Divide (/ line_ename_obj_length 2))
      (setq line_ename_obj_start_xy (strcat (rtos line_ename_obj_start_x 2 8) "," (rtos line_ename_obj_start_y 2 8)))

        
        (if 
          (or 
            ; (> line_ename_obj_start_y line_ename_obj_end_y)
            (= existing_rotate 90.0)
          )
          (progn
            (setq dynamic_name "001 - DYNAMIC LV")
            (command "insert" dynamic_name line_ename_obj_start_xy 1 0)
            (setq dynamic_last (entlast))
            (setq dynamic_last_obj (vlax-ename->vla-object dynamic_last))
        
            (setq H (LM:setdynpropvalue dynamic_last_obj "H" line_ename_obj_length))
            (setq W (LM:setdynpropvalue dynamic_last_obj "W" 25))
          )
        )
        
        
        (setq i_line_ins (+ i_line_ins 1))
    )
)
(setq my_line_set (ssget  '((0 . "insert"))))
(command "pselect" my_line_set)


(defun c:ListProperties (/ obj ename propList)
  (setq obj (vlax-ename->vla-object (car (entsel "Select an object:"))))
  (setq obj (car (entsel "Select an object:")))
  (setq obj_get (entget obj))
  (setq obj_obj (vlax-ename->vla-object obj))
  (setq propList (vlax-get-property obj_obj "Distance name"))
  (vlax-get obj_obj "DistName")
  (vlax-property-available-p obj_obj)
  (vlax-Dump-Object obj_obj)
  (setq vertexData (vlax-curve-getStartParam obj))
  (if propList
    (progn
      (prompt "\nProperties:")
      (foreach prop propList
        (prompt (strcat "\n" prop))
      )
    )
    (prompt "\nNo properties available for the selected object.")
  )
  (princ)
)


(defun C:DumpIt ( / ent) 
 (while (setq ent (entsel)) 
   (vlax-Dump-Object 
     (vlax-Ename->Vla-Object (car ent)) 
   ) 
 ) 
 (princ) 
)

