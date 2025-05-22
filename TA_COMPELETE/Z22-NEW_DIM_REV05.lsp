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
;main mode
  (defun c:r90c_ROTALE_90+CPOY ()
    (setq ss_select (ssget "_I"))
    (setq ss_select_total (sslength ss_select))
    ; (setq grid_main (car (entsel)))
    (setq ss_select_i 0)
    (while 
      (< ss_select_i ss_select_total)
      (setq grid_main (ssname ss_select ss_select_i))
      (setq grid_main_obj (vlax-ename->vla-object grid_main))
      (setq grid_ins_xy 
        (list
          (car (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint grid_main_obj))))
          (cadr (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint grid_main_obj))))
        )
      )
      (setq H (LM:getdynpropvalue grid_main_obj "H"))
      (setq OFFSET (LM:getdynpropvalue grid_main_obj "OFFSET"))
      
      (command "insert" "000-GRID_LINE_DYN" grid_ins_xy 1 0)
      (setq new (entlast))
      (setq _pi  3.14285714285714)
      (setq new_angle (LM:round (*(/ (vla-get-rotation (vlax-ename->vla-object new)) _pi) 180)))
      
      (LM:SETDYNPROPVALUE (vlax-ename->vla-object new) "H" H)
      (LM:SETDYNPROPVALUE (vlax-ename->vla-object new) "OFFSET" OFFSET)

      (setq new_angle (vla-put-rotation (vlax-ename->vla-object new) 1.570796326794896))

    
      (setq ss_select_i (+ ss_select_i 1))
    )
  )
  (defun c:mg_move_grid ()
    (setq mv_main (entget (car (entsel "\n OK GO"))))
    (setq mv_assoc_ename (assoc -1 mv_main))
    (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
    (setq mv_ename (cdr mv_assoc_ename))
    
    (setq mv_obj (vlax-ename->vla-object mv_ename))
    (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
    (setq mv_obj_ins_x (car mv_obj_ins_xy))
    (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
    (setq mv_obj_ins_z 0)
    
    (setq finding_xy (getpoint))
    (setq finding_x (car finding_xy))
    (setq fvc (strcat (rtos finding_x 2 8) "," (rtos mv_obj_ins_y 2 8) "," (rtos mv_obj_ins_z 2 8)))
    
    (command "copy" mv_ename "" mv_obj_ins_xy fvc)
    
    (c:mg_move_grid_last)
  
  )
  (defun c:mg_move_grid_last ()
    (setq p 1)
    (setq pp 10)
    (while 
      (< p pp)
      (setq mv_main_last (entlast))
      ; (setq mv_main (entget (car (entsel "\n OK GO"))))
      
      ; (setq mv_assoc_ename (assoc -1 mv_main))
      ; (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
      ; (setq mv_ename (cdr mv_assoc_ename))
      
      (setq mv_obj (vlax-ename->vla-object mv_main_last))
      (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
      (setq mv_obj_ins_x (car mv_obj_ins_xy))
      (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
      (setq mv_obj_ins_z 0)
      
      (setq finding_xy (getpoint))
      (setq finding_x (car finding_xy))
      
      
      
      (setq fvc (strcat (rtos finding_x 2 8) "," (rtos mv_obj_ins_y 2 8) "," (rtos mv_obj_ins_z 2 8)))
      
      (command "copy" mv_main_last "" mv_obj_ins_xy fvc "")
      (setq p (+ p 1))
    )  
  )
  (defun c:mvx_reset_x_grid ()
    
    (setq mv_main (entget (car (entsel "\nSELECT \nMAIN \nFOR \nMAKE \nPLATTERN"))))
    
    (setq mv_assoc_ename (assoc -1 mv_main))
    (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
    (setq mv_ename (cdr mv_assoc_ename))
    
    (setq mv_obj (vlax-ename->vla-object mv_ename))
    (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint mv_obj))))
    (setq mv_obj_ins_x (car mv_obj_ins_xy))
    (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
    (setq mv_obj_ins_z 0)
    
      ;sub_func_for_fillter_effectivename_and_angle
        (setq my-efname-set (ssget  '((0 . "INSERT"))))
        (setq total_ssget (sslength my-efname-set))
        (setq ie 0)
        (setq ename-list '())
        (setq ef_name "000-GRID_LINE_DYN")
          (while 
            (< ie total_ssget)
            (setq blk_obj (ssname my-efname-set ie))
            (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
            (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
            (setq fixed_angle (fix rotation_to_angle) )
            
            (setq ss (LM:effectivename blk_obj_Set))

            (if (and (= ef_name ss) (or (= fixed_angle 0) (= fixed_angle 180)))
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
          (setq mySet ff )
      ;sub_func_for_fillter_effectivename_and_angle
    
    (setq entityCount (sslength mySet))
      (setq i 0)
    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq ejc (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
      (setq o (vlax-ename->vla-object ejc))
      (setq o_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o))))
      (setq jx_x (car o_ins_xy))
      (setq jx_y (cadr o_ins_xy))
      (setq jx_z 0)

      (setq insertionPoint (vlax-3d-point jx_x mv_obj_ins_y jx_z))
      (vla-put-insertionpoint o insertionPoint)
      (setq i (+ i 1)) 
    ) 
  )
  (defun c:mvy_reset_y_grid ()
    
    (setq mv_main (entget (car (entsel "\n NEWJEAN MAIN"))))
    
    (setq mv_assoc_ename (assoc -1 mv_main))
    (setq mv_assoc_cdr_ename (cdr (assoc -1 mv_main)))
    (setq mv_ename (cdr mv_assoc_ename))
    
    (setq mv_obj (vlax-ename->vla-object mv_ename))
    (setq mv_obj_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint mv_obj))))
    (setq mv_obj_ins_x (car mv_obj_ins_xy))
    (setq mv_obj_ins_y (cadr mv_obj_ins_xy))
    (setq mv_obj_ins_z 0)
    
      ;sub_func_for_fillter_effectivename_and_angle
        (setq my-efname-set (ssget  '((0 . "INSERT"))))
        (setq total_ssget (sslength my-efname-set))
        (setq ie 0)
        (setq ename-list '())
        (setq ef_name "000-GRID_LINE_DYN")
          (while 
            (< ie total_ssget)
            (setq blk_obj (ssname my-efname-set ie))
            (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
            (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
            (setq fixed_angle (fix rotation_to_angle) )
            
            (setq ss (LM:effectivename blk_obj_Set))

            (if (and (= ef_name ss) (or (= fixed_angle 90) (= fixed_angle 270)))
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
          (setq mySet ff )
      ;sub_func_for_fillter_effectivename_and_angle
    
    (setq entityCount (sslength mySet))
      (setq i 0)
    (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq ejc (ssname mySet i)) ; ดึง entity ที่ลำดับ i จาก mySet
      (setq o (vlax-ename->vla-object ejc))
      (setq o_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o))))
      (setq jx_x (car o_ins_xy))
      (setq jx_y (cadr o_ins_xy))
      (setq jx_z 0)

      (setq insertionPoint (vlax-3d-point mv_obj_ins_x jx_y jx_z))
      (vla-put-insertionpoint o insertionPoint)
      (setq i (+ i 1)) 
    ) 
  )
  (defun c:ddxx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
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
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:ddyy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
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
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hhxx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
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
              (setq o_ii3_ins_y (+ o_ii_ins_y_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))          
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hhyy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
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
              (setq o_ii3_ins_x (- o_ii_ins_x_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))        
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hmx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
      (setq sorted-enames (sort-entities-by-x my-selection-set))
      (setq total-entities (length sorted-enames))
      (setq i1st 0)
      (setq iilast (- total-entities 1))
    
            (setq total-entities (length sorted-enames))
            
            ; (while (< i total-entities)
              ; part i
              (setq entity_i (nth i1st sorted-enames))
                (setq o_i (vlax-ename->vla-object entity_i))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
              
              ; part ii
              (setq entity_ii (nth iilast sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_y (+ o_ii_ins_y_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))          
            
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hmy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getreal (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
      (setq sorted-enames (sort-entities-by-x my-selection-set))
      (setq total-entities (length sorted-enames))
      (setq i1st 0)
      (setq iilast (- total-entities 1))
    
            (setq total-entities (length sorted-enames))
            
              ; (while (< i total-entities)
                ; part i
              (setq entity_i (nth i1st sorted-enames))
                (setq o_i (vlax-ename->vla-object entity_i))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
              
              ; part ii
              (setq entity_ii (nth iilast sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_x (- o_ii_ins_x_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new o_ii_ins_xy_new o_ii3_ins_xy_new)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))        
            
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;mirror mode
  (defun c:llxx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
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
                (setq h1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
              
              ; part ii
              (setq entity_ii (nth ii sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq h2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_y (+ o_ii_ins_y_off OF_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))
              
              ;part i+ii low offset dim
              (setq total_h (+ h1 OFFSET OFFSET))
              (setq o_ii4_ins_y_low (- o_ii_ins_y_off total_h))
              (setq o_ii4_ins_y_offset_low (- o_ii4_ins_y_low OF_DIM))
              (setq o_i_ins_xy_new_low (strcat (rtos o_i_ins_x 2 8) "," (rtos o_ii4_ins_y_low 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_low 2 8) ))
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_offset_low 2 8) ))
              
              

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)
              
              ; (setq o_i_ins_mirror (strcat (rtos o_i_ins_x 2 8) "," (setq o_i_ins_mirror_y (rtos (- (+ o_i_ins_y OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8))))
              ; (setq o_ii_ins_mirror (strcat (rtos o_ii_ins_x 2 8) "," (setq o_ii_ins_mirror_y (rtos (- (+ o_ii_ins_y OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8))))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_y (atof o_i_ins_mirror_y))
              ; (setq real_o_ii_ins_mirror_y (atof o_ii_ins_mirror_y))
              ; (setq point3 (vlax-3d-point o_i_ins_x real_o_i_ins_mirror_y 0)
              ;       point4 (vlax-3d-point o_ii_ins_x real_o_ii_ins_mirror_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))          
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:llyy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
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
                (setq h1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
              
              ; part ii
              (setq entity_ii (nth ii sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq h2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_x (- o_ii_ins_x_off OF_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ;part i+ii low offset dim
              (setq total_h (+ h1 OFFSET OFFSET))
              (setq o_ii4_ins_x_low (+ o_ii_ins_x_off total_h))
              (setq o_ii4_ins_x_offset_low (+ o_ii4_ins_x_low OF_DIM))
              (setq o_i_ins_xy_new_low (strcat (rtos o_ii4_ins_x_low 2 8) "," (rtos o_i_ins_y 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_ii4_ins_x_low 2 8) "," (rtos o_ii_ins_y 2 8) ))
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii4_ins_x_offset_low 2 8) "," (rtos o_ii4_ins_y_offset_low 2 8) ))
              
              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)
              
              ; (setq o_i_ins_mirror (strcat (setq o_i_ins_mirror_x (rtos (+ (- o_i_ins_x OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8)) "," (rtos o_i_ins_y 2 8)))
              ; (setq o_ii_ins_mirror (strcat (setq o_ii_ins_mirror_x (rtos (+ (- o_ii_ins_x OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8)) "," (rtos o_ii_ins_y 2 8)))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_x (atof o_i_ins_mirror_x))
              ; (setq real_o_ii_ins_mirror_x (atof o_ii_ins_mirror_x))
            
              ; (setq point3 (vlax-3d-point real_o_i_ins_mirror_x o_i_ins_y 0)
              ;       point4 (vlax-3d-point real_o_ii_ins_mirror_x o_ii_ins_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))        
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hlxx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
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
                (setq h1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
              
              ; part ii
              (setq entity_ii (nth ii sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq h2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_y (+ o_ii_ins_y_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))

              ;part i+ii low offset dim
              (setq total_h1 (+ h1 OFFSET OFFSET))
              (setq total_h2 (+ h2 OFFSET OFFSET))
              
              (setq o_i4_ins_y_low (- o_i_ins_y_off total_h1))
              (setq o_ii4_ins_y_low (- o_ii_ins_y_off total_h2))
              
              (setq o_i4_ins_y_offset_low (- o_i4_ins_y_low HI_DIM))
              (setq o_ii4_ins_y_offset_low (- o_ii4_ins_y_low HI_DIM))
              
              (setq o_i_ins_xy_new_low (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i4_ins_y_low 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_low 2 8) ))
              
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_offset_low 2 8) ))
              
              

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)
              
              ; (setq o_i_ins_mirror (strcat (rtos o_i_ins_x 2 8) "," (setq o_i_ins_mirror_y (rtos (- (+ o_i_ins_y OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8))))
              ; (setq o_ii_ins_mirror (strcat (rtos o_ii_ins_x 2 8) "," (setq o_ii_ins_mirror_y (rtos (- (+ o_ii_ins_y OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8))))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_y (atof o_i_ins_mirror_y))
              ; (setq real_o_ii_ins_mirror_y (atof o_ii_ins_mirror_y))
              ; (setq point3 (vlax-3d-point o_i_ins_x real_o_i_ins_mirror_y 0)
              ;       point4 (vlax-3d-point o_ii_ins_x real_o_ii_ins_mirror_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))          
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hlyy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
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
                (setq h1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
              
              ; part ii
              (setq entity_ii (nth ii sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq h2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_x (- o_ii_ins_x_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ;part i+ii low offset dim
              (setq total_h (+ h1 OFFSET OFFSET))
              (setq o_ii4_ins_x_low (+ o_ii_ins_x_off total_h))
              (setq o_ii4_ins_x_offset_low (+ o_ii4_ins_x_low HI_DIM))
              (setq o_i_ins_xy_new_low (strcat (rtos o_ii4_ins_x_low 2 8) "," (rtos o_i_ins_y 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_ii4_ins_x_low 2 8) "," (rtos o_ii_ins_y 2 8) ))
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii4_ins_x_offset_low 2 8) "," (rtos o_ii4_ins_y_offset_low 2 8) ))
              
              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)
              
              ; (setq o_i_ins_mirror (strcat (setq o_i_ins_mirror_x (rtos (+ (- o_i_ins_x OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8)) "," (rtos o_i_ins_y 2 8)))
              ; (setq o_ii_ins_mirror (strcat (setq o_ii_ins_mirror_x (rtos (+ (- o_ii_ins_x OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8)) "," (rtos o_ii_ins_y 2 8)))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_x (atof o_i_ins_mirror_x))
              ; (setq real_o_ii_ins_mirror_x (atof o_ii_ins_mirror_x))
            
              ; (setq point3 (vlax-3d-point real_o_i_ins_mirror_x o_i_ins_y 0)
              ;       point4 (vlax-3d-point real_o_ii_ins_mirror_x o_ii_ins_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
              
              (setq i (+ i 1))
              (setq ii (+ i 1))        
            )
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hlmx ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getreal (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
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
    
    ;making_dim_
      (setq my-selection-set ff )
      (setq sorted-enames (sort-entities-by-x my-selection-set))
      (setq total-entities (length sorted-enames))
      (setq i1st 0)
      (setq iilast (- total-entities 1))
    
            (setq total-entities (length sorted-enames))
            
            ; (while (< i total-entities)
              ; part i
                (setq entity_i (nth i1st sorted-enames))
                  (setq o_i (vlax-ename->vla-object entity_i))
                  (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                  (setq H1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                  (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                  (setq o_i_ins_x (car o_i_ins_xy))
                  (setq o_i_ins_y (cadr o_i_ins_xy))
                  (setq o_i_ins_y_off (+ (cadr o_i_ins_xy) OFFSET))
                (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i_ins_y_off 2 8) ))
              
              ; part ii
                (setq entity_ii (nth iilast sorted-enames))
                  (setq o_ii (vlax-ename->vla-object entity_ii))
                  (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                  (setq H2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                  (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                  (setq o_ii_ins_x (car o_ii_ins_xy))
                  (setq o_ii_ins_y (cadr o_ii_ins_xy))
                  (setq o_ii_ins_y_off (+ (cadr o_ii_ins_xy) OFFSET))
                (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii_ins_y_off 2 8) ))
              
              ; part i+ii offset dim
                (setq o_ii3_ins_y (+ o_ii_ins_y_off HI_DIM))
                (setq o_ii3_ins_xy_new (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii3_ins_y 2 8) ))
    
              ;part i+ii low offset dim
              (setq total_h1 (+ h1 OFFSET OFFSET))
              (setq total_h2 (+ h2 OFFSET OFFSET))
              
              (setq o_i4_ins_y_low (- o_i_ins_y_off total_h1))
              (setq o_ii4_ins_y_low (- o_ii_ins_y_off total_h2))
              
              (setq o_i4_ins_y_offset_low (- o_i4_ins_y_low HI_DIM))
              (setq o_ii4_ins_y_offset_low (- o_ii4_ins_y_low HI_DIM))
              
              (setq o_i_ins_xy_new_low (strcat (rtos o_i_ins_x 2 8) "," (rtos o_i4_ins_y_low 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_low 2 8) ))
              
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii_ins_x 2 8) "," (rtos o_ii4_ins_y_offset_low 2 8) ))
              
              

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)
    
              ; (setq o_i_ins_mirror (strcat (rtos o_i_ins_x 2 8) "," (setq o_i_ins_mirror_y (rtos (- (+ o_i_ins_y OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8))))
              ; (setq o_ii_ins_mirror (strcat (rtos o_ii_ins_x 2 8) "," (setq o_ii_ins_mirror_y (rtos (- (+ o_ii_ins_y OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8))))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_y (atof o_i_ins_mirror_y))
              ; (setq real_o_ii_ins_mirror_y (atof o_ii_ins_mirror_y))
              ; (setq point3 (vlax-3d-point o_i_ins_x real_o_i_ins_mirror_y 0)
              ;       point4 (vlax-3d-point o_ii_ins_x real_o_ii_ins_mirror_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
            
              
              
              (setq i (+ i 1))
              (setq ii (+ i 1))          
            
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
  (defun c:hlmy ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    ;sub_func_for_fillter_effectivename
    
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
    ;sub_func_for_sorting_part
    
    ;making_dim_
      (setq my-selection-set ff )
      (setq sorted-enames (sort-entities-by-x my-selection-set))
      (setq total-entities (length sorted-enames))
      (setq i1st 0)
      (setq iilast (- total-entities 1))
    
            (setq total-entities (length sorted-enames))
            
              ; (while (< i total-entities)
                ; part i
              (setq entity_i (nth i1st sorted-enames))
                (setq o_i (vlax-ename->vla-object entity_i))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "OFFSET"))
                (setq H1 (LM:getdynpropvalue (vlax-ename->vla-object entity_i) "H"))
                (setq o_i_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_i))))
                (setq o_i_ins_x (car o_i_ins_xy))
                (setq o_i_ins_y (cadr o_i_ins_xy))
                (setq o_i_ins_x_off (- (car o_i_ins_xy) OFFSET))
              (setq o_i_ins_xy_new (strcat (rtos o_i_ins_x_off 2 8) "," (rtos o_i_ins_y 2 8) ))
              
              ; part ii
              (setq entity_ii (nth iilast sorted-enames))
                (setq o_ii (vlax-ename->vla-object entity_ii))
                (setq OFFSET (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "OFFSET"))
                (setq H2 (LM:getdynpropvalue (vlax-ename->vla-object entity_ii) "H"))
                (setq o_ii_ins_xy (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o_ii))))
                (setq o_ii_ins_x (car o_ii_ins_xy))
                (setq o_ii_ins_y (cadr o_ii_ins_xy))
                (setq o_ii_ins_x_off (- (car o_ii_ins_xy) OFFSET))
              (setq o_ii_ins_xy_new (strcat (rtos o_ii_ins_x_off 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ; part i+ii offset dim
              (setq o_ii3_ins_x (- o_ii_ins_x_off HI_DIM))
              (setq o_ii3_ins_xy_new (strcat (rtos o_ii3_ins_x 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              ;part i+ii low offset dim
              (setq total_h1 (+ h1 OFFSET OFFSET))
              (setq total_h2 (+ h2 OFFSET OFFSET))
              
              (setq o_i4_ins_x_low (+ o_i_ins_x_off total_h1))
              (setq o_ii4_ins_x_low (+ o_ii_ins_x_off total_h2))
              
              (setq o_i4_ins_x_offset_low (+ o_i4_ins_x_low HI_DIM))
              (setq o_ii4_ins_x_offset_low (+ o_ii4_ins_x_low HI_DIM))
            
              (setq o_i_ins_xy_new_low (strcat (rtos o_i4_ins_x_low 2 8) "," (rtos o_i_ins_y 2 8) ))
              (setq o_ii_ins_xy_new_low (strcat (rtos o_i4_ins_x_low 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              (setq o_ii4_ins_xy_new_low (strcat (rtos o_ii4_ins_x_offset_low 2 8) "," (rtos o_ii_ins_y 2 8) ))
              
              

              (command "dimstyle" "r" "DIM E-E")
              (setvar "DIMSCALE" sc)
              (command "dimlinear" o_i_ins_xy_new_low o_ii_ins_xy_new_low o_ii4_ins_xy_new_low)

              ; (setq o_i_ins_mirror (strcat (setq o_i_ins_mirror_x (rtos (+ (- o_i_ins_x OFFSET) (/ (+ OFFSET h1 OFFSET) 2)) 2 8)) "," (rtos o_i_ins_y 2 8)))
              ; (setq o_ii_ins_mirror (strcat (setq o_ii_ins_mirror_x (rtos (+ (- o_ii_ins_x OFFSET) (/ (+ OFFSET h2 OFFSET) 2)) 2 8)) "," (rtos o_ii_ins_y 2 8)))
              ; (setq dim (entlast))
              ; (setq dim_obj (vlax-ename->vla-object dim))
              ; (setq real_o_i_ins_mirror_x (atof o_i_ins_mirror_x))
              ; (setq real_o_ii_ins_mirror_x (atof o_ii_ins_mirror_x))
            
              ; (setq point3 (vlax-3d-point real_o_i_ins_mirror_x o_i_ins_y 0)
              ;       point4 (vlax-3d-point real_o_ii_ins_mirror_x o_ii_ins_y 0)
              ; )
              ; (vla-mirror dim_obj point3 point4)
              ; (vla-delete dim_obj)
            
              (setq i (+ i 1))
              (setq ii (+ i 1))        
            
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Chaning Dim Part
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
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))    
      
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
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
      
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
        (setq new_s0_dimst (vla-put-TextMovement s0 "1"))
      
      (setq i (1+ i))
    )  
  )
;

;radius_mode
  (defun c:MDR_MULTI_DIM_RADIUS( / ss i e p r )
  (setq scl (cond ( (getdist (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
  (setq sc (* scl 10))
  (setvar "dimscale" sc)
  (if (setq ss (ssget '((0 . "arc"))))
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i)))
            p (cdr (assoc 10 (entget e)))
            r (cdr (assoc 40 (entget e)))
      )
      (vl-cmdf "_.dimradius" (list e (polar p 0 r)) "_non" (polar p 0 r))
    )
  )
  (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












(defun c:d2test_str-end ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 0))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
    ;sub_func_for_sorting_part
      (defun sort_by_X (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
      )
      (defun sort_by_y (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
      (defun sort_by_number (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadddr a) (cadddr b))))))
      )
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
    
    ;making_dim_
      
      (setq my-selection-set ff )
      (setq sorted-enames_ (sort-entities-by-x my-selection-set))
      (setq total-entities_ (length sorted-enames_))

      (setq sorted-enames_i 0)
      (setq sorted-enames_total (length sorted-enames_))
      (setq sorted-enames_bot_ins_xyz ())  
  
      (while
        (< sorted-enames_i sorted-enames_total )
        (setq sorted-enames_ename (nth sorted-enames_i sorted-enames_))
        ; (command "pselect" (nth 2 sorted-enames_) "")
        (setq sorted-enames_obj (vlax-ename->vla-object sorted-enames_ename))
        (setq H (LM:getdynpropvalue sorted-enames_obj "H"))
        (setq OFFSET (LM:getdynpropvalue sorted-enames_obj "OFFSET"))
        (setq sorted-enames_ins_xyz 
          (list
                (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))              ;crood_x
            (-  (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj)))) H OFFSET)   ;crood_y
                (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))            ;crood_z
                sorted-enames_i
          )
        )
        ; (cadddr sorted-enames_ins_xyz)

        (setq sorted-enames_bot_ins_xyz (cons sorted-enames_ins_xyz sorted-enames_bot_ins_xyz))
        (setq bot_ins_xyz_newsort_ (sort_by_number sorted-enames_bot_ins_xyz))
        (setq bot_ins_xyz_newsort_total (length bot_ins_xyz_newsort_))
        (setq sorted-enames_i (+ sorted-enames_i 1))
      )
  
      
      (setq bot_ins_xyz_newsort_max_y (nth 0 (sort_by_y sorted-enames_bot_ins_xyz)))
  
      (setq start_bot_ins_xyzno_newsort_ (nth 0 bot_ins_xyz_newsort_))
      (setq end_bot_ins_xyzno_newsort_ (nth (- bot_ins_xyz_newsort_total 1) bot_ins_xyz_newsort_))
  
      (setq start_bot_ins_xyz_newsort_ (vl-remove  (nth 3 start_bot_ins_xyzno_newsort_) start_bot_ins_xyzno_newsort_))
      (setq end_bot_ins_xyz_newsort_ (vl-remove  (nth 3 end_bot_ins_xyzno_newsort_) end_bot_ins_xyzno_newsort_))
      (setq str-end_ins_xyz (sort_by_X (append (list start_bot_ins_xyz_newsort_) (list end_bot_ins_xyz_newsort_))))
      (setq offset_bot_ins_xyz_newsort_ '())
      (command "dimstyle" "r" "DIM E-E")
      (setvar "DIMSCALE" sc)
              
      (cond
        (
          (and 
            (<= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
            (<= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_bot_ins_xyz_newsort_
              (list
                (nth 0 bot_ins_xyz_newsort_max_y)
                (- (nth 1 bot_ins_xyz_newsort_max_y) HI_DIM)
                (nth 2 bot_ins_xyz_newsort_max_y)
              )
            )
          )
          (command "dimlinear" start_bot_ins_xyz_newsort_ end_bot_ins_xyz_newsort_  "h" offset_bot_ins_xyz_newsort_)
        )
        (
          (and 
            (>= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
            (>= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_bot_ins_xyz_newsort_
              (list
                (nth 0 bot_ins_xyz_newsort_max_y)
                (- (nth 1 bot_ins_xyz_newsort_max_y) HI_DIM)
                (nth 2 bot_ins_xyz_newsort_max_y)
              )
            )
          )
          (command "dimlinear" start_bot_ins_xyz_newsort_ end_bot_ins_xyz_newsort_ "h" offset_bot_ins_xyz_newsort_ )
        )
      )
      ; (command "dimstyle" "r" "DIM E-E")
      ; (setvar "DIMSCALE" sc)
      ; (command "dimlinear" start_bot_ins_xyz_newsort_ end_bot_ins_xyz_newsort_ offset_bot_ins_xyz_newsort_ )
    
                        
            
  
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  
)
(defun c:d3test_str-end ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (= fixed_angle 90))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
    ;sub_func_for_sorting_part
      (defun sort_by_X (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
      )
      (defun sort_by_y (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
      (defun sort_by_number (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadddr a) (cadddr b))))))
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

        ; (reverse sorted-ename-list)
        
        ; (command "pselect" (nth 0 sorted-ename-list) "")
      )
    ;sub_func_for_sorting_part
    
    ;making_dim_
      
      (setq my-selection-set ff )
      (setq sorted-enames_ (sort-entities-by-x my-selection-set))
      (setq total-entities_ (length sorted-enames_))

      (setq sorted-enames_i 0)
      (setq sorted-enames_total (length sorted-enames_))
      (setq sorted-enames_bot_ins_xyz ())  
  
      (while
        (< sorted-enames_i sorted-enames_total )
        (setq sorted-enames_ename (nth sorted-enames_i sorted-enames_)) 
        ; (command "pselect" (nth 0 sorted-enames_) "")  
        (setq sorted-enames_obj (vlax-ename->vla-object sorted-enames_ename))
        (setq H (LM:getdynpropvalue sorted-enames_obj "H"))
        (setq OFFSET (LM:getdynpropvalue sorted-enames_obj "OFFSET"))
        (setq sorted-enames_ins_xyz 
          (list
             (+ (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj)))) H OFFSET)    ;crood_x
                (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))             ;crood_y
                (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))            ;crood_z
                sorted-enames_i
          )
        )
        ; (cadddr sorted-enames_ins_xyz)

        (setq sorted-enames_bot_ins_xyz (cons sorted-enames_ins_xyz sorted-enames_bot_ins_xyz))
          
        (setq bot_ins_xyz_newsort_total (length bot_ins_xyz_newsort_))
        (setq sorted-enames_i (+ sorted-enames_i 1))
      )
      
      (setq bot_ins_xyz_newsort_ (sort_by_number sorted-enames_bot_ins_xyz))
      (setq bot_ins_xyz_newsort_total (length bot_ins_xyz_newsort_))
      (setq bot_ins_xyz_newsort_max_x (nth 0 (reverse (sort_by_x sorted-enames_bot_ins_xyz))))
  
      (setq start_bot_ins_xyzno_newsort_ (nth 0 bot_ins_xyz_newsort_))
      (setq end_bot_ins_xyzno_newsort_ (nth (- bot_ins_xyz_newsort_total 1) bot_ins_xyz_newsort_))
  
      (setq start_bot_ins_xyz_newsort_ (vl-remove  (nth 3 start_bot_ins_xyzno_newsort_) start_bot_ins_xyzno_newsort_))
      (setq end_bot_ins_xyz_newsort_ (vl-remove  (nth 3 end_bot_ins_xyzno_newsort_) end_bot_ins_xyzno_newsort_))
  
      (setq str-end_ins_xyz (sort_by_X (append (list start_bot_ins_xyz_newsort_) (list end_bot_ins_xyz_newsort_))))
      (setq offset_bot_ins_xyz_newsort_ '())
      (command "dimstyle" "r" "DIM E-E")
      (setvar "DIMSCALE" sc)
              
      (cond
        (
          (and 
            (<= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
            (<= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_bot_ins_xyz_newsort_
              (list
                (+ (nth 0 bot_ins_xyz_newsort_max_x) HI_DIM)
                (nth 1 bot_ins_xyz_newsort_max_x)
                (nth 2 bot_ins_xyz_newsort_max_x)
              )
            ) 
            (command "dimlinear" end_bot_ins_xyz_newsort_  start_bot_ins_xyz_newsort_ "v" offset_bot_ins_xyz_newsort_)
            ; (command "dimlinear" start_bot_ins_xyz_newsort_ end_bot_ins_xyz_newsort_ )
          )
          (princ "\n")
        )
        (
          (and 
            (>= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
            (>= (cadr (car str-end_ins_xyz)) (cadr (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_bot_ins_xyz_newsort_
              (list
                (+ (nth 0 bot_ins_xyz_newsort_max_x) HI_DIM)
                (nth 1 bot_ins_xyz_newsort_max_x)
                (nth 2 bot_ins_xyz_newsort_max_x)
              )
            )
            (command "dimlinear" end_bot_ins_xyz_newsort_ start_bot_ins_xyz_newsort_ "v" offset_bot_ins_xyz_newsort_ )
          )
          
         (princ "\n")
        )
      )
      ; (command "dimstyle" "r" "DIM E-E")
      ; (setvar "DIMSCALE" sc)
      ; (command "dimlinear" start_bot_ins_xyz_newsort_ end_bot_ins_xyz_newsort_ offset_bot_ins_xyz_newsort_ )
    
                        
            
  
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  
)
(defun c:d5test_str-end ()
    ;sub_func
      (c:ucs_world)
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
    ;sub_func
    
    ; เริ่ม part คำนวณ SCALE
      (setq scl (cond ( (getint (strcat "\nSpecify Dim Scale Overall <" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
      (setq sc (* scl 10)) ; 5 = 50 2 = 20
      (setq of_point 4); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq hi_point 9); OF_DIM OFFSET FROM pt2 = 6 mm.
      (setq OF_DIM (* (* scl 1) of_point))
      (setq HI_DIM (* (* scl 1) hi_point))
    ; จบ part คำนวณ SCALE
    
    ;sub_func_for_fillter_effectivename
      (setq my-efname-set (ssget  '((0 . "INSERT"))))
      (setq total_ssget (sslength my-efname-set))
      (setq ie 0)
      (setq ename-list '())
      (setq ef_name "000-GRID_LINE_DYN")
        (while 
          (< ie total_ssget)
          (setq blk_obj (ssname my-efname-set ie))
          (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
          (setq rotation_to_angle (*(/ (vla-get-rotation blk_obj_Set) 3.14) 180))
          (setq fixed_angle (fix rotation_to_angle) )
          
          (setq ss (LM:effectivename blk_obj_Set))

          (if (and (= ef_name ss) (/= fixed_angle 90) (/= fixed_angle 180) (/= fixed_angle 0) (/= fixed_angle 270))
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
        (setq my-selection-set ff )
    
    ;sub_func_for_fillter_effectivename
    
    ;sub_func_for_sorting_part
      (defun sort_by_X (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
      )
      (defun sort_by_y (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
      (defun sort_by_number (list_) ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadddr a) (cadddr b))))))
      )
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
    
    ;making_dim_
      
      (setq my-selection-set ff )
      (setq sorted-enames_ (sort-entities-by-x my-selection-set))
      (setq total-entities_ (length sorted-enames_))

      (setq sorted-enames_i 0)
      (setq sorted-enames_total (length sorted-enames_))
      (setq sorted-enames_top_ins_xyz ())  
  
      (while
        (< sorted-enames_i sorted-enames_total )
        (setq sorted-enames_ename (nth sorted-enames_i sorted-enames_)) 
        ; (command "pselect" (nth 4 sorted-enames_) "")
        (setq sorted-enames_obj (vlax-ename->vla-object sorted-enames_ename))
        (setq sorted-enames_rotation (+ 90 (atof (angtos (vla-get-rotation sorted-enames_obj)))))
        (setq H (LM:getdynpropvalue sorted-enames_obj "H"))
        (setq OFFSET (LM:getdynpropvalue sorted-enames_obj "OFFSET"))
        
        ;formula_for_finding_crood_xy
          ; L = OFFSET (dynblock)
          ; x2 = x1 + (L*(cos(x)))
          ; y2 = y1 + (L*(sin(x)))
          (setq Fcos (cos (* sorted-enames_rotation (/ pi 180))))
          (setq Fsin (sin (* sorted-enames_rotation (/ pi 180))))
        
        
        ; (setq sorted-enames_ins_xyz 
        ;   (list
        ;      (+ (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj)))) (* (+ OFFSET HI_DIM) Fcos) )            ;crood_x
        ;      (+ (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))(* (+ OFFSET HI_DIM) Fsin) )            ;crood_y
        ;         (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))                            ;crood_z
        ;         sorted-enames_i
        ;   )
        ; )
        (setq sorted-enames_ins_xyz 
          (list
             (+ (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj)))) (* (+ OFFSET ) Fcos) )            ;crood_x
             (+ (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))(* (+ OFFSET ) Fsin) )            ;crood_y
                (caddr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint sorted-enames_obj))))                            ;crood_z
                sorted-enames_i
          )
        )
        ; (command "point" (vl-remove (nth 3 sorted-enames_ins_xyz) sorted-enames_ins_xyz) )
        ; (cadddr sorted-enames_ins_xyz)

        (setq sorted-enames_top_ins_xyz (cons sorted-enames_ins_xyz sorted-enames_top_ins_xyz))
        (setq top_ins_xyz_newsort_ (sort_by_number sorted-enames_top_ins_xyz))
        (setq top_ins_xyz_newsort_total (length top_ins_xyz_newsort_))
        (setq sorted-enames_i (+ sorted-enames_i 1))
        (setq top_ins_xyz_newsort_i 0)
        (setq top_ins_xyz_newsort_ii 1)
      )
    ;
  ; (setq x1y1 ())
  (while
    (< top_ins_xyz_newsort_i top_ins_xyz_newsort_total)
    (setq top_ins_xyz_newsort_ename_A_xyzno (nth top_ins_xyz_newsort_i top_ins_xyz_newsort_ ))
    (setq top_ins_xyz_newsort_ename_A_xyz (vl-remove (nth 3 top_ins_xyz_newsort_ename_A_xyzno) top_ins_xyz_newsort_ename_A_xyzno))
    (setq top_ins_xyz_newsort_ename_B_xyzno (nth top_ins_xyz_newsort_ii top_ins_xyz_newsort_ ))
    (setq top_ins_xyz_newsort_ename_B_xyz (vl-remove (nth 3 top_ins_xyz_newsort_ename_B_xyzno) top_ins_xyz_newsort_ename_B_xyzno))
    (command "dimlinear" top_ins_xyz_newsort_ename_A_xyz top_ins_xyz_newsort_ename_B_xyz "R" sorted-enames_rotation )
  )
  (command "dimaligned" (59112.1 -27862.0))
      (setq start_top_ins_xyzno_newsort_ (nth 0 top_ins_xyz_newsort_))
      (setq start_top_ins_xyz_newsort_ (vl-remove  (nth 3 start_top_ins_xyzno_newsort_) start_top_ins_xyzno_newsort_))
      (setq end_top_ins_xyzno_newsort_ (nth (- top_ins_xyz_newsort_total 1) top_ins_xyz_newsort_))
      (setq end_top_ins_xyz_newsort_ (vl-remove  (nth 3 end_top_ins_xyzno_newsort_) end_top_ins_xyzno_newsort_))
      (setq str-end_ins_xyz (sort_by_X (append (list start_top_ins_xyz_newsort_) (list end_top_ins_xyz_newsort_))))
      (setq offset_top_ins_xyz_newsort_ '())
      (command "dimstyle" "r" "DIM E-E")
      (setvar "DIMSCALE" sc)
              
      (cond
        (
          (and 
            (< (car (car str-end_ins_xyz)) (car (cadr str-end_ins_xyz)))
            (< (car (car str-end_ins_xyz)) (car (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_top_ins_xyz_newsort_
              (list
                (+ (nth 0 end_top_ins_xyz_newsort_) HI_DIM)
                (nth 1 end_top_ins_xyz_newsort_)
                (nth 2 end_top_ins_xyz_newsort_)
              )
            ) 
            ; (command "point" offset_top_ins_xyz_newsort_)
            (command "dimlinear" start_top_ins_xyz_newsort_ end_top_ins_xyz_newsort_ "v" offset_top_ins_xyz_newsort_ )
          )
          (princ "\n")
        )
        (
          (and 
            (> (car (car str-end_ins_xyz)) (car (cadr str-end_ins_xyz)))
            (> (car (car str-end_ins_xyz)) (car (cadr str-end_ins_xyz)))
          ) 
          (progn 
            (setq offset_top_ins_xyz_newsort_
              (list
                (+ (nth 0 start_top_ins_xyz_newsort_) HI_DIM)
                (nth 1 start_top_ins_xyz_newsort_)
                (nth 2 start_top_ins_xyz_newsort_)
              )
            )
            (command "dimlinear" start_top_ins_xyz_newsort_ end_top_ins_xyz_newsort_ offset_top_ins_xyz_newsort_ )
          )
          
         (princ "\n")
        )
      )
      ; (command "dimstyle" "r" "DIM E-E")
      ; (setvar "DIMSCALE" sc)
      ; (command "dimlinear" start_top_ins_xyz_newsort_ end_top_ins_xyz_newsort_ offset_top_ins_xyz_newsort_ )
    
                        
            
  
      (setvar "osmode" 1215)
      (c:ucs_previ)
    ;making_dim_
  
)







(setq angle 135) ; กำหนดค่ามุมให้กับตัวแปร angle
(setq )
(setq result (cos (* angle (/ pi 180)))) ; คำนวณค่าของ cos(135 องศา)






(defun LM:sublst ( lst idx len )
    (cond
        (   (null lst) nil)
        (   (< 0  idx) (LM:sublst (cdr lst) (1- idx) len))
        (   (null len) lst)
        (   (< 0  len) (cons (car lst) (LM:sublst (cdr lst) idx (1- len))))
    )
)
(defun c:chos_change_osmode ()
  (cond 
    (
     (and 
       (= (getvar "osmode") 0)
       (= (getvar "osmode") 0)
     )
     (progn 
       (setvar "osmode" 1215) ;;;;;;;;; interger FOR USER INPUT
     )
    )
    ((and 
       (= (getvar "osmode") 1215)
       (= (getvar "osmode") 1215)
     )
     (progn 
       (setvar "osmode" 0) ;;;;;;;;; interger FOR USER INPUT
     )
    )
  )
)

