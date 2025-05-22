 ;Standard_module
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
;






;ต้นแบบใช้แบ่ง xcilp block on pline close

  (setq ss_rec (ssget 
                (list 
                  (cons 0 "POLYLINE,LWPOLYLINE")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (setq ss_rec_total (sslength ss_rec))

  (setq block_data (car (entsel "specify block cad")))
  
  (setq block_data_obj (vlax-ename->vla-object block_data))
  (setq block_data_obj_inspt_XY
         (list 
          (car (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint block_data_obj ))))
          (cadr (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint block_data_obj ))))
         )
  )
  (setq ss_rec_i 0)
  (setq block_data_obj_inspt_i 0)
  (setq block_data_obj_inspt_total ss_rec_total)
  (setq new_block_data (ssadd))
  (ssadd block_data new_block_data)

  
  (while
    (< block_data_obj_inspt_i (- ss_rec_total 1))
    
    (command "insert" "XA-UNKNOWN_SIZE" block_data_obj_inspt_XY 1 0)
    
    (setq new_ (entlast))
    (setq new_rec (ssadd new_ new_block_data))
    
    (setq block_data_obj_inspt_i (+ block_data_obj_inspt_i 1))
    
  )

  (setq new_block_data_total (sslength new_block_data))
  (setq new_block_data_i 0)
  (setvar "XCLIPFRAME" 0)
  (while
    (< ss_rec_i ss_rec_total)
    (setq new_block_data_ename (ssname new_block_data new_block_data_i ))
    (setq ss_rec_ename (ssname ss_rec ss_rec_i))
    (command "xclip" new_block_data_ename  "" "new" "s" ss_rec_ename )
    
    (setq new_block_data_i (+ new_block_data_i 1))
    (setq ss_rec_i (+ ss_rec_i 1))
    
    
  )


  


  ()




  (setq ss_text (ssget 
                (list 
                  (cons 0 "text")       ;type of object
                  ; (cons 8 "000 - GRID")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
  )
  (setq ss_text_total (sslength ss_text))
  (setq ss_text_i 0)
  (while
    (< ss_text_i ss_text_total)
    (setq ss_text_ename (ssname ss_text ss_text_i ))
    (setq ss_text_obj (vlax-ename->vla-object ss_text_ename))
    (vla-get-stylename ss_text_obj)
    (vla-put-stylename ss_text_obj "TA")
    (setq ss_text_i (+ ss_text_i 1 ))
  )
(setvar "TEXTSTYLE" "NewTextStyle")


(setq styleData (tblsearch "style" style))

(defun c:ChangeFontForAllTextStyles (/ ss)
  (setq ss (ssget "X" '((0 . "STYLE"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq style (ssname ss i))
        (setq styleData (tblsearch "style" style))
        (if styleData
          (progn
            (setq fontName "NewFontName") ; ตั้งชื่อฟอนต์ที่คุณต้องการให้ Text Style ใช้
            (setq styleData (subst (cons 3 fontName) (assoc 3 styleData) styleData))
            (entmod styleData)
          )
        )
        (setq i (1+ i))
      )
      (princ "\nFont name changed for all Text Styles.")
    )
    (princ "\nNo Text Styles found.")
  )
  (princ)
)
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

;defun
(defun c:fdd ()
(setq w (cond ( (getreal (strcat "\nSpecify width folding door <" (rtos (setq w (cond (w) (1.0) ) ) ) "> : " ) ) ) (w) ) )

(setq abx (vlax-ename->vla-object (car (entsel))))
(setq abx_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint abx))))
(setq H (LM:getdynpropvalue abx "H"))
; (setq W (LM:getdynpropvalue abx "W"))
(setq C 1000)
; pythagoras formula find W
(setq W^2 (* W W))
(setq C^2 (* C C))
(setq n_H (- C^2 W^2))
(setq n_H2 (sqrt n_H))
(LM:setdynpropvalue abx "H" n_H2)
(LM:setdynpropvalue abx "W" W)

(vla-put-insertionpoint abx (vlax-3d-point abx_ins_xyz) )
)




(setq )


(defun C:BlkEdit (/ Drawing ent blk)
  (vl-load-com)
  (setq Drawing (vla-get-activedocument (vlax-get-acad-object)))
  (if (/= (getvar 'refeditname) "")
    (progn
      (setq blk (vla-item (vla-get-blocks Drawing) (getvar 'refeditname)))
      (vl-cmdf "_.Refclose" "S")
      (vlax-for x blk
        (vla-put-Color x 0)
      )
      (vla-Regen Drawing acAllViewports)
    )
    (progn
      (if (eq (cdr (assoc 0 (setq ent (entget (car (entsel "\nSelect Blocks")))))) "INSERT")
        (progn
          (setq blk (vla-item (vla-get-blocks Drawing) (cdr (assoc 2 ent))))
          (vlax-for x blk
            (vla-put-Color x 2)
          )
          (sssetfirst nil (ssadd (cdr (assoc -1 ent))))
          (vl-cmdf "-REFEDIT" "O" "A" "N")
        )
        (Prompt "\nNothing Selected")
      )  
    )
  )
  (princ)
)

(setq ss (car (entsel)))
(setq ss_get (entget ss))
(setq ss_obj (vlax-ename->vla-object ss))
(vla-item ss_obj 2)


(defun c:cc1_conduitbylayer (/ n ent)
  (vl-load-com)
  (setq n -1)
  (if (setq ss1 (ssget "_X" '((62 . 160))))
    ;;CREATE SELECTION SET RJP-took out the (0 . "LINE") filter
    (while (setq ent (ssname ss1 (setq n (1+ n))))
      (vla-put-color (vlax-ename->vla-object ent) 1)
    )
  )
) ;END OF ROUTINE


(vl-load-com)
(defun c:chcol (/ adoc layers selset blk_lst blk_def)
  (if (setq selset (ssget '((0 . "INSERT"))))
    (progn (vla-startundomark (setq adoc (vla-get-activedocument (vlax-get-acad-object))))
           (setq layers (mapcar (function
                                  (lambda (layer)
                                    (list layer
                                          (mapcar (function
                                                    (lambda (prop / tmp)
                                                      (setq tmp (vlax-get-property layer (car prop)))
                                                      (vl-catch-all-apply (function (lambda () (vlax-put-property layer (car prop) (cdr prop)))))
                                                      (cons (car prop) tmp)
                                                      ) ;_ end of lambda
                                                    ) ;_ end of function
                                                  (list (cons "lock" :vlax-false)
                                                        (cond "freeze"
                                                              :vlax-false
                                                              ) ;_ end of cond
                                                        ) ;_ end of list
                                                  ) ;_ end of mapcar
                                          ) ;_ end of list
                                    ) ;_ end of lambda
                                  ) ;_ end of function
                                ((lambda (/ res) (vlax-for item (vla-get-layers adoc) (setq res (cons item res))) res))
                                ) ;_ end of mapcar
                 selset (mapcar (function (lambda (x) (vla-get-effectivename (vlax-ename->vla-object x))))
                                ((lambda (/ tab item)
                                   (repeat (setq tab  nil
                                                 item (sslength selset)
                                                 ) ;_ end setq
                                     (setq tab (cons (ssname selset (setq item (1- item))) tab))
                                     ) ;_ end of repeat
                                   ) ;_ end of lambda
                                 )
                                ) ;_ end of mapcar
                 ) ;_ end of setq
           (foreach item selset
             (if (not (member item blk_lst))
               (setq blk_lst (cons item blk_lst))
               ) ;_ end of if
             ) ;_ end of foreach
           (foreach item blk_lst
             (if (equal (vla-get-isxref (setq blk_def (vla-item (vla-get-blocks adoc)))) :vlax-false)
               (progn (vlax-for sub blk_def
                        (vla-put-color sub
                                       (if (wcmatch (strcase (vla-get-objectname sub)) "*HATCH*")
                                         0 ; acByBlock
                                         256 ; acByLayer
                                         ) ;_ end of if
                                       ) ;_ end of vla-put-color
                        ) ;_ end of vlax-for
                      ) ;_ end of progn
               ) ;_ end of if
             ) ;_ end of foreach
           (foreach item layer
             (foreach pr (cdr layer)
               (vl-catch-all-apply (function (lambda () (vlax-put-property item (car pr) (cdr pr)))))
               ) ;_ end of foreach
             ) ;_ end of foreach
           (vla-regen adoc acactiveviewport)
           (vla-endundomark adoc)
           ) ;_ end of progn
    ) ;_ end of if
  (princ)
) ;_ end of defun

(command ".-bedit" "11x" "bclose")

(command ".-bedit" "22x" "bclose")


(setq entblock (car (entsel "\nSelect \nBlock \nFor \nColor")))

(if

)

(setq ref_insert nil)
(while (not ref_insert)
  (setq ref_insert (entsel "\nSelect a circle "))
  (if (and ref_insert (= (cdr (assoc 0 (entget (car ref_insert)))) "INSERT"))
      (setq ref_insert (car ref_insert))
      (progn
        (setq ref_insert nil)
        (alert "Please select a block.")
      )
  )
)
(setq ref_insert_obj (vlax-ename->vla-object ref_insert))
(set ref_insert_efname (LM:effectivename ref_insert_obj))
(command ".-bedit" "22x" "bclose")
(setq ss_group (ssget "x"
              (list 
                (cons 0 "insert")       ;type of object
                ; (cons 8 "000 - GRID")   ;kind of layer 
                ; (cons 2 "SSSS")       ;kind of nameblock
                ; (cons 62 1)           ;kind of color call sign with color code index
              )
            )
)
(command "pselect" ss_group "")