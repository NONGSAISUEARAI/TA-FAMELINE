;changing_PROP
  ; all line_layer_color
    ; spare layer
      ; (setq LayList(command "-layer" "?" "*" "" ""))
    ; layer part
      (setq LY-0 "0") 
      (setq LY-000-A_XREF "000 - A_XREF") 
      (setq LY-000-BUBBLE "000 - BUBBLE") 
      (setq LY-000-DEFPOINTS "DEFPOINTS") 
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
    ;
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
    ;
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

      (setq li-TA_L_000-10 "TA_L_000.10.LIN") 
      (setq li-TA_L_000-25 "TA_L_000.25.LIN") 
      (setq li-TA_L_000-50 "TA_L_000.50.LIN") 
      (setq li-TA_L_001-00 "TA_L_001.00.LIN") 
      (setq li-TA_L_002-00 "TA_L_002.00.LIN") 
      (setq li-TA_L_003-00 "TA_L_003.00.LIN") 
      (setq li-TA_L_005-00 "TA_L_005.00.LIN") 
      (setq li-TA_L_025-00 "TA_L_025.00.LIN") 
      (setq li-TA_L_050-00 "TA_L_050.00.LIN") 
      (setq li-TA_L_100-00 "TA_L_100.00.LIN") 
    ;
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
  (defun c:DEF_LAYER_ () 
    ; command part
    (command "CHPROP" "L" "" "LA" LY-000-DEFPOINTS "")
    (command "CHPROP" "L" "" "C" c-black "")
    (command "CHPROP" "L" "" "LT" li-bylayer "")
    (command "pselect" "L" "")
    ;
  )
;
(defun LM:getdynpropvalue (blk prp) 
  (setq prp (strcase prp))
  (vl-some 
    '(lambda (x) 
       (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)
(defun LM:vl-getattributevalue (blk tag) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
     )
    (vlax-invoke blk 'getattributes)
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
(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
(defun LM:effectivename ( obj )
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
)
(vl-load-com)
(princ)
;Lee_Mac main_func.
  ;Copy_Layout
    (defun c:CCLA_COPYLYOUT ( / *error* oCMDECHO ctab name namer namei nname n i x )
      
      (defun *error* (errmsg)
        (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
          (princ (strcat "\nError: " errmsg)))
        (setvar 'CMDECHO oCMDECHO)
        (princ))
      
      (setq oCMDECHO (getvar 'CMDECHO))
      (setvar 'CMDECHO 0)

      ; --------------------------------------------------------------------------------------
      
      (setq ctab (getvar "ctab"))
      (setq name (getstring (strcat "\nLayout to duplicate <" ctab ">: ")))
      (if (= name "") (setq name ctab))
      (setq n (1+ (strlen name))
            namei "")
      
      (while (and (> n 1)
                  (wcmatch (setq x (substr name (setq n (1- n)) 1)) "#"))
        (setq namei (strcat x namei)))
      
      (setq namer (substr name 1 (- (strlen name) (strlen namei)))
            namei (atoi namei))
      
      (initget 6)
      (setq n (getint "\nHow many copies: ")
            i (+ 1 namei n))
      
      (repeat n
        (setq nname (strcat namer (itoa (setq i (1- i)))))
        (if (member nname (layoutlist))
          (princ (strcat "\nLayout '" nname " ' already exists."))
          (command "_.LAYOUT" "_Copy" name (strcat namer "a")
                  "_.LAYOUT" "_Rename" (strcat namer "a") nname)))

      (setvar 'CMDECHO oCMDECHO)
      (princ)
    )
  ;
  ;Renumber Layouts
    ;;                                                                      ;;
    ;;  This program enables the user to automatically sequentially         ;;
    ;;  renumber all Paperspace layouts, with an optional prefix and/or     ;;
    ;;  suffix.                                                             ;;
    ;;                                                                      ;;
    ;;  The layouts are renumbered in the order in which they appear in     ;;
    ;;  the active drawing, with a configurable parameter defining the      ;;
    ;;  number of digits constituting the numerical portion of the layout   ;;
    ;;  name.                                                               ;;
    ;;                                                                      ;;
    ;;  The user may optionally predefine a fixed prefix and/or suffix      ;;
    ;;  within the 'Program Parameters' section of the source code below,   ;;
    ;;  or, if such parameters are set to nil, the program will prompt the  ;;
    ;;  user to specify the prefix and suffix upon invoking the command.    ;;
    ;;----------------------------------------------------------------------;;
    ;;  Author:  Lee Mac, Copyright © 2020  -  www.lee-mac.com              ;;
    ;;----------------------------------------------------------------------;;
    ;;  Version 1.0    -    2020-01-26                                      ;;
    ;;                                                                      ;;
    ;;  - First release.                                                    ;;
    ;;----------------------------------------------------------------------;;
    ;;  Version 1.1    -    2020-09-05                                      ;;
    ;;                                                                      ;;
    ;;  - Added option to specify layout starting number.                   ;;
    ;;----------------------------------------------------------------------;;

    (defun c:rl ( / *error* int lst lyn ord pad pre sed suf tmp )

        (defun *error* ( msg )
            (LM:endundo (LM:acdoc))
            (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
                (princ (strcat "\nError: " msg))
            )
            (princ)
        )

        (setq

    ;;----------------------------------------------------------------------;;
    ;;                          Program Parameters                          ;;
    ;;----------------------------------------------------------------------;;

            ;; Optional Predefined Layout Prefix
            ;; Set to nil to prompt the user, or "" for no prefix.
            pre nil

            ;; Optional Predefined Layout Suffix
            ;; Set to nil to prompt the user, or "" for no suffix.
            suf nil

            ;; Optional Predefined Starting Number
            ;; Set to nil to prompt the user
            int nil

            ;; Number of Numerical Digits
            ;; e.g. 1 = "1", 2 = "01", 3 = "001"
            pad 2

    ;;----------------------------------------------------------------------;;

        )

        ;; Obtain a valid (optional) prefix & suffix
        (or (= 'str (type pre)) (setq pre (LM:rl:validstring "\nSpecify prefix <none>: ")))
        (or (= 'str (type suf)) (setq suf (LM:rl:validstring "\nSpecify suffix <none>: ")))
        (or (= 'int (type int)) (setq int (cond ((getint "\nSpecify starting number <1>: ")) (1))))
        (or (and (= 'int (type pad)) (<= 0 pad)) (setq pad 0))
        
        ;; Obtain list of layout objects, current names, and sort index
        (vlax-for lyt (vla-get-layouts (LM:acdoc))
            (if (= :vlax-false (vla-get-modeltype lyt))
                (setq lst (cons lyt lst)
                      lyn (cons (strcase (vla-get-name lyt)) lyn)
                      ord (cons (vla-get-taborder lyt) ord)
                )
            )
        )

        ;; Construct a unique seed for temporary renaming
        (setq lyn (cons (strcase pre) lyn)
              sed "%"
        )
        (while (vl-some '(lambda ( x ) (wcmatch x (strcat "*" sed "*"))) lyn)
            (setq sed (strcat sed "%"))
        )

        ;; Temporarily rename layouts to ensure no duplicate keys when renumbering
        (LM:startundo (LM:acdoc))
        (setq tmp 0)
        (foreach lyt lst
            (vla-put-name lyt (strcat sed (itoa (setq tmp (1+ tmp)))))
        )

        ;; Rename layouts in tab order, with prefix & suffix
        (foreach idx (vl-sort-i ord '<)
            (vla-put-name
                (nth idx lst)
                (strcat pre (LM:rl:padzeros (itoa int) pad) suf)
            )
            (setq int (1+ int))
        )
        
        (LM:endundo (LM:acdoc))
        (princ)
    )

    ;;----------------------------------------------------------------------;;

    (defun LM:rl:validstring ( msg / rtn )
        (while
            (not
                (or
                    (= "" (setq rtn (getstring t msg)))
                    (snvalid (vl-string-trim " " rtn))
                )
            )
            (princ (strcat "\nThe layout name cannot contain the characters \\<>/?\":;*|,=`"))
        )
        rtn
    )

    ;;----------------------------------------------------------------------;;

    (defun LM:rl:padzeros ( str len )
        (if (< (strlen str) len) (LM:rl:padzeros (strcat "0" str) len) str)
    )

    ;;----------------------------------------------------------------------;;

    ;; Start Undo  -  Lee Mac
    ;; Opens an Undo Group.

    (defun LM:startundo ( doc )
        (LM:endundo doc)
        (vla-startundomark doc)
    )

    ;;----------------------------------------------------------------------;;

    ;; End Undo  -  Lee Mac
    ;; Closes an Undo Group.

    (defun LM:endundo ( doc )
        (while (= 8 (logand 8 (getvar 'undoctl)))
            (vla-endundomark doc)
        )
    )

    ;;----------------------------------------------------------------------;;

    ;; Active Document  -  Lee Mac
    ;; Returns the VLA Active Document Object

    (defun LM:acdoc nil
        (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
        (LM:acdoc)
    )

    ;;----------------------------------------------------------------------;;

    (vl-load-com)
    (princ
        (strcat
            "\n:: RenumberLayouts.lsp | Version 1.1 | \\U+00A9 Lee Mac "
            ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2020")
            " www.lee-mac.com ::"
            "\n:: Type \"RL\" to Invoke ::"
        )
    )
    (princ)

    ;;----------------------------------------------------------------------;;
    ;;                             End of File                              ;;
    ;;----------------------------------------------------------------------;;
  ;
;


(defun c:SS3_SET_NAME_VIEWPORT ()
  (setq main (car (entsel)))

  (setq NAME_VIEWPORT_OLD (LM:vl-getattributevalue (vlax-ename->vla-object main) "NAME_VIEWPORT"))
  ; (setq DRAWING_NO2 (LM:vl-getattributevalue (vlax-ename->vla-object main) "DRAWING_NO2"))
  
  
  
  
  (setq my_real_ename (ssget (list 
                      ;  (cons 8 "0") 
                       (cons 0 "INSERT") 
                      ;  (cons 2 "d2d")
                     ) 
              ) 
  )
  (setq entityCount (sslength my_real_ename))
  
  (setq i 0)
  (while (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq ejc (ssname my_real_ename i)) ; ดึง entity ที่ลำดับ i จาก my_real_ename
      (setq o (vlax-ename->vla-object ejc))

      ; (setq entity1 (ssname my_real_ename 1)) 
      ; (setq entity2 (ssname my_real_ename 2))
      ; (setq entity3 (ssname my_real_ename 0)) 
      ; (setq entity4 (ssname my_real_ename 3)) 

      (setq o (vlax-ename->vla-object ejc))
      (setq ii (1+ i)) ; เริ่มต้น i ที่ 101
      (setq NAME_VIEWPORT_NEW (strcat "M1" (if (< ii 10) "0" "") (itoa ii))) ; สร้างชื่อใหม่โดยใช้ i และแปลงเป็นสตริง
      (LM:vl-setattributevalue o "NAME_VIEWPORT" NAME_VIEWPORT_NEW)
     
      ; (setq M_D (strcat "M10" (itoa (1+ i))))
      ; (LM:vl-setattributevalue o "FAC" M_D)
  
      ; เพิ่มลำดับ
      (setq i (1+ i))
  )
)
(defun c:REE3_RENUMBER_DRAWING_TITTLE_BLOCK_AND_VIEWPORT_BORDER ()
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
  
  (defun LM:effectivename ( obj )
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
  )

  (setq my_gv_port (ssget  '((0 . "INSERT"))))
  
    ; setting mode for choose name efblk
      (defun get-title-block-string (mode) 
        (if (= mode 1) 
          "LNAD - A4 TITLE BLOCK PART REV01"
          (if (= mode 2) 
            "GV2"
            "Unknown Mode"
          )
        )
      )
      (setq mode-value (cond ( (getint (strcat "\nSpecify object \nmode 1 = Tittle_Block \nmode 2 = GV2_Layout_Border \n<" (rtos (setq mode-value (cond (mode-value) (1.0) ) ) ) "> : \nไม่มี MODE 3 นะคนดีย์" ) ) ) (mode-value) ) )
      
      (setq result-mode (get-title-block-string mode-value))
      (princ (strcat "Result: " result-mode))
    ; setting mode for choose name efblk
  
    ; filtter name blk part
      (setq ename-list '())
      ; (setq ef_name "LNAD - A4 TITLE BLOCK PART REV01")
      (setq ef_name result-mode)
      ; (setq ef_name (getstring "insert_NAME_BLOCK")) ;::::method 1
      
      ; (setq select_name (car (entsel "select_NAME_BLOCK"))) ;::::method 2
      ; (setq select_name_obj (vlax-ename->vla-object select_name))
      ; (setq ef_name (LM:effectivename select_name_obj))
    
      (setq total_ssget (sslength my_gv_port))
      (setq ie 0)
      (setq ename-list '())

      (while 
        (< ie total_ssget)
        (setq blk_obj (ssname my_gv_port ie))
        (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
        (setq ss (LM:effectivename blk_obj_Set))

          (if (= ef_name ss)
            (progn
              (setq ename-list (cons blk_obj ename-list)) 
            )
            (princ "\n")

          )
        (setq ie (+ ie 1))
      )
      (princ "\n")
      (princ (setq total_ename-list (length ename-list)))
      (setq my_gv_port_SSF (ssadd))
        (foreach ename ename-list
          (ssadd ename my_gv_port_SSF)
        )
    ; filtter name blk part

  (setq sorted-enames (sort-entities-by-x my_gv_port_SSF))
  (setq total-entities (length sorted-enames))
  (setq i 0)
  (setq ii 101) ; เริ่มต้น ii ที่ 101
  (setq iii 1) ; เริ่มต้น ii ที่ 101
  
  (setq PREFIX_NAME_VIEWPORT (getstring "SPECIFY NAME VIEWPORT")) ; PREFIX สำหรับ DRAWING_NUMBER
  
          (setq total-entities (length sorted-enames))
          (while (< i total-entities)
              (setq entity (nth i sorted-enames))
              ; ทำสิ่งที่คุณต้องการกับ ename ที่ปรากฏในตำแหน่งที่ i ใน sorted-enames

              (setq o (vlax-ename->vla-object entity))
              
              
              (setq NAME_VIEWPORT_NEW (strcat PREFIX_NAME_VIEWPORT (if (< ii 10) "0" "") (itoa ii))) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              (setq SHEET_NO_NEW (strcat (itoa iii) " of " (itoa total-entities))) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
              (setq TOTAL_SHEET_NO_NEW (strcat (itoa total-entities) " PAGE")) ; สร้างชื่อใหม่โดยใช้ ii และแปลงเป็นสตริง
            
              (LM:vl-setattributevalue o "DRAWING_NO2" NAME_VIEWPORT_NEW)
              (LM:vl-setattributevalue o "SHEET_NO." SHEET_NO_NEW)
              (LM:vl-setattributevalue o "TOTAL_SHEET_NO." TOTAL_SHEET_NO_NEW)
            
              (LM:vl-setattributevalue o "NAME_VIEWPORT" NAME_VIEWPORT_NEW)
              



              (setq i (+ i 1))
              (setq ii (+ ii 1)) ; เพิ่ม ii ทีละ 1   
              (setq iii (+ iii 1)) ; เพิ่ม iii ทีละ 1   
            
          )
)

;for continue command
(defun c:LLOP1_SEND_MODEL_TO_LAYOUT ()
  (command "ucs" "")
  (setvar "osmode" 0)
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
    ;selection_part
      (setq my_gv_port (ssget
                    (list 
                      (cons 0 "INSERT")       ;type of object
                      ; (cons 8 "000 - GRID")   ;kind of layer 
                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                      ; (cons 62 1)           ;kind of color call sign with color code index
                    )
                  )
      )
    ;selection_part

    ;setting_reference_name_part
      ;mode1
        (setq ef_name "GV2")
      ;end_mode1
      ;mode2
      ;   (setq ef_name (getstring "insert_NAME_BLOCK"))
      ;end_mode2
      ;mode3
        ; (setq ef_ename_1st (entsel "select_NAME_BLOCK"))
        ;   (setq ef_ename_2nd (car ef_ename_1st))
        ;   (setq ef_ename_obj (vlax-ename->vla-object ef_ename_2nd))
        ; (setq ef_name (LM:effectivename ef_ename_obj))
      ;end_mode3
    ;setting_reference_name_part
    (setq total_ssget (sslength my_gv_port))
    (setq i 0)
    (setq ename_list '())
    (while 
      (< i total_ssget)
      (setq blk_obj (ssname my_gv_port i))
      (setq blk_obj_Set (vlax-ename->vla-object blk_obj))
      (setq ss (LM:effectivename blk_obj_Set))

        (if (= ef_name ss)
          (progn
            (setq ename_list (cons blk_obj ename_list)) 
          )
          (princ "\n")
            
        )
      (setq i (+ i 1))
    )
      (princ "\n")
    (princ (setq total_ename_list (length ename_list)))
    (setq my_real_ename (ssadd))
    (foreach ename ename_list
      (ssadd ename my_real_ename)
    )
  ; SFB_FINDING_SPECFILY_NAME_BLOCK

  ; part2
    ; (setq my_real_ename (ssget (list 
    ;                     (cons 8 "defpoints") 
    ;                     (cons 0 "INSERT") 
    ;                     ;  (cons 2 "d2d")
    ;                   ) 
    ;             ) 
    ; )
    (setq entityCount (sslength my_real_ename))
    (setq i 0) ; ตั้งค่าลำดับเริ่มต้น

    (while 
      (< i entityCount) ; เมื่อลำดับยังไม่ถึง entityCount
      (setq MAI (ssname my_real_ename i)) ; ดึง entity ที่ลำดับ i จาก my_real_ename
      ; (setq entity1 (ssname my_real_ename 1)) 
      ; (setq entity2 (ssname my_real_ename 2))
      ; (setq entity3 (ssname my_real_ename 0)) 
      ; (setq entity4 (ssname my_real_ename 3)) 
      
      (setq MAI_EN (entget MAI))
      (setq BOT_INSERT_P (cdr (assoc 10 MAI_EN)))
      
      
      (princ "\n STAR_X =")
      (princ (setq BOT_INSERT_X (car BOT_INSERT_P)))
      
      (princ "\n STAR_Y =")
      (princ (setq BOT_INSERT_Y (cadr BOT_INSERT_P)))  
    
      (setq BOT_INSERT_XY (strcat
                            (rtos BOT_INSERT_X 2 8)
                            ","
                            (rtos BOT_INSERT_Y 2 8)
                          )
      )
      (setq BOT_INSERT_XY_test (list BOT_INSERT_X BOT_INSERT_Y))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GET_DATA_PART
      (princ "\n DYN_BASE_P X =")
      (princ (setq KJ1 (fix (LM:getdynpropvalue (vlax-ename->vla-object MAI) "BASE_P X"))))
      (princ "\n DYN_BASE_P Y =")
      (princ (setq KJ2 (fix (LM:getdynpropvalue (vlax-ename->vla-object MAI) "BASE_P Y"))))

      (princ "\n DYN_H =")
      (princ (setq KJH (LM:getdynpropvalue (vlax-ename->vla-object MAI) "H")))
      (princ "\n DYN_L =")
      (princ (setq KJL (LM:getdynpropvalue (vlax-ename->vla-object MAI) "L")))

      (princ "\n DYN_SC =")
      (princ (setq KJsc (LM:getdynpropvalue (vlax-ename->vla-object MAI) "SC")))
      (princ (setq KJscsc (rtos KJsc 2 2)))

      (setq AA_1 (LM:vl-getattributevalue (vlax-ename->vla-object MAI) "NAME_VIEWPORT"))
      (princ AA_1)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GET_DATA_PART
    (princ "\n")

    (setq TOP_INSERT_X (+ KJL BOT_INSERT_X))
    (setq TOP_INSERT_Y (+ KJH BOT_INSERT_Y))

    (setq TOP_INSERT_XY (strcat (rtos TOP_INSERT_X 2 8) "," (rtos TOP_INSERT_Y 2 8)))
    (setq TOP_INSERT_XY_test (list TOP_INSERT_X TOP_INSERT_Y))

    (setq RE_X (/ KJ1 -1))
    (setq RE_Y (/ KJ2 -1))


    (setq MID_X (+ RE_X (/ KJL 2)))
    (setq MID_Y (+ RE_Y (/ KJH 2)))

    (setq MID_Xsc (/ MID_X KJsc))
    (setq MID_Ysc (/ MID_Y KJsc))

    ; (setq MID_Xsc (+ (+ RE_X (/ KJL 2)) KJsc))
    ; (setq MID_Ysc (+ (+ RE_Y (/ KJH 2)) KJsc))
    (setq MID_INSERT_XY (strcat (rtos MID_Xsc 2 8) "," (rtos MID_Ysc 2 8)))   
    (setq MID_INSERT_XY_test (list MID_Xsc MID_Ysc))   
      
    (command "CTAB" AA_1)
    
    (command "MVIEW" "NEW" BOT_INSERT_XY TOP_INSERT_XY "" MID_INSERT_XY)
    ; (command "MVIEW" "NEW" BOT_INSERT_XY TOP_INSERT_XY)
    ; (command "MVIEW" "NEW" BOT_INSERT_XY TOP_INSERT_XY)

    ; (command "MVIEW" "NEW" -9259.30199316 -37693.04728139 -9259.30199316 -37693.04728139)
    ; ; (command "MVIEW" "NEW" BOT_INSERT_XY_test TOP_INSERT_XY_test "" MID_INSERT_XY_test)
    ; (command "REGEN")
    ; (command "MVIEW" "NEW" BOT_INSERT_XY TOP_INSERT_XY )
    ; ; (command "MVIEW" "NEW" -14338 -38100 -14041 -37890 )
    


      (vl-load-com)
    (setq afc (entlast))
    (setq afc_gt (entget afc))
    (setq a_app (vlax-get-acad-object)
          a_doc (vla-get-ActiveDocument a_app)
          obj   (vlax-ename->vla-object afc) ; obj   (vlax-ename->vla-object (car (entsel "Select a pviewport entity: "))) ;OPTION 2
    )
    (setq cen_ (assoc 10 afc_gt))
    (setq cen_x (cadr cen_))
    (setq cen_y (caddr cen_))
    (princ (setq cen_xy (strcat (rtos cen_x) "," (rtos cen_y))))
    (princ (vla-get-height obj))


    (setq real_box (/ (/ KJH KJsc) (vla-get-height obj)))
      (setq re_real_box (rtos real_box 2 2))
    (setq inp_cus_sc (/ 1 KJsc))
    (vla-put-customscale obj inp_cus_sc)
    (command "_.SCALE" afc "" cen_xy re_real_box)


    ; (vla-ZoomCenter (vlax-get-acad-object) (vlax-3d-point obj) 1)
    (setq kk 0.2)
    (princ "\n")
    (princ (vla-get-customscale obj))
    (princ "\n anno")
    (princ (vla-get-height obj))
    (princ "\n vla1")
    (princ (vla-get-standardscale obj))
    (princ "\n vla2")
    (princ (vla-get-standardscale2 obj))
    
    (command "zoom" "Extents")
    (command "CTAB" "MODEL")
  
    
      ; เพิ่มลำดับ
      (setq i (1+ i))
    )
    (setvar "osmode" 1199)
    (command "ucs" "p")
  ; part2
)

(defun c:SS1S_set1_re_basepoint_plot () 
  ; Lee_macdonel_part
    (defun LM:setdynpropvalue (blk prp val) 
      (setq prp (strcase prp))
      (vl-some 
        '(lambda (x) 
          (if (= prp (strcase (vla-get-propertyname x))) 
            (progn 
              (vla-put-value x 
                              (vlax-make-variant val 
                                                (vlax-variant-type (vla-get-value x))
                              )
              )
              (cond (val) (t))
            )
          )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
      )
    )
  ; Lee_macdonel_part  
  ; part_ref_viewport
    (setq FP (entget (car (entsel "SELECT FOR REF_TITTLE_BLOCK"))))
    (setq enam (assoc -1 FP))
    (setq BBB (assoc 10 FP))

    (setq cbx (cadr BBB))
    (setq cby (caddr BBB))
  ; part_ref_viewport
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
    ;selection_part
      (setq my-set_ef_name_selection-set (ssget
                    (list 
                      (cons 0 "INSERT")       ;type of object
                      ; (cons 8 "000 - GRID")   ;kind of layer 
                      ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                      ; (cons 62 1)           ;kind of color call sign with color code index
                    )
                  )
      )
    ;selection_part
    ;setting_refernce_name_part
      ; mode1
        (setq ef_name "GV1")
      ; end_mode1
      ;mode2
        ; (setq ef_name (getstring "insert_NAME_BLOCK"))
      ;end_mode2
      ;mode3
        ; (setq ef_ename_1st (entsel "select_NAME_BLOCK"))
        ;   (setq ef_ename_2nd (car ef_ename_1st))
        ;   (setq ef_ename_obj (vlax-ename->vla-object ef_ename_2nd))
        ; (setq ef_name (LM:effectivename ef_ename_obj))
      ;end_mode3
    ;setting_refernce_name_part
  ; SFB_FINDING_SPECFILY_NAME_BLOCK
  ; part_viewport
    (setq total_my_vp_blk_real_enam (sslength my-set_ef_name_selection-set))
    (setq no_vp 0)
    (while 
      (< no_vp total_my_vp_blk_real_enam)
      (setq vp_main (ssname my-set_ef_name_selection-set no_vp))
      (setq vp_main_entget (entget vp_main))
      (setq vp_main_obj (vlax-ename->vla-object vp_main))
      (setq vp_main_xy (cdr (assoc 10 vp_main_entget)))
      (setq vp_main_x (car vp_main_xy))
      (setq vp_main_y (cadr vp_main_xy))
      (setq wx (/(- vp_main_x cbx) -1))
      (setq wy (/ (- vp_main_y cby) -1))
      
      
      (LM:setdynpropvalue vp_main_obj "BASE_P X" (rtos wx 2 2))
      (LM:setdynpropvalue vp_main_obj "BASE_P Y" (rtos wy 2 2))
      (setq no_vp (+ no_vp 1))
    )
  ; part_viewport

)

(defun cn_counta_layout ()
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layout-count (vla-get-Count (vla-get-Layouts doc)))
  (setq layout-name (vla-get-name doc))
  (vla-put-name doc AAA)
  
  
  
  
  (princ (strcat "จำนวน layouts ทั้งหมด: " (itoa layout-count)))
  (princ)
  
  (setq layout-list '())
    (setq layout-name (vla-get-Name layout))
    (setq layout-list (cons layout-name layout-list))
  
  (foreach layout layout-list
    (princ (strcat layout "\n"))
  )
)

(defun c:mk1_makeboder_gv2 ()
  (defun sort_by_X (list_) ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b)))))))
  (defun sort_by_y (list_) ;เรียงชุดข้อมูลตามแนวแกน
   (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b)))))))

  (setq bkgv_ (entget (car (entsel "\nstart \nmaking \nborder \nGV_2 \nblock"))))
  (setq bkgv_ename (cdr (assoc -1 bkgv_)))
  (setq bkgv_obj (vlax-ename->vla-object bkgv_ename))
  
  (setq bkgv_get10 (vl-remove-if-not '(lambda (x) (= 10 (car x))) bkgv_))
  (setq bkgv_get10_sort (sort_by_X (mapcar 'cdr bkgv_get10)))
  (setq bkgv_get10_sort (sort_by_Y (mapcar 'cdr bkgv_get10)))
  
  (setq my_ti_blk_set (ssget 
                            (list 
                              (cons 0 "INSERT") ;type of object
                              ; (cons 8 "000 - GRID")   ;kind of layer
                              ; (cons 2 "SSSS")       ;kind of nameblock
                              ; (cons 62 1)           ;kind of color call sign with color code index
                            )
                          )
  )
  ; find_ti_block
    (setq my_ti_blk_set_total (sslength my_ti_blk_set)) 
    (setq my_ti_blk_set_ief 0)
    (setq ssefname_A4 "LNAD - A4 TITLE BLOCK PART REV01")
    (setq ssefname_A4 "LNAD - A3 TITLE BLOCK PART REV01")
    (setq _ti_ename-list '())
    (while
      (< my_ti_blk_set_ief my_ti_blk_set_total)
      (setq my_ti_blk_set_ename (ssname my_ti_blk_set my_ti_blk_set_ief))
      (setq my_ti_blk_set_obj (vlax-ename->vla-object my_ti_blk_set_ename))
      (setq my_ti_blk_set_efname (LM:effectivename my_ti_blk_set_obj))
      (setq my_ti_blk_set_dyn_sc (LM:getdynpropvalue my_ti_blk_set_obj "SC"))
      
      (if 
        (or
         (= my_ti_blk_set_efname ssefname_A4)
         (= my_ti_blk_set_efname ssefname_A3)
        ) 
        (progn 
          (setq _ti_ename-list (cons my_ti_blk_set_ename _ti_ename-list))
        )
        (princ "\n")
      )
      (setq my_ti_blk_set_ief (+ my_ti_blk_set_ief 1))
    )
    (princ (setq total_ti_ename-list (length _ti_ename-list)))
    (setq final_ti_ename (ssadd))
      (foreach _ti_ename _ti_ename-list 
        (ssadd _ti_ename final_ti_ename)
      )
    (setq final_ti_ename_total (sslength final_ti_ename))
  ;

  (setq final_ti_ename_ins_xyz (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vla-get-insertionpoint 
                                    (vlax-ename->vla-object 
                                      (ssname final_ti_ename 0)
                                    )
                                  )
                                )
                              )
  )
 
  
  (setq bkgv_get10_sort_x1y1 (nth 0 bkgv_get10_sort))
  (setq bkgv_get10_sort_x2y2 (nth 1 bkgv_get10_sort))
  (setq bkgv_get10_sort_x3y3 (nth 2 bkgv_get10_sort))
  (setq bkgv_get10_sort_x4y4 (nth 3 bkgv_get10_sort))
  
  (setq L (- (car bkgv_get10_sort_x3y3) (car bkgv_get10_sort_x1y1)))
  (setq H (- (cadr bkgv_get10_sort_x4y4) (cadr bkgv_get10_sort_x1y1)))
  
  (setq base_p_x_val (- (car final_ti_ename_ins_xyz) (car bkgv_get10_sort_x1y1)))
  (setq base_p_y_val (- (cadr final_ti_ename_ins_xyz) (cadr bkgv_get10_sort_x1y1)))
  
  
  (setvar "osmode" 0)
  (command "insert" "GV2" bkgv_get10_sort_x1y1 1 0)
  (setq lasted_ (entlast))
  (setq lasted_objid (itoa (vla-get-objectid (vlax-ename->vla-object lasted_))))
  (setq SC_VIEWPORT (strcat "SC_VPORT :" "%<\\AcObjProp Object(%<\\_ObjId " lasted_objid ">%).Parameter(34).UpdatedDistance \\f " "%lu2%pr0"">%"))
  (setq SEQ_VIEWPORT (strcat "%<\\AcObjProp Object(%<\\_ObjId " lasted_objid ">%).Parameter(18).lookupString \\f " "%lu2%pr0"">%"))
  
  
  (LM:setdynpropvalue (vlax-ename->vla-object lasted_ ) "SC" my_ti_blk_set_dyn_sc)
  (vla-put-insertionpoint (vlax-ename->vla-object lasted_) (vlax-3d-point bkgv_get10_sort_x1y1))
  (LM:setdynpropvalue (vlax-ename->vla-object lasted_ ) "BASE_P X" base_p_x_val)
  (LM:setdynpropvalue (vlax-ename->vla-object lasted_ ) "BASE_P Y" base_p_y_val)
  
  (LM:setdynpropvalue (vlax-ename->vla-object lasted_ ) "H" H)
  (LM:setdynpropvalue (vlax-ename->vla-object lasted_ ) "L" L)
  (LM:vl-setattributevalue (vlax-ename->vla-object lasted_ ) "SEQ_VIEWPORT" SEQ_VIEWPORT)
  (LM:vl-setattributevalue (vlax-ename->vla-object lasted_ ) "SC_VPORT" SC_VIEWPORT)
  (command "._updatefield" lasted_ "")
  (vla-delete bkgv_obj)
  (c:DEF_LAYER_)
  (setvar "osmode" 1215)
  ; %<\AcObjProp Object(%<\_ObjId 2000122932784>%).Parameter(34).UpdatedDistance \f "%lu2%pr0">%
  ; %<\AcObjProp Object(%<\_ObjId 2033853131152>%).XEffectiveScaleFactor \f "%lu2%pr0">%
  ; %<\AcObjProp Object(%<\_ObjId 2000122932784>%).Parameter(18).lookupString \f "%tc1">%
)


(Setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel)))))


(SETQ NewRTBlock (itoa (vla-get-objectid  (vlax-ename->vla-object (car (entsel))))))

(defun c:vvest (/ s x y doc objtable numrows rowheight pt1 colwidth curspace)
  ;; Tharwat 26. 08. 2015 ;
  ;; mods by BIGAL 29.08.2015 now as table

  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq curspace (vla-get-modelspace doc))
  (setq pt1 (vlax-3d-point (getpoint "\nPick point for top left hand of table:  "))) 
  (princ "\nSelect LWpolylines to export to a Table :")
  (setq s (ssget '((0 . "LWPOLYLINE"))))
  (if (/= s nil)
    (progn
  ; now do table 
    (setq numrows (+ 2 (sslength s)))
    (setq numcolumns 3)
    (setq rowheight 7)
    (setq colwidth 25)
    (setq objtable (vla-addtable curspace pt1 numrows numcolumns rowheight colwidth))
    (vla-settext objtable 0 0 "Pline lengths")
    (vla-setcolumnwidth objtable 0 10)
    (vla-setcolumnwidth objtable 1 25)
    (vla-settext objtable 1 0 "Pline") 
    (vla-settext objtable 1 1 "Length")
    (vla-settext objtable 1 2 "ID")
    (vla-SetTextHeight Objtable (+ acDataRow acHeaderRow acTitleRow) 2.5)
    (vla-SetAlignment Objtable acDataRow acMiddleCenter)

    (setq x 1)
    (SETQ Y 2)
    (setq r -1)
    ;((lambda (r / e)
          (while (setq e (vlax-ename->vla-object(ssname s (setq r (1+ r)))))
          (vla-settext objtable Y 0 (rtos x 2 0))        
          (vla-settext objtable Y 1 (rtos (cvunit (vla-get-length e) "mm" "m") 2 4))
          (vla-settext objtable Y 2 (vla-get-handle e ) )
          (setq x (1+ x ))
          (setq y (1+ Y ))
  ); while
      ; )) ;lambda
      )   ;progn
    (alert "You have not picked any plines run again")
    )     ; if      
  (princ)

) ; defun

