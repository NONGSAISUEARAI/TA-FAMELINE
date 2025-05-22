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
  ;
;
(defun c:Z51_res0_reset_all_text (/ Oldtstyle Sttxt Userfont error)
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

;Z44_changing_prop
  ; all line_layer_color
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
  ;reset linetype layer color
    (defun c:SZ_setzero_layer ()
      (command "-layer" "s" LY-0 "")
      (command "-color" c-bylayer "")
      (command "-LINETYPE" "s" li-bylayer "")
    )
  ;
; 
;make_bubble_detail
  (defun c:Z441_make_bubble_detail ()
    ;setting_variable_value
      (setvar "XCLIPFRAME" 0)
      (setvar "WIPEOUTFRAME" 0)
    ;
    ;1st_process_filter_object
      (setq REF_LWPOLYLINE_or_Circle nil)
      (while 
        (= REF_LWPOLYLINE_or_Circle nil)
        (setq REF_LWPOLYLINE_or_Circle (car (entsel "\nSelect a LWPOLYLINE ")))
        (cond 
          ((and (= REF_LWPOLYLINE_or_Circle nil)
             ;  (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "LWPOLYLINE")
             ;  (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "CIRCLE")
           )
           (progn 
             (setq REF_2nd_LWPOLYLINE nil)
             (alert "Please select a LWPOLYLINE.")
           )
           (princ "\n")
          )
          ((and (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "LWPOLYLINE")
             ;  (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "CIRCLE")
            ;  (= REF_LWPOLYLINE_or_Circle nil)
           )
           (progn 
             (setq REF_LWPOLYLINE_or_Circle_assocTYPE (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))))
           )
           (princ "\n")
          )
          ((and (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "CIRCLE")
            ;  (= (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))) "LWPOLYLINE")
            ;  (= REF_LWPOLYLINE_or_Circle nil)
           )
           (progn 
             (setq REF_LWPOLYLINE_or_Circle_assocTYPE (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))))
           )
           (princ "\n")
          )
        )
      )
      (princ "\n ASSOC TPYE IS ")
      (princ (cdr (assoc 0 (entget REF_LWPOLYLINE_or_Circle))))
      (princ "\n 1st_process_complete")
      (princ "\n 1st_process_complete")
      (princ "\n 1st_process_complete")
    ;
    ;2nd_process_changing_OBJ_PROP
    (setq REF_LWPOLYLINE_or_Circle_obj (vlax-ename->vla-object REF_LWPOLYLINE_or_Circle))
      (cond 
        ( (and (= REF_LWPOLYLINE_or_Circle_assocTYPE "LWPOLYLINE")
          )
          (progn 
            (vla-put-layer REF_LWPOLYLINE_or_Circle_obj LY-000-BUBBLE)
            (vla-put-linetype REF_LWPOLYLINE_or_Circle_obj li-TA_L_005-00)
            (vla-put-linetypescale REF_LWPOLYLINE_or_Circle_obj 1)
            (setq fillet_conner (cond ( (getreal (strcat "\nSpecify fillet_conner \nอิอิอิ \n<" (rtos (setq fillet_conner (cond (fillet_conner) (1.0) ) ) ) "> : " ) ) ) (fillet_conner) ) )
            (command "fillet" "r" fillet_conner )
            (command "fillet" "p" REF_LWPOLYLINE_or_Circle)
          )
         (princ "\n")
        )
        ( (and (= REF_LWPOLYLINE_or_Circle_assocTYPE "CIRCLE")
          )
          (progn 
            (vla-put-layer REF_LWPOLYLINE_or_Circle_obj LY-000-BUBBLE)
            (vla-put-linetype REF_LWPOLYLINE_or_Circle_obj li-TA_L_005-00)
            (vla-put-linetypescale REF_LWPOLYLINE_or_Circle_obj 1)
          )
          (princ "\n")
        )
      )
      (princ "Bubble Detail layer's name is ")
      (princ (vla-get-layer REF_LWPOLYLINE_or_Circle_obj ))
      (princ "Bubble Detail linetype's name is ")
      (princ (vla-get-linetype REF_LWPOLYLINE_or_Circle_obj li-TA_L_005-00))
      (princ "\n 2nd_process_complete")
      (princ "\n 2nd_process_complete")
      (princ "\n 2nd_process_complete")
    ;
  )
;






























;
;bubble line
    (defun c:BBUB_bubble_()
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
      (command "CHPROP" select_group "" "LA" LY-000-BUBBLE "")
      (command "CHPROP" select_group "" "C" c-yellow "")
      (command "CHPROP" select_group "" "LT" li-TA_L_001-00 "") 
      (command "pselect" select_group "")
    ;
  )
;