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
;sub_function
  (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
  )
  (defun sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
    (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
  )
  (defun LM:RemoveNth (n l / i) 
    (setq i -1)
   (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
  )
;

(defun c:z565_autoplot_block ()
  (if ;expire_func
    ; (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
    (< (setq now (rtos (getvar "cdate") 2 4)) (rtos 20240608.2168 2 4))
    (progn
      ;sub_func
        (defun sort-by-x_ver1 (list) 
          (setq sorted-list (vl-sort list 
                                      (function 
                                        (lambda (a b) 
                                          (< (cadr a) (cadr b))
                                        )
                                      )
                            )
          )
        )
      ;
      ; ;pre_var
        ;   (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
        ;   (setq lyout (vla-get-activelayout adoc))
        ;   (setq styleSheet "monochrome.ctb")
        ;   (setq cfgName "AutoCAD PDF (High Quality Print).pc3")
        ;   (vla-RefreshPlotDeviceInfo lyout)
        ;   (vla-put-ConfigName lyout CfgName)
        ;   (vla-put-PaperUnits lyout acMillimeters)
        ;   (vla-put-plotrotation lyout ac0degrees)
        ;   (vla-put-PlotType lyout acWindow)
        ;   ; (vla-put-PlotType lyout acExtents)
            ; (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
            ; (vla-get-CanonicalMediaName lyout )
        ;   (vla-put-PlotWithLineweights lyout :vlax-false)
        ;   (vla-put-PlotWithPlotStyles lyout :vlax-true)
        ;   (vla-put-StyleSheet lyout styleSheet)
        ;   (vla-put-CenterPlot lyout :vlax-true)
        ;   (vla-put-plotrotation lyout ac0degrees)
      ; ;
      ;user_input_paper size on PLOT
        (initget "1 2")
          (setq mode-paper_size nil)
          (setq mode-paper_size (cond ( (getkword (strcat " \nspecify paper size on PLOT \nmode 1 = ISO_full_bleed_A4_ \nmode 2 = ISO_full_bleed_A3_ \n<" (rtos (setq mode-paper_size (cond (mode-paper_size) (1.0) ) ) ) "> : " ) ) ) (mode-paper_size) ) )
          (cond
            ((and
              (= mode-paper_size "1")
            )
            (progn
              (setq paper_size "ISO_full_bleed_A4_(210.00_x_297.00_MM)")
            )
            )
            ((and
              (= mode-paper_size "2")
            )
            (progn
              (setq paper_size "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
            )
            )
          )
      ;
      ;user_input_paper size on PLOT
        (initget 1 "1 2 3 ")
          (setq mode-T-BLK nil)
          (setq mode-T-BLK (cond ( (getkword 
                                     (strcat " \nspecify paper size on PLOT \nmode 1 = LNAD - A4 TITLE BLOCK PART REV01 \nmode 2 = LAND - A3 TITLE BLOCK \nmode 3 = Another\n<" (rtos (setq mode-T-BLK (cond (mode-T-BLK) (1.0) ) ) ) "> : " 
                                     )
                                   ) 
                                  ) 
                             (mode-T-BLK) 
                           ) 
          )
          (cond
            ((and
              (= mode-T-BLK "1")
            )
            (progn
              (setq efname "LNAD - A4 TITLE BLOCK PART REV01")
            )
            )
            ((and 
               (= mode-T-BLK "2")
             )
            (progn
              (setq efname "LAND - A3 TITLE BLOCK")
            )
            )
            ((and 
               (= mode-T-BLK "3")
              ;  (= mode-T-BLK "3 Use have to select_block")
             )
              (progn
                (setq mode_t-blk-3 nil)
                (while (not mode_t-blk-3)
                  (setq mode_t-blk-3 (car (entsel "specify block")))
                  (if (/= mode_t-blk-3 nil)
                    (progn
                      (if 
                        (or
                          (/= (cdr (assoc 0 (entget mode_t-blk-3))) "INSERT")
                        )
                        (progn
                          (setq mode_t-blk-3 nil)
                          (alert "Please select block only \nจริงๆไม่แนะนำให้เลือก Block อันอื่นนะ\nเป็นไปได้กลับไปใช้\nmode 1 mode 2 เถอะ\nจากใจคนเลี้ยงหมา")
                        )
                        (setq efname (LM:effectivename (vlax-ename->vla-object mode_t-blk-3)))
                      )
                    )
                    (alert "Please select block")
                  )
                )
              )
            )
          )
      ;
      ;user_input_mode_val_for_layout_header
        (cond
          ((and 
             (= mode-T-BLK "3")
           ) 
            (progn 
              (setq mode-header_3 (getstring (strcat "\nspecify prefix name file <" "A" "> : ")))
            )
          )
        )
      ;
      ;selection_set_ss_BLK_
        (setq ss_BLK_ (ssget 
                        (list 
                          (cons 0 "insert") ;type of object
                          ;  (cons 8 "000 - GRID") ;kind of layer
                          ; (cons 2 "SSSS")       ;kind of nameblock
                          ; (cons 62 1)           ;kind of color call sign with color code index
                        )
                      )
        )
      ;
      ;preloop
        (setq ss_BLK_i 0)
        (setq ss_BLK_total (sslength ss_BLK_))
        (setq ins_ ())
        (setq ipp 0)
        (setq ip 100)
        (setq file_name ())
        (setq sorted_ename_ ())
        (setq Fil_Sort_ename_ ())
      ;
      (while ;get_ss_data_and_sumary
        (< ss_BLK_i ss_BLK_total)
        ;get_data_all_ss_BLK_
          ;get_data_ss_BLK_ename
            (setq ss_BLK_ename (ssname ss_BLK_ ss_BLK_i))
            (setq ss_BLK_obj (vlax-ename->vla-object ss_BLK_ename))
          ;
          ;get_efective_name
            (setq ss_BLK_efname (LM:effectivename ss_BLK_obj))
          ;
          ;get_insertion_point
            (setq ss_BLK_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_BLK_obj))))
          ;
          ;get_boundary_blk
            (vla-getboundingbox ss_BLK_obj 'ins_min 'ins_max)
            (setq ins_min (vlax-safearray->list ins_min))
            (setq ins_max (vlax-safearray->list ins_max))
          ;
          ;sumary_data
            (setq sum_ss_ (list 
                            ss_BLK_ename
                            (car ss_BLK_ins_xyz)
                            (cadr ss_BLK_ins_xyz)
                            ss_BLK_efname
                            ins_min
                            ins_max
                          )
            )
            (setq sorted_ename_ (cons sum_ss_ sorted_ename_))
          ;
        ;
        (setq ss_BLK_i (+ ss_BLK_i 1))
      )
      ;preloop
        (setq sorted_ename_total (length sorted_ename_))
        (setq sorted_ename_i 0)
      ;   
      (while ;fillet_efname
        (< sorted_ename_i (length sorted_ename_))
        (setq sorted_ename_list (nth sorted_ename_i sorted_ename_))
        (setq sorted_ename_efname (nth 3 sorted_ename_list))
          (if (= sorted_ename_efname efname) 
            (progn
              (setq Fil_Sort_ename_ (cons sorted_ename_list Fil_Sort_ename_))
            )
            (princ "\n")
          )
        (setq sorted_ename_i (+ sorted_ename_i 1))
      )
      ;sorting_list-by_x
        (setq Fil_Sort_ename_ (sort-by-x_ver1 Fil_Sort_ename_))
      ;
      ;preloop
        (setq Fil_Sort_ename_i 0)
        (setq file_name ())
      ;
      (while ;get_num_file
        (< Fil_Sort_ename_i (length Fil_Sort_ename_))
        (setq Fil_Sort_ename_list (car (nth Fil_Sort_ename_i Fil_Sort_ename_)))
        (setq Fil_Sort_ename_obj (vlax-ename->vla-object Fil_Sort_ename_list))
        ;make_file_name
          (cond
            ((and 
               (= mode-T-BLK "1")
             )
              (progn
                (setq mode-header (LM:vl-getattributevalue Fil_Sort_ename_obj "DRAWING_NO2" ))
                (setq file (strcat (getvar 'DWGPREFIX) 
                                  (substr (setq dwg (getvar 'DWGNAME)) 
                                          1
                                          (- (strlen dwg) 4)
                                  )
                                  "_"
                                  mode-header
                                  ; (itoa (setq ip (1+ ip)))
                                  ".pdf"
                          )
                )
              )
            )
            ((and 
               (= mode-T-BLK "2")
             )
              (progn
                ; (setq mode-header (LM:vl-getattributevalue Fil_Sort_ename_obj "DRAWING_NO2" ))
                ; (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
                (setq mode-header mode-header)
                (setq file (strcat (getvar 'DWGPREFIX) 
                                  (substr (setq dwg (getvar 'DWGNAME)) 
                                          1
                                          (- (strlen dwg) 4)
                                  )
                                  "_"
                                  mode-header
                                  (itoa (setq ip (1+ ip)))
                                  ".pdf"
                          )
                )
              )
            )
            ((and 
               (= mode-T-BLK "3")
             )
              (progn
                ; (setq mode-header (LM:vl-getattributevalue Fil_Sort_ename_obj "DRAWING_NO2" ))
                ; (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
                (setq mode-header mode-header_3)
                (setq file (strcat (getvar 'DWGPREFIX) 
                                  (substr (setq dwg (getvar 'DWGNAME)) 
                                          1
                                          (- (strlen dwg) 4)
                                  )
                                  "_"
                                  mode-header
                                  (itoa (setq ip (1+ ip)))
                                  ".pdf"
                          )
                )
              )
            )
          )
          
          (if (findfile file) 
            (vl-file-delete file)
          )
          (setq file_name (cons file file_name))
          (setq file_name (vl-sort file_name '<))

          (setq Fil_Sort_ename_i (+ Fil_Sort_ename_i 1))
          
      )
      ;
      ;make_plot_to_file
        ; (vla-SetWindowToPlot lyout (vlax-2d-point new_lowest_ins_) (vlax-2d-point new_highest_ins))
        ; (vla-PlotToFile (vla-get-Plot adoc) (strcat (getvar 'dwgprefix) "draw" (itoa ss_LWPOLYLINE_i)))
          
          
          (setq cmd (getvar 'cmdecho))
          (setvar 'cmdecho 0)
          (command "tilemode" "1")
          ;pre_loop
            (setq file_name_i 0)
            (setq Fil_Sort_ename_i 0)
            (setq Fil_Sort_ename_total (length Fil_Sort_ename_))
          ;
          (while 
            (< Fil_Sort_ename_i Fil_Sort_ename_total)
            (command "-plot" "Yes" ;Detailed Plot Configuration?
                    "model" ;Model or Layout
                    "DWG To PDF.pc3" ;Printer Name
                    ; "ISO_full_bleed_A3_(420.00_x_297.00_mm)" ;Paper Size
                    paper_size ;Paper Size
                    "m" ;Paper Units Millimeters
                    "Landscape" ;Orientation
                    "No" ;Plot Upside Down
                    "Window" ;Plot Area                
                    (nth 4 (nth Fil_Sort_ename_i Fil_Sort_ename_)) ;vla-getboundingbox
                    (nth 5 (nth Fil_Sort_ename_i Fil_Sort_ename_)) ;vla-getboundingbox
                    ;llpt				;Lower left corner of window
                    ;urpt				;Upper right corner of window
                    "Fit" ;Plot Scale	"Fit"
                    "Center" ;Plot Offset
                    "yes" ;Use Plot Style?
                    "ta3.ctb" ;Plot Style Name
                    "Yes" ;Plot Lineweights?
                    "" ;Enter shade plot setting ["As displayed"/"Wireframe"/"Hidden"/"Visual styles"/"Rendered"]
                    (nth file_name_i file_name)  ; Name file and Location
                    "n" "y"
            )
            
            (setq Fil_Sort_ename_i (+ Fil_Sort_ename_i 1))
            (setq file_name_i (+ file_name_i 1))
          )
      ;
      ;reture_var
        (setvar "filedia" 1)
      ;
      )
      (alert "\n\n\n\nSorry z564_autoplot is expired\nPlease Contact Ta.Trairat@gmail.com\n\n\n\n")
  ) 
)


; ;pre_var
      ;   (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
      ;   (setq lyout (vla-get-activelayout adoc))
      ;   (setq styleSheet "monochrome.ctb")
      ;   (setq cfgName "AutoCAD PDF (High Quality Print).pc3")
      ;   (vla-RefreshPlotDeviceInfo lyout)
      ;   (vla-put-ConfigName lyout CfgName)
      ;   (vla-put-PaperUnits lyout acMillimeters)
      ;   (vla-put-plotrotation lyout ac0degrees)
      ;   ; (vla-put-PlotType lyout acWindow)
      ;   (vla-put-PlotType lyout acExtents)
      ;   (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
      ;   (vla-put-PlotWithLineweights lyout :vlax-true)
      ;   (vla-put-PlotWithPlotStyles lyout :vlax-true)
      ;   (vla-put-StyleSheet lyout styleSheet)
      ;   (vla-put-CenterPlot lyout :vlax-true)
      ;   (vla-put-plotrotation lyout ac0degrees)
      ; ;
      ; ;name-layout
      ;   (vla-put-name lyout "mmm")
      ; ;

  ; (setvar "filedia" 0)
  ; (setq dwg (strcat "2010_" (getvar "dwgname")))
  ; (setq i 0)
  ; (setq file (strcat (getvar 'DWGPREFIX) 
  ;                    (itoa 2010)
  ;                    "_"
  ;                    (substr (setq dwg (getvar 'DWGNAME)) 1 (- (strlen dwg) 4))
  ;                    ".pdf"
  ;            )
  ; )
  ; (command "+saveas" "dwg" "2010" "" file )
  ; (command "close")
;



 
