(vl-load-com) (princ)
(defun getdrawinglayouts ( dwg / doc idx rtn )
    (if (setq doc (LM:getdocumentobject dwg))
        (progn
            (vlax-for lyt (vla-get-layouts doc)
                (setq rtn (cons (vla-get-name lyt) rtn)
                      idx (cons (vla-get-taborder lyt) idx)
                )
            )
            (vlax-release-object doc)
            (mapcar '(lambda ( n ) (nth n rtn)) (vl-sort-i idx '<))
        )
    )
)
(defun LM:getdocumentobject ( dwg / app dbx dwl err vrs )
    (cond
        (   (not (setq dwg (findfile dwg))) nil)
        (   (cdr
                (assoc (strcase dwg)
                    (vlax-for doc (vla-get-documents (setq app (vlax-get-acad-object)))
                        (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                    )
                )
            )
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list app
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument"
                                (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-open (list dbx dwg))))
            (prompt (strcat "\n" (vl-catch-all-error-message err)))
        )
        (   dbx   )
    )
)
;; ในส่วนของ code ที่เพิ่มเติม  
(defun c:ALLPSLT0_ALL_PSLTSCALE ()
  ; (setq dwg "NEW_BEE.dwg") ; เปลี่ยนเป็นชื่อไฟล์ที่ต้องการ
  (setq dwg (getvar "dwgname"))
  (setq layout-names (getdrawinglayouts dwg))
  (princ "\nรายชื่อ layouts ทั้งหมด:\n")
  (mapcar '(lambda (layout) (princ (strcat layout "\n"))) layout-names)
  (princ)
  (setq total_layout-names (length layout-names ))
  (setq i 0)
  (while 
    (< i total_layout-names)
    (setq layname_GO (nth i layout-names))
    (command "ctab" layname_GO)
    ; command_part
      ; (setq ss (ssget "_X" '((0 . "*"))))
      ; (command "erase" ss "")
    (setvar "psltscale" 0)
    ; command_part
    (setq i (+ i 1))
  )
)
(defun c:ALLCL_CLEAR_ALL_LAYOUT ()
  ; (setq dwg "NEW_BEE.dwg") ; เปลี่ยนเป็นชื่อไฟล์ที่ต้องการ
  (setq dwg (getvar "dwgname"))
  (setq layout-names (getdrawinglayouts dwg))
  (princ "\nรายชื่อ layouts ทั้งหมด:\n")
  (mapcar '(lambda (layout) (princ (strcat layout "\n"))) layout-names)
  (princ)
  (setq total_layout-names (length layout-names ))
  (setq i 1)
  (while 
    (< i total_layout-names)
    (setq layname_GO (nth i layout-names))
    (command "ctab" layname_GO)
    ; command_part
      (setq ss (ssget "_X" '((0 . "*"))))
      (command "erase" ss "")
    
    ; command_part
    (setq i (+ i 1))
  )
)
(defun c:ALLsetA4 ()
  ; (setq dwg "NEW_BEE.dwg") ; เปลี่ยนเป็นชื่อไฟล์ที่ต้องการ
  (setq dwg (getvar "dwgname"))
  (setq layout-names (getdrawinglayouts dwg))
  (princ "\nรายชื่อ layouts ทั้งหมด:\n")
  (mapcar '(lambda (layout) (princ (strcat layout "\n"))) layout-names)
  (princ)
  (setq total_layout-names (length layout-names ))
  (setq i 1)
  (while 
    (< i total_layout-names)
    (setq layname_GO (nth i layout-names))
    (command "ctab" layname_GO)
    ; command_part
      (TA:runtoset_layout_)
      (defun TA:runtoset_layout_ ()
        ;set_var_plot
          ; (setq ss (car (entsel)))
          (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
          (setq lyout (vla-get-activelayout adoc))
          (setq styleSheet "TA3.ctb")
          (setq cfgName "AutoCAD PDF (High Quality Print).pc3")
          (vla-RefreshPlotDeviceInfo lyout)
          (vla-put-ConfigName lyout CfgName)
          
          (vla-put-PaperUnits lyout acMillimeters)
          (vla-put-plotrotation lyout ac0degrees)
          ; (vla-put-PlotType lyout acWindow)
          (vla-put-PlotType lyout acExtents)
          (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A4_(210.00_x_297.00_MM)")
          ; (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
          (vla-put-PlotWithLineweights lyout :vlax-true)
          (vla-put-PlotWithPlotStyles lyout :vlax-true)
          (vla-put-StyleSheet lyout styleSheet)
          (vla-put-CenterPlot lyout :vlax-true)
          (vla-put-standardscale lyout acScaleToFit )
          (vla-put-plotrotation lyout ac90degrees)
        
        
          (vla-put-PaperUnits lyout acMillimeters)
        ;
      )
    ; command_part
    (setq i (+ i 1))
  )
)


      




;; Get Document Object  -  Lee Mac
;; Retrieves the VLA Document Object for the supplied filename.
;; The Document Object may be present in the Documents collection, or obtained through ObjectDBX.
;; It is the callers responsibility to release such object.