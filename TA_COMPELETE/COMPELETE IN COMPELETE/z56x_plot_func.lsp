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
;Get_Data Function Ta Trai
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
  (defun TA:get_name_block_in_drawing (mode)
    ;user_input_
      (setq mode "mode")
    ;
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

    
    (setq filter_sorted_block_total (length filter_sorted_block_))
    (princ "\n                  |=====================|")
    (princ "\n                  | TOTAL BLOCK IN FILE |")
    (princ (strcat "\n                  |     = " (itoa filter_sorted_block_total) " block     |"))
    (princ "\n                  |          set        |")
    (princ "\n                  |---------------------|\n")
    (setq filter_sorted_block_ filter_sorted_block_)

  )
  (defun TA:get_name_layer_in_drawing ()
    (setq layers_list_ ())
    (setq layers_obj (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
    ;method_1_get_layers_name
      (vlax-for lay layers_obj
        (setq layers_list (append layers_list (list (vla-get-Name lay))))
      )
    ;
    ;method_2_get_layers_name
      (setq layers_i 0)
      (while (< layers_i (vla-get-count layers_obj))
        (setq layers_ (vla-item layers_obj layers_i))
        (setq layers_list_ (append layers_list_ (list (vla-get-name layers_))))
        (setq layers_i (1+ layers_i))
      )
    ;
    ;summary_layer_data
      (setq layers_list_total (length layers_list_))
      (princ "\n                  |=====================|")
      (princ "\n                  | TOTAL LAYERS IN FILE |")
      (princ (strcat "\n                  |     = " (itoa layers_list_total) " layer     |"))
      (princ "\n                  |          set        |")
      (princ "\n                  |---------------------|")
      (setq layers_list_ layers_list_)
    ;
  )
  (defun TA:ename+vla-get-insertionpoint (obj) ;ประกอบ entity_name กับ insertion_poiint ให้เป็น list (BLOCK ONLY)
    (if (= (vla-get-objectname obj) "AcDbBlockReference")
      (progn
        ;get_data
          (setq ename_ (vlax-vla-object->ename obj)) 
          
          (setq _inspt (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint obj))))
        ;
        ;sum_data_to_list
          (setq sum_list (list 
                          ename_
                          _inspt
                          ;add more
                        )
          )
        ;
      )
      (setq error "OBJECT IS NOT BLOCK")
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
  (defun TA:Get_Pline_vertext_angle_case1 (ename_) 
    (setq s_ (cadr (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq d1 (angtos (angle (nth 1 s_) (nth 2 s_))))
    (setq s_i 0)
    (setq s_ii 1)
    (setq end_val_ nil)
    (setq sum_ ())
    (while (< s_i (length s_))
        (setq pt_1 (nth s_i s_))
        (setq pt_3 (nth s_i s_))
        (if (= s_ii (length s_)) 
          (progn 
            (setq end_val_ "end")
            (setq pt_3 (nth (- s_i 1) s_))
          )
          (setq pt_2 (nth s_ii s_))
        )
        (setq angle_pt (atof (angtos (angle pt_1 pt_2))))
        (if (= end_val_ "end") 
          (progn
            (setq angle_pt (atof (angtos (angle pt_3 pt_1))))
            (setq sum (append  (list pt_1) (list angle_pt s_ii) ))
          )
          (setq sum (append  (list pt_1) (list angle_pt s_ii)))
        )
        
        (setq sum_ (cons sum sum_))
        (setq s_i (+ s_i 1))
        (setq s_ii (+ s_ii 1))
    )
    (setq result_ (reverse sum_))
  )
  (defun TA:Get_Pline_vertext_angle_case2 (ename_) 
    (setq s_ (cadr (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq d1 (angtos (angle (nth 1 s_) (nth 2 s_))))
    (setq s_i 0)
    (setq s_ii 1)
    (setq end_val_ nil)
    (setq sum_ ())
    (while (< s_i (length s_))
        (setq pt_1 (nth s_i s_))
        (if (= s_ii (length s_)) 
          (progn 
            (setq end_val_ "end")
          )
          (setq pt_2 (nth s_ii s_))
        )
        (setq angle_pt (atof (angtos (angle pt_1 pt_2))))
        (if (= end_val_ "end") 
          (progn 
            (setq sum (append  (list pt_1) (list end_val_ s_ii) ))
          )
          (setq sum (append  (list pt_1) (list angle_pt s_ii)))
        )
        
        (setq sum_ (cons sum sum_))
        (setq s_i (+ s_i 1))
        (setq s_ii (+ s_ii 1))
    )
    (setq result_ (reverse sum_))
  )
  (defun TA:Get_Pline_vertext_ins_point_ (ename_) ;must be (car (entsel))
    (setq Get_Pline_ (entget ename_))
    (setq Get_Pline_vtx_pt (vl-remove-if-not '(lambda (x) (= 10 (car x))) Get_Pline_))
    (setq Get_Pline_vtx_pt_ (mapcar 'cdr Get_Pline_vtx_pt))
    (setq ename+Get_Pline_vtx_pt_ (list
                                    ename_
                                    Get_Pline_vtx_pt_
                                  )
    
    )
  )
  (defun TA:get_vertex_len_ (ename_)
    ; (setq ref_line (car (entsel)))
    (setq vertex_total (length (TA:Get_Pline_vertext_ins_point_ ename_)))
    (setq sum_ ())

    (setq vertex_i 0)
    (setq vertex_ii 1)

    (while (< vertex_i vertex_total)
      ;get_data_len
      (setq len_i (vlax-curve-getDistatParam ss vertex_i) )
      (setq len_ii (vlax-curve-getDistatParam ss vertex_ii))
      ;
      ;sum
        (setq sum (list (- len_ii len_i)))
        (setq sum_ (append sum sum_ ))
      ;
      (setq vertex_i (+ vertex_i 1))
      (setq vertex_ii (+ vertex_ii 1))
    )
    ;summary_reverse
      (setq sum_len (reverse sum_))
    ;
  )
  (defun TA:midpoint (ins_start_ ins_end_) ;คำนวณ midpoint จากระยะหัว-ท้าย ของเส้น
    ; (setq ins_start_ (list ins_start_x1 ins_start_y1))
    ; (setq ins_end_ (list ins_end_x1 ins_end_y1))
    (setq ins_mid_ (list 
                 (/ (+ (car ins_start_) (car ins_end_)) 2.0)
                 (/ (+ (cadr ins_start_) (cadr ins_end_)) 2.0)
               )
    )
  )
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
(defun c:z560_sorting_title_blk ()
  ;sub_function
    (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
    )
    (defun sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
    )
  ;
  ; (setq REF_block_ nil)
    ; (while ;filer_val
    ;   (not REF_block_)
    ;   (setq REF_block_ (car (entsel "\nSelect a First BLOCK ")))
    ;   (if ;checking nil or bot
    ;     (/= REF_block_ nil) 
    ;       (progn
    ;        (if 
    ;          (and 
    ;            (/= (cdr (assoc 0 (entget REF_block_))) "INSERT")
    ;          )
    ;          (progn 
    ;            (setq REF_block_ nil)
    ;            (alert "Please select a BLOCK")
    ;          )
    ;          (princ "\n")
    ;        )
    ;       )
    ;     (alert "Please select a BLOCK")
    ;   )
    ;   (if ;checking, is blk name correct ?
    ;     (= REF_block_ nil)
    ;     (progn
    ;       (setq REF_block_ nil)
    ;       (alert "Please select a BLOCK")
    ;     )
    ;     (cond ;if name is correct or incorrect
    ;       ((or 
    ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) 
    ;             "LNAD - A4 TITLE BLOCK PART REV01"
    ;         )
    ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) "LAND - A3 TITLE BLOCK")
    ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) "GV3")
    ;       ) 
    ;         (progn 
    ;           (setq REF_block_obj (vlax-ename->vla-object REF_block_))
    ;           (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
    ;         )
    ;         (princ "\n")
    ;       )
    ;       ((or 
    ;         (/= (LM:effectivename REF_block_obj) 
    ;             "LNAD - A4 TITLE BLOCK PART REV01"
    ;         )
    ;         (/= (LM:effectivename REF_block_obj) "LAND - A3 TITLE BLOCK")
    ;         (/= (LM:effectivename (vlax-ename->vla-object REF_block_)) "GV3")
    ;       ) 
    ;         (progn 
    ;           (setq REF_block_ nil)
    ;           (alert "Please select a TITLE BLOCK NAME\nLNAD - A4 TITLE BLOCK PART REV01 \nLAND - A3 TITLE BLOCK \nAutoviewport GV3")
    ;         )
    ;         (princ "/n")
    ;       )
    ;     )
    ;   )
    ;   (cond 
    ;     ((and 
    ;         (= (LM:effectivename REF_block_obj) 
    ;           "LNAD - A4 TITLE BLOCK PART REV01"
    ;         )
    ;       )
    ;       (progn 
    ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "drawing_no1"))
    ;       )
    ;     )
    ;     ((and 
    ;         (= (LM:effectivename REF_block_obj) 
    ;           "LAND - A3 TITLE BLOCK"
    ;         )
    ;       )
    ;       (progn 
    ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "มาตราส่วน_VALUE"))
            
    ;       )
    ;     )
    ;     ((and 
    ;         (= (LM:effectivename REF_block_obj) 
    ;           "GV3"
    ;         )
    ;       )
    ;       (progn 
    ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "NAME_VIEWPORT"))
    ;       )
    ;     )
    ;   )
    ; )
  ;
  ;user_input_mode_val_for_layout_header
    (initget 1 "1 2 3")
    (setq mode-header nil)
    (setq mode-header (cond ( (getkword (strcat "\nspecify_name_blk \nmode 1 = Mock_up \nmode 2 = Drawing_detail\nmode 3 = specify header_drawing_namefile\n<" (rtos (setq mode-header (cond (mode-header) (1.0) ) ) ) "> : " ) ) ) (mode-header) ) )
    (cond
      ((and
        (= mode-header "1")
      )
      (progn
        (setq header "M")
      )
      )
      ((and
        (= mode-header "2")
      )
      (progn
        (setq header  "D")
      )
      )
      ((and
        (= mode-header "3")
      )
      (progn
        (setq header  (getstring "specify header_drawing_namefile"))
      )
      )
    )
  ;
  ;
  ;user_input_mode_val_for_efectivename
    (setq mode-val nil)
    (while ;user_input_mode_val
      (not mode-val)
      (if ;return_base_val_to_user_input
        (/= mode-val-val nil)
        (progn
          (princ "/n")
        )
        (setq mode-val-val 2)
      )
      ;user_input_mode_val
        (setq mode-val (cond ( (getint (strcat "\nspecify_name_blk \nmode 1 = A4-tittle_blk \nmode 2 = A3-tittle_blk \nmode 3 = GV3<" (rtos (setq mode-val-val (cond (mode-val-val) (mode-val) ) ) ) "> : " ) ) ) (mode-val-val) ) )
      ;
      (if ;incorrect_case
        (and
          (/= mode-val 1)
          (/= mode-val 2)
          (/= mode-val 3)
        )
        (progn
          (setq mode-val-val mode-val )
          (setq mode-val nil)
          (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
        )
        (princ mode-val)
      )
      (if ;correct_case
        (or
          (= mode-val 1)
          (= mode-val 2)
          (= mode-val 3)
        )
        (progn
          (setq mode-val-val mode-val )
        )
        (princ mode-val)
      )
    )
    (cond ;specify_efective_name_by_user_input
      ((and
        (= mode-val 1)
       )
       (progn
        (setq blk_efname "LNAD - A4 TITLE BLOCK PART REV01")
       )
      )
      ((and
        (= mode-val 2)
       )
       (progn
        (setq blk_efname  "LAND - A3 TITLE BLOCK")
       )
      )
      ((and
        (= mode-val 3)
       )
       (progn
        (setq blk_efname  "GV3")
       )
      )
    )
  ;
  ;ss_tiltle_blk or gv3
    (setq ss_blk_set_ (ssget 
                        (list 
                          (cons 0 "INSERT") ;type of object
                          ; (cons 8 "000 - GRID")   ;kind of layer
                          ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                          ; (cons 62 1)           ;kind of color call sign with color code index
                        )
                      )
    )
    (setq ss_blk_set_total (sslength ss_blk_set_))
  ;
  ;filter_selection_set_and_sorting_
    (setq ss_blk_set_i 0)
    (setq sorted_set_ ())
    (while ;filter_selection_set
      (< ss_blk_set_i ss_blk_set_total)
      (setq ss_blk_set_ename (ssname ss_blk_set_ ss_blk_set_i))
      (setq ss_blk_set_obj (vlax-ename->vla-object ss_blk_set_ename))
      (setq ss_blk_set_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_blk_set_obj))))
      (setq ss_blk_set_efname (LM:effectivename ss_blk_set_obj)) 
      (setq ss_blk_set_ename+ins (list 
                                  ss_blk_set_ename
                                  (car ss_blk_set_ins)
                                  (cadr ss_blk_set_ins)
                                )
      ) 
      (if
        (= ss_blk_set_efname blk_efname )
        (progn
          (setq sorted_set_ (sort_by_X (cons ss_blk_set_ename+ins sorted_set_)))
        )
        (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
      )
      (setq ss_blk_set_i (+ ss_blk_set_i 1))
      (setq sorted_set_total (length sorted_set_))
    )
    (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos 100 2 0) "%") )
  ;
  ;condition_data_for_attibute
    (setq sorted_set_i 0)
    (setq inum 100)
    (setq ipage 0)
  
    (while
      (< sorted_set_i sorted_set_total)
      (setq Layout_num (strcat header (itoa (setq inum (1+ inum)))))
      (setq sorted_set_list (car (nth sorted_set_i sorted_set_)))
      (setq sorted_set_obj (vlax-ename->vla-object sorted_set_list))
      (cond 
          ((and 
              (= (LM:effectivename sorted_set_obj) 
                "LNAD - A4 TITLE BLOCK PART REV01"
              )
            )
            (progn 
              (setq sorted_set_obj_vpname_1 (LM:vl-setattributevalue sorted_set_obj "drawing_no2" Layout_num ))
              (setq sorted_set_obj_vpname_2 (LM:vl-setattributevalue sorted_set_obj "SHEET_NO." (strcat (itoa (setq ipage (1+ ipage))) " of " (itoa sorted_set_total)) ))
              (setq sorted_set_obj_vpname_3 (LM:vl-setattributevalue sorted_set_obj "TOTAL_SHEET_NO." (itoa sorted_set_total) ))
            )
          )
          ((and 
              (= (LM:effectivename sorted_set_obj) 
                "LAND - A3 TITLE BLOCK"
              )
            )
            (progn 
              (setq sorted_set_obj_vpname_ (LM:vl-setattributevalue sorted_set_obj "แผ่นที่_VALUE" Layout_num))
              
            )
          )
          ((and 
              (= (LM:effectivename sorted_set_obj) 
                "GV3"
              )
            )
            (progn 
              (setq sorted_set_obj_vpname_ (LM:vl-setattributevalue sorted_set_obj "NAME_VIEWPORT" Layout_num))
            )
          )
        )
      (setq sorted_set_i (+ sorted_set_i 1))
    )
  ;
)
(defun c:z561_make_vp_border ()
  ;sub-func
    (defun z57m_sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (car a) (car b))))))
    )
    (defun z57m_sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
    )
  ;
  ;variable_set
    (setq old_osmode (getvar "osmode" ))
    (setvar "osmode" 0)
  ;
  ;get_data_LWPOLYLINE for find lowest and highest point
    (setq REF_1st_LWPOLYLINE nil)
    (while (not REF_1st_LWPOLYLINE)
      (setq REF_1st_LWPOLYLINE (entsel "\nSelect a First LWPOLYLINE "))
      (if 
        (and REF_1st_LWPOLYLINE (= (cdr (assoc 0 (entget (car REF_1st_LWPOLYLINE)))) "LWPOLYLINE"))
          (setq REF_1st_LWPOLYLINE (car REF_1st_LWPOLYLINE))
          
          (progn
            (setq REF_1st_LWPOLYLINE nil)
            (alert "Please select a LWPOLYLINE")
          )
      )
      (if
        (/= REF_1st_LWPOLYLINE nil)
        (progn 
          (setq REF_1st_LWPOLYLINE_obj (vlax-ename->vla-object REF_1st_LWPOLYLINE))
          ;filter assoc 10
            (setq REF_1st_LWPOLYLINE_10th_data (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget REF_1st_LWPOLYLINE)))
            (setq REF_1st_LWPOLYLINE_10th_data_mapcar_assoc (mapcar 'cdr REF_1st_LWPOLYLINE_10th_data))
            (setq REF_1st_LWPOLYLINE_10th_data_mapcar_x (mapcar 'car REF_1st_LWPOLYLINE_10th_data_mapcar_assoc))
            (setq REF_1st_LWPOLYLINE_10th_data_mapcar_y (mapcar 'cadr REF_1st_LWPOLYLINE_10th_data_mapcar_assoc))
          ;
          ;make_highest_insertion_point_val
            (setq new_highest_ins 
              (list
                (setq highest_ins_x (car (vl-sort REF_1st_LWPOLYLINE_10th_data_mapcar_x '>)))
                (setq highest_ins_y (car (vl-sort REF_1st_LWPOLYLINE_10th_data_mapcar_y '>)))
              )
            )
          ;
          ;make_lowest_insertion_point_val
            (setq new_lowest_ins_ 
              (list
                (car (setq lowest_ins_x (vl-sort REF_1st_LWPOLYLINE_10th_data_mapcar_x '<)))
                (car (setq lowest_ins_y (vl-sort REF_1st_LWPOLYLINE_10th_data_mapcar_y '<)))
              )
            )
          ;
        )
        (princ "/n")
      )
    )
  ;
  ;remove_old_Rectangle_create_new_Rectangle
    (setvar "osmode" 0)
    (if
      (/= REF_1st_LWPOLYLINE nil)
      (progn
        (vla-erase REF_1st_LWPOLYLINE_obj)
        (command "rectangle" new_lowest_ins_ new_highest_ins "")
        (setq get_rec_ (entlast))
        (setq get_rec_obj (vlax-ename->vla-object get_rec_))
        (setq get_rec_obj_10th_data (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget get_rec_)))
        (setq get_rec_obj_10th_data_mapcar_assoc (mapcar 'cdr get_rec_obj_10th_data))
        (setq get_rec_width (- (car (nth 1 get_rec_obj_10th_data_mapcar_assoc)) (car (nth 0 get_rec_obj_10th_data_mapcar_assoc))))
        (setq get_rec_height (- (cadr (nth 3 get_rec_obj_10th_data_mapcar_assoc)) (cadr (nth 0 get_rec_obj_10th_data_mapcar_assoc))))
      )
    )
    (setvar "osmode" old_osmode)
  ;
  ;REF_blk_for_GV2
    (setq REF_block_ nil)
    (while (not REF_block_)
        (setq REF_block_ (entsel "\nSelect a First BLOCK "))
        (if 
          (and REF_block_ (= (cdr (assoc 0 (entget (car REF_block_)))) "INSERT"))
            (setq REF_block_ (car REF_block_))
            
            (progn
              (setq REF_block_ nil)
              (alert "Please select a BLOCK")
            )
        )
        (if
          (/= REF_block_ nil)
          (progn 
            (setq REF_block_obj (vlax-ename->vla-object REF_block_))
          )
          
          
          (princ "/n")
        )
        (cond
          ((or 
             (= (LM:effectivename REF_block_obj) 
                "LNAD - A4 TITLE BLOCK PART REV01"
             )
             (= (LM:effectivename REF_block_obj) "LAND - A3 TITLE BLOCK")
           ) 
            (progn 
              (setq REF_block_obj (vlax-ename->vla-object REF_block_))
              (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
            )
            (princ "\n")
          )
          ((or 
             (/= (LM:effectivename REF_block_obj) 
                 "LNAD - A4 TITLE BLOCK PART REV01"
             )
             (/= (LM:effectivename REF_block_obj) "LAND - A3 TITLE BLOCK")
           ) 
            (progn 
              (setq REF_block_ nil)
              (alert "Please select a TITLE BLOCK NAME\nLNAD - A4 TITLE BLOCK PART REV01 \nLAND - A3 TITLE BLOCK")
            )
            (princ "/n")
          )
        )
        (cond 
          ((and 
             (= (LM:effectivename REF_block_obj) 
                "LNAD - A4 TITLE BLOCK PART REV01"
             )
           )
           (progn 
             (setq REF_block_obj_sc_ (LM:getdynpropvalue REF_block_obj "SC"))
             (setq DRAWING_NO2 (LM:vl-getattributevalue REF_block_obj "DRAWING_NO2"))
             
            
             
           )
          )
          ((and 
             (= (LM:effectivename REF_block_obj) 
                "LAND - A3 TITLE BLOCK"
             )
           )
           (progn 
             (setq REF_block_obj_sc_ (vla-get-xscalefactor REF_block_obj))
           )
          )
        )
    )
  ;
  ;insertion_GV2
    (setvar "osmode" 0)
    (command "insert" "GV3" new_lowest_ins_ 1 0)
    ;get_data_GV2
      (setq GV2_ (entlast))
      (setq GV2_obj (vlax-ename->vla-object GV2_))
      ;set_dyn_data
        (cond 
          ((or
             (= (LM:effectivename REF_block_obj) 
                "LNAD - A4 TITLE BLOCK PART REV01"
             )
             (= (LM:effectivename REF_block_obj) 
                "LAND - A3 TITLE BLOCK"
             )
           )
           (progn
            (LM:setdynpropvalue GV2_obj "sc" REF_block_obj_sc_ )
           )
           (princ "/n")
          )
        )
         
         
        (vla-put-insertionpoint GV2_obj (vlax-3d-point (car new_lowest_ins_)(cadr new_lowest_ins_) 0))
        (LM:setdynpropvalue GV2_obj "base_p x" (-  (car REF_block_obj_ins) (car new_lowest_ins_) ))
        (LM:setdynpropvalue GV2_obj "base_p y" (-  (cadr REF_block_obj_ins) (cadr new_lowest_ins_) ))
        
        (LM:setdynpropvalue GV2_obj "l" get_rec_width)
        (LM:setdynpropvalue GV2_obj "h" get_rec_height)
      ;
      ;set_att
        
        (setq GV2_obj_id (itoa (vla-get-objectid GV2_obj)))
        (setq REF_block_obj_id (itoa (vla-get-objectid REF_block_obj)))

        (setq SC_VIEWPORT (strcat "SC_VPORT :" "%<\\AcObjProp Object(%<\\_ObjId " GV2_obj_id ">%).Parameter(34).UpdatedDistance \\f " "%lu2%pr0"">%"))
        (setq SEQ_VIEWPORT (strcat "%<\\AcObjProp Object(%<\\_ObjId " GV2_obj_id ">%).Parameter(18).lookupString \\f " "%lu2%pr0"">%"))


        (LM:vl-setattributevalue GV2_obj "SEQ_VIEWPORT" SEQ_VIEWPORT)
        (LM:vl-setattributevalue GV2_obj "SC_VPORT" SC_VIEWPORT)
        (LM:vl-setattributevalue GV2_obj "NAME_VIEWPORT" DRAWING_NO2)
        
        (command "._updatefield" lasted_ "")
        (command "regen")
      ;
    ;
  ;
  ;ease_get_rec_obj
    (vla-erase get_rec_obj)
  ;
  
  (setvar "osmode" 1215)
      ;::::method 2
      ; (setq select_name_obj (vlax-ename->vla-object select_name))
      ; (setq efname_blk (LM:effectivename select_name_obj))
  
)
(defun c:z562_create_new_layout ()

  ;get_all_layout
    (setq doc (vla-get-activedocument (vlax-get-acad-object)))
    (setq layouts (vla-get-layouts doc))
    (setq layout-names '())
    (vlax-for layout layouts
      (setq layout-name (vla-get-name layout))
      (setq layout-names (cons layout-name layout-names))
      (setq layout-names (vl-remove "Model" layout-names))         
    )
  ;
  ;use-input-specify_name_blk
    (initget "1 2 3")
    (setq mode-v nil)
    (setq mode-v (cond ( (getkword (strcat "\nspecify_name_blk \nmode 1 = A4-tittle_blk \nmode 2 = A3-tittle_blk \nmode 3 = GV3<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : " ) ) ) (mode-v) ) )
    (cond
      ((and
        (= mode-v "1")
       )
       (progn
        (setq blk_efname "LNAD - A4 TITLE BLOCK PART REV01")
       )
      )
      ((and
        (= mode-v "2")
       )
       (progn
        (setq blk_efname  "LAND - A3 TITLE BLOCK")
       )
      )
      ((and
        (= mode-v "3")
       )
       (progn
        (setq blk_efname  "GV3")
       )
      )
    )
  ;
  ;ss_tiltle_blk or gv3
    (setq ss_blk_set_ (ssget 
                        (list 
                          (cons 0 "INSERT") ;type of object
                          ; (cons 8 "000 - GRID")   ;kind of layer
                          ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                          ; (cons 62 1)           ;kind of color call sign with color code index
                        )
                      )
    )
    (setq ss_blk_set_total (sslength ss_blk_set_))
  ;
  ;filter_selection_set_and_sorting_and_create
    (setq ss_blk_set_i 0)
    (setq sorted_set_ ())
    (while ;filter_selection_set
      (< ss_blk_set_i ss_blk_set_total)
      (setq ss_blk_set_ename (ssname ss_blk_set_ ss_blk_set_i))
      (setq ss_blk_set_obj (vlax-ename->vla-object ss_blk_set_ename))
      (setq ss_blk_set_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_blk_set_obj))))
      (setq ss_blk_set_efname (LM:effectivename ss_blk_set_obj)) 
      (setq ss_blk_set_ename+ins (list 
                                   ss_blk_set_ename
                                   (car ss_blk_set_ins)
                                   (cadr ss_blk_set_ins)
                                 )
      ) 
      (if
        (= ss_blk_set_efname blk_efname )
        (progn
          (setq sorted_set_ (sort_by_X (cons ss_blk_set_ename+ins sorted_set_)))
        )
        (princ "/n")
      )
      (setq ss_blk_set_i (+ ss_blk_set_i 1))
      (setq sorted_set_total (length sorted_set_))
    )
    (setq sorted_set_i 0)
    (while ;create_name_layout
      (< sorted_set_i (length sorted_set_))
      (setq sorted_set_list (car (nth sorted_set_i sorted_set_)))
      (setq sorted_set_obj (vlax-ename->vla-object sorted_set_list))
      (setq sorted_set_efname (LM:Effectivename sorted_set_obj))
      
      (cond ;condition-to-get-layout_name
        ((and
          (= sorted_set_efname "LNAD - A4 TITLE BLOCK PART REV01")
        )
        (progn
          (setq get_layout_name (LM:vl-getattributevalue sorted_set_obj "DRAWING_NO2"))
        )
        )
        ((and
          (= sorted_set_efname  "LAND - A3 TITLE BLOCK")
        )
        (progn
          (setq get_layout_name (LM:vl-getattributevalue sorted_set_obj "แผ่นที่_value"))
        )
        )
        ((and
          (= sorted_set_efname  "GV3")
        )
        (progn
          (setq get_layout_name (LM:vl-getattributevalue sorted_set_obj "NAME_VIEWPORT"))
        )
        )
      )
      
      (setq layout-names_i 0)
      
      (while ;delete-exiting-layout
        (< layout-names_i (length layout-names))
        (setq layout-names-list (nth layout-names_i layout-names))
          (if
            (= (vla-get-name (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) layout-names-list)) get_layout_name)
            (progn
              (vla-delete (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) layout-names-list))
            )
            (princ "/n")
          )
        (setq layout-names_i (+ layout-names_i 1))
      )
      ;create-layout_and_go
        (command "layout" "n" get_layout_name)
        (command "ctab" get_layout_name)
      ;
      ;set_var_plot
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
        (vla-put-CanonicalMediaName lyout "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
        (vla-put-PlotWithLineweights lyout :vlax-true)
        (vla-put-PlotWithPlotStyles lyout :vlax-true)
        (vla-put-StyleSheet lyout styleSheet)
        (vla-put-CenterPlot lyout :vlax-true)
        (vla-put-plotrotation lyout ac0degrees)
      ;
      ;delete_all_in_layout
        (setq ss (ssget "_X" '((0 . "*"))))
        (command "erase" ss "")
      ;
      (setq sorted_set_i (+ sorted_set_i 1))
    )
  ;
)
(defun c:z563_autoviewport ()
  ;setting_variable_function
    (setq old_OSMODE (getvar "OSMODE"))
    (setq new_OSMODE (setvar "OSMODE" 0))
    (command "ucs" "")
  ;
  ;selection_group_and_filter_GV_BLOCk 
    (setq my_gv_port_ (ssget ;
                      (list 
                        (cons 0 "INSERT") ;type of object
                        ; (cons 8 "000 - GRID")   ;kind of layer
                        ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                        ; (cons 62 1)           ;kind of color call sign with color code index
                      )
                    )
    )
    (setq new_EF_name "GV3")
    (setq my_gv_port_total (sslength my_gv_port_))
    (setq my_gv_port_i 0) 
    (setq ssname_list ())
    (while
      (< my_gv_port_i my_gv_port_total)
      (setq my_gv_port_ename (ssname my_gv_port_ my_gv_port_i))
      (setq my_gv_port_obj (vlax-ename->vla-object my_gv_port_ename))
      (setq ex_EF_name (LM:effectivename my_gv_port_obj))
        (if (= new_EF_name ex_EF_name) 
          (progn 
            (setq ssname_list (cons my_gv_port_ename ssname_list))
          )
          (princ "\n")
        )
      (setq my_gv_port_i (+ my_gv_port_i 1))
    )
    (setq ef_filter_gv2_ (ssadd))
    (foreach ename ssname_list 
      (ssadd ename ef_filter_gv2_)
    )
    
  ;
  ;sorting_coordinate_x
    (setq ssname_sort_x ())
    (setq GV2_x ())
    (setq ef_filter_gv2_total (sslength ef_filter_gv2_))
    (setq ef_filter_gv2_i 0)
    (while
     (< ef_filter_gv2_i ef_filter_gv2_total)
     (setq ef_filter_gv2_ename (ssname ef_filter_gv2_ ef_filter_gv2_i))
      (setq ef_filter_gv2_obj (vlax-ename->vla-object ef_filter_gv2_ename))
      (setq ef_filter_gv2_obj_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ef_filter_gv2_obj))))
      (setq sort_ef_filter_gv2_obj_ins_xyz 
             (list
               ef_filter_gv2_ename
               (car ef_filter_gv2_obj_ins_xyz)
               (cadr ef_filter_gv2_obj_ins_xyz)
             )
      )
      (setq ssname_sort_x (cons sort_ef_filter_gv2_obj_ins_xyz ssname_sort_x))
      (setq ef_filter_gv2_i (+ ef_filter_gv2_i 1))
     
    )
    (setq GV2_ss_sorted_ (sort_by_X ssname_sort_x))
    (setq sorted-ename-list '())
    (foreach xx GV2_ss_sorted_
      (setq sorted-ename-list (cons (car xx) sorted-ename-list))
    )
    (setq filter_gv2_1 (reverse sorted-ename-list))
    (setq filter_gv2_ (ssadd))
    (foreach ename filter_gv2_1 
      (ssadd ename filter_gv2_)
    )
    (sslength filter_gv2_)
    

    
  ;
  
  ;get_gv2_dynamic_data
    (setq filter_gv2_total (sslength filter_gv2_))
    (setq filter_gv2_i 0)
    (setq layout_num 1)
    (while
      (setq filter_gv2_ename (ssname filter_gv2_ filter_gv2_i))
      (setq filter_gv2_obj (vlax-ename->vla-object filter_gv2_ename))

        
        
      
        (setq DYN_sc (LM:GETDYNPROPVALUE filter_gv2_obj "SC"))
        (setq DYN_L (LM:GETDYNPROPVALUE filter_gv2_obj "L"))
        (setq DYN_H (LM:GETDYNPROPVALUE filter_gv2_obj "H"))
        (setq DYN_base_p_x (LM:GETDYNPROPVALUE filter_gv2_obj "base_p x"))
        (setq DYN_base_p_y (LM:GETDYNPROPVALUE filter_gv2_obj "base_p y"))
        (setq DYN_base_p_y (LM:GETDYNPROPVALUE filter_gv2_obj "base_p y"))
      
        (setq name_viewport (LM:vl-getattributevalue filter_gv2_obj "name_viewport"))
        ; (setq name_viewport (LM:vl-setattributevalue filter_gv2_obj "name_viewport" (strcat "Layout" (rtos layout_num 2 0 ))))

        (setq filter_gv2_obj_ins_xyz (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint filter_gv2_obj))))
        (setq filter_gv2_obj_base_p_tittle_blk
              (list
                (+ (car filter_gv2_obj_ins_xyz) DYN_base_p_x)
                (+ (cadr filter_gv2_obj_ins_xyz) DYN_base_p_y)
              )
        
        )
        (setq filter_gv2_obj_oposite_ins_xyz
              (list
                (+ (car filter_gv2_obj_ins_xyz) DYN_L)
                (+ (cadr filter_gv2_obj_ins_xyz) DYN_H)
              )
        
        )
        (setq filter_gv2_obj_half_oposite_ins_xyz
              (list
                (/ (- (+ (car filter_gv2_obj_ins_xyz) (/ DYN_L 2)) (car filter_gv2_obj_base_p_tittle_blk)) DYN_sc )
                (/ (- (+ (cadr filter_gv2_obj_ins_xyz) (/ DYN_H 2)) (cadr filter_gv2_obj_base_p_tittle_blk)) DYN_sc)
              )
        
        )
        (setq filter_gv2_obj_half_ins_xyz
              (list
                (- (car filter_gv2_obj_half_oposite_ins_xyz) (car filter_gv2_obj_ins_xyz))
                (- (cadr filter_gv2_obj_half_oposite_ins_xyz) (cadr filter_gv2_obj_ins_xyz))
              )
        
        )
        
      

        ; (command "pselect" filter_gv2_ename )
        (if 
          (/= name_viewport "") 
          (progn 
            (command "ctab" name_viewport )
            (command "mview" "ne" filter_gv2_obj_ins_xyz filter_gv2_obj_oposite_ins_xyz "" filter_gv2_obj_half_oposite_ins_xyz)
            (setq get_viewport_ (entlast))
            (setq get_viewport_obj (vlax-ename->vla-object get_viewport_))
            (setq vp_height (vla-put-height get_viewport_obj (/ DYN_H DYN_sc)))
            (setq vp_weights (vla-put-width get_viewport_obj (/ DYN_L DYN_sc)))
          )
          (princ "/n")
        )
      (setq filter_gv2_i (+ filter_gv2_i 1))
      (setq layout_num (+ layout_num 1))
      
    )
   
  ;
  ;return_osmode_val
    (setvar "OSMODE" old_OSMODE )
  ;
  ;zoom extent_layout
    (command "zoom" "e")
  ;
)
(defun c:z564_autoplot_pline ()
  (if ;expire_func
    ; (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
    (> (setq now (rtos (getvar "cdate") 2 4)) (rtos (- (atof now) 0.005) 2 4))
    (progn
      ;sub_func
        (defun sort-by-xx (list) 
          (setq sorted-list (vl-sort list 
                                      (function 
                                        (lambda (a b) 
                                          (< (car (car a)) (car (car b)))
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
      ;user_input_Size
        (initget "1 2")
          (setq mode-v nil)
          (setq mode-v (cond ( (getkword (strcat " \nmode 1 = A4 \nmode 2 = A3 \n<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : " ) ) ) (mode-v) ) )
          (cond
            ((and
              (= mode-v "1")
            )
            (progn
              (setq paper_size "ISO_full_bleed_A4_(210.00_x_297.00_MM)")
            )
            )
            ((and
              (= mode-v "2")
            )
            (progn
              (setq paper_size "ISO_full_bleed_A3_(420.00_x_297.00_mm)")
            )
            )
          )
      ;
      ;user_input_mode_val_for_layout_header
          
          
          (setq mode-header (cond ( (getstring (strcat "\nspecify_name_blk \nmode 1 = Mock_up \nmode 2 = Drawing_detail<" (setq mode-header (cond (mode-header) (1.0) ) ) "> : " ) ) ) (mode-header) ) )
          
        ;
      ;selection_set_LWPOLYLINE
        (setq ss_LWPOLYLINE_ (ssget 
                                  (list 
                                    (cons 0 "LWPOLYLINE") ;type of object
                                    ;  (cons 8 "000 - GRID") ;kind of layer
                                    ; (cons 2 "SSSS")       ;kind of nameblock
                                    ; (cons 62 1)           ;kind of color call sign with color code index
                                  )
                                )
        )
      ;
      ;preloop
        (setq ss_LWPOLYLINE_i 0)
        (setq ss_LWPOLYLINE_total (sslength ss_LWPOLYLINE_))
        (setq ins_ ())
        (setq ipp 0)
        (setq ip 100)
        (setq file_name ())
      ;
      (while ;get_point_pline
        (< ss_LWPOLYLINE_i ss_LWPOLYLINE_total)
        ;get_data_all_ss
          ;get_data_ss_LWPOLYLINE_
            (setq ss_LWPOLYLINE_ename (ssname ss_LWPOLYLINE_ ss_LWPOLYLINE_i))
            (setq ss_LWPOLYLINE_obj (vlax-ename->vla-object ss_LWPOLYLINE_ename))
          ;
          ;filter assoc 10
            (setq ss_LWPOLYLINE_obj_10th_data (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget ss_LWPOLYLINE_ename)))
            (setq ss_LWPOLYLINE_obj_10th_data_mapcar_assoc (mapcar 'cdr ss_LWPOLYLINE_obj_10th_data))
            (setq ss_LWPOLYLINE_obj_10th_data_mapcar_x (mapcar 'car ss_LWPOLYLINE_obj_10th_data_mapcar_assoc))
            (setq ss_LWPOLYLINE_obj_10th_data_mapcar_y (mapcar 'cadr ss_LWPOLYLINE_obj_10th_data_mapcar_assoc))
          ;
          ;make_highest_insertion_point_val
            (setq new_highest_ins 
              (list
                (setq highest_ins_x (car (vl-sort ss_LWPOLYLINE_obj_10th_data_mapcar_x '>)))
                (setq highest_ins_y (car (vl-sort ss_LWPOLYLINE_obj_10th_data_mapcar_y '>)))
                0
              )
            )
          ;
          ;make_lowest_insertion_point_val
            (setq new_lowest_ins_ 
              (list
                (car (setq lowest_ins_x (vl-sort ss_LWPOLYLINE_obj_10th_data_mapcar_x '<)))
                (car (setq lowest_ins_y (vl-sort ss_LWPOLYLINE_obj_10th_data_mapcar_y '<)))
                0
              )
            )
          ;
          ; 
            (setq sum (list new_lowest_ins_ new_highest_ins))
            (setq ins_ (cons sum  ins_))
            (setq ins_ (sort-by-xx (reverse ins_)))
            (setq ins_list (nth 0 (car ins_)))


            
        
          ;
        ;
        (setq ss_LWPOLYLINE_i (+ ss_LWPOLYLINE_i 1))
      )
      (while ;get_num_file
        (< ipp ss_LWPOLYLINE_total)
        ;make_file_name
          
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
          (if (findfile file) 
            (vl-file-delete file)
          )
          (setq file_name (cons file file_name))
          (setq file_name (vl-sort file_name '<))

          (setq ipp (+ ipp 1))
          
      )
      ;
      ;make_plot_to_file
        ; (vla-SetWindowToPlot lyout (vlax-2d-point new_lowest_ins_) (vlax-2d-point new_highest_ins))
        ; (vla-PlotToFile (vla-get-Plot adoc) (strcat (getvar 'dwgprefix) "draw" (itoa ss_LWPOLYLINE_i)))
          (setq ins_total (length ins_))
          (setq ins_i 0)
          (setq cmd (getvar 'cmdecho))
          (setvar 'cmdecho 0)
          (command "tilemode" "1")
          (while 
            (< ins_i ins_total)
            (command "-plot" "Yes" ;Detailed Plot Configuration?
                    "model" ;Model or Layout
                    "DWG To PDF.pc3" ;Printer Name
                    ; "ISO_full_bleed_A3_(420.00_x_297.00_mm)" ;Paper Size
                    paper_size ;Paper Size
                    "m" ;Paper Units Millimeters
                    "Landscape" ;Orientation
                    "No" ;Plot Upside Down
                    "Window" ;Plot Area
                      
                    (car (nth ins_i ins_)) 
                    (cadr (nth ins_i ins_)) 

                    ;llpt				;Lower left corner of window
                    ;urpt				;Upper right corner of window
                    "Fit" ;Plot Scale	"Fit"
                    "Center" ;Plot Offset
                    "yes" ;Use Plot Style?
                    "grayscale.ctb" ;Plot Style Name
                    "Yes" ;Plot Lineweights?
                    "" ;Enter shade plot setting ["As displayed"/"Wireframe"/"Hidden"/"Visual styles"/"Rendered"]
                    (nth ins_i file_name)  ; Name file and Location
                    "n" "y"
            )
            (setq ins_i (+ ins_i 1))
          )
      ;
      ;reture_var
        (setvar "filedia" 1)
      ;
    )
    (alert "\n\n\n\nSorry z564_autoplot is expired\nPlease Contact Ta.Trairat@gmail.com\n\n\n\n")
  ) 
)
(defun c:z565_autoplot_block ()
  (if ;expire_func
    ; (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
    (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
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
               (= mode-T-BLK "3 Use have to select_block")
             )
              (progn
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
              (setq mode-header (cond ( (getstring (strcat "\nspecify prefix name file <" (setq mode-header (cond (mode-header) (1.0) ) ) "> : " ) ) ) (mode-header) ) )
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
                (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
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
                (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
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
                    "TA3.ctb" ;Plot Style Name
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

(defun c:z566_sorting_layout_tab ()
  ;get_all_layout
    (setq doc (vla-get-activedocument (vlax-get-acad-object)))
    (setq layouts (vla-get-layouts doc))
    (setq layout-names_ '())
    (vlax-for layout layouts
      (setq layout-name (vla-get-name layout))
      (setq layout-names_ (cons layout-name layout-names_))
      (setq layout-names_ (vl-remove "Model" layout-names_))         
    )
  ;
  ;preloop_and_while sorting_tab
    ; (setq sort_patt_ (vl-sort layout-names_ '<)) เก็บไว้ก่อน นึกไม่ออกจะเอาไปทำอะไร
    ; (setq sort_patt_ (vl-sort layout-names_ '>)) เก็บไว้ก่อน นึกไม่ออกจะเอาไปทำอะไร
    (setq sort_patt_ layout-names_)
    (setq sort_patt_i 0)
    (while (< sort_patt_i (length sort_patt_))
      (setq tabname (nth sort_patt_i sort_patt_))
      (setq TabOrder1 (vla-put-taborder (vla-Item (vla-get-Layouts doc) TabName ) 1))
      (setq sort_patt_i (+ sort_patt_i 1))
    )
  ;
)
(defun c:z567_rename_layout_tab ()
  ;sub_func
    (defun sort_layout_ (list_)  ;เรียงชุดข้อมูลตามแนวแกน
      (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
    )
  ;
  ;RESET_get_all_layout
    (defun call_tabname ()
      
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (setq layouts (vla-get-layouts doc))
      (setq layout-names_ '())
      (vlax-for layout layouts
        (setq layout-name (vla-get-name layout))
        (setq layout-names_ (cons layout-name layout-names_))
        (setq layout-names_ (vl-remove "Model" layout-names_))         
      )
      (length layout-names_)
    )
    (call_tabname)
  ;
  ;RESET_preLoop_and_While Sorting_tabname_layout_realtime
    (defun call_seuq  ()
        (setq layout-names_i 0)
        (setq sequ-names_i 0)
        (setq sum_tab_names ())
        (while (< layout-names_i (length layout-names_))
          ;get_data_name_and_sequence
          (setq layout-names_tabname (nth layout-names_i layout-names_))
          (setq sequ (vla-get-taborder (vla-Item (vla-get-Layouts doc) layout-names_tabname)))
          (setq Sorted_tab_name (list 
                                  layout-names_tabname
                                  sequ
                                  sequ-names_i
                                )
          )
          (setq sum_tab_names (cons Sorted_tab_name sum_tab_names))
          (setq layout-names_i (+ layout-names_i 1))
          (setq sequ-names_i (+ sequ-names_i 1))
          
          (setq Sorted_tab_names_ (sort_layout_ sum_tab_names))
        )
    )
    (call_seuq)
  ;
  ;RESET_preLoop_and_While rename_tabname
    (setq drawing_num_ 1000)
    (setq drawing_header "reset")
    (setq Sorted_tab_names_i 0)
    (while (< Sorted_tab_names_i (length Sorted_tab_names_))
      (setq Sorted_tab_names_list (car (nth Sorted_tab_names_i Sorted_tab_names_)))
      (setq Sorted_tab_num_list (caddr (nth Sorted_tab_names_i Sorted_tab_names_)))
      
      (setq drawing_num_ (+ drawing_num_ 1))
      (setq drawing_header+num_ (strcat drawing_header "-" (rtos drawing_num_ 2 0)))
      (vla-put-name (vla-Item (vla-get-Layouts doc) Sorted_tab_names_list) drawing_header+num_)
      
      (setq Sorted_tab_names_i (+ Sorted_tab_names_i 1))
    )
  ;
  ;user_input
    (setq drawing_header (getstring "specify drawing header"))
    (setq drawing_num_ (getint "specify drawing number"))
  ;
  ;preLoop_and_While rename_tabname
    (call_tabname) ;recall_command
    (call_seuq) ;recall_command
    (setq Sorted_tab_names_i 0)
    (while (< Sorted_tab_names_i (length Sorted_tab_names_))
      (setq Sorted_tab_names_list (car (nth Sorted_tab_names_i Sorted_tab_names_)))
      (setq Sorted_tab_num_list (caddr (nth Sorted_tab_names_i Sorted_tab_names_)))
      
      (setq drawing_num_ (+ drawing_num_ 1))
      (setq drawing_header+num_ (strcat drawing_header "-" (rtos drawing_num_ 2 0)))
      (vla-put-name (vla-Item (vla-get-Layouts doc) Sorted_tab_names_list) drawing_header+num_)
      
      (setq Sorted_tab_names_i (+ Sorted_tab_names_i 1))
    )
  ;
)
(defun c:z568_preset_plotter ()
  (defun TA:preset_plotter_mode_1 ()
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
      (defun TA:preset_plotter_mode_2 ()
        ;set_var_plot
          ; (setq ss (car (entsel)))
          (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
          (setq lyout (vla-get-activelayout adoc))
          (setq styleSheet "monochrome.ctb")
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
  ;user_input_mode_to_preset_plot_var_
    (setq scl (cond ( (getint (strcat "\nspecify_mode \nmode 1 = TA:VERSION \nmode 2 = MONOCHROME \nmode 3 = NOT AVALIABLE<" (rtos (setq scl (cond (scl) (1.0) ) ) ) "> : " ) ) ) (scl) ) )
  ;
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
    (setvar "psltscale" 0)

    ; command_part
    
      ;main_idean_command_
        (cond
          ((= mode-val 1)
            (progn
              (TA:preset_plotter_mode_1 )
            )
          )
          ((= mode-val 2)
            (progn
              (TA:preset_plotter_mode_2 )
            )
          )
        )
      ;
      (defun TA:preset_plotter_mode_1 ()
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
      (defun TA:preset_plotter_mode_2 ()
        ;set_var_plot
          ; (setq ss (car (entsel)))
          (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
          (setq lyout (vla-get-activelayout adoc))
          (setq styleSheet "monochrome.ctb")
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



;NM_BU2_func_
  (defun c:nm565_autoplot_block ()
    (if ;expire_func
      ; (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
      (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
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
                (setq efname "LNAD - A4 TITLE BLOCK PART REV01-NMBU2")
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
                  (setq mode-header (LM:vl-getattributevalue Fil_Sort_ename_obj "DRAWING_NO2_NM" ))
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
                      (cond 
                        ((= mode-T-BLK "3")
                        (progn 
                          "TA3 - FOR NORMIE.ctb" ;IF U USE mode 3 we r not a same
                        )
                        )
                        ((or (= mode-T-BLK "1") (= mode-T-BLK "2"))
                        (progn 
                          "TA3.ctb" ;Plot Style Name
                        )
                        )
                      )
                      
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
  (defun c:nm560_sorting_title_blk ()
    ;sub_function
      (defun sort_by_X (list_)  ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
      (defun sort_by_y (list_)  ;เรียงชุดข้อมูลตามแนวแกน
        (setq sortedLst (vl-sort list_ (function (lambda (a b) (< (cadr a) (cadr b))))))
      )
    ;
    ; (setq REF_block_ nil)
      ; (while ;filer_val
      ;   (not REF_block_)
      ;   (setq REF_block_ (car (entsel "\nSelect a First BLOCK ")))
      ;   (if ;checking nil or bot
      ;     (/= REF_block_ nil) 
      ;       (progn
      ;        (if 
      ;          (and 
      ;            (/= (cdr (assoc 0 (entget REF_block_))) "INSERT")
      ;          )
      ;          (progn 
      ;            (setq REF_block_ nil)
      ;            (alert "Please select a BLOCK")
      ;          )
      ;          (princ "\n")
      ;        )
      ;       )
      ;     (alert "Please select a BLOCK")
      ;   )
      ;   (if ;checking, is blk name correct ?
      ;     (= REF_block_ nil)
      ;     (progn
      ;       (setq REF_block_ nil)
      ;       (alert "Please select a BLOCK")
      ;     )
      ;     (cond ;if name is correct or incorrect
      ;       ((or 
      ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) 
      ;             "LNAD - A4 TITLE BLOCK PART REV01"
      ;         )
      ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) "LAND - A3 TITLE BLOCK")
      ;         (= (LM:effectivename (vlax-ename->vla-object REF_block_)) "GV3")
      ;       ) 
      ;         (progn 
      ;           (setq REF_block_obj (vlax-ename->vla-object REF_block_))
      ;           (setq REF_block_obj_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint REF_block_obj))))
      ;         )
      ;         (princ "\n")
      ;       )
      ;       ((or 
      ;         (/= (LM:effectivename REF_block_obj) 
      ;             "LNAD - A4 TITLE BLOCK PART REV01"
      ;         )
      ;         (/= (LM:effectivename REF_block_obj) "LAND - A3 TITLE BLOCK")
      ;         (/= (LM:effectivename (vlax-ename->vla-object REF_block_)) "GV3")
      ;       ) 
      ;         (progn 
      ;           (setq REF_block_ nil)
      ;           (alert "Please select a TITLE BLOCK NAME\nLNAD - A4 TITLE BLOCK PART REV01 \nLAND - A3 TITLE BLOCK \nAutoviewport GV3")
      ;         )
      ;         (princ "/n")
      ;       )
      ;     )
      ;   )
      ;   (cond 
      ;     ((and 
      ;         (= (LM:effectivename REF_block_obj) 
      ;           "LNAD - A4 TITLE BLOCK PART REV01"
      ;         )
      ;       )
      ;       (progn 
      ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "drawing_no1"))
      ;       )
      ;     )
      ;     ((and 
      ;         (= (LM:effectivename REF_block_obj) 
      ;           "LAND - A3 TITLE BLOCK"
      ;         )
      ;       )
      ;       (progn 
      ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "มาตราส่วน_VALUE"))
              
      ;       )
      ;     )
      ;     ((and 
      ;         (= (LM:effectivename REF_block_obj) 
      ;           "GV3"
      ;         )
      ;       )
      ;       (progn 
      ;         (setq REF_block_obj_vpname_ (LM:vl-getattributevalue REF_block_obj "NAME_VIEWPORT"))
      ;       )
      ;     )
      ;   )
      ; )
    ;
    ;user_input_mode_val_for_layout_header
      (initget 1 "1 2 3")
      (setq mode-header nil)
      (setq mode-header (cond ( (getkword (strcat "\nspecify_name_blk \nmode 1 = Mock_up \nmode 2 = Drawing_detail\nmode 3 = specify header_drawing_namefile\n<" (rtos (setq mode-header (cond (mode-header) (1.0) ) ) ) "> : " ) ) ) (mode-header) ) )
      (cond
        ((and
          (= mode-header "1")
        )
        (progn
          (setq header "M")
        )
        )
        ((and
          (= mode-header "2")
        )
        (progn
          (setq header  "D")
        )
        )
        ((and
          (= mode-header "3")
        )
        (progn
          (setq header  (getstring "specify header_drawing_namefile"))
        )
        )
      )
    ;
    ;
    ;user_input_mode_val_for_efectivename
      (setq mode-val nil)
      (while ;user_input_mode_val
        (not mode-val)
        (if ;return_base_val_to_user_input
          (/= mode-val-val nil)
          (progn
            (princ "/n")
          )
          (setq mode-val-val 2)
        )
        ;user_input_mode_val
          (setq mode-val (cond ( (getint (strcat "\nspecify_name_blk \nmode 1 = A4-tittle_blk \nmode 2 = A3-tittle_blk \nmode 3 = GV3<" (rtos (setq mode-val-val (cond (mode-val-val) (mode-val) ) ) ) "> : " ) ) ) (mode-val-val) ) )
        ;
        (if ;incorrect_case
          (and
            (/= mode-val 1)
            (/= mode-val 2)
            (/= mode-val 3)
          )
          (progn
            (setq mode-val-val mode-val )
            (setq mode-val nil)
            (alert "\n\n\n\Please input mode 1 2 or 3\n\n\n\n")
          )
          (princ mode-val)
        )
        (if ;correct_case
          (or
            (= mode-val 1)
            (= mode-val 2)
            (= mode-val 3)
          )
          (progn
            (setq mode-val-val mode-val )
          )
          (princ mode-val)
        )
      )
      (cond ;specify_efective_name_by_user_input
        ((and
          (= mode-val 1)
        )
        (progn
          (setq blk_efname "LNAD - A4 TITLE BLOCK PART REV01-NMBU2")
        )
        )
        ((and
          (= mode-val 2)
        )
        (progn
          (setq blk_efname  "LAND - A3 TITLE BLOCK")
        )
        )
        ((and
          (= mode-val 3)
        )
        (progn
          (setq blk_efname  "GV3")
        )
        )
      )
    ;
    ;ss_tiltle_blk or gv3
      (setq ss_blk_set_ (ssget 
                          (list 
                            (cons 0 "INSERT") ;type of object
                            ; (cons 8 "000 - GRID")   ;kind of layer
                            ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                            ; (cons 62 1)           ;kind of color call sign with color code index
                          )
                        )
      )
      (setq ss_blk_set_total (sslength ss_blk_set_))
    ;
    ;filter_selection_set_and_sorting_
      (setq ss_blk_set_i 0)
      (setq sorted_set_ ())
      (while ;filter_selection_set
        (< ss_blk_set_i ss_blk_set_total)
        (setq ss_blk_set_ename (ssname ss_blk_set_ ss_blk_set_i))
        (setq ss_blk_set_obj (vlax-ename->vla-object ss_blk_set_ename))
        (setq ss_blk_set_ins (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ss_blk_set_obj))))
        (setq ss_blk_set_efname (LM:effectivename ss_blk_set_obj)) 
        (setq ss_blk_set_ename+ins (list 
                                    ss_blk_set_ename
                                    (car ss_blk_set_ins)
                                    (cadr ss_blk_set_ins)
                                  )
        ) 
        (if
          (= ss_blk_set_efname blk_efname )
          (progn
            (setq sorted_set_ (sort_by_X (cons ss_blk_set_ename+ins sorted_set_)))
          )
          (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos (* ss_blk_set_i (/ 100 (float ss_blk_set_total))) 2 0) "%"))
        )
        (setq ss_blk_set_i (+ ss_blk_set_i 1))
        (setq sorted_set_total (length sorted_set_))
      )
      (princ (strcat "\nfilter_selection_set_and_sorting is processing " (rtos 100 2 0) "%") )
    ;
    ;condition_data_for_attibute
      (setq sorted_set_i 0)
      (setq inum 100)
      (setq ipage 0)
    
      (while
        (< sorted_set_i sorted_set_total)
        (setq Layout_num (strcat header (itoa (setq inum (1+ inum)))))
        (setq sorted_set_list (car (nth sorted_set_i sorted_set_)))
        (setq sorted_set_obj (vlax-ename->vla-object sorted_set_list))
        (cond 
            ((and 
                (= (LM:effectivename sorted_set_obj) 
                  "LNAD - A4 TITLE BLOCK PART REV01-NMBU2"
                )
              )
              (progn 
                (setq sorted_set_obj_vpname_1 (LM:vl-setattributevalue sorted_set_obj "drawing_no2_NM" Layout_num ))
                (setq sorted_set_obj_vpname_1 (LM:vl-setattributevalue sorted_set_obj "drawing_no2" Layout_num ))
                ; (setq sorted_set_obj_vpname_2 (LM:vl-setattributevalue sorted_set_obj "SHEET_NO." (strcat (itoa (setq ipage (1+ ipage))) " of " (itoa sorted_set_total)) ))
                (setq sorted_set_obj_vpname_3 (LM:vl-setattributevalue sorted_set_obj "TOTAL_SHEET_NO._NM" (strcat (itoa (+ 1 ipage)) "/" (itoa sorted_set_total)) ))
                
                (setq sorted_set_obj_vpname_3 (LM:vl-setattributevalue sorted_set_obj "SHEET_NO." (strcat (itoa (+ 1 ipage)) "/" (itoa sorted_set_total)) ))
                (setq sorted_set_obj_vpname_3 (LM:vl-setattributevalue sorted_set_obj "TOTAL_SHEET_NO." (itoa sorted_set_total) ))
                
                
              )
            )
            ((and 
                (= (LM:effectivename sorted_set_obj) 
                  "LAND - A3 TITLE BLOCK"
                )
              )
              (progn 
                (setq sorted_set_obj_vpname_ (LM:vl-setattributevalue sorted_set_obj "แผ่นที่_VALUE" Layout_num))
                
              )
            )
            ((and 
                (= (LM:effectivename sorted_set_obj) 
                  "GV3"
                )
              )
              (progn 
                (setq sorted_set_obj_vpname_ (LM:vl-setattributevalue sorted_set_obj "NAME_VIEWPORT" Layout_num))
              )
            )
          )
        (setq sorted_set_i (+ sorted_set_i 1))
        (setq ipage (+ ipage 1))
      )
    ;
  )
;


(defun c:z565normie_autoplot_block ()
  (if ;expire_func
    ; (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
    (< (setq now (rtos (getvar "cdate") 2 4)) (rtos (+ (atof now) 0.005) 2 4))
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
               (= mode-T-BLK "3 Use have to select_block")
             )
              (progn
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
              (setq mode-header (cond ( (getstring (strcat "\nspecify prefix name file <" (setq mode-header (cond (mode-header) (1.0) ) ) "> : " ) ) ) (mode-header) ) )
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
                (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
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
                (setq mode-header (getstring "ใส่เองไปก่อนคนทำ กำลังแก้โค้ดตอน 6 หกโมงเช้า เลยเวลานอนมา 1 ชั่วโมง"))
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
                    "TA3 - FOR NORMIE.ctb" ;Plot Style Name
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



 
