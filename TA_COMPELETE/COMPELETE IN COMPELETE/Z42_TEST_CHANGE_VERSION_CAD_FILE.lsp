(vl-load-com)
(princ)
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
;Dynamic Funcion Lee Mac
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
(defun LM:vl-getattributevalue (blk tag) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
     )
    (vlax-invoke blk 'getattributes)
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
(defun LM:StringSubst ( new old str / inc len )
      (setq len (strlen new)
            inc 0
      )
      (while (setq inc (vl-string-search old str inc))
          (setq str (vl-string-subst new old str inc)
                inc (+ inc len)
          )
      )
      str
    )
;EF_NAME_TO_ATT_TEXT_BLOCK_FUNC
  (defun TA:fillter_block_name_ (blk_ename_ )
    ;LEE-MAC_SUB_FUNC
      (defun LM:StringSubst ( new old str / inc len )
        (setq len (strlen new)
              inc 0
        )
        (while (setq inc (vl-string-search old str inc))
            (setq str (vl-string-subst new old str inc)
                  inc (+ inc len)
            )
        )
        str
      )
    ;
    ;Example_for_testing_
      ; (setq blk_ename_ (car (entsel "specify Object")))
    ;
    ;
    (setq blk_obj_ (vlax-ename->vla-object blk_ename_))

    (if (= (vla-get-objectname blk_obj_ ) "AcDbBlockReference")
      (progn
        (setq blk_raw_efname_ (LM:effectivename blk_obj_ ))
        ;user_setting_for_filter_ename_
          ; (setq blk_raw_efname_ "000 - DYNAMIC_VIEW_ASSCESSORIES_PHILLIPS_HEAD_SELF_DRILLING_SCREW_No.8_COMBINE_VIEW") ;FOR EXAMPLE CODING 
          (setq blk_raw_efname_ (LM:StringSubst "" "000 - " blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "DYNAMIC" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "VIEW" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "ASSCESSORIES" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "COMBINE" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "FRONT" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "TOP" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "SIDE" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "BOT" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "BACK" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "CUT" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "ARRAY+" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "ARRAY" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "" "SET" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "_" "__" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "_" "___" blk_raw_efname_))
          (setq blk_raw_efname_ (LM:StringSubst "_" "____" blk_raw_efname_))
          
        ;
        ;front_name_setting_
          ;preloop_and_while_for_delete "_"
            (setq i 0)
            (setq ii 1)
            (setq test_text_ (substr blk_raw_efname_ ii 1))
            (while (= test_text_ "_")
              (setq test_text_ (substr blk_raw_efname_ ii 1))
              (if (/= i 0)
                (progn
                  (setq ii (+ ii 1))
                  (setq i (+ i 1))
                )
                (setq i (+ i 1))
              )
            )
            (setq blk_raw_efname__strlen (strlen blk_raw_efname_))
            (if (= ii 1)
              (progn
                (setq final_text_ blk_raw_efname_)
              )
              (setq final_text_ (substr blk_raw_efname_ (setq new_i (- ii 1)) blk_raw_efname__strlen))
            )
            

        ;
        ;bot_name_setting_
          (setq l 0)
          (setq ll 1)
          (setq final_text_l (strlen final_text_))
          (setq test_text_ (substr final_text_ final_text_l 1)) 
          (while (= test_text_ "_")
            (setq test_text_ (substr final_text_ final_text_l 1))
            (setq final_text_l (- final_text_l 1))
          )
          (setq final_text_last (+ final_text_l 1))
          (setq final_text_final (substr final_text_ 1 final_text_last))
        ;
      )
    )
  )
  (defun TA:remove_att_value_ (att_obj_ except_att_list_ )
    ;Note By Code_Developer
    ;This command is designed to work exclusively with attribute blocks only.
    ;It will remove all attributes in the block, but it allows certain attributes to be excluded by adding their names to an exception list.
    ;att_obj_ = vla-ename > object
    ;except_att_list_ = (list "att1" "att2" etc.)
    ;example code (TA:remove_att_value_ att_obj_ (list))
    ;example_code (setq except_att_list_ (list "NO.CODE")) 
    ;The operation of the command will read the block's attribute values and generate a set of text. n 
    ;

    ;fillter_EF_name_object_
      ; (setq att_ename_ nil)
      ; (setq att_obj_ nil)
      ; (while (= att_ename_ nil)
        ;user_input_data_  
          ; (setq att_ename_ (car (entsel "specify Object")))
        ;
        ;fillter_object_type_
          (if 
            (and
              ; (if (/= att_ename_ nil) (progn (setq att_obj_ (vlax-ename->vla-object att_ename_)) ))
              (= (vla-get-objectname att_obj_) "AcDbBlockReference")
              (= (vla-get-hasattributes att_obj_) :vlax-true )
            )
            (progn
              (setq att_tag_list_ (LM:vl-getattributevalue-tag-TA-Modifies att_obj_))
            )
            ; (setq att_ename_ nil)
          )
          (if 
            (or
              ; (= att_ename_ nil)
              (/= (vla-get-objectname att_obj_) "AcDbBlockReference")
              (if (= (vla-get-objectname att_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes att_obj_) :vlax-true ) ))
            )
            (progn
              (alert "Please selection attribute object")
            )
          )
        ;
      ; )
    ;
    ;preloop_and_while_for_make_sequnce_attribute
      (setq att_tag_list_i 0)
      (while (< att_tag_list_i (length att_tag_list_))
        (setq att_tag_list_tagname_ (nth  att_tag_list_i att_tag_list_))
        (if (= (TA:FIND_DUPLICATE_LIST att_tag_list_tagname_ except_att_list_) "N" )
          (progn
            (LM:vl-setattributevalue att_obj_ att_tag_list_tagname_ "" )
          )
        )
        (setq att_tag_list_i (+ att_tag_list_i 1))
      )
    ;
    
  )
  (defun c:fillter_blockname_to_text_b2text ()
    ;NOTE-FROM_DEV
      ;this is code for filler_efectivename_block_name
    ;
    ;preloop_and_while
      (setq return_code_ nil)
      (while (and 
              (= return_code_ nil)
              
            )
        ;user_input_
          (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
          (setq fillter_block_name_ (TA:fillter_block_name_ eee_ename_))
          (setq dynamic_ename_ (car (entsel "specify Object for input fillter_block_name")))
          (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
        ;
        (if (and 
              (= (setq alert_val_1 (vla-get-objectname dynamic_obj_)) "AcDbBlockReference")
              (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "CONTENT NAME3 LEFT - BUBBLE v6")
              (/= eee_ename_ dynamic_ename_)
            )
          (progn
            (LM:vl-setattributevalue dynamic_obj_ "name1" fillter_block_name_)
            (setq return_code_"full")
          )
          (setq return_code_ nil)
        )
        (if (and 
              (= return_code_ nil)
              (/= alert_val_1 "AcDbBlockReference")
            )
          (progn
            (alert (strcat "this object is " alert_val_1 "\n" "block name = nil\n" "something is incorrect"  ))
            (setq return_code_ nil)
          )

        )

      )
    ;
  )
  (defun c:fillter_EFname_to_namefile_EF2namefile_ ()
    ;NOTE-FROM_DEV
      ;this is code for filler_efectivename_block_name
    ;
    ;preloop_and_while
      (setq return_code_ nil)
      (while (and 
              (= return_code_ nil)
              
            )
        ;user_input_
          (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
          (setq fillter_block_name_ 
                 (LM:StringSubst 
                   "_"
                   "__"
                   (LM:StringSubst 
                     "_"
                     "___"
                     (TA:fillter_block_name_ eee_ename_)
                   )
                 )
          )
          (setq dynamic_ename_ (car (entsel "specify Object for input fillter_block_name")))
          (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
        ;
        (if (and 
              (= (setq alert_val_1 (vla-get-objectname dynamic_obj_)) "AcDbBlockReference")
              (or 
                (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "LNAD - A4 TITLE BLOCK PART REV01" )
                (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "LNAD - A4 TITLE BLOCK PART REV01nm" )
              )
              (/= eee_ename_ dynamic_ename_)
            )
          (progn
            (LM:vl-setattributevalue dynamic_obj_ "NAMEFILE" fillter_block_name_)
            (setq return_code_"full")
          )
          (setq return_code_ nil)
        )
        (if (and 
              (= return_code_ nil)
              (/= alert_val_1 "AcDbBlockReference")
            )
          (progn
            (alert (strcat "this object is " alert_val_1 "\n" "block name = nil\n" "something is incorrect"  ))
            (setq return_code_ nil)
          )

        )

      )
    ;
  )
  (defun c:effectivename_to_att_EF2text ()
    ;NOTE-FROM_DEV
      ;this is code for filler_efectivename_block_name
    ;
    ;preloop_and_while
      (setq return_code_ nil)
      (while (and 
              (= return_code_ nil)
              
            )
        ;user_input_
          (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
          (setq fillter_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
          (setq dynamic_ename_ (car (entsel "specify Object for input fillter_block_name")))
          (setq dynamic_obj_ (vlax-ename->vla-object dynamic_ename_))
        ;
        (if (and 
              (= (setq alert_val_1 (vla-get-objectname dynamic_obj_)) "AcDbBlockReference")
              (= (setq alert_val_2 (LM:effectivename dynamic_obj_)) "000 - BOX_EF_NAME")
              (/= eee_ename_ dynamic_ename_)
            )
          (progn
            (LM:vl-setattributevalue dynamic_obj_ "nameblk" fillter_block_name_)
            (setq return_code_"full")
          )
          (setq return_code_ nil)
        )
        (if (and 
              (= return_code_ nil)
              (/= alert_val_1 "AcDbBlockReference")
            )
          (progn
            (alert (strcat "this object is " alert_val_1 "\n" "block name = nil\n" "something is incorrect"  ))
            (setq return_code_ nil)
          )

        )

      )
    ;
  )
  (defun c:rename_block_RENBLK ()
    ;NOTE-FROM_DEV
      ;this is code for filler_efectivename_block_name
    ;
    ;user_input
      (setq eee_ename_ (car (entsel "specify Object for fillter_block_name")))
      (setq old_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
      (setq new_ename_ (car (entsel "specify Object for input fillter_block_name")))
      (setq new_ename_obj_ (vlax-ename->vla-object new_ename_))
    ;
    ;checking_process
      (if 
        (and 
          (= (setq alert_val_1 (vla-get-objectname eee_ename_obj_)) "AcDbBlockReference")
          (= (setq alert_val_2 (LM:effectivename new_ename_obj_)) "000 - BOX_EF_NAME")
          (/= eee_ename_ dynamic_ename_)
        )
        (progn
          ;get_data_old_EF_NAME_
            (setq old_block_name_ (LM:effectivename (setq eee_ename_obj_ (vlax-ename->vla-object eee_ename_))))
          ;
          ;get_data
            (setq new_ef_name_ (LM:vl-getattributevalue new_ename_obj_ "nameblk"  ))
          ;
        )
        (alert "SOMETHING BAD")
      )
    ;
    ;Rename_process
      (command "rename" "B" old_block_name_ new_ef_name_)
    ;
        


  )
  (defun c:limit_content_name_[limittext_] ()
    ;Note By Code_Developer
    ;This command is designed to work exclusively with a block named 'CONTENT NAME3 LEFT - BUBBLE v6'.
    ;The operation of the command will read the block's attribute values and regenerate By this command can limit number of chhracters per line.
    ;Due to this code is designed to work exclusively with a block named "CONTENT NAME3 LEFT - BUBBLE v6"
    ;Fully command must have sub-functions with names starting with TA: or LM:
    ;
    ;
    ;
    ;fillter_content_name_object_
      (setq content_ename_ nil)
      (while (= content_ename_ nil)
        ;user_input_data
          (setq content_ename_ (car (entsel "specify Object")))
          (setq limit_text_length (cond ( (getint (strcat "\nSpecify limit_text_count   <" (rtos (setq limit_text_length (cond (limit_text_length) (1.0) ) ) ) "> : " ) ) ) (limit_text_length) ) )
        ;
        ;fillter_object_type_
          (if 
            (and
              (if (/= content_ename_ nil) (progn (setq content_obj_ (vlax-ename->vla-object content_ename_)) ))
              (= (vla-get-objectname content_obj_) "AcDbBlockReference")
              (= (vla-get-hasattributes content_obj_) :vlax-true )
              (= (LM:effectivename content_obj_) "CONTENT NAME3 LEFT - BUBBLE v6" )
            )
            (progn ;MAIN_IDEA
              ;getdata_limit_text_
                (setq content_tag_list_ (LM:removenth 9 (LM:vl-getattributevalues content_obj_)))
                (setq content_tag_list_length_ (strlen 
                                                (cdr (nth 0 content_tag_list_))
                                                (cdr (nth 1 content_tag_list_))
                                                (cdr (nth 2 content_tag_list_))
                                                (cdr (nth 3 content_tag_list_))
                                                (cdr (nth 4 content_tag_list_))
                                                (cdr (nth 5 content_tag_list_))
                                                (cdr (nth 6 content_tag_list_))
                                                (cdr (nth 7 content_tag_list_))
                                                (cdr (nth 8 content_tag_list_))
                                                
                                                "_"
                                              )
                )
                (setq content_tag_list_value_ (strcat 
                                                (cdr (nth 0 content_tag_list_))
                                                (cdr (nth 1 content_tag_list_))
                                                (cdr (nth 2 content_tag_list_))
                                                (cdr (nth 3 content_tag_list_))
                                                (cdr (nth 4 content_tag_list_))
                                                (cdr (nth 5 content_tag_list_))
                                                (cdr (nth 6 content_tag_list_))
                                                (cdr (nth 7 content_tag_list_))
                                                (cdr (nth 8 content_tag_list_))
                                                
                                                "_"
                                              )
                )
                
                (setq sum_text_ (list content_tag_list_length ))
                (setq sum_text_name (list content_tag_list_value_))
                (setq sum_text_name (LM:lst->str sum_text_name ""))
                (setq sum_text_name (LM:str->lst sum_text_name "_"))
                (setq sum_text_name (TA:REMOVE_VAL_LIST_ "" sum_text_name ))
                
                (setq sum_text_ (TA:REMOVE_VAL_LIST_ nil sum_text_))
              ;
              ;preparing_summary_data_
                (setq sum_text_i 0)
                (setq sum_text_len 0)
                ; (setq new_text_line_0 ())
                (setq new_text_line_1 ())
                (setq new_text_line_2 ())
                (setq new_text_line_3 ())
                (setq new_text_line_4 ())
                (setq new_text_line_5 ())
                (setq new_text_line_6 ())
                (setq new_text_line_7 ())
                (setq new_text_line_8 ())
                (setq new_text_line_9 ())
                (setq new_text_line_10 ())
              ;
              ;summary_limit_text_to_line
                (setq limit_text_length limit_text_length)
                (while (and (< sum_text_i (length sum_text_name)) (< sum_text_len 500))
                  (setq sum_text_ename_name (nth  sum_text_i sum_text_name))
                  (setq sum_text_len (+ (+ sum_text_len (strlen sum_text_ename_name) ) 1))
                  ;main_IDEA_limit_text_process
                    (cond
                      ( ;line_1
                        (<= sum_text_len limit_text_length)
                        (progn
                          (setq new_text_line_1 (cons sum_text_ename_name new_text_line_1 ))
                        )
                        (princ "line_1")
                      )
                      ( ;line_2
                        (and (> sum_text_len (* limit_text_length 1)) (< sum_text_len (* limit_text_length 2)) )
                        (progn
                          (setq new_text_line_2 (cons sum_text_ename_name new_text_line_2 ))
                        )
                        (princ "line_2")
                      )
                      ( ;line_3
                        (and (>= sum_text_len (* limit_text_length 2)) (< sum_text_len (* limit_text_length 3)) )
                        (progn
                          (setq new_text_line_3 (cons sum_text_ename_name new_text_line_3 ))
                        )
                        (princ "line_3")
                      )
                      ( ;line_4
                        (and (>= sum_text_len (* limit_text_length 3)) (< sum_text_len (* limit_text_length 4)) )
                        (progn
                          (setq new_text_line_4 (cons sum_text_ename_name new_text_line_4 ))
                        )
                        (princ "line_4")
                      )
                      ( ;line_5
                        (and (>= sum_text_len (* limit_text_length 4)) (< sum_text_len (* limit_text_length 5)) )
                        (progn
                          (setq new_text_line_5 (cons sum_text_ename_name new_text_line_5 ))
                        )
                        (princ "line_5")
                      )
                      ( ;line_6
                        (and (>= sum_text_len (* limit_text_length 6)) (< sum_text_len (* limit_text_length 7)) )
                        (progn
                          (setq new_text_line_6 (cons sum_text_ename_name new_text_line_6 ))
                        )
                        (princ "line_6")
                      )
                      ( ;line_7
                        (and (>= sum_text_len (* limit_text_length 7)) (< sum_text_len (* limit_text_length 8)) )
                        (progn
                          (setq new_text_line_7 (cons sum_text_ename_name new_text_line_7 ))
                        )
                        (princ "line_7")
                      )
                      ( ;line_8
                        (and (>= sum_text_len (* limit_text_length 8)) (< sum_text_len (* limit_text_length 9)) )
                        (progn
                          (setq new_text_line_8 (cons sum_text_ename_name new_text_line_8 ))
                        )
                        (princ "line_8")
                      )
                      ( ;line_9
                        (and (>= sum_text_len (* limit_text_length 9)) (< sum_text_len (* limit_text_length 10)) )
                        (progn
                          (setq new_text_line_9 (cons sum_text_ename_name new_text_line_9 ))
                        )
                        (princ "line_9")
                      )
                    )
                  ;
                  (setq sum_text_i (+ sum_text_i 1))
                )
                (setq limit_text_value_set_ 
                                            (TA:REMOVE_VAL_LIST_ 
                                              ""
                                              (list 
                                                (setq new_text_line_1_sum (LM:lst->str (reverse new_text_line_1) "_" ) )
                                                (setq new_text_line_2_sum (LM:lst->str (reverse new_text_line_2) "_" ) )
                                                (setq new_text_line_3_sum (LM:lst->str (reverse new_text_line_3) "_" ) )
                                                (setq new_text_line_4_sum (LM:lst->str (reverse new_text_line_4) "_" ) )
                                                (setq new_text_line_5_sum (LM:lst->str (reverse new_text_line_5) "_" ) )
                                                (setq new_text_line_6_sum (LM:lst->str (reverse new_text_line_6) "_" ) )
                                                (setq new_text_line_7_sum (LM:lst->str (reverse new_text_line_7) "_" ) )
                                                (setq new_text_line_8_sum (LM:lst->str (reverse new_text_line_8) "_" ) )
                                                (setq new_text_line_9_sum (LM:lst->str (reverse new_text_line_9) "_" ) )
                                              )
                                            )
                )
              ;
              ;Clear_att_process_
                (setq except_att_list_ (list "NO.CODE") )
                (TA:remove_att_value_ content_obj_ except_att_list_)
              ;
              ;preloop_and_while_limit_text_to_line
                  (setq limit_text_i 0)
                  (while (< limit_text_i (length limit_text_value_set_))
                    (setq content_tag_list_att (car (nth limit_text_i content_tag_list_))) ;att_tag
                    (setq limit_text_list_ (nth limit_text_i limit_text_value_set_)) ;att_value
                    
                    (if (/= limit_text_list_ nil)
                      (progn
                        (LM:vl-setattributevalue content_obj_ content_tag_list_att (strcat limit_text_list_ "_") )
                      )
                      (LM:vl-setattributevalue content_obj_ content_tag_list_att "" )
                    )
                
                    (setq limit_text_i (+ limit_text_i 1))
                  )
              ;
              ;indicate_visibility_BLK_atttribute
                (setq limit_text_vis_ (TA:REMOVE_VAL_LIST_ "" limit_text_value_set_))
                (LM:setdynpropvalue content_obj_ "content_line" (rtos(length limit_text_vis_)2 0))
              ;
            
            )
            (setq content_ename_ nil)
          )
            
          
          
          (if
            (or
              (= content_ename_ nil)
              (/= (vla-get-objectname content_obj_) "AcDbBlockReference")
              (if (= (vla-get-objectname content_obj_) "AcDbBlockReference") (progn (/= (vla-get-hasattributes content_obj_) :vlax-true ) ))
            )
            (progn
              (alert "Please selection attribute object")
            )
          )
        ;
      )
    ;
  )

;

; (defun c:CSF2_create_superfile_supershy () ;ยกเลิกเพื่อไปใช้ c:CSF2_create_superfile_supershy
;   (setq FILENAME (GETVAR 'DWGNAME))
;   (setq example_location "C:/TA/TA_WORK/2/nn2222922" ) ;this is "C:/TA/TA_WORK/2/nn2222922.dwg" (nn2222922 is file)
;   (setq ef_name "LNAD - A4 TITLE BLOCK PART REV01")
  
;   ; select block part
;     (setq REF_BLK (car (entsel "\nSelect a block ")))
;     (setq REF_GET (cdr (assoc 0 (entget REF_BLK))))
;     (setq REF_BLK_OBJ (vlax-ename->vla-object REF_BLK))
    
;         (if (and (= REF_GET "INSERT") 
;                  (= (setq name_blk (LM:effectivename REF_BLK_OBJ)) ef_name) 
;                  (not(=(setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK")) ""))
;             )
;             (progn
;               (princ "\nMAIN_FLODER = ")
;               (princ "\n            = ")
;               (princ superlink)
;             )
;             ; (setq REF_BLK nil)
;             (alert "Please select a again จ้า.")
;         )

;       (setq super_namefile (LM:vl-getattributevalue REF_BLK_OBJ "NAMEFILE"))
;       (setq super_shy super_namefile)
  
;   ; select block part

;     (COMMAND "_SAVEAS" "2010" super_shy (SETQ FILENAME (GETVAR 'DWGNAME)))
; )
(defun C:CSF1_CREATE_MAIN_AND_SUB_FLODER ()
  ;entsel_part
    (setq REF_BLK (car (entsel "\nSelect a block ")))
    
    (setq REF_BLK_OBJ (vlax-ename->vla-object REF_BLK))
    (setq ef_name "LNAD - A4 TITLE BLOCK PART REV01")

      (if 
        (and 
          (= (setq REF_GET (cdr (assoc 0 (entget REF_BLK)))) "INSERT")
          (= REF_GET "INSERT") 
          (= (setq name_blk (LM:effectivename REF_BLK_OBJ)) ef_name) 
          (not(=(setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK")) ""))
          
        )
        (progn
          (princ "PASS")
        )
        ; (setq REF_BLK nil)
        (alert "Please select a again จ้า.")
      )
    (setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK"))
    (setq super_location superlink)
  ;entsel_part

  ; MAIN_MKDIR_PART
    (defun LM:createdirectory ( dir )
        (   (lambda ( fun )
                (   (lambda ( lst ) (fun (car lst) (cdr lst)))
                    (vl-remove "" (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
                )
            )
            (lambda ( root lst / dir )
                (if lst
                    (if (or (vl-file-directory-p (setq dir (strcat root "\\" (car lst)))) (vl-mkdir dir))
                        (fun dir (cdr lst))
                    )
                )
            )
        )
        (vl-file-directory-p dir)
      ;C:\TA\TA_WORK\2
    )
    (defun LM:str->lst ( str del / pos )
        (if (setq pos (vl-string-search del str))
            (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
            (list str)
        )
    )
  ; MAIN_MKDIR_PART
  
  ; SUB_MKDIR_PART
    (setq sub_mkdir_in_01_DRAWING (strcat super_location "\\" "01_FILE_IN" "\\" "01_DRAWING"))
    (setq sub_mkdir_in_02_SKP (strcat super_location "\\" "01_FILE_IN" "\\" "02_SKP"))
    (setq sub_mkdir_in_03_BIM (strcat super_location "\\" "01_FILE_IN" "\\" "03_BIM"))
    (setq sub_mkdir_in_04_AREA (strcat super_location "\\" "01_FILE_IN" "\\" "4_AREA"))
    (setq sub_mkdir_in_05_EST_FOR_PRICE (strcat super_location "\\" "01_FILE_IN" "\\" "05_EST_FOR_PRICE"))
    (setq sub_mkdir_in_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_IN" "\\" "06_EST_FOR_MOCK_UP"))
    (setq sub_mkdir_in_07_E-MAIL (strcat super_location "\\" "01_FILE_IN" "\\" "07_E-MAIL"))

    (setq sub_mkdir_out_01_DRAWING (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
    (setq sub_mkdir_out_02_SKP (strcat super_location "\\" "01_FILE_OUT" "\\" "02_SKP"))
    (setq sub_mkdir_out_03_BIM (strcat super_location "\\" "01_FILE_OUT" "\\" "03_BIM"))
    (setq sub_mkdir_out_04_AREA (strcat super_location "\\" "01_FILE_OUT" "\\" "4_AREA"))
    (setq sub_mkdir_out_05_EST_FOR_PRICE (strcat super_location "\\" "01_FILE_OUT" "\\" "05_EST_FOR_PRICE"))
    (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
    (setq sub_mkdir_out_07_E-MAIL (strcat super_location "\\" "01_FILE_OUT" "\\" "07_E-MAIL"))
  ; SUB_MKDIR_PART

  ;CREATE_FLODER_PART
    (LM:createdirectory super_location) ;Main_Designer_Floder+Project_Floder
  
    (LM:createdirectory sub_mkdir_in_01_DRAWING)
    (LM:createdirectory sub_mkdir_in_02_SKP)
    (LM:createdirectory sub_mkdir_in_03_BIM)
    (LM:createdirectory sub_mkdir_in_04_AREA)
    (LM:createdirectory sub_mkdir_in_05_EST_FOR_PRICE)
    (LM:createdirectory sub_mkdir_in_06_EST_FOR_MOCK_UP)
    (LM:createdirectory sub_mkdir_in_07_E-MAIL)

    (LM:createdirectory super_location)
    (LM:createdirectory sub_mkdir_out_01_DRAWING)
    (LM:createdirectory sub_mkdir_out_02_SKP)
    (LM:createdirectory sub_mkdir_out_03_BIM)
    (LM:createdirectory sub_mkdir_out_04_AREA)
    (LM:createdirectory sub_mkdir_out_05_EST_FOR_PRICE)
    (LM:createdirectory sub_mkdir_out_06_EST_FOR_MOCK_UP)
    (LM:createdirectory sub_mkdir_out_07_E-MAIL)
  ;CREATE_FLODER_PART
  
  
)
(defun C:CSF2_CREATE_TYPICAL_DETAIL_FOLDER+FILENAME_&_SAVE_ ()
  ;Subfunc
    (defun LM:str->lst ( str del / pos )
        (if (setq pos (vl-string-search del str))
            (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
            (list str)
        )
    )
    (defun LM:createdirectory ( dir )
        (   (lambda ( fun )
                (   (lambda ( lst ) (fun (car lst) (cdr lst)))
                    (vl-remove "" (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
                )
            )
            (lambda ( root lst / dir )
                (if lst
                    (if (or (vl-file-directory-p (setq dir (strcat root "\\" (car lst)))) (vl-mkdir dir))
                        (fun dir (cdr lst))
                    )
                )
            )
        )
        (vl-file-directory-p dir)
      ;C:\TA\TA_WORK\2
    )
    (defun LM:StringSubst ( new old str / inc len )
      (setq len (strlen new)
            inc 0
      )
      (while (setq inc (vl-string-search old str inc))
          (setq str (vl-string-subst new old str inc)
                inc (+ inc len)
          )
      )
      str
    )
  ;
  ;entsel_part
    (setq REF_BLK (car (entsel "\nSelect a block ")))
    
    (setq REF_BLK_OBJ (vlax-ename->vla-object REF_BLK))
    (setq ef_name "LNAD - A4 TITLE BLOCK PART REV01")

      (if 
        (and 
          (= (setq REF_GET (cdr (assoc 0 (entget REF_BLK)))) "INSERT")
          (= REF_GET "INSERT") 
          (= (setq name_blk (LM:effectivename REF_BLK_OBJ)) ef_name) 
          (not(=(setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK")) ""))
        )
        (progn
          (princ "PASS")
          (setq superlink (LM:vl-setattributevalue REF_BLK_OBJ "SUPERLINK" "C:\\\\TA\\\\TA_WORK\\\\FAMELINE_TYPICAL_DETAIL_DATA_2025"))
        )
        ; (setq REF_BLK nil)
        (alert "Please select a again จ้า.")
      )
    (setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK"))
    (setq super_location superlink)
  ;entsel_part
  ;product floder name
    (setq product_1                     "1 = ALUMINIUM_SUN_LOUVERS") 
    (setq product_2                     "2 = ALUMINIUM_LITEWOOD") 
    (setq product_3                     "3 = ไม่มี") 
    (setq product_4                     "4 = ไม่มี")
    (setq product_5                     "5 = ไม่มี")
    (setq product_mode_value_ (cond 
                                   ((getint 
                                      (strcat product_1 
                                              "\n"
                                              product_2
                                              "\n"
                                              product_3
                                              "\n"
                                              product_4
                                              "\n"
                                              product_5
                                              "\n"
                                              "<"
                                              (rtos 
                                                (setq product_mode_value_ (cond 
                                                                               (product_mode_value_)
                                                                               (1.0)
                                                                             )
                                                )
                                              )
                                              "> : "
                                      )
                                    )
                                   )
                                   (product_mode_value_)
                                 )
    )
    (if (/= product_mode_value_ nil)
      (progn
        (cond
          (;product_mode_value__case_1
            (and
                (= product_mode_value_ 1)
            )
            (progn
              (setq product_name_ "ALUMINIUM_SUN_LOUVERS")
              (setq super_location (strcat 
                                     superlink 
                                     "\\\\" 
                                     product_name_
                                     "" 
                                   ) 
              )
              (setq project_name (LM:vl-setattributevalue REF_BLK_OBJ "PRELIM_NAME_PROJECT_1" product_name_))
              (setq project_name_2 (LM:vl-getattributevalue REF_BLK_OBJ "PRELIM_NAME_PROJECT_2" ))
            )
          )
        )
        ; (setq PRELIM_NAME_PROJECT_2 (LM:vl-getattributevalue REF_BLK_OBJ "PRELIM_NAME_PROJECT_2" ))
        (setq TYP_PATH_FLODER_ (LM:vl-setattributevalue REF_BLK_OBJ "SUPERLINK" super_location))
        (setq TYP_FILENAME (strcat TYP_PATH_FLODER_ "\\\\" (LM:vl-getattributevalue REF_BLK_OBJ "NAMEFILE"  )) )
        (setq super_namefile (LM:vl-getattributevalue REF_BLK_OBJ "NAMEFILE"))
        
        ;create_directory_process_
          (LM:createdirectory (setq main_path_ TYP_PATH_FLODER_))
            (LM:createdirectory (setq secondary_path_ (strcat TYP_PATH_FLODER_ "\\" project_name_2 )))
              (LM:createdirectory (setq sub_secondary_path_1_CAD_ (strcat TYP_PATH_FLODER_ "\\" project_name_2 "\\\\" "TYPICAL_DETAIL" "\\\\" "CAD" "\\\\")))
                (setq sub_secondary_path_1_CAD_FILE_ (strcat sub_secondary_path_1_CAD_ (LM:vl-getattributevalue REF_BLK_OBJ "NAMEFILE")) )
                (setq Directory_save_path_ (LM:StringSubst "/" "\\\\" sub_secondary_path_1_CAD_FILE_))
                ; (command "_SAVEAS" "2010" Directory_save_path_ (SETQ aaa (GETVAR 'DWGNAME)))
                (command "_SAVEAS" "2010" Directory_save_path_ )
              (setq sub_secondary_path_2_PDF (LM:createdirectory (strcat TYP_PATH_FLODER_ "\\" project_name_2 "\\" "TYPICAL_DETAIL" "\\" "PDF" "\\")))
        ;
      )
      
    )
    
    
  ;
  
)


(defun c:CSF3_create_superfile_supershy_REV02 ()
  (defun LM:StringSubst ( new old str / inc len )
    (setq len (strlen new)
          inc 0
    )
    (while (setq inc (vl-string-search old str inc))
        (setq str (vl-string-subst new old str inc)
              inc (+ inc len)
        )
    )
    str
  )
  (setq FILENAME (GETVAR 'DWGNAME))
  (setq example_location "C:/TA/TA_WORK/2/nn2222922" ) ;this is "C:/TA/TA_WORK/2/nn2222922.dwg" (nn2222922 is file)
  (setq ef_name "LNAD - A4 TITLE BLOCK PART REV01")
  
  ; select block part
    (setq REF_BLK (car (entsel "\nSelect a block ")))
    (setq REF_GET (cdr (assoc 0 (entget REF_BLK))))
    (setq REF_BLK_OBJ (vlax-ename->vla-object REF_BLK))
    
        (if (and (= REF_GET "INSERT") 
                 (= (setq name_blk (LM:effectivename REF_BLK_OBJ)) ef_name) 
                 (not(=(setq superlink (LM:vl-getattributevalue REF_BLK_OBJ "SUPERLINK")) ""))
            )
            (progn
              (princ "\nMAIN_FLODER = ")
              (princ "\n            = ")
              (princ superlink)
            )
            ; (setq REF_BLK nil)
            (alert "Please select a again จ้า.")
        )
    
    (setq modified-superlink (vl-string-translate "\\" "/" superlink ))
      (defun Replace (oldText newText text / i) 
        (if (/= oldText newText) 
          (while (setq i (vl-string-search oldText text)) 
            (setq text (vl-string-subst 
                         newText
                         oldText
                         text
                         i
                       )
            )
          )
        )
        text
      )
    (setq REDEFINED_superlink (Replace "\\\\" "/" superlink))
        (defun get-title-block-string (mode)
          
          (setq sub_mkdir_out_01_DRAWING (strcat REDEFINED_superlink "/" "01_FILE_OUT" "/" "01_DRAWING" "/"))
          (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat REDEFINED_superlink "/" "01_FILE_OUT" "/" "06_EST_FOR_MOCK_UP" "/"))
          (if (= mode 1) 
              ; (setq sub_mkdir_out_01_DRAWING (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
              sub_mkdir_out_01_DRAWING
            (if (= mode 2) 
              ; (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
              sub_mkdir_out_06_EST_FOR_MOCK_UP
            )
          )
        )
        (setq mode-v 1)
        (setq mode-v (cond ( (getint (strcat "\nSpecify object \nmode 1 = Drawing_detail \nmode 2 = Mockup \nmode 3 = Typical_detail\n<" (rtos (setq mode-v (cond (mode-v) (1.0) ) ) ) "> : " ) ) ) (mode-v) ) )
        (setq mode_location (get-title-block-string mode-v))
        (princ (strcat "Result: " mode_location))

     

  ; select block part

  ;get-name-file 
    ;get-time
      (setq YYYYMMDD (rtos (getvar 'cdate) 2 0))
      (setq YYYY (substr YYYYMMDD 1 4))
      (setq MM (substr YYYYMMDD 5 2))
      (setq DD (substr YYYYMMDD 7 2))
      (setq YY_MM_DD (strcat YYYY "_" MM "_" DD))
    ;
    ;get-type_of_work
      (defun get-name_file (mode)
        (setq out_01_DRAWING_type_of_work (strcat "_" "DRAWING" " " "DETAIL" "_"))
        (setq out_06_EST_FOR_MOCK_UP_type_of_work (strcat "_" "ESTMP" "_"))
        (if (= mode 1) 
            ; (setq sub_mkdir_out_01_DRAWING (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
            out_01_DRAWING_type_of_work
          (if (= mode 2) 
            ; (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
            out_06_EST_FOR_MOCK_UP_type_of_work
          )
        )
      )
      ; (setq mode-n (cond ( (getint (strcat "\nSpecify work-type <" (rtos (setq mode-n (cond (mode-n) (1.0) ) ) ) "> : " ) ) ) (mode-n) ) )
      (setq mode-name_file (get-name_file mode-v))
    ;
    ;get-project-code
      (defun get-name_project (mode)
        (setq out_01_DRAWING_project-name (substr (LM:vl-getattributevalue REF_BLK_OBJ "NAME_JOB_1") 24 6))
        (setq out_06_EST_FOR_MOCK_UP_project-name (strcat ""))
        (if (= mode 1) 
            ; (setq sub_mkdir_out_01_DRAWING (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
            out_01_DRAWING_project-name
          (if (= mode 2) 
            ; (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
            out_06_EST_FOR_MOCK_UP_project-name
          )
        )
      )
      ; (setq mode-n (cond ( (getint (strcat "\nSpecify work-type <" (rtos (setq mode-n (cond (mode-n) (1.0) ) ) ) "> : " ) ) ) (mode-n) ) )
      (setq mode-name_project (get-name_project mode-v))
      (setq project_code (substr (LM:vl-getattributevalue REF_BLK_OBJ "NAME_JOB_1") 24 6))
    ;
    ;get-name-project
      (setq project_name1 (LM:vl-getattributevalue REF_BLK_OBJ "NAME_PROJECT_1"))
      (setq project_name2 (LM:vl-getattributevalue REF_BLK_OBJ "NAME_PROJECT_2"))
      (setq project_name_sum (strcat "_" project_name1 "_" project_name2))
    ;get-mock-up_code
      (defun get-mock-up_code (mode)
        (setq out_01_DRAWING_mock-up_code (strcat ""))
        (setq out_06_EST_FOR_MOCK_UP_mock-up_code (LM:vl-getattributevalue REF_BLK_OBJ "NAME_JOB_2"))
        (if (= mode 1) 
            ; (setq sub_mkdir_out_01_DRAWING (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
            out_01_DRAWING_mock-up_code
          (if (= mode 2) 
            ; (setq sub_mkdir_out_06_EST_FOR_MOCK_UP (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
            out_06_EST_FOR_MOCK_UP_mock-up_code
          )
        )
      )
      (setq mode-mock-up_code (get-mock-up_code mode-v))
      (setq mock-up_code (strcat "-" (LM:vl-getattributevalue REF_BLK_OBJ "NAME_JOB_2")))
    ; 
  ;get-name-file
  ;summary code
    (LM:vl-setattributevalue REF_BLK_OBJ "NAMEFILE" (strcat YY_MM_DD mode-name_file project_code project_name_sum mock-up_code))
    (setq super_namefile (LM:vl-getattributevalue REF_BLK_OBJ "NAMEFILE"))
    (setq super_shy (strcat mode_location super_namefile ))
    (COMMAND "_SAVEAS" "2010" super_shy (SETQ FILENAME (GETVAR 'DWGNAME)))
    
    (princ (strcat "\nsave file at"))
   
              (princ "\n            - ")  
              (princ "\nMAIN_FLODER = ")
              (princ mode_location)
              (princ "\n            - ")    
              (princ "\n            - ")     
              (princ "\nFILE_NAME   = ")
              (princ super_namefile)
              (princ "\n            - ") 
  ;  
)



(defun c:Z420A_MAKE_PROJECT_FLODER_MKFL ()
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named 'LNAD - A4 TITLE BLOCK PART REV01' 
  ;Having the intention of deliberately using "lnad" instead of "land."
  ;No reason
  ;The principle of the code's functionality is designed to store the data name "superlink" in Att-data, which is used to collect folder paths.
  ;ATT-name "Superlink" is the base for creating subfolders 
  ;
  ;SUB-FUNC-LIBRARY
    (defun LM:createdirectory ( dir )
          (   (lambda ( fun )
                (   (lambda ( lst ) (fun (car lst) (cdr lst)))
                    (vl-remove "" (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
                )
            )
            (lambda ( root lst / dir )
                (if lst
                    (if (or (vl-file-directory-p (setq dir (strcat root "\\" (car lst)))) (vl-mkdir dir))
                        (fun dir (cdr lst))
                    )
                )
            )
        )
        (vl-file-directory-p dir)
      ;C:\TA\TA_WORK\2
    )
    (defun LM:str->lst ( str del / pos )
        (if (setq pos (vl-string-search del str))
            (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
            (list str)
        )
    )
  ;
  ;while_select_block_on_condition_ for selection title block 
    (setq tiltle_BK_ nil)
    (while (= tiltle_BK_ nil)
      (setq tiltle_BK_ (car (entsel "specify tiltle_BK Object")))
      (if
        (and ;conditional_rule_for_select_object
          (/= tiltle_BK_ nil)
          (setq tiltle_BK_obj_ (vlax-ename->vla-object tiltle_BK_))
          (= (vla-get-objectname tiltle_BK_obj_ ) "AcDbBlockReference")
          (= (vla-get-isdynamicblock tiltle_BK_obj_ ) :vlax-true)
          (= (LM:effectivename tiltle_BK_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01")
        )
        (progn
          (setq super_location (LM:vl-getattributevalue tiltle_BK_obj_ "SUPERLINK"))
        )
        (alert "Object invalid Please try again")
      )
    )
  ;
  ; SUB_MKDIR_PART
    (setq sub_mkdir_in_01_DRAWING           (strcat super_location "\\" "01_FILE_IN" "\\" "01_DRAWING"))
    (setq sub_mkdir_in_02_SKP               (strcat super_location "\\" "01_FILE_IN" "\\" "02_SKP"))
    (setq sub_mkdir_in_03_BIM               (strcat super_location "\\" "01_FILE_IN" "\\" "03_BIM"))
    (setq sub_mkdir_in_04_AREA              (strcat super_location "\\" "01_FILE_IN" "\\" "4_AREA"))
    (setq sub_mkdir_in_05_EST_FOR_PRICE     (strcat super_location "\\" "01_FILE_IN" "\\" "05_EST_FOR_PRICE"))
    (setq sub_mkdir_in_06_EST_FOR_MOCK_UP   (strcat super_location "\\" "01_FILE_IN" "\\" "06_EST_FOR_MOCK_UP"))
    (setq sub_mkdir_in_07_E-MAIL            (strcat super_location "\\" "01_FILE_IN" "\\" "07_E-MAIL"))

    (setq sub_mkdir_out_01_DRAWING          (strcat super_location "\\" "01_FILE_OUT" "\\" "01_DRAWING"))
    (setq sub_mkdir_out_02_SKP              (strcat super_location "\\" "01_FILE_OUT" "\\" "02_SKP"))
    (setq sub_mkdir_out_03_BIM              (strcat super_location "\\" "01_FILE_OUT" "\\" "03_BIM"))
    (setq sub_mkdir_out_04_AREA             (strcat super_location "\\" "01_FILE_OUT" "\\" "4_AREA"))
    (setq sub_mkdir_out_05_EST_FOR_PRICE    (strcat super_location "\\" "01_FILE_OUT" "\\" "05_EST_FOR_PRICE"))
    (setq sub_mkdir_out_06_EST_FOR_MOCK_UP  (strcat super_location "\\" "01_FILE_OUT" "\\" "06_EST_FOR_MOCK_UP"))
    (setq sub_mkdir_out_07_E-MAIL           (strcat super_location "\\" "01_FILE_OUT" "\\" "07_E-MAIL"))
  ; 
  ;CREATE_FLODER_PART
    (LM:createdirectory super_location) ;Main_Designer_Floder+Project_Floder
  
    (LM:createdirectory sub_mkdir_in_01_DRAWING)
    (LM:createdirectory sub_mkdir_in_02_SKP)
    (LM:createdirectory sub_mkdir_in_03_BIM)
    (LM:createdirectory sub_mkdir_in_04_AREA)
    (LM:createdirectory sub_mkdir_in_05_EST_FOR_PRICE)
    (LM:createdirectory sub_mkdir_in_06_EST_FOR_MOCK_UP)
    (LM:createdirectory sub_mkdir_in_07_E-MAIL)

    (LM:createdirectory super_location)
    (LM:createdirectory sub_mkdir_out_01_DRAWING)
    (LM:createdirectory sub_mkdir_out_02_SKP)
    (LM:createdirectory sub_mkdir_out_03_BIM)
    (LM:createdirectory sub_mkdir_out_04_AREA)
    (LM:createdirectory sub_mkdir_out_05_EST_FOR_PRICE)
    (LM:createdirectory sub_mkdir_out_06_EST_FOR_MOCK_UP)
    (LM:createdirectory sub_mkdir_out_07_E-MAIL)
  ;
  
)
(defun c:Z420B_SAVE_PROJECT_FLODER_SAVEPROJECT_ ()
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named 'LNAD - A4 TITLE BLOCK PART REV01' 
  ;Having the intention of deliberately using "lnad" instead of "land."
  ;No reason
  ;The principle of the code's functionality is designed to store the data name "superlink" in Att-data, which is used to collect folder paths.
  ;SUB-FUNC-LIBRARY
    (defun LM:createdirectory ( dir )
          (   (lambda ( fun )
                (   (lambda ( lst ) (fun (car lst) (cdr lst)))
                    (vl-remove "" (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
                )
            )
            (lambda ( root lst / dir )
                (if lst
                    (if (or (vl-file-directory-p (setq dir (strcat root "\\" (car lst)))) (vl-mkdir dir))
                        (fun dir (cdr lst))
                    )
                )
            )
        )
        (vl-file-directory-p dir)
      ;C:\TA\TA_WORK\2
    )
    (defun LM:str->lst ( str del / pos )
        (if (setq pos (vl-string-search del str))
            (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
            (list str)
        )
    )
    (defun TA:saveas_project_ (version_file_ filepath_ filename_ )
      ;Note By Code_Developer
      ;principle of code is designed to reduce mutil-process of SAVE AS command by User must input 3 argument to "TA:saveas_project_"
      ;argument
      ;version_file_  = Autocad version file = integer
      ;filepath_      = 1st substr = C:/
      ;               = lasted substr = /
      ;filename_      = name_file for saving project
      ;main_idea
        (if 
          (and
            (= (substr filepath_ 1 3 ) "C:/")
            (= (substr filepath_ (strlen filepath_ ) (strlen filepath_ ) ) "/")
            (numberp version_file_)
            (or 
              (= version_file_ 2007) 
              (= version_file_ 2010)
              (= version_file_ 2013)
              (= version_file_ 2015)
              (= version_file_ 2018)
            
            )
          )
          (progn
            (setq filename+filepath_ (strcat filepath_ filename_))
            (command "_SAVEAS" version_file_ filename+filepath_ )
          )
          (alert "some_value = error")
        )
      ;
    )
  ;
  ;while_select_block_on_condition_ for selection title block 
    (setq tiltle_BK_ nil)
    (while (= tiltle_BK_ nil)
      (setq tiltle_BK_ (car (entsel "specify tiltle_BK Object")))
      (if
        (and ;conditional_rule_for_select_object
          (/= tiltle_BK_ nil)
          (setq tiltle_BK_obj_ (vlax-ename->vla-object tiltle_BK_))
          (= (vla-get-objectname tiltle_BK_obj_ ) "AcDbBlockReference")
          (= (vla-get-isdynamicblock tiltle_BK_obj_ ) :vlax-true)
          (= (LM:effectivename tiltle_BK_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01")
        )
        (progn
          (setq super_location (LM:vl-getattributevalue tiltle_BK_obj_ "SUPERLINK"))
          (setq Designer_code_  (substr (nth 8 (LM:str->lst super_location "\\")) 1 3))
          (setq Designer_name_  (substr (nth 8 (LM:str->lst super_location "\\")) 5 (strlen (nth 8 (LM:str->lst super_location "\\"))) ))
          (setq Designer_project-code_  (nth (- (length (LM:str->lst  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_JOB_1") "_")) 1)   (LM:str->lst  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_JOB_1") "_") ))
          (setq Designer_project-drawing-no_  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_JOB_2"))
        )
        (alert "Object invalid Please try again")
      )
    )
  ;
  ;get-time
    (setq YYYYMMDD (rtos (getvar 'cdate) 2 0))
    (setq YYYY (substr YYYYMMDD 1 4))
    (setq MM (substr YYYYMMDD 5 2))
    (setq DD (substr YYYYMMDD 7 2))
    (setq YY_MM_DD (strcat YYYY "_" MM "_" DD))
  ;
  ;get-filename and save process
    (setq get-filename (cond ( (getint (strcat "\nSpecify object \nmode 1 = Drawing_detail \nmode 2 = Mockup \nmode 3 = Typical_detail\n<" (rtos (setq get-filename (cond (get-filename) (1.0) ) ) ) "> : " ) ) ) (get-filename) ) )
    (cond
      (;Filename_case_1
        (and 
          (= get-filename 1)
          
          (/= super_location "" )
          (/= Designer_code_ "" )
          (/= Designer_name_ "" )
          (/= Designer_project-code_ "" )
          (= (substr Designer_project-drawing-no_  1 7 ) "DRAWING")
        )
        (progn
          (setq filename_ (strcat
                            YY_MM_DD
                            "_"
                            "DRAWING_DETAIL"
                            "_"
                            Designer_project-code_
                            "_"
                            (vl-string-subst "" "___"
                              (vl-string-subst "" "__"
                                (vl-string-subst "" Designer_name_  
                                  (strcat
                                    (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1")
                                    "_"
                                    (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2")
                                    "_"
                                    (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3")
                                  )
                                )
                              )
                            )
                            "_"
                            Designer_project-drawing-no_
                          )
          )   
          (setq filepath_ (strcat  
                            (LM:StringSubst "/" "\\\\" super_location)
                            "/01_FILE_OUT/01_DRAWING/"
                            
                          )
          )
          (setq filename+filepath_ (strcat filepath_ filename_))
          (TA:saveas_project_ 2010 filepath_ filename_)
          
          (princ "Filename_case_1")
        )
      )
      (;Filename_case_2
         (and 
          (= get-filename 2)
          
          (/= super_location "" )
          (/= Designer_code_ "" )
          (/= Designer_name_ "" )
          (/= Designer_project-code_ "" )
          (= (substr Designer_project-drawing-no_  1 4 ) "SPEC")
        )
        (progn
          (setq filename_ (strcat
                            YY_MM_DD
                            "_"
                            "ESTMP"
                            "_"
                            Designer_project-code_
                            "_"
                            (vl-string-subst "" "__"
                              (vl-string-subst "" Designer_name_  
                                (strcat
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "NAME_PROJECT_1")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "NAME_PROJECT_2")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "NAME_PROJECT_3")
                                )
                              )
                            )
                            "_"
                            Designer_project-drawing-no_
                          )
          )
          (setq filepath_ (strcat  
                            (LM:StringSubst "/" "\\\\" super_location)
                            "/01_FILE_OUT/06_EST_FOR_MOCK_UP/"
                            
                          )
          )
          (setq filename+filepath_ (strcat filepath_ filename_))
          (TA:saveas_project_ 2010 filepath_ filename_)
          (princ "Filename_case_2")
        )
      )
      (;Filename_case_3
         (and
            ()
            ()
         )
         (progn
           ()
           (princ "Filename_case_3")
         )
      )
      (;Filename_case_4
         (and
            ()
            ()
         )
         (progn
           ()
           (princ "Filename_case_4")
         )
      )
    )
    
  ;
)
(defun c:Z420C_SAVE_TYP_FLODER_SAVETYP_ ()
  ;Note By Code_Developer
  ;This command is designed to work exclusively with a block named 'LNAD - A4 TITLE BLOCK PART REV01' 
  ;Having the intention of deliberately using "lnad" instead of "land."
  ;No reason
  ;The principle of the code's functionality is designed to store the data name "superlink" in Att-data, which is used to collect folder paths.
  ;SUB-FUNC-LIBRARY
    (defun LM:createdirectory ( dir )
          (   (lambda ( fun )
                (   (lambda ( lst ) (fun (car lst) (cdr lst)))
                    (vl-remove "" (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
                )
            )
            (lambda ( root lst / dir )
                (if lst
                    (if (or (vl-file-directory-p (setq dir (strcat root "\\" (car lst)))) (vl-mkdir dir))
                        (fun dir (cdr lst))
                    )
                )
            )
        )
        (vl-file-directory-p dir)
      ;C:\TA\TA_WORK\2
    )
    (defun LM:str->lst ( str del / pos )
        (if (setq pos (vl-string-search del str))
            (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
            (list str)
        )
    )
    (defun LM:StringSubst ( new old str / inc len )
      (setq len (strlen new)
            inc 0
      )
      (while (setq inc (vl-string-search old str inc))
          (setq str (vl-string-subst new old str inc)
                inc (+ inc len)
          )
      )
      str
    )
    (defun TA:saveas_project_ (version_file_ filepath_ filename_ )
      ;Note By Code_Developer
      ;principle of code is designed to reduce mutil-process of SAVE AS command by User must input 3 argument to "TA:saveas_project_"
      ;argument
      ;version_file_  = Autocad version file = integer
      ;filepath_      = 1st substr = C:/
      ;               = lasted substr = /
      ;filename_      = name_file for saving project
      ;main_idea
        (if 
          (and
            (= (substr filepath_ 1 3 ) "C:/")
            (= (substr filepath_ (strlen filepath_ ) (strlen filepath_ ) ) "/")
            (numberp version_file_)
            (or 
              (= version_file_ 2007) 
              (= version_file_ 2010)
              (= version_file_ 2013)
              (= version_file_ 2015)
              (= version_file_ 2018)
            
            )
          )
          (progn
            (setq filename+filepath_ (strcat filepath_ filename_))
            (command "_SAVEAS" version_file_ filename+filepath_ )
          )
          (alert "some_value = error")
        )
      ;
    )
  ;
  ;while_select_block_on_condition_ for selection title block 
    (setq tiltle_BK_ nil)
    (while (= tiltle_BK_ nil)
      (setq tiltle_BK_ (car (entsel "specify tiltle_BK Object")))
      (if
        (and ;conditional_rule_for_select_object
          (/= tiltle_BK_ nil)
          (setq tiltle_BK_obj_ (vlax-ename->vla-object tiltle_BK_))
          (= (vla-get-objectname tiltle_BK_obj_ ) "AcDbBlockReference")
          (= (vla-get-isdynamicblock tiltle_BK_obj_ ) :vlax-true)
          (= (LM:effectivename tiltle_BK_obj_ ) "LNAD - A4 TITLE BLOCK PART REV01")
        )
        (progn
          (setq typical_drawing_path_location_ (LM:vl-setattributevalue tiltle_BK_obj_ "SUPERLINK" "C:\\\\TA\\\\TA_WORK\\\\FAMELINE_TYPICAL_DETAIL_DATA_2025"))
          (setq super_location (LM:vl-getattributevalue tiltle_BK_obj_ "SUPERLINK"))
          
          (setq Fameline_typical_detail_ (LM:vl-setattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1" "FAMELINE_TYPICAL_DETAIL"))
          
          (setq current_vis_ (LM:getdynpropvalue tiltle_BK_obj_ "TYPE_BLOCK"))
          (setq new_vis_ (LM:StringSubst "NOM." "PART" current_vis_ ))
          (LM:setdynpropvalue tiltle_BK_obj_ "TYPE_BLOCK" new_vis_)
          
          (LM:vl-setattributevalue tiltle_BK_obj_ "PRELIM_NAME_JOB_1" "")
          (LM:vl-setattributevalue tiltle_BK_obj_ "PRELIM_NAME_JOB_2" "")
          
          
          
          
          
          
        )
        (alert "Object invalid Please try again")
      )
    )
  ;
  ;get-time
    (setq YYYYMMDD (rtos (getvar 'cdate) 2 0))
    (setq YYYY (substr YYYYMMDD 1 4))
    (setq MM (substr YYYYMMDD 5 2))
    (setq DD (substr YYYYMMDD 7 2))
    (setq YY_MM_DD (strcat YYYY "_" MM "_" DD))
  ;
  ;get-filename and product name
    (setq product_1                     "1 = ALUMINIUM_SUN_LOUVERS") 
    (setq product_2                     "2 = ALUMINIUM_LITEWOOD") 
    (setq product_3                     "3 = ไม่มี") 
    (setq product_4                     "4 = ไม่มี")
    (setq product_5                     "5 = ไม่มี")
    (setq product_mode_value_ (cond 
                                   ((getint 
                                      (strcat product_1 
                                              "\n"
                                              product_2
                                              "\n"
                                              product_3
                                              "\n"
                                              product_4
                                              "\n"
                                              product_5
                                              "\n"
                                              "<"
                                              (rtos 
                                                (setq product_mode_value_ (cond 
                                                                               (product_mode_value_)
                                                                               (1.0)
                                                                             )
                                                )
                                              )
                                              "> : "
                                      )
                                    )
                                   )
                                   (product_mode_value_)
                                 )
    )
    (cond
      (;Filename_case_1
        (and 
          (= product_mode_value_ 1)
          (/= super_location "" )        
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1") "")
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2") "")
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3") "")  
        )
        (progn
          (setq product_name_ "ALUMINIUM_SUN_LOUVERS") 
          (setq product_name_att_ (LM:vl-setattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2" product_name_))
          
          (setq filename_ (strcat
                            YY_MM_DD
                            "_"
                            (vl-string-subst "" "__"
                                (strcat
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3")
                                )
                            )
                          )
          )   
          (setq filename_ (LM:vl-setattributevalue tiltle_BK_obj_ "NAMEFILE" filename_))
          (setq filepath_ (strcat  
                            (LM:StringSubst "/" "\\\\" typical_drawing_path_location_)
                            "/"
                            product_name_
                            "/"
                            (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3")
                            "/"
                          )
          )
          (setq filepath_ (LM:vl-setattributevalue tiltle_BK_obj_ "SUPERLINK" filepath_))
          
          (LM:createdirectory filepath_)
          (setq filename+filepath_ (strcat filepath_ filename_))
          (TA:saveas_project_ 2010 filepath_ filename_)
          (princ "Filename_case_1")
        )
      )
      (;Filename_case_2
         (and 
          (= product_mode_value_ 2)
          (/= super_location "" )        
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1") "")
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2") "")
          (/= (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3") "")  
        )
        (progn
          (setq product_name_ "ALUMINIUM_LITEWOOD") 
          (setq product_name_att_ (LM:vl-setattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2" product_name_))
          
          (setq filename_ (strcat
                            YY_MM_DD
                            "_"
                            (vl-string-subst "" "__"
                                (strcat
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_1")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_2")
                                  "_"
                                  (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3")
                                )
                            )
                          )
          )   
          (setq filename_ (LM:vl-setattributevalue tiltle_BK_obj_ "NAMEFILE" filename_))
          (setq filepath_ (strcat  
                            (LM:StringSubst "/" "\\\\" typical_drawing_path_location_)
                            "/"
                            product_name_
                            "/"
                            (LM:vl-getattributevalue tiltle_BK_obj_ "PRELIM_NAME_PROJECT_3")
                            "/"
                          )
          )
          (setq filepath_ (LM:vl-setattributevalue tiltle_BK_obj_ "SUPERLINK" filepath_))
          
          (LM:createdirectory filepath_)
          (setq filename+filepath_ (strcat filepath_ filename_))
          (TA:saveas_project_ 2010 filepath_ filename_)
          (princ "Filename_case_2")
        )
      )
      (;Filename_case_3
         (and
            ()
            ()
         )
         (progn
           ()
           (princ "Filename_case_3")
         )
      )
      (;Filename_case_4
         (and
            ()
            ()
         )
         (progn
           ()
           (princ "Filename_case_4")
         )
      )
    )
    
  ;
)



;import_from_outside
  (defun c:z663A_excel_to_cad_add_floder_path_and_project_name ()
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; DESCRIPTION_FROM_DEVELOPER
    ; this command can export_text_data_from_excel to attribute name with same block-name "LNAD - A4 TITLE BLOCK PART REV01" only in multi-selection
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
      (setq name_job_1 (getstring "specify name_job_1"))
      (setq name_job_2 (getstring "specify name_job_2"))
      
      (setq NAME_PROJECT_1 (getstring "specify NAME_PROJECT_1"))
      (setq NAME_PROJECT_2 (getstring "specify NAME_PROJECT_2"))
      (setq NAME_PROJECT_3 (getstring "specify NAME_PROJECT_3"))
    ;
    ;sueper_process
      (setq SUPERLINK (getstring "specify SUPERLINK"))
        (setq SUPERLINK1 (LM:str->lst SUPERLINK "\\")  )
        (setq SUPERLINK2 (LM:lst->str SUPERLINK1 "\\\\")  )
    ;

    ; (setq NAMEFILE (getstring "specify NAMEFILE")) ให้ตัวแปรนี้ใช้คู่กับ superlink
    
      
    
    
    
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (princ "next")
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;user_input get_effectivename 
      (setq get_effectivename_ nil)
      (setq get_effectivename_obj_val_ nil)
      ;preloop_and_while_
          (while (not get_effectivename_)
            (princ (setq get_effectivename_ (car (entsel "\nPlease Specify Attribute Block\n"))))
            (if (and ;condition rule
                  (/= get_effectivename_ nil)
                  (= (vla-get-hasattributes (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                  (= (LM:Effectivename (vlax-ename->vla-object get_effectivename_)) "LNAD - A4 TITLE BLOCK PART REV01")
                  ; (/= (vla-get-isdynamicblock (vlax-ename->vla-object get_effectivename_)) :vlax-true)
                )
              (progn ;add data
                ; (alert "\nplease select block object")
                ; (setq get_effectivename_ nil)
                ; (setq get_effectivename_obj_val_ nil)
                (setq get_effectivename_obj_val_ (LM:Effectivename (vlax-ename->vla-object get_effectivename_)))
                (setq REF_get_att_list_ (LM:vl-getattributevalues (vlax-ename->vla-object get_effectivename_)))
                ;add more
              )
              (setq get_effectivename_ nil)
            )
          )
      ;
    ;
    ;selection_set
      (if  ;pre_select_ssget_or_post_select_ssget
        (= 
          (setq ss_pre_filter_set_xx (ssget "I" 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
          nil
        )
        (progn 
          (setq ss_pre_filter_set_xx (ssget 
                                            (list 
                                              (cons 0 "INSERT") ;type of object
                                              ; (cons 8 "000 - GRID")   ;kind of layer
                                              ; (cons 2 "000-GRID_LINE_DYN")       ;kind of nameblock
                                              ; (cons 62 1)           ;kind of color call sign with color code index
                                            )
                                      )
          )
        )
        (sslength ss_pre_filter_set_xx)
      )
    ;
    ;filter_and_sorting_selection
      (setq _set_ (TA:EF_Filter_ss_set_ ss_pre_filter_set_xx get_effectivename_obj_val_))
      (setq TAR_filter_set_ (TA:standard_list_croodinate_sorting _set_ "X"))
    ;
    ;preloop_and_while
      (setq TAR_filter_set_i 0)
      (while (< TAR_filter_set_i (length TAR_filter_set_))
        (setq TAR_filter_set_ename_ (car (nth  TAR_filter_set_i TAR_filter_set_)))
        (setq TAR_filter_set_obj_ (vlax-ename->vla-object TAR_filter_set_ename_))

        ;Edit_ATT_PROCESS
          (setq TAR_filter_set_obj_att_list_ (LM:vl-getattributevalues TAR_filter_set_obj_ ))
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "name_job_1" name_job_1)
            (LM:vl-setattributevalue TAR_filter_set_obj_  "name_job_2" name_job_2)
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_JOB_1" name_job_1)
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_JOB_2" name_job_2)
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_1" NAME_PROJECT_1 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_2" NAME_PROJECT_2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAME_PROJECT_3" NAME_PROJECT_3 )
        
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_1" NAME_PROJECT_1 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_2" NAME_PROJECT_2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "PRELIM_NAME_PROJECT_3" NAME_PROJECT_3 )
            
            (LM:vl-setattributevalue TAR_filter_set_obj_  "SUPERLINK" SUPERLINK2 )
            (LM:vl-setattributevalue TAR_filter_set_obj_  "NAMEFILE" SUPERLINK2 ) ;ให้ตัวแปรนี้ใช้คู่กับ superlink
        ;
        
        (setq TAR_filter_set_i (+ TAR_filter_set_i 1))
      )
    ;
    
  )
;

