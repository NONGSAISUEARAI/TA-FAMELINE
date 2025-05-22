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
(defun c:BLRENAME()
;===========================================================================Turn off command line responses
(command "CMDECHO" 0)
;===========================================================================
(if (setq AllBlocks (ssget "_X" '((0 . "block")))) ; begin if
  (sslength allblocks)
(progn
(setq NumBlocks (sslength AllBlocks))
(setq Count 0)
	
  (repeat NumBlocks 					;;this cycles number of items
    (setq Ename (ssname AllBlocks Count))  		;;get entity name
    (setq Edata (entget Ename))
   
    (prompt "\n   Block found!! Please Wait......") ;amusement
    (setq Count (+ 1 Count))
	(princ "\n Count = [ ")
	(princ Count)
	(command "-rename" "block" AllBlocks Count )
	(princ " ] sorting next.....")
  )

 ) ;then
 (prompt "\n   No Block objects found!") ;else	
 )
(princ)
;===========================================================================Turn on command line responses
(setvar "CMDECHO" 1)
;===========================================================================
)


(defun c:listblocks (/ blockName bName tempList)
  (while
    
    (vlax-for blockName (vla-get-blocks
                  (vla-get-ActiveDocument (vlax-get-acad-object))
                )
      (if (= (vla-get-islayout blockName) :vlax-false)
        (setq tempList (cons (vla-get-name blockName) tempList))
      )
    )
    (reverse tempList)
  )
)

  (defun c:cac ()
    (setq block_list ())
    (setq multi_blocks (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
    (vlax-for block multi_blocks
    (setq block_list (append block_list (list (vla-get-name block))))
    )
    (setq new_sort_block_list (acad_strlsort block_list))
    
    (setq total_block_list (length block_list))
    (setq i_block 0)
    (setq block_list_x '())

    (while
      (< i_block total_block_list)
      (setq block_list_nth (nth i_block new_sort_block_list))
      
      (if (/= (substr block_list_nth 1 1) "*") ; ตรวจสอบว่าชื่อเริ่มต้นด้วย *
        (progn
          (setq block_list_x (cons block_list_nth block_list_x))
        )
      )
      (setq i_block (+ i_block 1))
    )
  
    
    (setq block_list_x_total (length block_list_x))
    (princ "\n         |=====================|")
    (princ "\n         | TOTAL BLOCK IN FILE |")
    (princ (strcat "\n         |     = " (itoa block_list_x_total) " block     |"))
    (princ "\n         |          set        |")
    (princ "\n         |---------------------|")
    
    (setq ii_block_list_x 0)
    ; in-block-loop
      (while
        (< ii_block_list_x block_list_x_total)
        (setq block_list_x_efname (nth ii_block_list_x block_list_x))
        (command "-bedit" block_list_x_efname)
        (setq c-grey08 8)
        (setq all_obj_in_block (ssget "x" 
                                          (list 
                                            ; (cons 0 "insert")       ;type of object
                                            ; (cons 8 "000 - GRID")   ;kind of layer
                                            ; (cons 2 "SSSS")       ;kind of nameblock
                                            ; (cons 62 1)           ;kind of color call sign with color code index
                                          )
                                  )
        )
        
        
        
        
        (command "chprop" all_obj_in_block "" "c" c-grey08 "")
        (command "_bclose" "s")
        (command "_attsync" "n" block_list_x_efname)
        (setq ii_block_list_x (+ ii_block_list_x 1))
      )
    ;
    ; all_obj_out-block-loop
      ; all_obj_mode
        (setq all_obj_out_block (ssget "x" 
                                      (list 
                                        ; (cons 0 "insert")       ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "SSSS")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                              )
        )
        (command "chprop" all_obj_out_block "" "c" c-grey08 "")
      ;
      ;dim_obj_mode
        ; (setq ss (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'objectname)) ;logic for testing
        ; (setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel))) 'objectname))  ;logic for testing
        (setq all_dim_out_block (ssget "x" 
                                        (list 
                                          (cons 0 "dimension")       ;type of object
                                          ; (cons 8 "000 - GRID")   ;kind of layer
                                          ; (cons 2 "SSSS")       ;kind of nameblock
                                          ; (cons 62 1)           ;kind of color call sign with color code index
                                        )
                                )
        )
        ; (command "pselect" all_dim_out_block "")
        (setq all_dim_out_block_total (sslength all_dim_out_block))
    
        (setq all_dim_out_block_iii 0)
        (while
          (< all_dim_out_block_iii all_dim_out_block_total)
          (setq all_dim_out_block_ename (ssname all_dim_out_block all_dim_out_block_iii))
          (setq all_dim_out_block_obj (vlax-ename->vla-object all_dim_out_block_ename))
          ; (vlax-dump-object all_dim_out_block_obj)
            (cond 
              ((and 
                  (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                      "AcDbAlignedDimension"
                  )
                )
                (progn 
                  (vla-put-textcolor all_dim_out_block_obj c-grey08)
                  (vla-put-dimensionlinecolor all_dim_out_block_obj c-grey08)
                  (vla-put-extensionlinecolor all_dim_out_block_obj c-grey08)
                )
              )
              ((and 
                  (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                      "AcDb2LineAngularDimension"
                  )
                )
                (progn 
                  (vla-put-textcolor all_dim_out_block_obj c-grey08)
                  (vla-put-dimensionlinecolor all_dim_out_block_obj c-grey08)
                  (vla-put-extensionlinecolor all_dim_out_block_obj c-grey08)
                )
              )
              ((and 
                  (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                      "AcDbRotatedDimension"
                  )
                )
                (progn 
                  (vla-put-textcolor all_dim_out_block_obj c-grey08)
                  (vla-put-dimensionlinecolor all_dim_out_block_obj c-grey08)
                  (vla-put-extensionlinecolor all_dim_out_block_obj c-grey08)
                )
              )
              ((and 
                  (= (vlax-get-property all_dim_out_block_obj 'objectname) 
                      "AcDbDiametricDimension"
                  )
                )
                (progn 
                  (vla-put-textcolor all_dim_out_block_obj c-grey08)
                  (vla-put-dimensionlinecolor all_dim_out_block_obj c-grey08)
                )
              )
            )
          ; (vla-put-textcolor all_dim_out_block_obj c-grey08)
          ; (vla-put-dimensionlinecolor all_dim_out_block_obj c-grey08)
          ; ; (vla-put-extensionlinecolor all_dim_out_block_obj c-grey08)
          (setq all_dim_out_block_iii (+ all_dim_out_block_iii 1))
        )
      ;
      ;attrib_obj_mode
        (setq all_leader_out_block (ssget "x" 
                                      (list 
                                        (cons 0 "leader")       ;type of object
                                        ; (cons 8 "000 - GRID")   ;kind of layer
                                        ; (cons 2 "SSSS")       ;kind of nameblock
                                        ; (cons 62 1)           ;kind of color call sign with color code index
                                      )
                              )
        )
        (setq all_leader_out_block_total (sslength all_leader_out_block))
        (setq all_leader_out_block_iii 0)
        (while
          (< all_leader_out_block_iii all_leader_out_block_total)
          (setq all_leader_out_block_ename (ssname all_leader_out_block all_leader_out_block_iii))
          (setq all_leader_out_block_obj (vlax-ename->vla-object all_leader_out_block_ename))
          (vla-put-dimensionlinecolor all_leader_out_block_obj c-grey08)
          (setq all_leader_out_block_iii (+ all_leader_out_block_iii 1))
        )
      ;
    ;
    
    

  )

; (setq ss (car (entsel))) ;logic for testing
; (setq ss_obj (vlax-ename->vla-object ss)) ;logic for testing
; (setq ss_efname (vlax-get-property ss_obj 'name)) ;logic for testing
; (setq ss (vlax-dump-object (vlax-ename->vla-object (car (entsel))) 'objectname))  ;logic for testing
; (vla-get-color ss_obj)
; (LM:VL-GETATTRIBUTEVALUES ss_obj)
; (vla-get-mtextattribute ss_obj)
; (command ".-attedit" "y" ss_efname "AA" )

