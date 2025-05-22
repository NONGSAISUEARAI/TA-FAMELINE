(defun c:T1-12 () 
  ;get doc a-cad
    (setq app (vlax-get-acad-object))
    (setq docs (vla-get-documents app))
    (setq docList_ '()) ;; สร้าง List ว่าง

    (vlax-for doc docs 
      (setq docList_ (cons doc docList_)) ;; เพิ่ม VLA-Object เข้าไปใน List
    )
    (length docList_)
  ;
  ;load lisp from location
    (load "C:\\TA\\TA_WORK\\AUTOCAD + VBA + LISP\\TA_COMPELETE\\QM-SAVEAS.lsp")
  
  ;
  ;preloop_and_while
    (setq doclist_i 0)
    ; (while (< doclist_i (length doclist_))
    (while (< doclist_i 3)
      (setq doclist_activesheet_ (nth  doclist_i doclist_))
      (vla-activate doclist_activesheet_)
      
      (c:qmsave)
      
      
      (setq doclist_i (+ doclist_i 1))
    )
  ;
)


; (repeat 20 
;   (load "C:\\TA\\TA_WORK\\AUTOCAD + VBA + LISP\\TA_COMPELETE\\QM-SAVEAS.lsp")
;   (c:qmsave) 
; ) 
; (repeat 20 (load "C:\\TA\\TA_WORK\\AUTOCAD + VBA + LISP\\TA_COMPELETE\\QM-SAVEAS.lsp")
;   (princ "1")
;   (princ "2")
   
; ) 

(defun c:T1-13 ()
  
  (setq i 0)
  (while
    (< i 10)
      (setq app (vlax-get-acad-object))
      (setq docs (vla-get-documents app))
      (setq docList_ '()) ;; สร้าง List ว่าง
      (vlax-for doc docs 
        (setq docList_ (cons doc docList_)) ;; เพิ่ม VLA-Object เข้าไปใน List
      )
      (setq doclist_activesheet_ (nth  i doclist_))
      (vla-activate doclist_activesheet_)
      (length docList_)
    
      (setq FILEPATH (getvar "DWGPREFIX"))
      (setq FILENAME (getvar "DWGNAME"))
      (setq sum (strcat FILEPATH FILENAME))
      (command "_SAVEAS" 
              "2010"
              sum
              "Y"
      )
    (setq i (+ i 1))
  )
)