;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (vlax-safearray->list llp) ls1)
                  ls2 (cons (vlax-safearray->list urp) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)
;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (vlax-safearray->list llp) ls1)
                  ls2 (cons (vlax-safearray->list urp) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)
;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (vlax-safearray->list llp) ls1)
                  ls2 (cons (vlax-safearray->list urp) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)

(defun c:ssboundingbox_SSBOX ( / box obj sel spc )
    (if (and (setq sel (ssget))
             (setq box (LM:ssboundingbox sel))
        )
        (progn
            (setq spc
                (vlax-get-property (vla-get-activedocument (vlax-get-acad-object))
                    (if (= 1 (getvar 'cvport))
                        'paperspace
                        'modelspace
                    )
                )
            )
            (if (equal 0.0 (apply '- (mapcar 'caddr box)) 1e-6)
                (progn
                    (setq obj
                        (vlax-invoke spc 'addlightweightpolyline
                            (apply 'append
                                (mapcar '(lambda ( x ) (mapcar '(lambda ( y ) ((eval y) box)) x))
                                   '(
                                        (caar   cadar)
                                        (caadr  cadar)
                                        (caadr cadadr)
                                        (caar  cadadr)
                                    )
                                )
                            )
                        )
                    )
                    (vla-put-closed obj :vlax-true)
                    (vla-put-elevation obj (caddar box))
                )
                (apply 'vlax-invoke 
                    (vl-list* spc 'addbox
                        (apply 'mapcar (cons '(lambda ( a b ) (/ (+ a b) 2.0)) box))
                        (apply 'mapcar (cons '- (reverse box)))
                    )
                )
            )
        )
    )
    (princ)
)
(vl-load-com) (princ)