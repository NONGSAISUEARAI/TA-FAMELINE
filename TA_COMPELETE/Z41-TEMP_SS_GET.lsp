(setq my-selection-set (ssget 
              (list 
                (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE,INSERT")       ;type of object
                ; (cons 8 "000 - GRID")   ;kind of layer 
                ; (cons 2 "SSSS")       ;kind of nameblock
                (cons 62 color_index)
              )
            )
)
(setq my-selection-set (ssget 
              (list 
                (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE,INSERT")       ;type of object
                ; (cons 8 "000 - GRID")   ;kind of layer 
                ; (cons 2 "SSSS")       ;kind of nameblock
                (cons 62 color_index)
              )
            )
)

(setq abx (entget (car (entsel "\n OK GO"))))


(setq my-selection-set (ssget 
                (list 
                  (cons 0 "line")       ;type of object
                  ; (cons 8 "Xref-elevation$0$DB - Wall")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
)

(command "pselect" my-selection-set "")

(setq my-selection-set (ssget 
                (list 
                  (cons 0 "hatch")       ;type of object
                  ; (cons 8 "Xref-elevation$0$DB - Wall")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
)

(command "pselect" my-selection-set "")


;;---------------------=={ Total Area }==---------------------;;
;;                                                            ;;
;;  Displays the total area of selected objects at the        ;;
;;  command line. The precision of the printed result is      ;;
;;  dependent on the setting of the LUPREC system variable.   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tarea ( / a i s )
    (if (setq s
            (ssget
               '(   (0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 80)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (setq a 0.0)
            (repeat (setq i (sslength s))
                (setq a (+ a (vlax-curve-getarea (ssname s (setq i (1- i))))))
            )
            (princ "\nTotal Area: ")
            (princ (rtos a 2))
        )
    )
    (princ)
)
(vl-load-com) (princ)



(setq my-selection-set (ssget 
                (list 
                  (cons 0 "insert")       ;type of object
                  (cons 8 "A02_LITEWOOD")   ;kind of layer 
                  ; (cons 2 "SSSS")       ;kind of nameblock
                  ; (cons 62 1)           ;kind of color call sign with color code index
                )
              )
)

(setq total_ss (sslength my-selection-set ))
(setq iss 0)
(while
  (< iss total_ss )
  (setq ss_ename (ssname my-selection-set iss))
  (setq ss_obj (vlax-ename->vla-object ss_ename))
  (setq real_angle (* (/ (vla-get-rotation ss_obj) 3.14) 180))
  (setq real_of_real_angle (+ real_angle 90))
  (setq return_angle (* (/ 180 real_of_real_angle) 3.14))
  (vla-put-rotation ss_obj (+ (vla-get-rotation ss_obj) return_angle))
  (setq iss (+ iss 1))

)
