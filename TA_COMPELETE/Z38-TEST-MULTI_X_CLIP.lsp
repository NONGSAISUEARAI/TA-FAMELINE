(defun c:foo ()
  (setq rec_bo (entsel "select boundary"))
  (setq my_real_ss (ssget (list 
                        ;  (cons 8 "0") 
                        (cons 0 "INSERT") 
                        ;  (cons 2 "d2d")
                      ) 
                ) 
    )

  (setq blk_element (sslength my_real_ss))
  (setq i_ss 0)
  (while
    (< i_ss blk_element )
    (setq blk_main (ssname my_real_ss i_ss))
    (command "xclip" blk_main "" "n" "s" rec_bo)
    (setq i_ss (1+ i_ss) )
  )
)

(setq mySet (ssget "C" ))