;example_defun
  ;; --------------------------------------------------------------------------- 
  ;; Function: tabsort 
  ;; Purpose : sort Tabs by the prefix then the first numbers found 
  ;; AUTHOR  Charles Alan Butler @ TheSwamp.org
  ;; --------------------------------------------------------------------------- 

  ;; Last Update 03/01/2006  CAB
  (defun C:TabSort (/ cnt doc lay)
  (vl-load-com)

  ;; --------------------------------------------------------------------------- 
  ;; Function: Num_sort 
  ;; Purpose : sort list of strings by the prefix then the first numbers found 
  ;; AUTHOR  Charles Alan Butler @ TheSwamp.org
  ;; Params  : tablst:    list of strings to sort
  ;; Returns : sorted list
  ;; ---------------------------------------------------------------------------
  (defun Num_Sort (tablst / tab ptr len loop tmp tmp2 sub lst)

    (defun vl-sort-it (lst func)
      (mapcar '(lambda (x) (nth x lst)) (vl-sort-i lst func))
    )

    (defun sort2 (tmp2 sub)
      (setq tmp2 (append
                    (vl-sort-it sub '(lambda (e1 e2) (< (cadr e1) (cadr e2))))
                    tmp2
                  )
      )
    )

    ;;  convert to a list (string) -> (prefix num string)
    (foreach tab tablst
      (setq ptr  1
            len  (strlen tab)
            loop t
      )
      (while loop
        (cond
          ((wcmatch "0123456789" (strcat "*" (substr tab ptr 1) "*"))
            (setq tmp  (cons (list (substr tab 1 (1- ptr))
                                  (atof (substr tab ptr))
                                  tab
                            )
                            tmp
                      )
                  loop nil
            )
          )
          ((> (setq ptr (1+ ptr)) len)
            ;;  no number in string
            (setq tmp  (cons (list tab nil tab) tmp)
                  loop nil
            )
          )
        )                     ; end cond stmt
      )
    )

    ;;  sort on the prefix
    (setq tmp (vl-sort-it tmp '(lambda (e1 e2) (< (car e1) (car e2)))))

    ;; Do a number sort on each group of matching prefex
    (setq idx (length tmp))
    (while (> (setq idx (1- idx)) -1)
      (cond
        ((not sub)
          (setq sub (List (nth idx tmp))
                str (car (nth idx tmp))
          )
        )
        ((= (car (nth idx tmp)) str) ; still in the group
          (setq sub (cons (nth idx tmp) sub))
        )
      )                       ; end cond stmt

      (if (= idx 0)           ; end of list
        (progn
          (setq tmp2 (sort2 tmp2 sub))
          (if (/= (car (nth idx tmp)) str)
            (setq tmp2 (append (list (nth idx tmp)) tmp2))
          )
          (setq str (car (nth idx tmp)))
        )
      )

      (if (/= (car (nth idx tmp)) str)
        ;; next group, so sort previous group
        (setq tmp2 (sort2 tmp2 sub)
              sub  (list (nth idx tmp))
              str  (car (nth idx tmp))
        )
      )
    )                         ; end while
    (setq lst (mapcar 'caddr tmp2))
    (princ)
    lst
  )                           ; end defun
  ;;==========================================================================

  (setq cnt 1
        doc (vla-get-activedocument (vlax-get-acad-object))
  )
  (foreach lay (num_sort (vl-remove "Model" (layoutlist)))
    (vla-put-taborder (vla-item (vla-get-layouts doc) lay) cnt)
    (setq cnt (1+ cnt))
  )
  (princ)
  )                           ; end defun
  (prompt "\nTabSort loaded, enter TabSort to run.")
  (princ)
  ;;==========================================================================
  ;;==========================================================================
  ;;==========================================================================
  ;;==========================================================================
;