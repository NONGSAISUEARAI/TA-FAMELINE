(defun stte (h)
  (setq ttitle_blk (entlast))
  (command "scale" "L" "" "0,0" h)
)

(stte 5)
