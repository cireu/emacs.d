;;; -*- lexical-binding:t ; -*-

(defhydra hydra-macrostep ()
  "
Expand/Collapse^^    ^Navigation^
------------------------------------
_e_xpand             _j_ next macro
_c_ollapse           _k_ prev macro
_q_uit
"
  ("e" macrostep-expand )
  ("c" macrostep-collapse )
  ("q" macrostep-collapse-all :exit t)
  ("j" macrostep-next-macro )
  ("k" macrostep-prev-macro ))

(provide 'elisp-util)
;;; elisp-util.el ends here
