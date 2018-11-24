;;; -*- lexical-binding:t ; -*-

(defun +snippets/expand-on-region ()
  "Only use this with `evil-mode'. Expands a snippet around a selected region
and switches to insert mode if there are editable fields."
  (interactive)
  (when (evil-visual-state-p)
    (evil-visual-select evil-visual-beginning evil-visual-end 'inclusive))
  (cl-letf (((symbol-function 'region-beginning) (lambda () evil-visual-beginning))
            ((symbol-function 'region-end)       (lambda () evil-visual-end)))
    (yas-insert-snippet))
  (when (yas-active-snippets)
    (evil-insert-state +1)))

(provide 'yas-util)
;;; yas-util.el ends here