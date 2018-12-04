;;; -*- lexical-binding:t ; -*-

(defun cm/yas-expand-on-region-or-insert ()
  "Only use this with `evil-mode'. Expands a snippet around a selected region
and switches to insert mode if there are editable fields. If don't select a
region, insert a snippet directly"
  (interactive)
  (when (evil-visual-state-p)
    (evil-visual-select evil-visual-beginning evil-visual-end 'inclusive))
  (cl-letf (((symbol-function 'region-beginning) (lambda () evil-visual-beginning))
            ((symbol-function 'region-end)       (lambda () evil-visual-end)))
    (yas-insert-snippet))
  (when (yas-active-snippets)
    (evil-insert-state +1)))

(defun cm/evil-aya-open-line ()
  "Call `evil-open-below', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field.  Call `evil-open-below' if nothing else applies."
  (interactive)
  (end-of-line)
  (cond ((expand-abbrev))
        ((progn
           (unless yas-global-mode
             (yas-global-mode 1))
           (yas--snippets-at-point))
         (yas-next-field-or-maybe-expand))
        ((ignore-errors
           (setq aya-invokation-point (point))
           (setq aya-invokation-buffer (current-buffer))
           (setq aya-tab-position (- (point) (line-beginning-position)))
           (let ((yas-fallback-behavior 'return-nil))
             (yas-expand))))
        ((and (fboundp 'tiny-expand)
              (funcall 'tiny-expand)))
        (t
         (evil-open-below 1))))

(provide 'snippets-utils)
;;; snippets-utils.el ends here
