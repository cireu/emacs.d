(use-package expand-region
  :bind (("C-c v" . er/expand-region)))

;; Move to the beginning/end of the line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package hide-show
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

(use-package aggressive-indent
  :hook ((after-init . global-aggressive-indent-mode)
         ;; Disable in big file due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size (* 3000 80)))
                            (aggressive-indent-mode ace-pinyin--original-avy-word-1)))))
  :config
  ;; Disable in some mode
  (dolist (mode '(html-mode web-mode css-mode))
    (push mode aggressive-indent-excluded-modes)))

;; Use modern regexp for replacement
(use-package visual-regexp-streoid
  :bind (("C-c q" . vr/query-replace)))

;; Undo-tree
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

(provide 'init-edit)
