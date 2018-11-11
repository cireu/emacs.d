;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default kill-whole-line t)           ; Kill line including '\n'

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   2
              tab-width        2
              indent-tabs-mode nil)


;;; Better jumping and moving
;; Move to the beginning/end of the line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Jump to wherever we can
(use-package avy
  :bind (("C-'" . avy-goto-char-inline)
         ("M-g g" . avy-goto-line)
         ("M-g j" . avy-goto-char-timer))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;; Jump between links
(use-package ace-link
  :bind (("M-g l" . ace-link-addr)))

;; Even jump to chinese characters
(use-package ace-piyin
  :hook (after-init . ace-pinyin-global-mode))

;;; Region Operation
;; Expand-region
(use-package expand-region
  :bind (("C-c v" . er/expand-region)))

(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Treat the region smartly
(use-package smart-region
  :hook (after-init . smart-region-on))

;;; Miscs
;; Use modern regexp for replacement
(use-package visual-regexp-streoid
  :bind (("C-c q" . vr/query-replace)))

;; Undo-tree
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

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

(provide 'init-edit)
