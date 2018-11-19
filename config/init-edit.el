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

;;; jump and navigation
;; Jump to wherever we can
(use-package avy
  :general
  (l-s
    "s" 'avy-goto-char-2
    "l" 'avy-goto-line)
  :config
  (setq avy-background t)
  (avy-setup-default))

;; Jump between links
(use-package ace-link
  :general
  ("M-g l" 'ace-link-addr))

;;; Region Operation
;; Expand-region
(use-package expand-region
  :general
  (vmap
    "v" 'er/expand-region
    "V" 'er/contract-region))

;;; Miscs
;; Use modern regexp for replacement
(use-package visual-regexp-streoid
  :general
  (l-s
    "r" 'vr/query-replace))


(use-package hide-show
  :ensure nil
  :general
  (:keymaps 'hs-minor-mode-map
            ("C-`" 'hs-toggle-hiding)))

(use-package aggressive-indent
  :hook ((prog-mode . global-aggressive-indent-mode)
         ;; Disable in big file due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size (* 3000 80)))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some mode
  (dolist (mode '(html-mode web-mode css-mode))
    (push mode aggressive-indent-excluded-modes)))

(use-package anzu
  :general
  ([remap query-replace] 'anzu-query-replace
   [remap query-replace-regexp] 'anzu-query-replace-regexp
   [remap isearch-query-replace] 'anzu-isearch-query-replace
   [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  (l-s
    "rr" 'anzu-query-replace
    "rp" 'anzu-query-replace-at-cursor-things
    "re" 'anzu-query-replace-regexp))

(provide 'init-edit)
