;; YASnippet
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind (:map global-map
              ("M-j" . yas-expand))
  :config (use-package yasnippet-snippets))

;; Paredit

(use-package paredit
  :preface
  ;; Paredit will add space when you type delimiters, which is annoying in non-lisp
  ;; language, this is a work-around to fix it
  (defun paredit/space-for-delimiter-p (endp delim)
    (or (member 'font-lock-keyword-face (text-properties-at (1- (point))))
        (not (derived-mode-p 'js2-mode
                             'typescript-mode
                             'python-mode))))
  :hook ((js2-mode
          typescript-mode
          python-mode
          lisp-mode
          lisp-interaction-mode
          emacs-lisp-mode
          ielm-mode
          slime-repl-mode-hook) . paredit-mode)
  :config (add-to-list 'paredit-space-for-delimiter-predicates #'paredit/space-for-delimiter-p))

;; Company
(use-package company
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defvar company-enable-yas
    "Enable yasnippet for all backends")
  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :bind (("M-/" . company-complete)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("TAB" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Flycheck
(use-package flycheck
  :hook (python-mode
         typescript-mode
         js2-mode) . flycheck-mode)

;; Show 

(use-package avy-flycheck
  :hook (flycheck-mode . avy-flycheck-setup))

(provide 'init-progs)
