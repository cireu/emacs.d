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
  :)


(use-package company-box
  :hook (company-mode . company-box-mode))

;; Flycheck
(use-package flycheck
  :hook (python-mode
         typescript-mode
         js2-mode) . flycheck-mode)

(use-package avy-flycheck
  :hook (flycheck-mode . avy-flycheck-setup))

(provide 'init-progs)
