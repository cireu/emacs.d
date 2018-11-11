;; -*- lexical-binding: t; -*-

;; `elisp-mode' is loaded at startup. In order to lazyload its config we need
;; to prevent it isn't loaded
(delq 'elisp-mode features)
;; ...until the first time `emacs-lisp-mode' runs
(advice-add #'emacs-lisp-mode :before #'cm/init-elisp-mode)

(defun cm/init-elisp-mode (&rest _)
  "Initializing the Emacs-lisp mode"
  ;; Some plugins (like yasnippet) run `emacs-lisp-mode' early, to parse some
  ;; elisp. This would prematurely trigger this function. In these cases,
  ;; `emacs-lisp-mode-hook' is let-bound to nil or its hooks are delayed, so if
  ;; we see either, keep pretending elisp-mode isn't loaded.
  (when (and emacs-lisp-mode-hook (not delay-mode-hooks))
    ;; Otherwise, announce to the world elisp-mode has been loaded, so `after!'
    ;; handlers can respond and configure elisp-mode as expected.
    (provide 'elisp-mode)
    (advice-remove #'emacs-lisp-mode #'cm/init-elisp-mode)))

;; Real config starts here
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c '" . ielm)
              ("C-c e b" . eval-buffer)
              ("C-c e f" . eval-defun)))

(use-package eldoc
  :ensure nil)

  (use-package macrostep
    :bind (:map emacs-lisp-mode-map
              ("C-c m e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c m e" . macrostep-expand)))



(provide 'init-emacs-lisp)
