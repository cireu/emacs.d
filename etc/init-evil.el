;;; -*- lexical-binding:t ; -*-

;; The dark side
(use-package evil
  :hook (after-init . evil-mode)
  :init
  ;; Avoid byte-compile warnings
  (defvar evil-want-C-u-scroll    t)
  (defvar evil-want-C-d-scroll    t)
  (defvar evil-want-Y-yank-to-eol t)
  (defvar evil-want-C-w-delete    t)
  (setq   evil-symbol-word-search t)
  :general
  ;; Make Evil not so evil
  (nvmap
    ;; "^" and "$" are hard to press, use emacs style key instead
    "C-a" 'evil-first-non-blank
    "C-e" 'evil-end-of-line)

  (mmap
    "C-a" 'evil-first-non-blank
    "C-e" 'evil-end-of-line)

  (imap
    "C-a" 'evil-first-non-blank
    "C-e" 'end-of-line        ; `evil-end-of-line' won't go to the eol correctly
    "C-f" 'forward-char
    "C-b" 'backward-char
    "C-n" 'next-line
    "C-p" 'previous-line
    "C-d" 'delete-char
    "C-y" 'yank
    "C-o" 'open-line
    "C-k" 'kill-line)

  ;; Rebound Universal arg to "SPC u"
  (l-spc
    "u" 'universal-argument))

;; All I need just clever F/f, `avy' will do the rest.
(use-package evil-snipe
  :init
  (setq evil-snipe-smart-case   t
        evil-snipe-scope        'whole-line
        evil-snipe-repeat-scope 'whole-line)
  :general
  (nvmap
    "f" 'evil-snipe-f
    "F" 'evil-snipe-F)
  :config (evil-snipe-override-mode +1))


(use-package evil-lion
  :general
  (l-spc
    "l" 'evil-lion-left
    "L" 'evil-lion-right)
  :config (evil-lion-mode))

(use-package evil-exchange
  :general
  (nvmap
    "gx" 'evil-exchange
    "gX" 'evil-exchange))

(use-package evil-nerd-commenter
  :general
  (nvmap
    "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

(use-package evil-embrace
  :init
  (cm/add-temp-hook 'evil-insert-state-entry-hook
    (evil-embrace-enable-evil-surround-integration))
  (cm/add-temp-hook 'evil-operator-state-entry-hook
    (evil-embrace-enable-evil-surround-integration)))

;; Undo-tree is a part of `evil'
(use-package undo-tree
  :general
  (l-spc
    "au" 'undo-tree-visualize)
  :config (global-undo-tree-mode +1))

(provide 'init-evil)
;;; init-evil.el ends here
