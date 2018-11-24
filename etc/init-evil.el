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
    "C-e" 'evil-end-of-line

    ;; For leader
    "s" nil
    "m" nil

    ;; "S" is generally equal to the "cc", so we make the "S" to act like the orginal "s"
    "S" 'evil-substitute

    ;; don't be afraid, we have `evil-snipe'
    "," 'evil-set-marker
    ";" 'other-window)

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
    "C-k" 'kill-line)

  ;; Rebound Universal arg to "SPC u"
  (l-spc "u" 'universal-argument))

(use-package evil-snipe
  ;; Only I want is smart f/F/t/T, `avy' will do the rest of the work
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'whole-line
        evil-snipe-repeat-scope 'whole-line)
  :general
  (nvmap
    "f" 'evil-snipe-f
    "F" 'evil-snipe-F
    "t" 'evil-snipe-t
    "T" 'evil-snipe-T)
  :config (evil-snipe-override-mode))

(use-package evil-lion
  :general
  (nvmap
    "gl" 'evil-lion-left
    "gL" 'evil-lion-right)
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
  :init
  (cm/add-temp-hook 'evil-insert-state-entry-hook   (global-evil-surround-mode))
  (cm/add-temp-hook 'evil-operator-state-entry-hook (global-evil-surround-mode)))

;; (use-package evil-embrace
;;   :hook (org-mode . embrace-org-mode)
;;   :config
;;   (setq evil-embrace-show-help-p nil)
;;   (evil-embrace-enable-evil-surround-integration))

;; Undo-tree is a part of `evil'
(use-package undo-tree
  :general
  (l-spc
    "au" 'undo-tree-visualize)
  :config (global-undo-tree-mode))

(provide 'init-evil)
;;; init-evil.el ends here
