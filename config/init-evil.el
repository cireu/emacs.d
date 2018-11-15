;; -*- lexical-binding:t ; -*-

;; Better way to manage key bindings
(use-package general
  :init
  (general-evil-setup 'with-shortname-maps)

  ;; The global leader
  (general-create-definer l-spc :states '(n v)
    :prefix "SPC")

  ;; The leader for the major-mode functions
  (general-create-definer l-semi :states '(n v)
    :prefix ";")

  ;; The leader for just jump between the source
  (general-create-definer l-s :states '(n v)
    :prefix "s"))

;; The dark side
(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t)
  (setq evil-symbol-word-search t
        evil-ex-substitute-global t)
  :general
  ;; Make Evil not so evil
  (nvmap
    "C-a" 'evil-first-non-blank
    "C-e" 'evil-end-of-line
    "s" nil ; "s" now reserved for the search-leader `l-s'
    ;; "S" is generally equal to the "cc", so we make the "S" to act like the orginal"s"
    "S" 'evil-substitute)

  ;; "^" and "$" are hard to press, use emacs style key instead
  (mmap
    "C-a" 'evil-first-non-blank
    "C-e" 'evil-end-of-line)

  (imap
    "C-a" 'evil-first-non-blank
    "C-e" 'end-of-line ; `evil-end-of-line' won't go to the eol correctly
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
  (setq evil-snipe-scope 'line
        evil-snipe-repeat-scope 'whole-line)
  :general
  (nvmap
    "f" 'evil-snipe-f
    "F" 'evil-snipe-F
    "t" 'evil-snipe-t
    "T" 'evil-snipe-T))

;; (use-package evil-visualstar
;;   :hook (evil-after-load . global-evil-visualstar-mode))

;; (use-package evil-nerd-commenter
;;   :hook
;;   :general
;;   (l-spc
;;     "cl" 'evilnc-comment-or-uncomment-lines))
