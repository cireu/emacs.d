;; -*- lexical-binding:t ; -*-

;; Better way to manage key bindings
(use-package general
  :init
  (general-evil-setup)

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
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-Y-yank-to-eol t)
  :hook (after-init . evil-mode)
  :config
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
    "C-d" 'delete-char
    "C-y" 'yank
    "C-w" 'kill-region
    "C-k" 'kill-line
    )
  )

(progn
  (use-package-ensure-elpa 'evil-snipe
                           '(t)
                           'nil)
  (unless
      (fboundp 'evil-end-of-line)
    (autoload
      (function evil-end-of-line)
      "evil-snipe" nil t))
  (general-def "C-c f" 'evil-end-of-line :package 'evil-snipe))

