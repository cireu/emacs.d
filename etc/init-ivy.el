;; Ivy
(use-package ivy
  :functions (ivy-thing-at-point
              ivy-switch-buffer)
  :defines (ivy-height ivy-format-function
                       ivy-use-selectable-prompt
                       ivy-use-virtual-buffers
                       ivy-on-del-error-function
                       ivy-height
                       ivy-count-format
                       ivy-on-del-error-function
                       ivy-format-function)
  :preface
  (defun cm/ivy-format-function-arrow-auto-fill (cands)
    "Transform CANDS into a string for minibuffer, but force the minibuffer to fit `ivy-height'"
    (ivy--format-function-generic
     (lambda (str)
       (concat "> " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "  " str (when (eq str (car (last cands)))
                          (apply #'concat (cl-loop repeat (- ivy-height (length cands))
                                                   collect "\n")))))
     cands
     "\n"))
  :defer 1
  :general
  ("C-c C-r" 'ivy-resume)
  (l-spc
    "bb" 'ivy-switch-buffer)
  (:keymaps 'ivy-minibuffer-map
            [escape] 'minibuffer-keyboard-quit)
  :config
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-on-del-error-function nil
        ivy-height 20
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-dynamic-exhibit-delay-ms 200
        ivy-format-function #'cm/ivy-format-function-arrow-auto-fill)
  (ivy-mode +1))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode))

(use-package ivy-hydra
  :general
  (:keymaps 'ivy-minibuffer-map
            "M-o" 'ivy-dispatching-done-hydra))

;; Swiper
(use-package swiper
  :preface
  (defun cm/swiper-region-or-symbol ()
    "Run `swiper' with the selected region or the symbol
around point as the initial input."
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (swiper input)))
  :general
  ("C-s" 'swiper)
  (nvmap "#" 'cm/swiper-region-or-symbol)
  (:keymaps 'swiper-map
            "M-q" 'swiper-query-replace)
  :config
  (setq swiper-action-recenter t))

(use-package counsel
  :general
  (:keymaps 'counsel-mode-map
            [remap swiper] 'counsel-grep-or-swiper)
  (l-spc                   'counsel-mode-map
    "SPC"                  'counsel-M-x

    "fr"                   'counsel-recentf
    "ff"                   'counsel-find-file
    "fL"                   'counsel-find-library

    "aL"                   'counsel-load-library
    "aP"                   'cousel-package

    "T"                    'counsel-load-theme

    "iu"                   'counsel-unicode-char)
  (l-s 'counsel-mode-map
    "r" 'counsel-rg
    "g" 'counsel-grep
    "p" 'counsel-pt)

  ;; "C-x j"         'counsel-mark-ring

  ;; "C-c c L"       'counsel-load-library
  ;; "C-c c P"       'counsel-package
  ;; "C-c c a"       'counsel-apropos
  ;; "C-c c e"       'counsel-colors-emacs
  ;; "C-c c f"       'counsel-find-library
  ;; "C-c c g"       'counsel-grep
  ;; "C-c c h"       'counsel-command-history
  ;; "C-c c i"       'counsel-git
  ;; "C-c c j"       'counsel-git-grep
  ;; "C-c c l"       'counsel-locate
  ;; "C-c c m"       'counsel-minibuffer-history
  ;; "C-c c o"       'counsel-outline
  ;; "C-c c p"       'counsel-pt
  ;; "C-c c r"       'counsel-rg
  ;; "C-c c s"       'counsel-ag
  ;; "T"       'counsel-load-theme
  ;; "iu"       'counsel-unicode-char
  ;; "C-c c w"       'counsel-colors-web
  ;; "C-c c z"       'counsel-fzf
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "pt")
           "pt -zS --nocolor --nogroup -e %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s .")))

;; Enchanced M-x
(use-package amx)

;; For better fuzzy searching
(use-package flx
  :init
  (setq ivy-re-builders-alist
        '((counsel-grep . ivy--regex-plus)
          (counsel-rg   . ivy--regex-plus)
          (counsel-pt   . ivy--regex-plus)
          (swiper       . ivy--regex-plus)
          (t            . ivy--regex-fuzzy))))

;; World Time
(use-package counsel-world-clock
  :general
  (l-spc
    :keymaps 'counsel-mode-map
    "ak" 'counsel-world-clock))

(provide 'init-ivy)
;; init-ivy.el ends here
