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
  :defer 1
  :init
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t

        ivy-initial-inputs-alist nil
        ivy-on-del-error-function nil

        ivy-height 17
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-format-function #'ivy-format-function-arrow

        ivy-on-del-error-function nil
        ivy-dynamic-exhibit-delay-ms 200)

  (cm/add-temp-hook 'pre-command-hook
    (require 'ivy))
  :general
  ("C-c C-r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            [escape] 'minibuffer-keyboard-quit)
  :config
  (ivy-mode +1))

(use-package ivy-rich
  :hook
  (ivy-mode . ivy-rich-mode)
  (ivy-rich-mode . (lambda ()
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name)))))

(use-package ivy-hydra
  :preface
  ;; use `hydra-ivy/body' automatically in some ivy-based functions
  (defvar cm/ivy-auto-enable-hydra-list '(ivy-switch-buffer
                                          counsel-recentf
                                          cm/swiper-region-or-symbol))
  ;; FIXME Force `ivy-hydra' exit when quit minibuffer
  :hook (minibuffer-exit-hook . hydra-ivy/nil)
  :init
  (dolist (func cm/ivy-auto-enable-hydra-list)
    (advice-add func :before (lambda (&rest _)
                               (cm/add-temp-hook 'minibuffer-setup-hook
                                 (hydra-ivy/body)))))
  :general
  (:keymaps 'ivy-minibuffer-map
            "M-o" 'ivy-dispatching-done-hydra
            "M-SPC" 'hydra-ivy/body)
  (:keymaps 'hydra-ivy/keymap
            "M-SPC" 'hydra-ivy/nil))

;; Swiper
(use-package swiper
  :general
  ("C-s" 'swiper)
  (l-s
    "s" 'swiper)
  (:keymaps 'swiper-map
            "M-q" 'swiper-query-replace)
  :config
  (setq swiper-action-recenter t))

(use-package counsel
  :general
  ([remap swiper] 'counsel-grep-or-swiper)
  (l-spc                   
    "SPC" 'counsel-M-x

    "bb"  'ivy-switch-buffer
    
    "fr"  'counsel-recentf
    "ff"  'counsel-find-file
    "fL"  'counsel-find-library

    "aL"  'counsel-load-library
    "aP"  'cousel-package

    "T"   'counsel-load-theme

    "iu"  'counsel-unicode-char

    "r"   'counsel-rg)
  

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
(use-package amx
  :init
  (setq amx-save-file (expand-file-name "amx-items" cm/cache-files-directory)))

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
    "ak" 'counsel-world-clock))

(defun cm/swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol
round point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

(l-spc "3" 'cm/swiper-region-or-symbol)

(provide 'init-ivy)
;; init-ivy.el ends here
