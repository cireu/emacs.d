;; Ivy, swiper, counsel
;; `ivy' is the dependency of the `swiper', and `swiper'is the dependency of the `counsel'
(use-package counsel
  :general
  ("C-s" 'swiper
   "C-c C-r" 'ivy-resume)
  (:keymaps 'counsel-mode-map
            [remap swiper] 'counsel-grep-or-swiper
            "C-x C-r" 'counsel-recentf
            "C-x j" 'counsel-mark-ring

            "C-c L" 'counsel-load-library
            "C-c P" 'counsel-package
            "C-c f" 'counsel-find-library
            "C-c g" 'counsel-grep
            "C-c h" 'counsel-command-history
            "C-c i" 'counsel-git
            "C-c j" 'counsel-git-grep
            "C-c l" 'counsel-locate
            "C-c r" 'counsel-rg
            "C-c z" 'counsel-fzf

            "C-c c L" 'counsel-load-library
            "C-c c P" 'counsel-package
            "C-c c a" 'counsel-apropos
            "C-c c e" 'counsel-colors-emacs
            "C-c c f" 'counsel-find-library
            "C-c c g" 'counsel-grep
            "C-c c h" 'counsel-command-history
            "C-c c i" 'counsel-git
            "C-c c j" 'counsel-git-grep
            "C-c c l" 'counsel-locate
            "C-c c m" 'counsel-minibuffer-history
            "C-c c o" 'counsel-outline
            "C-c c p" 'counsel-pt
            "C-c c r" 'counsel-rg
            "C-c c s" 'counsel-ag
            "C-c c t" 'counsel-load-theme
            "C-c c u" 'counsel-unicode-char
            "C-c c w" 'counsel-colors-web
            "C-c c z" 'counsel-fzf)
  (:keymaps 'swiper-map
            "M-q" 'swiper-query-replace)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s .")))


;; enchanced M-x
(use-package amx)

;; For better fuzzy search
(use-package flx)

(provide 'init-ivy)
