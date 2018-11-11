;; Ivy, swiper, counsel
(use-package counsel
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-q" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
    :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)
  ;; (setq ivy-initial-inputs-alist nil)

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

;; Enchanced M-x
(use-package smex)

;; Pop a childframe to show the completion
;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-parameters '((left-fringe . 5)
;;                                   (right-fringe . 5)))

;;   (dolist (func '(counsel-M-x
;;                   counsel-find-file
;;                   ivy-switch-buffer))
;;     (push `(,func . ivy-posframe-display-at-point) ivy-display-functions-alist))

;;   (ivy-posframe-enable))

(provide 'init-ivy)
