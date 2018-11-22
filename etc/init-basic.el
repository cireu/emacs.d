(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 60))

(use-package recentf
  :ensure nil
  :thook (find-file . ((recentf-mode)
                       (recentf-track-closed-file)))
  :init
  (setq recentf-save-file (expand-file-name "recentf" cm/cache-files-directory)
        recentf-max-saved-items 200)
  :config
  (dolist (file `(,(expand-file-name package-user-dir)
                  ,cm/cache-files-directory))))

;; Better auto save system
(use-package super-save
  :hook (after-init . super-save-mode)
  :init
  (setq super-save-auto-save-when-idle t)
  (setq auto-to-save-default nil))

(provide 'init-basic)
;;; init-basic.el ends here
