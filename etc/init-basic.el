;;; -*- lexical-binding:t ; -*-

;; `*scratch*' buffer
(setq-default initial-scratch-message ""
              default-directory (expand-file-name "~/"))

;; `lisp-interaction-mode' is useless and force run `prog-mode-hook' so we...
(setq initial-major-mode #'text-mode)

;; Don't use Windows' find program!
(setq find-program (expand-file-name "c:/msys64/usr/bin/find.exe"))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (setq savehist-file (expand-file-name "history" cm/cache-files-directory)))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file (expand-file-name "recentf" cm/cache-files-directory)
        recentf-max-saved-items 200)
  (cm/add-temp-hook 'find-file-hook
    (recentf-mode)
    (recentf-track-opened-file)))

;; Better auto save system
(use-package super-save
  :ensure nil ;; Force use from site-lisp
  :commands (super-save-mode)
  :hook (after-init . super-save-mode)
  :init
  (setq save-silently t
        super-save-auto-save-when-idle t
        super-save-idle-duration 1
        super-save-all-buffers t)
  (setq auto-save-list-file-prefix (expand-file-name ".save-"
                                                     (concat cm/cache-files-directory "/auto-save-list"))
        auto-to-save-default nil))


(defun cm/find-emacs-init-file ()
  "Find my emacs init file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun cm/jump-emacs-etc-directory ()
  "Jump to my emacs etc directory"
  (interactive)
  (dired cm/config-files-directory))

(defun cm/jump-emacs-lib-directory ()
  "Jump to my emacs lib directory"
  (interactive)
  (dired cm/library-files-directory))

(l-spc
  "fs" 'save-buffer
  "fS" 'evil-write-all
  "fei" 'cm/find-emacs-init-file
  "fed" 'cm/jump-emacs-etc-directory
  "fel" 'cm/jump-emacs-lib-directory)

(l-spc
  ";" 'eldoc-eval-expression)

(provide 'init-basic)
;;; init-basic.el ends here
