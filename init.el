;; -*- lexical-binding: t; -*-

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs don't support this config, use Emacs %s or above" minver)))

;; Avoid Emacs do GC during the initializing
(let ((init-time-gc-cons-thresold (* 100 1024 1024))
      (init-time-gc-cons-percentage 0.6)
      (run-time-gc-cons-thresold (* 20 1024 1024))
      (run-time-gc-cons-percentage 0.1))
  (setq gc-cons-thresold init-time-gc-cons-thresold
        gc-cons-percentage init-time-gc-cons-percentage)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold run-time-gc-cons-thresold
                                        gc-cons-percentage run-time-gc-cons-percentage)
                                  (add-hook 'focus-out-hook 'garbage-collect))))

(defmacro load-config! (config)
  "Load the config files"
  `(load ,(file-truename (format "~/.emacs.d/config/%s" config))))

(let ((file-name-handler-alist nil))

  (setq custom-file (concat user-emacs-directory "custom.el"))
  ;; Without this comment emacs25 add (package-initialize) here
  ;; Package management
  (load-config! init-package)

  ;; Key-bindings
  (load-config! init-evil)
  (load-config! init-hydra)

  ;; Basic
  (load-config! init-ivy)

  ;; Better editing
  (load-config! init-edit)

  ;; General programming functions
  (load-config! init-progs)

  ;; Programming Language
  (load-config! init-jts)
  (load-config! init-common-lisp)
  (load-config! init-emacs-lisp)
  (load-config! init-python)

  ;; Markup-language
  (load-config! init-plantuml)
  (load-config! init-org)
  (load-config! init-markdown)

  ;; UI
  (load-config! init-ui)

  ;; Chinese language support
  (load-config! init-chinese)

  ;; File-managemnet
  (load-config! init-dired)
  (load-config! init-treemacs)

  ;; Applications
  (load-config! init-wanderlust)
  (load-config! init-edit-server)
  (load-config! init-gnus)
  (load-config! init-emms)

  ;; Load custom file
  (when (file-exists-p custom-file) (load custom-file)))
