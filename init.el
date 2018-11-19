;; -*- lexical-binding: t; -*-
(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs don't support this config, use Emacs %s or above" minver)))

;; Avoid Emacs do GC during the initializing
(let ((init-time-gc-cons-threshold (* 100 1024 1024))
      (init-time-gc-cons-percentage 0.6)
      (run-time-gc-cons-threshold (* 20 1024 1024))
      (run-time-gc-cons-percentage 0.1))
  (setq gc-cons-threshold init-time-gc-cons-threshold
        gc-cons-percentage init-time-gc-cons-percentage)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold run-time-gc-cons-threshold
                                        gc-cons-percentage run-time-gc-cons-percentage)
                                  (add-hook 'focus-out-hook 'garbage-collect))))

(defmacro cm/load-config (config)
  "Load the config files"
  `(load ,(file-truename (format "~/.emacs.d/config/%s" config))))

(let ((file-name-handler-alist nil))

  (setq custom-file (concat user-emacs-directory "custom.el"))
  ;; Without this comment emacs25 add (package-initialize) here
  ;; Package management
  (cm/load-config init-package)

  ;; Key-bindings
  (cm/load-config init-evil)
  (cm/load-config init-hydra)

  ;; Basic
  (cm/load-config init-ivy)

  ;; Better editing
  (cm/load-config init-edit)

  ;; General programming functions
  (cm/load-config init-progs)

  ;; Programming Language
  (cm/load-config init-jts)
  (cm/load-config init-common-lisp)
  (cm/load-config init-emacs-lisp)
  (cm/load-config init-python)

  ;; Markup-language
  (cm/load-config init-plantuml)
  (cm/load-config init-org)
  (cm/load-config init-markdown)

  ;; UI
  (cm/load-config init-ui)

  ;; Chinese language support
  (cm/load-config init-chinese)

  ;; File-managemnet
  (cm/load-config init-dired)
  (cm/load-config init-treemacs)

  ;; Applications
  (cm/load-config init-wanderlust)
  (cm/load-config init-edit-server)
  (cm/load-config init-gnus)
  (cm/load-config init-emms)

  ;; Load custom file
  (when (file-exists-p custom-file) (load custom-file)))
