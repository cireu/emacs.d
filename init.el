;; -*- lexical-binding: t; -*-
(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs don't support this config, use Emacs %s or above" minver)))

;; Avoid Emacs do GC during the initializing
(let ((init-time-gc-cons-threshold (* 100 1024 1024))
      (init-time-gc-cons-percentage 0.6)
      (run-time-gc-cons-threshold (* 20 1024 1024))
      (run-time-gc-cons-percentage 0.1)
      (default-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-time-gc-cons-threshold
        gc-cons-percentage init-time-gc-cons-percentage)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq file-name-handler-alist default-file-name-handler-alist)
                                  (setq gc-cons-threshold run-time-gc-cons-threshold
                                        gc-cons-percentage run-time-gc-cons-percentage)
                                  (add-hook 'focus-out-hook 'garbage-collect))))


;; (defmacro cm/load-config (config)
;;   "Load the config files"
;;   `(load ,(file-truename (format "~/.emacs.d/config/%s" config))))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Load path
(cl-pushnew (expand-file-name "site-lisp" user-emacs-directory) load-path)
(cl-pushnew (expand-file-name "lisp" user-emacs-directory) load-path)

(setq custom-file (concat user-emacs-directory "custom.el"))
;; Without this comment emacs25 add (package-initialize) here
;; Package management
(require 'init-package)

;; Key-bindings
(require 'init-evil)
(require 'init-hydra)

;; Basic
(require 'init-ivy)

;; Better editing
(require 'init-edit)

;; General programming functions
(require 'init-progs)

;; Programming Language
(require 'init-jts)
(require 'init-common-lisp)
(require 'init-emacs-lisp)
(require 'init-python)

;; Markup-language
(require 'init-plantuml)
(require 'init-org)
(require 'init-markdown)

;; UI
(require 'init-ui)

;; Chinese language support
(require 'init-chinese)

;; File-managemnet
(require 'init-dired)
(require 'init-treemacs)

;; Applications
(require 'init-wanderlust)
(require 'init-edit-server)
(require 'init-gnus)
(require 'init-emms)

;; Load custom file
(when (file-exists-p custom-file) (load custom-file))
