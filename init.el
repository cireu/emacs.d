;;; -*- lexical-binding: t; -*-
(setq debug-on-error t)

(eval-when-compile
  (let ((minver "26.1"))
    (when (version< emacs-version minver)
      (error "Your Emacs don't support this config, use Emacs %s or above" minver))))

;; Set up constants
(defvar cm/my-config-files-directory (expand-file-name "etc" user-emacs-directory)
  "Where the configuration files were stored.")

(defvar cm/cache-file-directory (expand-file-name "var" user-emacs-directory)
  "The directory to store the dotfiles create by different extensions.")

(defvar cm/library-file-directory (expand-file-name "lib" user-emacs-directory)
  "The directory to store extensions files, whether from ELPA or Github.")

(defvar cm/third-party-file-directory (expand-file-name "opt" user-emacs-directory)
  "The directory to store third party binary tools.")

(setq custom-file (expand-file-name "custom.el" cm/my-config-files-directory))

;; Avoid Emacs do GC during the initializing
(let ((init-time-gc-cons-threshold (* 100 1024 1024))
      (init-time-gc-cons-percentage 0.6)
      (run-time-gc-cons-threshold (* 20 1024 1024))
      (run-time-gc-cons-percentage 0.1)
      (default-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-time-gc-cons-threshold
        gc-cons-percentage init-time-gc-cons-percentage)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq file-name-handler-alist default-file-name-handler-alist)
                                  (setq gc-cons-threshold run-time-gc-cons-threshold
                                        gc-cons-percentage run-time-gc-cons-percentage)
                                  (add-hook 'focus-out-hook 'garbage-collect))))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Load path
(push 'load-path cm/my-config-files-directory)

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

;; Markup-language
(require 'init-plantuml)
(require 'init-org)
(require 'init-markdown)

;; Programming Language
(require 'init-jts)
(require 'init-common-lisp)
(require 'init-emacs-lisp)
(require 'init-python)

;; UI
(require 'init-ui)

;; Chinese language support
(require 'init-chinese)

;; File-managemnet
(require 'init-dired)
(require 'init-treemacs)

;; Applications
(require 'init-eshell)
(require 'init-wanderlust)
(require 'init-gnus)
(require 'init-emms)

;; Load custom file
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
