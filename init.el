;;; -*- lexical-binding:t ; -*-
;; (setq debug-on-error t)

(eval-when-compile
  (let ((minver "26.1"))
    (when (version< emacs-version minver)
      (error "Your Emacs don't support this config, use Emacs %s or above" minver))))

(defvar cm/config-files-directory (expand-file-name "etc" user-emacs-directory)
  "The directory to store configuration files.")

(defvar cm/cache-files-directory (expand-file-name "var" user-emacs-directory)
  "The directory to store the dotfiles create by different extensions.")

(defvar cm/library-files-directory (expand-file-name "lib" user-emacs-directory)
  "The directory to store extensions files, whether from ELPA or Github.")

(defvar cm/third-party-files-directory (expand-file-name "opt" user-emacs-directory)
  "The directory to store third party binary tools.")

(unless (file-directory-p cm/cache-files-directory)
  (mkdir cm/cache-files-directory))

(setq custom-file (expand-file-name "custom.el" cm/config-files-directory))

(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

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


(defmacro cm/load (file)
  "Load my config files"
  `(load ,(expand-file-name (symbol-name file) cm/config-files-directory) nil :no-message))

;; Package management
(cm/load init-package)

;; Benchmark
(cm/load init-benchmarking)

;; Key-bindings
(cm/load init-keybindings)
(cm/load init-evil)

;; UI
(cm/load init-ui)

;; Basic
(cm/load init-windows)
(cm/load init-basic)
(cm/load init-ivy)

;; Better editing
(cm/load init-edit)

;; Completions (YAS, Company and others)
(cm/load init-completion)

;; ;; Markup-language
;; (cm/load init-plantuml)
;; (cm/load init-org)
;; (cm/load init-markdown)

;; ;; Programming Language
;; (cm/load init-jts)
(cm/load init-emacs-lisp)
;; (cm/load init-common-lisp)
;; (cm/load init-python)

;; ;; Chinese language support
(cm/load init-chinese)

;; ;; File-managemnet
(cm/load init-ranger)
;; (cm/load init-treemacs)

;; ;; Applications
(cm/load init-shell)
;; (cm/load init-wanderlust)
;; (cm/load init-gnus)
;; (cm/load init-emms)

(provide 'init)
;;; init.el ends here
