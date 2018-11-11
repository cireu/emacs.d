;; -*- lexical-binding: t; -*-

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs don't support this config, use Emacs %s or above" minver)))

;; Avoid Emacs do GC during the initializing
(let ((init-time-gc-cons-thresold (* 100 1024 1024))
      (init-time-gc-cons-precentage 0.6)
      (run-time-gc-cons-thresold (* 20 1024 1024))
      (run-time-gc-cons-precentage 0.1))
  (setq gc-cons-thresold init-time-gc-cons-thresold
        gc-cons-percentage init-time-gc-cons-percentage)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold run-time-gc-cons-thresold
                                        gc-cons-percentage run-time-gc-cons-precentage)
                                  (add-hook 'focus-out-hook 'garbage-collect))))

(defmacro require-init (config)
  "Load the config files"
  `(load ,(file-truename (format "~/.emacs.d/config/%s" config))))

(let ((file-name-handler-alist nil))

  ;; Avoid the emacs custom-variables system
  (setq custom-file (concat user-emacs-directory "custome-variables.el"))
  ;; (load custom-file)

  ;; Without this comment emacs25 add (package-initialize) here
  ;; Package management
  (require-init init-package)

  ;; Basic
  (require-init init-hydra)
  (require-init init-ivy)

  ;; Better editing
  (require-init init-edit)

  ;; General programming functions
  (require-init init-progs)

  ;; Programming Language
  (require-init init-jts)
  (require-init init-common-lisp)
  (require-init init-emacs-lisp)
  (require-init init-python)

  ;; Markup-language
  (require-init init-plantuml)
  (require-init init-org)
  (require-init init-markdown)

  ;; UI
  (require-init init-ui)

  ;; Chinese language support
  (require-init init-chinese)

  ;; File-managemnet
  (require-init init-ranger)
  (require-init init-treemacs)

  ;; Applications
  (require-init init-wanderlust)
  (require-init init-edit-server)
  (require-init init-gnus)
  (require-init init-emms)

  )
