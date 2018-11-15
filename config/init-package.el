;; -*- lexical-binding: t; -*-

;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Set up the ELPA source
(setq package-archives
      '(
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org"   . "https://elpa.emacs-china.org/org/")))

;; Fire up the `package.el'
(setq package-enable-at-startup nil)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(eval-when-compile
  (require 'use-package))

;; Extensions
(use-package package-utils
  :init
  (defalias 'cm/upgrade-all 'package-utils-upgrad-all)
  (defalias 'cm/upgrade-all-and-restart 'package-utils-upgrade-all-and-restart))

(use-package elpa-mirror
  :init
  (defalias 'cm/backup-extensions 'elpamr-create-mirror-for-installed))

(provide 'init-package)
