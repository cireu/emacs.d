;; -*- lexical-binding: t; -*-

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility

(defvar cm/elpa-directory (expand-file-name
                           (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                           cm/library-files-directory)
  "The versioned elpa directory")

(defvar cm/site-lisp-directory (expand-file-name
                                "site-lisp"
                                cm/library-files-directory)
  "The packages won't exist on ELPA")

(setq package-user-dir cm/elpa-directory)

;; Set up the ELPA source
(setq package-archives
      '(("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org"   . "https://elpa.emacs-china.org/org/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")))


(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; site-lisp load path
(let ((default-directory (file-name-as-directory cm/site-lisp-directory)))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t)

;; Fire up `use-package'
(eval-when-compile
  (require 'use-package))

(defvar cm//transient-counter 0)
(defmacro cm/add-temp-hook (hook &rest forms)
  "Attaches transient forms to a HOOK.
HOOK can be a quoted hook or a sharp-quoted function (which will be advised).
These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "cm-transient-hook-%s" (cl-incf cm//transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))

;; Extensions
(use-package package-utils
  :init
  (defalias 'cm/upgrade-all 'package-utils-upgrade-all)
  (defalias 'cm/upgrade-all-and-restart 'package-utils-upgrade-all-and-restart))

(use-package elpa-mirror
  :init
  (defalias 'cm/backup-extensions 'elpamr-create-mirror-for-installed))

(provide 'init-package)
;;; init-package.el ends here
