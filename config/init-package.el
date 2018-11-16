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
  (require 'use-package)

  (defun use-package-normalize/:thook (name keyword args)
    (use-package-as-one (symbol-name keyword) args
      #'(lambda (label arg)
          (unless (or (use-package-non-nil-symbolp arg) (consp arg))
            (use-package-error
             (concat label " a <symbol/function> "
                     "or (<symbol/functions or list of symbols/functions> . <symbol or function>) "
                     "or list of these")))
          (use-package-normalize-pairs
           #'(lambda (k)
               (or (use-package-non-nil-symbolp k)
                   (and k (let ((every t))
                            (while (and every k)
                              (if (and (consp k)
                                       (use-package-non-nil-symbolp (car k)))
                                  (setq k (cdr k))
                                (setq every nil)))
                            every))))
           #'use-package-recognize-function
           name label arg))))

  (defalias 'use-package-autoloads/:thook 'use-package-autoloads-mode)

  (defvar cm/transient-hook-count 0)

  (defun use-package-handler/:thook (name _keyword args rest state)
    (use-package-concat
     (use-package-process-keywords name rest state)
     (cl-mapcan (lambda (def)
                  (let ((syms (car def))
                        (fun (cdr def)))
                    (when fun
                      (mapcar
                       (lambda (sym)
                         (let* ((fn (intern
                                     (format "cm/thook-fn-name-%d" cm/transient-hook-count)))
                                (add-clause (cond
                                             ((functionp sym)
                                              `(advice-add #',sym :before #',fn))
                                             ((symbolp sym)
                                              `(add-hook ',sym #',fn))))
                                (self-remove-clause (list (cond
                                                           ((functionp sym)
                                                            `(advice-remove #',sym #',fn))
                                                           ((symbolp sym)
                                                            `(remove-hook ',sym #',fn)))
                                                          `(unintern ',fn nil))))
                           (cl-incf cm/transient-hook-count 1)
                           `(progn
                              (fset ',fn (lambda (&rest _)
                                           (apply #',fun _)
                                           ,@self-remove-clause))
                              ,add-clause)))
                       (if (use-package-non-nil-symbolp syms)
                           (list syms)
                         syms)))))
                (use-package-normalize-commands args)))))

;; Extensions
(use-package package-utils
  :init
  (defalias 'cm/upgrade-all 'package-utils-upgrad-all)
  (defalias 'cm/upgrade-all-and-restart 'package-utils-upgrade-all-and-restart))

(use-package elpa-mirror
  :init
  (defalias 'cm/backup-extensions 'elpamr-create-mirror-for-installed))

(provide 'init-package)
;;; init-package.el ends here
