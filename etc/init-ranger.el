;;; -*- lexical-binding:t ; -*-

(defun cm/create-new-functions (&rest _)
  "No docstring hahaah"
  )

(use-package ranger
  :defer 3
  :general
  (l-spc
    "ar" 'ranger
    "ad" 'deer
    "fj" 'dired)
  :init
  (setq ranger-cleanup-eagerly nil
        ranger-show-hidden t
        ranger-hide-cursor nil
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-dont-show-binary t
        ranger-max-preview-size 10)
  (cm/add-temp-hook #'find-file
    (require 'ranger))
  (cm/add-temp-hook #'dired
    (require 'ranger))
  :config
  (ranger-override-dired-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ranger)
;;; init-ranger.el ends here
