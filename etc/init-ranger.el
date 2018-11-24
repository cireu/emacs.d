;;; -*- lexical-binding:t ; -*-

(use-package ranger
  :defer 1
  :general
  (l-spc
    "ar" 'ranger
    "ad" 'deer
    "fj" 'dired)
  :init
  (setq ranger-cleanup-eagerly nil
        ranger-show-hidden t
        ranger-hide-cursor t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-dont-show-binary t
        ranger-max-preview-size 10)
  (cm/add-temp-hook 'pre-command-hook
    (require 'ranger))
  :config
  (ranger-override-dired-mode +1))

(provide 'init-ranger)
;;; init-ranger.el ends here