;;; -*- lexical-binding:t ; -*-

(defvar cm/eshell-private-data-directory (expand-file-name "eshell" cm/cache-files-directory)
  "The directory store cache files generated by eshell")

;; Use msys2 bash shell
(setq explicit-shell-file-name "bash.exe")

(use-package aweshell
  :ensure nil
  :general
  (l-spc
    "ae" 'aweshell-toggle))

(provide 'init-shell)
;;; init-shell.el ends here
