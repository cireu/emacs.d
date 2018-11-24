;;; -*- lexical-binding:t ; -*-

(defun cm/find-emacs-init-file ()
  "Find my emacs init file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun cm/jump-emacs-etc-directory ()
    "Jump to my emacs etc directory"
  (interactive)
  (dired cm/config-files-directory))

(provide 'basic-util)
;;; basic-util.el ends here
