;;; -*- lexical-binding:t ; -*-

(defun cm/find-emacs-init-file ()
  "Find my emacs init file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun cm/jump-emacs-etc-directory ()
  "Jump to my emacs etc directory"
  (interactive)
  (dired cm/config-files-directory))

(defun cm/jump-emacs-lib-directory ()
  "Jump to my emacs lib directory"
  (interactive)
  (dired cm/library-files-directory))

(defun cm/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))) nil t)))

(defun cm/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defun cm/switch-to-message-buffer ()
  "Switch to the `*Messages*' buffer. Create it first if needed"
  (interactive)
  (let ((exists (get-buffer "*Messages*")))
    (switch-to-buffer (get-buffer-create "*Messages*"))))

(provide 'basic-utils)
;;; basic-utils.el ends here
