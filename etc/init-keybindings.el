;; Better way to manage key bindings
(use-package general
  :init
  (general-evil-setup :with-shortname-maps)

  ;; The global leader
  (general-create-definer l-spc :states '(n v)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; The leader for the major-mode functions
  (general-create-definer l-m :states '(n v)
    :prefix ","
    :non-normal-prefix "M-m"))

;; Keep the key bindings same to terminal
(general-define-key "C-h" 'delete-backward-char)
(general-define-key "M-h" 'backward-kill-word)
(general-define-key "<f1>" 'help-command)

;; Hydra
(use-package hydra)

;; Record your key frequency
(use-package keyfreq
  :init
  (setq keyfreq-file (expand-file-name ".emacs.keyfreq"
                                       cm/cache-files-directory)
        keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock"
                                            cm/cache-files-directory))
  (cm/add-temp-hook 'pre-command-hook
    (require 'keyfreq)
    (keyfreq-mode +1)
    (keyfreq-autosave-mode +1)))
