;; Better way to manage key bindings
(use-package general
  :init
  (general-evil-setup :with-shortname-maps)

  ;; The global leader
  (general-create-definer l-spc :states '(n v)
    :prefix "SPC")

  ;; The leader for the major-mode functions
  (general-create-definer l-m :states '(n v)
    :prefix "m")

  ;; The leader for just jump between the source
  (general-create-definer l-s :states '(n v)
    :prefix "s"))

;; Hydra
(use-package hydra)
