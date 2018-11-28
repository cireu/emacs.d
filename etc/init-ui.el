;; -*- lexical-binding: t; -*-

;; Suppress the GUI features
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode -1))

(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Set the cursor type
(setq cursor-type '(bar . 2))

;; Set font
;; TODO May use `cnfonts' to set CJK fonts
(set-frame-font "Sarasa Mono SC 11" nil t)

;; Set up the themes
(use-package doom-themes
  :init
  (load-theme 'doom-solarized-light)
  (cm/add-temp-hook #'org-mode
    (doom-themes-org-config)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init))

;; Show necessary meta-data in minibuffer instead of using mode-line
;; (use-package awesome-tray
;;   :load-path "lib/site-lisp/awesome-tray"
;;   :commands awesome-tray-mode
;;   :init
;;   (setq awesome-tray-mode-line-active-color "#3f444a"
;;         awesome-tray-git-update-duration 30
;;         awesome-tray-active-modules '("parent-dir" "git" "mode-name" "date"))
;;   :hook (after-init . awesome-tray-mode))

;; All-the-icons
(use-package all-the-icons
  :commands (all-the-icons-material))

;; Misc
(fset #'yes-or-no-p #'y-or-n-p)

(setq-default fill-column 80)

(setq line-move-visual nil)
(setq track-eol t) ; Keep the cursor at the end of the line. Requires the `line-move-visual' is nil
(setq inhibit-compacting-font-caches t)

;; Prettify Symbol
(add-hook 'org-mode-hook #'global-prettify-symbols-mode)
(add-hook 'prog-mode-hook #'global-prettify-symbols-mode)

;; Stop the blinking cursor
(blink-cursor-mode -1)

(provide 'init-ui)
;;; init-ui.el ends here
