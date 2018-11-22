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
  (load-theme 'doom-one)
  :config
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

;; Show necessary meta-data in minibuffer instead of using mode-line
(use-package awesome-tray
  :load-path "site-lisp/lazycat-collection/awesome-tray"
  :commands awesome-tray-mode
  :init
  (setq awesome-tray-mode-line-active-color "#3f444a"
        awesome-tray-git-update-duration 30)
  :hook (after-init . awesome-tray-mode))

;; Misc
(fset #'yes-or-no-p #'y-or-n-p)

(setq-default fill-column 100)

(setq line-move-visual nil)
(setq track-eol t) ; Keep the cursor at the end of the line. Requires the `line-move-visual' is nil
(setq inhibit-compacting-font-caches t)

;; Stop the blinking cursor
(blink-cursor-mode -1)

("#000000" "#000000" "#000000" "#000000" "#010101" "#010101" "#020202" "#030303" "#050505" "#090909" "#0E0E0E" "#171717" "#252525" "#3C3C3C" "#616161" "#616161" "#9E9E9E" "#9E9E9E" "#C3C3C3" "#DADADA" "#E8E8E8" "#F1F1F1" "#F6F6F6" "#FAFAFA" "#FCFCFC" "#FDFDFD" "#FEFEFE" "#FEFEFE" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF")


(provide 'init-ui)
