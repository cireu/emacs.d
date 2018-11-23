;; Restore old window config
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Enforce rules for popups
(use-package shackle
  :hook (after-init . shackle-mode)
  :config
  (defun view-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))
  (bind-key "C-h z" #'view-last-popup-buffer)

  ;; Add keyword: `autoclose'
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (loop for (window . buffer) in shackle--popup-window-list
                if (and (window-live-p window)
                        (equal (window-buffer window) buffer))
                collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list))))))

  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

  ;; rules
  (setq shackle-default-size 0.4)
  (setq shackle-rules
        '(("*Help*" :select t :align 'below :autoclose t)
          ("*compilation*" :size 0.25 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 0.25 :align 'below :autoclose t)
          ("*ert*" :same t)
          ("*info*" :select t :inhibit-window-quit t :same t)
          ("*Backtrace*" :select t :size 20 :align 'below)
          ("*Warnings*" :size 12 :align 'below :autoclose t)
          ("*Messages*" :size 12 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t :inhibit-window-quit t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          (apropos-mode :size 0.3 :align 'below :autoclose t)
          (Buffer-menu-mode :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (grep-mode :size 25 :align 'below :autoclose t)
          (profiler-report-mode :popup t)
          (tabulated-list-mode :align 'below)
          ("^ ?\\*" :regexp t :select t :align 'below :autoclose t))))

(use-package window-numbering
  :hook (after-init . window-numbering-mode))

;; Quickly switch windows
(use-package ace-window
  :commands (ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit 'font-lock-keyword-face :height 2.0))))
  (aw-mode-line-face    ((t (:inherit 'mode-line-emphasis :bold t))))
  :config
  (ace-window-display-mode))

(use-package windmove
  :preface
  ;; 
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-window ()
    "
Movement^^   ^Split^         ^Switch^     ^Resize^
-----------------------------------------------------
_h_ Left     _v_ertical      _b_uffer     _q_ X left
_j_ Down     _x_ horizontal  _f_ind files _w_ X Down
_k_ Top      _z_ undo        _a_ce 1      _e_ X Top
_l_ Right    _Z_ reset       _s_wap       _r_ X Right
_F_ollow     _D_elete Other  _S_ave       max_i_mize
_SPC_ cancel _o_nly this     _d_elete
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("q" hydra-move-splitter-left)
    ("w" hydra-move-splitter-down)
    ("e" hydra-move-splitter-up)
    ("r" hydra-move-splitter-right)
    ("b" ivy-switch-buffer)
    ("f" counsel-find-file)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("o" delete-other-windows)
    ("i" ace-delete-other-windows)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo)))
    ("Z" winner-redo)
    ("SPC" nil))
  (global-set-key (kbd "C-c C-w") 'hydra-window/body)
  ;; }}
  :ensure nil
  :general
  (l-spc
    "w" 'hydra-window/body)
  :hook (after-init . windmove-default-keybindings))

(provide 'init-window)
;;; init-window.el ends here
