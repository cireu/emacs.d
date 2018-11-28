;;; -*- lexical-binding:t ; -*-

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
---------------------------------------------------------
_h_ Left     _3_ vertical    _b_uffer     _q_ X left
_j_ Down     _2_ horizontal  _f_ind files _w_ X Down
_k_ Top      _z_ undo        _R_ecentf    _e_ X Top
_l_ Right    _Z_ reset       _d_elete     _r_ X Right
_F_ollow     _o_nly this                  _G_olden ratio
_SPC_ cancel                              
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("G" balance-windows)
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("R" counsel-recentf)
  ("F" follow-mode)
  ("3" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("2" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" save-buffer)
  ("d" delete-window)
  ("o" delete-other-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))

(provide 'windows-funcs)
;;; windows-funcs.el ends here
