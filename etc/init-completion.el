;; Company
(use-package company
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :defer 2
  :thook (post-self-insert-hook . ((require 'company)))
  :preface
  (defvar company-enable-yas
    "Enable yasnippet for all backends")
  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
	    (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  :general
  ("M-/" 'company-complete
   "C-c C-y" 'company-yasnippet)
  (:keymaps 'company-active-map
	    "C-p" 'company-select-previous
	    "C-n" 'company-select-next
	    "TAB" 'company-complete-common-or-cycle
	    "S-TAB" 'company-select-previous)
  (:keymaps 'company-search-map
	    "C-p" 'company-select-previous
	    "C-n" 'company-select-next)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 12            ; bigger popup window
	company-idle-delay .4               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (setq company-show-numbers t)

  ;;(company-statistics-mode)
  (global-company-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-delay 2)
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
              (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
              (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint"               :height 0.8 :face 'all-the-icons-pink))
        company-box-icons-lsp
        `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
          (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
          (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
          (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
          (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
          (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
          (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
          (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
          (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
          (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
          (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
          (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
          (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
          (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
          (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
          (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
          (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
          (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
          (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
          (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
          (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
          (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
          (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
          (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
          (25 . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red)))))

;; YASnippet
(use-package yasnippet
  :hook ((org-mode
          prog-mode
          snippet-mode) . yas-minor-mode-on)
  :thook (yas-minor-mode-hook . ((yas-reload-all)))
  :init (setq yas-snippet-dirs `(,(expand-file-name "snippets" cm/library-files-directory)))
  :general (:keymaps 'yas-minor-mode-map "TAB" 'yas-expand)
  :config
  (use-package yasnippet-snippets))

(provide 'init-progs)
;;; init-progs.el ends here
