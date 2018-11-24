;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("pv" "(provide '`(replace-regexp-in-string \"\\.el$\" \"\" (buffer-name))`)\n;;; `(buffer-name)` ends here" "provide" nil nil nil "c:/Users/asd/.emacs.d/lib/snippets/emacs-lisp-mode/provide" nil nil)
                       ("lb" ";;; -*- lexical-binding:t ; -*-" "lexical-binding" nil nil nil "c:/Users/asd/.emacs.d/lib/snippets/emacs-lisp-mode/lexical-binding" nil nil)
                       ("ifun" "(defun $1 ($2)\n  \"$3\"\n  (interactive$4)\n  $0)" "interactive-funcs" nil nil nil "c:/Users/asd/.emacs.d/lib/snippets/emacs-lisp-mode/interactive-funcs" nil nil)))


;;; Do not edit! File generated at Sat Nov 24 10:08:19 2018
