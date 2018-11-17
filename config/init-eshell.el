(use-package aweshel
  :load-path "site-lisp/lazycat-collection/aweshell"
  :commands (aweshell-toggle
             aweshell-next
             aweshell-prev)
  :general
  (l-spc
    "ae" 'aweshell-toggle))
