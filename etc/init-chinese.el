;;; -*- lexical-binding:t ; -*-

(use-package pyim
  :init
  (setq pyim-dcache-directory (expand-file-name "pyim/dcache/" cm/cache-files-directory))
  (setq pyim-default-scheme 'xiaohe-shuangpin))

(provide 'init-chinese)
;;; init-chinese.el ends here
