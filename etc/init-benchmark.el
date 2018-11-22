(use-package benchmark-init
  :commands (benchmark-init/activate benchmark-init/deactivate)
  :init
  (benchmark-init/activate)
  ;; (run-with-idle-timer 1 nil #'benchmark-init/deactivate))
  :hook (after-init . benchmark-init/deactivate))

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
