;;; -*- lexical-binding:t ; -*-

(defun cm/swiper-region-or-symbol ()
    "Run `swiper' with the selected region or the symbol
round point as the initial input."
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (swiper input)))

(provide 'ivy-util)
;;; ivy-util.el ends here
