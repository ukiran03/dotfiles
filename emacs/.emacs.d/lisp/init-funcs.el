
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

(defun uk-toggle-time-bat-status ()
  "Toggle both `display-time-mode` and `display-battery-mode` together."
  (interactive)
  (if (and display-time-mode display-battery-mode)
      (progn
        (display-time-mode -1)
        (display-battery-mode -1))
    (progn
      (display-time-mode 1)
      (display-battery-mode 1))))

(provide 'init-funcs)
