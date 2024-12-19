;;; init-tab-bar.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

;; ;; ‘Time’ on tab-bar
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(use-package time
  :init
  (setq display-time-interval 1)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %d %b, %I:%M%p"))
(display-time-mode)

(use-package tab-bar
  :ensure nil
  :config
  (defun tab-bar-format-menu-bar ()
    "Produce the Menu buttom for the tab bar that shows the menu bar."
    `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                tab-bar-menu-bar :help "Menu Bar")))
  (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   (when (version< "28.0" emacs-version) 1)
         tab-bar-tab-name-truncated-max 24
	     tab-bar-close-button-show nil
         tab-bar-new-button-show nil
         tab-bar-new-tab-choice 'ibuffer))

(provide 'init-tab-bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tab-bar.el ends here
