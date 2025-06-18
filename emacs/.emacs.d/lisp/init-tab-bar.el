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
  (setq display-time-format "(%d %b, %I:%M%p)"))
;; (display-time-mode)

(use-package tab-bar
  :ensure nil
  :if (>= emacs-major-version 28)
  :after cus-face
  :defer
  :bind-keymap ("H-t" . tab-prefix-map)
  :bind
  (("C-M-<tab>" . tab-bar-switch-to-next-tab)
   ("C-M-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("H-<tab>" . tab-bar-switch-to-next-tab)
   ("H-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("s-u" . tab-bar-history-back)
   :map tab-prefix-map
   ("H-t" . tab-bar-select-tab-by-name))
  :custom-face
  (tab-bar-tab ((t (:inherit font-lock-function-name-face))))
  :config
  (defun tab-bar-format-menu-bar ()
    "Produce the Menu buttom for the tab bar that shows the menu bar."
    `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                tab-bar-menu-bar :help "Menu Bar")))
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   (when (version< "28.0" emacs-version) 1)
         tab-bar-tab-name-truncated-max 24
         tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count
         tab-bar-new-tab-choice 'ibuffer)
  (setq tab-bar-format '(tab-bar-format-menu-bar
                         tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator
                         ;; tab-bar-format-add-tab
                         tab-bar-format-align-right
                         tab-bar-format-global)
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil))

(provide 'init-tab-bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tab-bar.el ends here
