;;; init-ui.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f6>" . modus-themes-toggle)
         ("C-<f6>" . modus-themes-select))
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold))

  ;; ‘Mode-line’'
  :custom
  (modus-themes-common-palette-overrides
   '((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  :init
  (load-theme 'modus-vivendi :no-confirm))

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-day)))

;; `Fonts'
;; (use-package fontaine
;;   :config
;;   (setq fontaine-presets
;;         '((Iosevka
;;            :default-family "Iosevka Nerd Font"
;;            :default-height 110
;;            :default-weight medium)
;;           (Uosevka
;;            :default-family "Uosevka"
;;            :default-height 100
;;            :default-weight medium))))
(use-package fontaine
  :config
  ;; https://typeof.net/Iosevka/
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Extended"
           :default-weight normal
           :default-height 100
           :fixed-pitch-family "Iosevka Term"
           :fixed-pitch-weight semi-light
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Etoile"
           :variable-pitch-weight semi-light
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil ; use whatever the underlying face has
           :italic-slant italic
           :line-spacing 1)
          (large
           :inherit regular
           :default-height 150)))
  (fontaine-set-preset 'regular)
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :config (setq olivetti-body-width 0.62))

(use-package doom-modeline
  :disabled
  ;; :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 3)
  (setq doom-modeline-check-icon nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-buffer-encoding nil))


;; ;; Frame transparence
(use-package transwin
  :disabled
  :bind (("C-M-9" . transwin-inc)
         ("C-M-8" . transwin-dec)
         ("C-M-7" . transwin-toggle))
  :init
  (setq transwin-parameter-alpha 'alpha-background))

(add-to-list 'default-frame-alist '(alpha-background . 100))


;;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :ensure t
  :functions (prot/breadcrumb-local-mode)
  :hook ((text-mode prog-mode) . prot/breadcrumb-local-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > ")

  (defun prot/breadcrumb-local-mode ()
    "Enable `breadcrumb-local-mode' if the buffer is visiting a file."
    (when buffer-file-name
      (breadcrumb-local-mode 1))))

;; Icons
(use-package nerd-icons
  ;; :init
  ;; (setq nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(use-package kaolin-themes)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
