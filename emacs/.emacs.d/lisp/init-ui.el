;;; init-ui.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package modus-themes
  :ensure nil
  :demand t
  :bind (("<f6>" . modus-themes-toggle)
         ("C-<f6>" . modus-themes-select))
  :config
  (setq modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold))
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))

  ;; ‘Mode-line’
  :custom
  (modus-themes-common-palette-overrides
   '((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  (modus-vivendi-tinted-palette-overrides
   '((comment fg-dim)))
  :init
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-day)))

(defun uk-choose-font ()
  "Allow the user to choose between two fonts: Iosevka Medium Extended-10 and Iosevka Medium-11."
  (interactive)
  (let ((font (completing-read
               "Choose a font: "
               '("Iosevka Medium-11.25"
                 "Iosevka Medium-14.25"
                 "Iosevka Medium-17.25")
               nil t)))
    (set-frame-font font nil t)))

;; Nice writing
(use-package olivetti
  :blackout
  :bind ("<f7>" . olivetti-mode)
  :config
  ;; (setq olivetti-body-width 0.62)
  (setq olivetti-style 'fancy))

(use-package doom-modeline
  ;; :disabled
  ;; :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-bar-width 4)
  ;; (setq doom-modeline-height 9)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 3)
  (setq doom-modeline-check-icon nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-buffer-encoding nil))

(use-package doom-themes
  :ensure t
  :config
  (use-package solaire-mode
    :ensure t
    :config
    (solaire-global-mode 1)))

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
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(use-package kaolin-themes
  :config
  (setq kaolin-themes-org-scale-headings nil))

;; [29-05-2025] TODO: Setup `fontaine' package

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
