;;; init-ui.el --- User Interface Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Clean, modular UI settings featuring Modus, Ef, and Fontaine packages.

;;; Code:

;;;; Fonts & Typography (Fontaine)
(use-package fontaine
  :disabled
  :ensure t
  :hook (after-init . fontaine-mode)
  :bind ("C-c f" . fontaine-set-preset)
  :config
  ;; Define your Iosevka steps here instead of manual strings
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Medium"
           :default-size 11.25)
          (medium
           :default-family "Iosevka Medium"
           :default-size 13)
          (large
           :default-family "Iosevka Medium"
           :default-size 15.5)
          (huge
           :default-family "Iosevka Medium"
           :default-size 17.25)))
  ;; Set default startup preset
  (fontaine-set-preset 'regular))

;;;; Theme Configurations
(use-package modus-themes
  :ensure t
  :demand t
  :custom
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
  ;; Palette overrides
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)))
  (modus-vivendi-tinted-palette-overrides
   '((comment fg-dim))))

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-day)))

(use-package kaolin-themes
  :ensure t
  :config
  (setq kaolin-themes-org-scale-headings nil))


;;;; Writing & Layout
(use-package olivetti
  :ensure t
  :defer t
  :config
  (setq olivetti-style 'fancy))


;;;; UI Elements & Context
(use-package breadcrumb
  :ensure t
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


;;;; Theme Persistence Engine

(defvar my-theme 'modus-vivendi-tinted
  "The color theme to load on startup.")

(defun my-save-theme-to-custom-file (variable value)
  "Set VARIABLE to VALUE and write it as a plain (setq ...) to `custom-file`."
  (set variable value)
  (when (and custom-file (file-writable-p custom-file))
    (with-temp-buffer
      (if (file-exists-p custom-file)
          (insert-file-contents custom-file)
        (insert ";; Custom file\n"))

      (goto-char (point-min))
      (if (re-search-forward (format "^[ \t]*;*[ \t]*(setq %s .*)" variable) nil t)
          (replace-match (format "(setq %s '%s)" variable value) nil nil)
        (goto-char (point-max))
        (insert (format "\n(setq %s '%s)\n" variable value)))

      (write-region (point-min) (point-max) custom-file)
      (message "Saved theme: %s" value))))

;; Intercept consult-theme selections
(advice-add #'consult-theme :after
            (lambda (theme &rest _)
              "Save the theme chosen via consult-theme."
              (when theme
                (my-save-theme-to-custom-file 'my-theme theme))))

;; Load custom file and seed theme choice
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Initialization hook to safely load the theme *after* all packages are ready
(add-hook 'after-init-hook
          (lambda ()
            (unless (bound-and-true-p my-theme)
              (setq my-theme 'modus-vivendi-tinted))
            (load-theme my-theme :no-confirm)))

(provide 'init-ui)
;;; init-ui.el ends here
