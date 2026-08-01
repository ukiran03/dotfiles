;;; init-treesit.el --- Tree-sitter configuration via treesit-auto.  -*- lexical-binding: t -*-

;;; Commentary:
;; Fully automated Tree-sitter management using `treesit-auto`.
;; Handles grammar installations, mode remappings, and Combobulate integration.

;;; Code:

;; =============================================================================
;; 1. Core Tree-sitter & Automation Setup
;; =============================================================================

(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)))

(use-package treesit-auto
  :straight t
  :custom
  ;; 'prompt will ask before cloning; set to t if you want silent automatic installs
  (treesit-auto-install 'prompt)
  :config
  ;; Automatically adds all missing grammars to `treesit-language-source-alist`
  (treesit-auto-add-to-alist)
  ;; Wire up all `major-mode-remap-alist` entries automatically
  (global-treesit-auto-mode))


;; =============================================================================
;; 2. Structural Editing with Combobulate
;; =============================================================================

(use-package combobulate
  :disabled
  :straight nil
  ;; Installs Combobulate directly via Emacs 29+ built-in VC package manager
  :vc (:url "https://github.com/mickeynp/combobulate.git" :branch "master")
  :custom
  (combobulate-key-prefix "H-m")
  ;; Enable combobulate-mode for all Tree-sitter powered programming modes
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (go-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)))

(provide 'init-treesit)
;;; init-treesit.el ends here
