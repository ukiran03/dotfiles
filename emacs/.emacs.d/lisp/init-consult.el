;;; init-consult.el --- Consult Enhancements -*- lexical-binding: t -*-

(use-package consult
  :ensure t
  ;; Replace standard bindings lazily
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c T" . consult-theme)

         ([remap Info-search] . consult-info)
         ("C-x C-r" . consult-recent-file)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ([remap point-to-register] . consult-register-store)
         ([remap jump-to-register] . consult-register)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop)

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . my/consult-imenu-multi-right-aligned)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)

         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :preface
  (declare-function consult-imenu--select "consult")
  (declare-function consult-imenu--items-safe "consult")

  (defun my/consult-imenu-multi-right-aligned (&optional query)
    "Consult-imenu-multi with right-aligned buffer names."
    (interactive "P")
    (require 'cl-lib)
    (let* ((query (if (keywordp (car-safe query)) query
                    `(:sort alpha
                            :mode ,major-mode
                            :directory ,(and (not query) 'project))))
           (prompt-and-buffers (consult--buffer-query-prompt "Go to item" query))
           (buffers (cdr prompt-and-buffers))
           (align 60)) ; Expanded padding for wider screens / longer identifiers

      (consult-imenu--select
       (car prompt-and-buffers)
       (consult--slow-operation "Collecting items..."
         (cl-mapcan (lambda (buf)
                      (with-current-buffer buf
                        (mapcar
                         (lambda (item)
                           (cons (concat (car item)
                                         (propertize " " 'display
                                                     `(space :align-to ,align))
                                         (propertize
                                          (buffer-name) 'face 'completions-annotations))
                                 (cdr item)))
                         (consult-imenu--items-safe))))
                    buffers)))))

  :init
  ;; Register Formatting Enhancements
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Global Xref Hooks
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Preview rules Engine
  (setq consult-preview-key nil) ; Global fallback configuration

  (consult-customize
   consult-line consult-line-multi consult-imenu
   :preview-key 'any

   consult-theme
   :preview-key '(:debounce 0.5 any)

   consult-ripgrep consult-git-grep consult-grep
   consult-goto-line consult-flymake consult-outline
   consult-buffer consult-project-buffer consult-buffer-other-tab
   consult-buffer-other-frame consult-buffer-other-window
   consult-bookmark consult-recent-file
   :preview-key '("C-."))

  ;; Dynamic Live-Preview Toggle Engine
  (defvar-local consult-toggle-preview-orig nil)
  (defun consult-toggle-preview ()
    "Command to quickly toggle preview behaviors on the fly."
    (interactive)
    (if consult-toggle-preview-orig
        (progn
          (setq consult--preview-function consult-toggle-preview-orig
                consult-toggle-preview-orig nil)
          (message "Preview ON"))
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)
      (message "Preview OFF")))

  ;; Safe injection into the Vertico environment map
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "M-P") #'consult-toggle-preview))

  ;; Global Context Helpers
  (setq consult-narrow-key "<")
  (add-to-list 'consult-mode-histories '(eat-mode)))

(provide 'init-consult)
