;;; init-window.el --- Window & Buffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Window, workspace, and buffer routing management rules.

;;; Code:

;;;; Built-in Window Customizations
(use-package emacs
  :straight (:type built-in)
  :custom
  ;; Silence annoying logs entirely, preventing them from stealing focus
  (display-buffer-alist
   '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
     (display-buffer-no-window)
     (allow-no-window . t))))

(use-package window
  :straight (:type built-in)
  :bind
  (("H-0" . delete-window)
   ("H-1" . delete-other-windows)
   ("H-2" . split-window-below)
   ("H-3" . split-window-right)
   ("H-," . previous-buffer)
   ("H-." . next-buffer)))

;;;; Restoring Window States (Winner Mode)
(use-package winner
  :straight (:type built-in)
  :commands (winner-undo winner-redo)
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :hook
  (after-init . winner-mode))

;;;; Positional/Structural Operations
(use-package transpose-frame
  :straight t
  :defer t)

(use-package windmove
  :straight (:type built-in)
  :custom
  (windmove-create-window nil)
  :bind
  (("C-M-<up>"    . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>"  . windmove-down)
   ("C-M-<left>"  . windmove-left)
   ("C-M-S-<up>"    . windmove-swap-states-up)
   ;; Note: Overlaps Org Mode structure edits
   ("C-M-S-<right>" . windmove-swap-states-right)
   ("C-M-S-<down>"  . windmove-swap-states-down)
   ("C-M-S-<left>"  . windmove-swap-states-left)))

;;;; Buffer Aesthetics & Navigation
(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

(use-package breadcrumb
  :straight t
  :preface
  (defun prot/breadcrumb-local-mode ()
    "Enable `breadcrumb-local-mode' only if the buffer is visiting a file."
    (when buffer-file-name
      (breadcrumb-local-mode 1)))
  :custom
  (breadcrumb-project-max-length 0.5)
  (breadcrumb-project-crumb-separator "/")
  (breadcrumb-imenu-max-length 1.0)
  (breadcrumb-imenu-crumb-separator " > ")
  :hook
  ((text-mode prog-mode) . prot/breadcrumb-local-mode))

;;;; Window Maximization/Zooming
(use-package zoom-window
  :straight t
  :bind
  ("H-w z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color
   (if (facep 'pulsar-blue)
       (face-attribute 'pulsar-blue :background)
     "DarkBlue")))

;;;; Popup Management (Popper)
(use-package popper
  :straight t
  :preface
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's
content."
    (fit-window-to-buffer win
                          (floor (frame-height) 3)
                          (floor (frame-height) 3)))

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g' if no region is active."
    (when (and (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Dictionary\\*"
     "^\\*eldoc.*\\*$"
     "\\*apheleia-.*-log\\*"
     "\\*Async Shell Command\\*"
     "\\*compilation\\*"
     go-test-mode
     racket-repl-mode
     help-mode
     compilation-mode))
  (popper-echo-dispatch-actions t)
  (popper-mode-line
   '(:eval
     (propertize " POP" 'face
                 '(:inherit mode-line-emphasis :slant normal :weight bold))))
  (popper-window-height #'my-popper-fit-window-height)

  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))

  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

;;;; Structural Focus & Selection (Ace Window)
(use-package ace-window
  :straight t
  :bind
  ("H-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-background t)
  (aw-minibuffer-flag t)
  (aw-display-mode-overlay t)
  (aw-ignore-current nil)
  (aw-dispatch-alist
   '((?x aw-delete-window "Delete Window")
     (?s aw-swap-window "Swap Window")
     (?b aw-switch-buffer-in-window "Switch Buffer")
     (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?m aw-move-window "Move Window")
     (?! delete-other-windows "Delete other Windows")
     (?@ aw-split-window-vert "Split Window Below")
     (?# aw-split-window-horz "Split Window Right")
     (?? aw-show-dispatch-help)
     (?q keyboard-quit)))
  :custom-face
  (aw-leading-char-face
   ((t (:inherit font-lock-keyword-face
                 :foreground unspecified
                 :bold t
                 :height 2.0))))
  (aw-minibuffer-leading-char-face
   ((t (:inherit font-lock-keyword-face
                 :bold t
                 :height 1.5))))
  (aw-mode-line-face
   ((t (:inherit mode-line-emphasis
                 :bold t))))
  :config
  (define-advice aw-select (:around (fun &rest r) cursor-stuff)
    "Hide the cursor in non-selected windows during ace selection."
    (let ((cursor-in-non-selected-windows nil))
      (apply fun r))))

;;;; Interactive Target Buffers via Ace Selection
(defvar ace-choose-window-prefix-window nil)

(defun ace-choose-window-prefix ()
  "Interactively pick which window to show the next command's buffer in."
  (interactive)
  (setq ace-choose-window-prefix-window nil)
  (let ((aw-dispatch-always t))
    (aw-select "Choose window"
               (lambda (window)
                 (setq ace-choose-window-prefix-window window))))
  (when ace-choose-window-prefix-window
    (display-buffer-override-next-command
     (lambda (buffer alist)
       (select-window ace-choose-window-prefix-window)
       (setq alist (append '((inhibit-same-window . nil)) alist))
       (cons (or (display-buffer-same-window buffer alist)
                 (display-buffer-use-some-window buffer alist))
             'reuse))
     nil "[same-window]")
    (message "Display next command buffer in the chosen window...")))

(provide 'init-window)
;;; init-window.el ends here
