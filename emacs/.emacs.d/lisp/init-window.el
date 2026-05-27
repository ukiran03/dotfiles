;;; init-window.el --- Window & Buffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Window, workspace, and buffer routing management rules.

;;; Code:

(require 'use-package)

;;;; Built-in Window Customizations
(use-package emacs
  :ensure nil
  :init
  ;; Silence annoying logs entirely, preventing them from stealing focus
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t))))

(use-package window
  :ensure nil
  :bind
  (("H-0" . delete-window)
   ("H-1" . toggle-delete-other-windows) ; Handled via your custom/built-in toggle
   ("H-2" . split-window-below)
   ("H-3" . split-window-right)
   ("H-," . previous-buffer)
   ("H-." . next-buffer)))

;;;; Restoring Window States (Winner Mode)
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

;;;; Positional/Structural Operations
(use-package transpose-frame
  :defer t)

(use-package windmove
  :ensure nil
  :bind
  (("C-M-<up>"    . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>"  . windmove-down)
   ("C-M-<left>"  . windmove-left)
   ("C-M-S-<up>"    . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; Note: Overlaps Org Mode structure edits
   ("C-M-S-<down>"  . windmove-swap-states-down)
   ("C-M-S-<left>"  . windmove-swap-states-left))
  :init
  (setq windmove-create-window nil))

;;;; Buffer Aesthetics & Navigation
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

(use-package breadcrumb
  :ensure t
  :hook ((text-mode prog-mode) . prot/breadcrumb-local-mode)
  :config
  (setq breadcrumb-project-max-length 0.5
        breadcrumb-project-crumb-separator "/"
        breadcrumb-imenu-max-length 1.0
        breadcrumb-imenu-crumb-separator " > ")

  (defun prot/breadcrumb-local-mode ()
    "Enable `breadcrumb-local-mode' only if the buffer is visiting a file."
    (when buffer-file-name
      (breadcrumb-local-mode 1))))

;;;; Window Maximization/Zooming
(use-package zoom-window
  :ensure t
  :bind ("H-w z" . zoom-window-zoom)
  :config
  ;; Safely falling back if pulsar isn't loaded yet
  (setq zoom-window-mode-line-color
        (if (facep 'pulsar-blue)
            (face-attribute 'pulsar-blue :background)
          "DarkBlue")))

;;;; Popup Management (Popper)
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
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
  (setq popper-group-function 'popper-group-by-project)
  (setq popper-echo-dispatch-actions t)

  (popper-mode 1)
  (popper-echo-mode 1)
  :config
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer win (floor (frame-height) 3) (floor (frame-height) 3)))

  (setq popper-window-height #'my-popper-fit-window-height)

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g' if no region is active."
    (when (and (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

;;;; Structural Focus & Selection (Ace Window)
(use-package ace-window
  :ensure t
  :bind ("H-o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 2.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.5))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :config
  (setq aw-scope 'frame
        aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
        aw-background t
        aw-minibuffer-flag t
        aw-display-mode-overlay t
        aw-ignore-current nil)

  (define-advice aw-select (:around (fun &rest r) cursor-stuff)
    "Hide the cursor in non-selected windows during ace selection."
    (let ((cursor-in-non-selected-windows nil))
      (apply fun r)))

  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?s aw-swap-window "Swap Window")
          (?b aw-switch-buffer-in-window "Switch Buffer")
          (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?m aw-move-window "Move Window")
          (?! delete-other-windows "Delete other Windows")
          (?@ aw-split-window-vert "Split Window Below")
          (?# aw-split-window-horz "Split Window Right")
          (?? aw-show-dispatch-help)
          (?q keyboard-quit))))

;;;; Interactive Target Buffers via Ace Selection
(defvar ace-choose-window-prefix-window nil)

(defun ace-choose-window-prefix ()
  "Interactively pick which window to show the next command's buffer in."
  (interactive)
  (setq ace-choose-window-prefix-window nil)
  (let ((aw-dispatch-always t))
    (aw-select "Choose window"
               (lambda (window) (setq ace-choose-window-prefix-window window))))
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
