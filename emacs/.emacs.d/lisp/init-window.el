;; init-window.el --- Window & Buffer configurations.	-*- lexical-binding: t -*-

;; Commentry

;; `Resources': https://www.reddit.com/r/emacs/comments/179t67l/window_management_share_your_displaybufferalist/
;; Window & Buffer management

(use-package emacs
  :ensure nil
  :init
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t))))


(use-package transpose-frame)

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
				                      "*Compile-Log*"
				                      "*inferior-lisp*"
				                      "*Fuzzy Completions*"
				                      "*Apropos*"
				                      "*Help*"
				                      "*cvs*"
				                      "*Buffer List*"
				                      "*Ibuffer*"
				                      "*esh command on file*")))

;; (defun split-and-follow-horizontally ()
;;   (interactive)
;;   (split-window-below)
;;   (balance-windows)
;;   (other-window 1))
;; (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

;; (defun split-and-follow-vertically ()
;;   (interactive)
;;   (split-window-right)
;;   (balance-windows)
;;   (other-window 1))
;; (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package window
  :ensure nil
  :bind
  (("H-o" . other-window)
   ("H-0" . delete-window)
   ("H-1" . delete-other-windows)
   ("H-2" . split-window-below)
   ("H-3" . split-window-right)
   ("H-," . previous-buffer)
   ("H-." . next-buffer)))

(use-package zoom-window
  :ensure t
  :bind ("H-w z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color (face-attribute 'pulsar-blue :background)))

;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left))
  :config
  (setq windmove-create-window nil)) ; Emacs 27.1

;;; ‘Prot’
;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config
;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;; <https://github.com/joaotavora/breadcrumb>
(use-package breadcrumb
  ;; :disabled
  :ensure t
  :functions (breadcrumb-local-mode)
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

(use-package shackle
  :disabled
  ;; <https://depp.brause.cc/shackle/>
  ;; https://github.com/liuyinz/emacs.d/blob/fab845516917d4a076a382e01cbc2a1d91f18e4c/core/init-window.el
  :ensure t
  :hook (after-init . shackle-mode))

;; (defun my-popper-group-function ()
;;   "Return the appropriate popper group function based on the current project."
;;   (if (project-current)
;;       #'popper-group-by-project
;;     #'popper-group-by-directory))

(use-package popper
  :ensure t
  :config
  (setq popper-group-function 'popper-group-by-project)
  (setq popper-echo-dispatch-actions t)	;Kill popup buffers with k
                                        ;Raise popup buffers with ^
  :bind (("C-`"   . popper-toggle)
	     ("M-`"   . popper-cycle)
	     ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-mode-line
        '(:eval (let ((face 'mode-line-emphasis))  ; Bind the face variable
		          (format " %s "
			              (nerd-icons-octicon "nf-oct-pin" :face face)))))

  (setq popper-reference-buffers
	    '("\\*Messages\\*"
	      "Output\\*$"
	      "\\*Dictionary\\*"
	      "^\\*eldoc.*\\*$"
	      "\\*Async Shell Command\\*"
          racket-repl-mode
	      help-mode
	      compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)  ; For echo area hints
  :config
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and ;(called-interactively-p 'interactive)
		     (not (region-active-p))
		     popper-open-popup-alist)
	    (let ((window (caar popper-open-popup-alist)))
	      (when (window-live-p window)
	        (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))



;; CHECKOUT: `ace-window'
(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t
			              (:inherit font-lock-keyword-face
				                    :foreground unspecified
				                    :bold t
				                    :height 3.0))))
  (aw-minibuffer-leading-char-face ((t
				                     (:inherit font-lock-keyword-face
					                           :bold t
					                           :height 1.5))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis
				                   :bold t))))
  :hook (after-init . ace-window-display-mode)
  :bind
  ;; ([remap other-window] . ace-window)
  ("H-o" . ace-window)
  :config
  (setq aw-scope 'frame
	    aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) ;
	    ;; aw-keys '(?q ?w ?e ?t ?h ?j ?k ?l ?p)
	    aw-background nil
	    aw-minibuffer-flag t
	    aw-display-mode-overlay t
	    aw-ignore-current nil)
  (define-advice aw-select (:around (fun &rest r) cursor-stuff) ; hide cursor
    (let ((cursor-in-non-selected-windows nil))
      (apply fun r)))
  (setq aw-dispatch-alist
	    '((?x aw-delete-window "Delete Window")
	      (?s aw-swap-window "Swap Window")
	      (?b aw-switch-buffer-in-window "Switch Buffer")
	      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
	      (?m aw-move-window "Move Window")
	      ;; (?n tab-window-detach "Detach to new-tab")
	      ;; (?f tear-off-window "Tear window to new frame")
	      (?! delete-other-windows "Delete other Windows")
	      (?@ aw-split-window-vert "Split Window Below")
	      (?# aw-split-window-horz "Split Window Right")
	      (?? aw-show-dispatch-help)
	      (?q keyboard-quit))))
;; `link:' <https://github.com/JasZhe/elisp-hacks/blob/main/jaszhe-hacks.org>
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
       (cons (or
	          (display-buffer-same-window buffer alist)
	          (display-buffer-use-some-window buffer alist))
	         'reuse))
     nil "[same-window]")
    (message "Display next command buffer in the chosen window...")))

;; CHECKOUT: `switchy-window.el' <https://elpa.gnu.org/packages/switchy-window.html>

(provide 'init-window)
