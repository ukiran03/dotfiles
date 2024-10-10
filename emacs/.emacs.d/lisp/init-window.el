;; init-window.el --- Window & Buffer configurations.	-*- lexical-binding: t -*-

;; Commentry
;; Window & Buffer management


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
(use-package zoom-window
  :ensure t
  :bind
  ("H-w z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color (face-attribute 'mode-line-highlight :background)))

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

(defun uk-toggle-window-split ()
  "Toggle the states of two windows(only)"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))
    (user-error "`toggle-window-split' only supports two windows")))

;;; ‘Prot’
;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config
;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Header line context of symbol/heading (breadcrumb.el)
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
  ;; <https://depp.brause.cc/shackle/>
  :ensure t
  )

(use-package popper
  :ensure t
  :config
  ;; (setq popper-group-function #'popper-group-by-directory)
  (setq popper-group-function nil)

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
  (setq popper-group-function (if (project-current) #'popper-group-by-project nil))
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


;; CHECKOUT: `switchy-window.el' <https://elpa.gnu.org/packages/switchy-window.html>
;; CHECKOUT: `ace-window'

(provide 'init-window)
