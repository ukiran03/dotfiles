;;; init-simple.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package simple
  :ensure nil
  :bind (("C-S-w" . uk-simple-copy-line)
         ("C-S-<backspace>" . uk-kill-whole-line)
         ("H-q" . read-only-mode))
  :config
  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))

  (defun uk-kill-whole-line ()
    "If the region is active, delete all whole lines under the region.
If the region is not active, `kill-whole-line' line at the point."
    (interactive)
    (if (region-active-p)
        (let ((start (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
              (end (save-excursion
                     (goto-char (region-end))
                     (line-end-position))))
          (kill-region start end))
      (kill-whole-line))))

(use-package simple
  :ensure nil
  :config
  (defun my-show-trailing-whitespace ()
    "Enable trailing whitespace display when visiting a file."
    (when buffer-file-name
      (setq show-trailing-whitespace t)))
  :hook (find-file . my-show-trailing-whitespace))

(use-package display-line-numbers
  :ensure nil
  :commands (uk-toggle-line-numbers)
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'relative)
  :config
  (defun uk-toggle-line-numbers ()
    "Toggle line numbers.
Cycles through regular, relative and no line numbers. The order
depends on what `display-line-numbers-type' is set to. If you're
using Emacs 26+, and visual-line-mode is on, this skips relative
and uses visual instead.  See `display-line-numbers' for what
these values mean."
    (interactive)
    (defvar uk--line-number-style display-line-numbers-type)
    (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
           (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
           (queue (memq uk--line-number-style order))
           (next (if (= (length queue) 1)
                     (car order)
                   (car (cdr queue)))))
      (setq uk--line-number-style next)
      (setq display-line-numbers next)
      (message "Switched to %s line numbers"
               (pcase next
                 (`t "normal")
                 (`nil "disabled")
                 (_ (symbol-name next)))))))

(provide 'init-simple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-simple.el ends here
