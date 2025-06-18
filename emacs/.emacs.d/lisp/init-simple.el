;;; init-simple.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package simple
  :ensure nil
  :bind (("C-S-w" . uk-simple-copy-line)
         ("C-w" . uk-kill-region)
         ("C-S-<backspace>" . uk-kill-whole-line)
         ("H-q" . read-only-mode)
         ("H-Q" . view-mode))
  :config
  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))

  (defun uk-kill-whole-line (&optional arg)
    "If the region is active, delete all whole lines under the region.
With prefix ARG, act as normal `kill-whole-line'
If the region is not active, `kill-whole-line' line at the point."
    (interactive "p")
    (if (region-active-p)
        (let ((start (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
              (end (save-excursion
                     (goto-char (region-end))
                     (line-end-position))))
          (kill-region start end))
      (kill-whole-line arg)))

  (defun uk-kill-region ()
    (interactive)
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))))

(use-package simple
  :ensure nil
  :config
  (defun uk-toggle-show-trailing-whitespace ()
    "toggles show-trailing-whitespace"
    (interactive)
    (if show-trailing-whitespace (setq show-trailing-whitespace nil)
      (setq show-trailing-whitespace t))))

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

(defcustom uk-comment-keywords
  '("TODO" "NOTE" "REVIEW" "FIXME")
  "List of strings with keywords used by `uk-comment+timestamp'."
  :type '(repeat string)
  :group 'uk-simple)

(defun uk-comment+timestamp (&optional arg)
  "Insert a comment with a keyword and optional timestamp.
With prefix ARG (\\[universal-argument]), include both date and time.
Without ARG, include only the date."
  (interactive "P")
  (let* ((keyword (completing-read "Keyword: " uk-comment-keywords nil t))
         (date-format "%d-%m-%Y")
         (time-format "%H:%M")
         (format (if arg
                     (concat date-format " " time-format)
                   date-format)))
    (comment-dwim nil)
    (insert "[" (format-time-string format) "]")
    (insert " " keyword ": ")))


(provide 'init-simple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-simple.el ends here
