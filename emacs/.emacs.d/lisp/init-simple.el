;;; init-simple.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :straight nil
  :init
  ;; (size-indication-mode 1)
  (column-number-mode 1)
  :config
  (setq-default fill-column 79))

(use-package simple
  :straight nil
  :bind (("C-S-w" . uk-simple-copy-line)
         ("C-w" . uk-kill-region)
         ("C-z" . nil)
         ("<f2>" . nil)
         ("C-<return>" . dwim-new-line)
         ("S-SPC" . uk-insert-space-after-cursor)
         ("C-S-<backspace>" . uk-kill-whole-line)
         ("C-y" . uk-smart-yank)
         ("H-q" . read-only-mode)
         ("H-Q" . view-mode))
  :config
  (defun uk-insert-space-after-cursor ()
    "Inserts a space after the cursor without moving the point."
    (interactive)
    (save-excursion
      (insert " ")))

  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))

  (defun uk-smart-yank (arg)
    "Wrapper for `yank`.
With no prefix argument, it behaves like a normal `yank`.  With a
standard prefix argument `C-u` (ARG is '(4)), it replaces the current
line."
    (interactive "P")
    (if (equal arg '(4))
        ;; If C-u was pressed, replace the current line
        (let ((beg (line-beginning-position))
              (end (line-beginning-position 2))) ; Captures the newline character too
          (delete-region beg end)
          (yank)
          ;; Ensure the yanked text stays neatly on its own line
          (unless (string-suffix-p "\n" (current-kill 0))
            (insert "\n"))
          )
      ;; Otherwise, perform a standard yank
      (yank arg)))

  ;; This restores the built-in active-mark/delete-selection features
  (put 'uk-smart-yank 'delete-selection 'yank)

  (defun dwim-new-line (&optional arg)
    "Insert newline(s) at end of line.
With ARG > 0, insert ARG newlines after current line.
With ARG < 0, insert |ARG| newlines after previous line.
If ARG is nil, insert one newline after current line."
    (interactive "P")
    ;; Default to 1 if no arg given
    (let ((n (prefix-numeric-value arg)))
      (when (< n 0)
        (forward-line -1)
        ;; (beginning-of-line)
        ;; (open-line 0)
        (setq n (- n)))
      (end-of-line)
      (newline n t)))

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
  :straight nil
  :diminish visual-line-mode
  :hook ((text-mode . visual-line-mode)
         (prog-mode . enable-trailing-whitespace)
         (markdown-mode . enable-trailing-whitespace)
         (conf-mode . enable-trailing-whitespace))
  :init
  (setq kill-whole-line t ; Kill line including '\n'
        track-eol t ; Keep cursor at end of lines. Requires line-move-visual nil.
        line-move-visual nil)
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  :config
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  (defun uk-toggle-show-trailing-whitespace ()
    "Toggle show-trailing-whitespace in the current buffer."
    (interactive)
    (setq show-trailing-whitespace (not show-trailing-whitespace))
    (message "show-trailing-whitespace set to: %s" show-trailing-whitespace)))

(use-package display-line-numbers
  :straight nil
  :commands (uk-toggle-line-numbers)
  :hook ((prog-mode yaml-mode conf-mode text-mode) . display-line-numbers-mode)
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
           (order (cons display-line-numbers-type
                        (remq display-line-numbers-type styles)))
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
  '("TODO" "DOUBT" "NOTE" "REVIEW" "FIXME" "REDO")
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

;; https://karthinks.com/software/even-more-batteries-included-with-emacs/
(define-advice copy-from-above-command (:around (func &optional arg) comment)
  (if (equal current-prefix-arg '(4))
      (progn
        (funcall func)
        (save-excursion
          (forward-line 0)
          (let ((ln (line-number-at-pos (point))))
            (backward-char)
            (skip-chars-backward "\n\t ")
            (unless (= (line-number-at-pos) ln)
              (comment-line 1)))))
    (funcall func arg)))

;;; TODO: write these functions
;; `isearch-kill-region': set mark, find DEST with isearch, kill the region
;;
;; `consult-copy-line': similar to `consult-line', but no preview, on
;; hit-enter copy line at point

(provide 'init-simple)
;;; init-simple.el ends here
