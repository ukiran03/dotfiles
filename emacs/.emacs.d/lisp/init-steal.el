;;; init-steal.el --- Performance Tweaks -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs Performance Tweaks.
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;;; Code:

;; Performance & Core Tweaks
(use-package emacs
  :straight (:type built-in)
  :preface
  (defun toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and (bound-and-true-p winner-mode)
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))

  :custom
  ;; Disable Bidirectional Text Scanning
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)

  ;; Skip Fontification During Input
  (redisplay-skip-fontification-on-input t)

  ;; Increase Process Output Buffer for LSP
  (read-process-output-max (* 4 1024 1024)) ; 4MB

  ;; Don’t Render Cursors in Non-Focused Windows
  (cursor-in-non-selected-windows t)
  (highlight-nonselected-windows nil)

  ;; Save the Clipboard Before Killing
  (save-interprogram-paste-before-kill t)

  ;; No Duplicates in the Kill Ring
  (kill-do-not-save-duplicates t)

  ;; Sane Syntax in re-builder
  (reb-re-syntax 'string)

  ;; Prevent ffap from Pinging Hostnames
  (ffap-machine-p-known 'reject)

  ;; Proportional Window Resizing
  (window-combination-resize t)

  :config
  ;; Recenter After save-place Restores Position
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter))))))

;; Persist the Kill Ring Across Sessions
(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  :config
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))))))

;;; Editing

;; Auto-Chmod Scripts on Save
(use-package sh-mode
  :straight (:type built-in)
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(provide 'init-steal)
;;; init-steal.el ends here
