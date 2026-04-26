;;;; Emacs Performance Tweaks
;;;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;;; Performance Tweaks

;; Disable Bidirectional Text Scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Skip Fontification During Input
(setq redisplay-skip-fontification-on-input t)

;; Increase Process Output Buffer for LSP
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; Don’t Render Cursors in Non-Focused Windows
(setq-default cursor-in-non-selected-windows t) ; nil to disable
(setq highlight-nonselected-windows t)          ; nil to disable

;;; Kill Ring (Emacs’s Clipboard History) and Clipboard

;; Save the Clipboard Before Killing
(setq save-interprogram-paste-before-kill t)

;; No Duplicates in the Kill Ring
(setq kill-do-not-save-duplicates t)

;; Persist the Kill Ring Across Sessions
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

;;; Editing

;; Auto-Chmod Scripts on Save
(use-package sh-mode
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Sane Syntax in re-builder
(setq reb-re-syntax 'string)

;; Prevent ffap from Pinging Hostnames
(setq ffap-machine-p-known 'reject)

;;; Windows

;; Proportional Window Resizing
(setq window-combination-resize t)

;; Reversible `C-x 1'
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))
;; (global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

;;; Misc

;; Faster Mark Popping: After the first C-u C-SPC you can keep pressing just
;; C-SPC to continue popping, uses `repeat-mode'
;; (setq set-mark-command-repeat-pop t) ; Already using it

;; Recenter After save-place Restores Position
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

(provide 'init-steal)
;;; init-perf.el ends here
