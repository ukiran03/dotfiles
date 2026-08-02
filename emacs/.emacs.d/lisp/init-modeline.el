;;; init-modeline.el --- Modeline configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Custom modeline that is stylistically close to the default.

;;; Code:

(defgroup uk-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

;;;; Faces

(defgroup uk-modeline-faces nil
  "Faces for my custom modeline."
  :group 'uk-modeline)

(defface uk-modeline-unimportant-face
  '((t (:inherit shadow :weight normal)))
  "Face used for less important mode line elements."
  :group 'uk-modeline-faces)

(defface uk-modeline-buffer-name
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying the value of `buffer-name'."
  :group 'uk-modeline-faces)

(defface uk-modeline-buffer-status-modified
  '((t (:inherit warning :weight bold)))
  "Face used for the 'buffer-modified' buffer status indicator."
  :group 'uk-modeline-faces)

(defface uk-modeline-buffer-status-read-only
  '((t (:inherit uk-modeline-unimportant-face)))
  "Face used for the 'buffer-read-only' buffer status indicator."
  :group 'uk-modeline-faces)

(defface uk-modeline-buffer-status-saved
  '((t (:inherit uk-modeline-unimportant-face)))
  "Face used for the 'buffer-saved' buffer status indicator."
  :group 'uk-modeline-faces)

(defface uk-modeline-major-mode
  '((t (:inherit bold)))
  "Face used for the major mode indicator."
  :group 'uk-modeline-faces)

(defface uk-modeline-vc-up-to-date
  '((t (:inherit success)))
  "Face used for the VC good status indicator."
  :group 'uk-modeline-faces)

(defface uk-modeline-unimportant-bold-face
  '((t :inherit (bold shadow)))
  "Face for modeline unimportant indicators."
  :group 'uk-modeline-faces)

(defface uk-modeline-bold-blue-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for modeline narrow indicators."
  :group 'uk-modeline-faces)

(defface uk-modeline-kbd-macro-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for modeline Keyboard Macro indicators."
  :group 'uk-modeline-faces)

;;;;;; Modeline Format
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e"
                uk-modeline-kbd-macro
                uk-modeline-narrow
                uk-modeline-remote-file
                uk-modeline-buffer-status
                uk-modeline-window-dedicated
                " "
                uk-modeline-buffer-name
                "  "
                uk-modeline-major-mode
                " "
                mode-line-process
                "  "
                (uk-modeline-vc-info uk-modeline-vc-info)
                "  "
                uk-modeline-misc-info
                "  "
                uk-modeline-flymake

                mode-line-format-right-align
                uk-modeline-text-scale
                "  "
                uk-modeline-cursor-position
                "  "
                uk-modeline-scroll
                " "))


;;;; Keyboard macro indicator
(defvar-local uk-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro" 'face 'uk-modeline-kbd-macro-face)))
  "Mode line construct displaying `mode-line-defining-kbd-macro' Specific
to the current window's mode line.")
(put 'uk-modeline-kbd-macro 'risky-local-variable t)

;;;; Narrow
(defvar-local uk-modeline-narrow
    '(:eval
      (when (and (buffer-narrowed-p)
                 (not (derived-mode-p
                       'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " ⥮" 'face 'uk-modeline-bold-blue-face)))
  "Mode line construct to display a narrowed buffer indicator.")
(put 'uk-modeline-narrow 'risky-local-variable t)

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional buffer))

;; Based on `flymake--mode-line-counter'.
(defun uk-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defmacro uk-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "uk-modeline-flymake-%s" type)) ()
     (when-let* ((count (uk-modeline-flymake-counter
                         ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'uk-modeline-unimportant-bold-face)
        (propertize count
                    'face ',(or face type))))))

(uk-modeline-flymake-type error "☣")
(uk-modeline-flymake-type warning "!")
(uk-modeline-flymake-type note "·" success)

(defvar-local uk-modeline-flymake
    `(:eval
      (when (bound-and-true-p flymake-mode)
        (list
         ;; See the calls to the macro `uk-modeline-flymake-type'
         '(:eval (uk-modeline-flymake-error))
         '(:eval (uk-modeline-flymake-warning))
         '(:eval (uk-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")
(put 'uk-modeline-flymake 'risky-local-variable t)

;;;; Misc info
(defvar-local uk-modeline-misc-info
    '(:eval
      ;; 1. Use the underlying variable, NOT the mode-line construct symbol
      (let ((misc-info (format-mode-line mode-line-misc-info)))
        (unless (string-blank-p misc-info)
          (propertize misc-info
                      'face 'uk-modeline-unimportant-face))))
  "Mode line construct to display cleaned up misc information.")
(put 'uk-modeline-misc-info 'risky-local-variable t)

;;;; Position, line:column

(defvar-local uk-modeline-cursor-position
    '(:eval
      (format-mode-line "%l:%c"))
  "Modeline construct to display the position of the cursor.")
(put 'uk-modeline-cursor-position 'risky-local-variable t)

;;;; Scroll Percentage
(defvar-local uk-modeline-scroll
    '(:eval
      (propertize (format-mode-line "%o%%")
                  'face 'uk-modeline-unimportant-face))
  "Mode line construct to display the buffer scroll percentage.")
(put 'uk-modeline-scroll 'risky-local-variable t)

;;;; Text scale
(defvar-local uk-modeline-text-scale
    '(:eval
      (when (and (boundp 'text-scale-mode-amount)
                 (/= text-scale-mode-amount 0))
        (propertize
         (format
          (if (> text-scale-mode-amount 0) " %+d" " %-d")
          text-scale-mode-amount)
         'face 'uk-modeline-unimportant-bold-face))))
(put 'uk-modeline-text-scale 'risky-local-variable t)

;;;; Buffer related

(defvar-local uk-modeline-buffer-name
    '(:eval
      (propertize (buffer-name) 'face 'uk-modeline-buffer-name))
  "Mode line construct to display the `buffer-name'")
(put 'uk-modeline-buffer-name 'risky-local-variable t)

(defvar-local uk-modeline-buffer-status
    '(:eval
      (cond ((and buffer-read-only (buffer-modified-p))
             (concat (propertize "  " 'face 'uk-modeline-buffer-status-read-only)
                     (propertize "* "  'face 'uk-modeline-buffer-status-modified)))
            (buffer-read-only
             (propertize "  " 'face 'uk-modeline-buffer-status-read-only))
            ((buffer-modified-p)
             (propertize " ** " 'face 'uk-modeline-buffer-status-modified))
            (t (propertize " -- " 'face 'uk-modeline-buffer-status-saved))))
  "Mode line construct for buffer modification state.")
(put 'uk-modeline-buffer-status 'risky-local-variable t)

(defvar-local uk-modeline-remote-file
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @"
                    'face 'error)))
  "Mode line construct for showing remote file name.")
(put 'uk-modeline-remote-file 'risky-local-variable t)

;;;; Dedicated window
(defvar-local uk-modeline-window-dedicated
    '(:eval (when (window-dedicated-p)
              (propertize "= "
                          'face 'uk-modeline-bold-blue-face
                          'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")
(put 'uk-modeline-window-dedicated 'risky-local-variable t)

;;;; Major mode
(defvar-local uk-modeline-major-mode
    '(:eval
      (propertize (concat
                   (uk-modeline-get-major-mode-icon major-mode)
                   (substring-no-properties (format-mode-line mode-name)))
                  'face 'uk-modeline-major-mode))
  "Mode line construct to display a major mode name.")
(put 'uk-modeline-major-mode 'risky-local-variable t)

(defvar uk-modeline-icons
  '((dired-mode . "")
    (wdired-mode . "")
    (archive-mode . "")
    (diff-mode . "")
    (comint-mode . "")
    (magit-mode . "")
    (t . ""))
  "Association list mapping major modes to Nerd Font icons.")

(defun uk-modeline-get-major-mode-icon (mode)
  "Return the icon string for the given MODE, supporting derived modes."
  (let ((icon (catch 'found
                (dolist (cell uk-modeline-icons)
                  (let ((key (car cell)))
                    (when (or (eq key t)
                              (and (symbolp key) (provided-mode-derived-p mode key)))
                      (throw 'found (cdr cell))))))))
    (if icon (concat icon " ") "")))

;;;; VC

(defvar-local uk-modeline-vc-info
    '(:eval
      (when-let* ((file buffer-file-name)
                  (backend (vc-backend file)))
        (let* ((branch (uk-modeline--vc-branch-name file backend))
               (vc-face (uk-modeline--vc-face file backend))
               ;; We safely reference the face here to ensure `nil` or `t`
               ;; cannot be passed.
               (face (if vc-face `(:inherit ,vc-face :weight bold) 'mode-line)))
          (uk-modeline--vc-text file branch face))))
  "Mode line construct to return propertized VC branch.")
(put 'uk-modeline-vc-info 'risky-local-variable t)

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun uk-modeline--vc-branch-name (file backend)
  "Return VC branch name or a short commit hash if detached for FILE with
BACKEND."
  (when-let* ((rev (vc-working-revision file backend)))
    (let ((branch (if (eq backend 'Git)
                      (or (vc-git--symbolic-ref file)
                          ;; Fallback: shorten the SHA if vc-git--symbolic-ref
                          ;; is nil
                          (substring rev 0 (min 7 (length rev))))
                    rev)))
      branch)))

(defun uk-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (format "%s %s"
          "" (propertize branch 'face face)))

(defun uk-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (if-let* ((state (vc-state file backend)))
      (uk-modeline--vc-get-face state)
    'mode-line))

(defun uk-modeline--vc-get-face (key)
  "Get face from KEY in `uk-modeline--vc-faces'."
  (or (alist-get key uk-modeline--vc-faces) 'mode-line))

(defvar uk-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . uk-modeline-vc-up-to-date))
  "VC state faces.")

(provide 'init-modeline)
;;; init-modeline.el ends here
