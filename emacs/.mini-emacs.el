;;; init-mini.el --- Centaur Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2018-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 1.1.0
;; Keywords: .emacs.d centaur

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Minimal configurations for debugging purpose.
;;

;;; Code:

;; Load path
;; (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
;; (push (expand-file-name "lisp" user-emacs-directory) load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
;; (setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka" :height 110)
;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; UI
;; (load-theme 'wombat t)
(load-theme 'modus-vivendi t)

;; (set-face-attribute 'default nil
;; 		            :family "Iosevka Extended"
;; 		            :height 95
;; 		            :weight 'medium)
(set-frame-font "Iosevka Medium-11.25" nil t)
;; (set-frame-font "Sarasa Mono SC SemiBold-11.25" nil t)

;; (unless (eq window-system 'ns)
;;   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; (when (fboundp 'horizontal-scroll-bar-mode)
;;   (horizontal-scroll-bar-mode -1))

(global-hl-line-mode 1)

;; (if (fboundp 'display-line-numbers-mode)
;;     (global-display-line-numbers-mode 1)
;;   (global-linum-mode 1))

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))
(when (fboundp 'which-key-mode) (which-key-mode 1))
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)))

(setq completion-auto-help 'visible)

;; Keybindings
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(global-set-key (kbd "C-c T") #'modus-themes-toggle)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(use-package simple
  :ensure nil
  :config
  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))
  (global-set-key (kbd "C-S-w") 'uk-simple-copy-line))

(global-set-key (kbd "M-o") 'other-window)

(use-package gdb-mi
  :ensure nil
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))


;; (use-package anzu
;;   :ensure t
;;   :bind (([remap query-replace] . anzu-query-replace)
;;          ([remap query-replace-regexp] . anzu-query-replace-regexp)
;;          :map isearch-mode-map
;;          ([remap isearch-query-replace] . anzu-isearch-query-replace)
;;          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
;;   :hook (after-init . global-anzu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
