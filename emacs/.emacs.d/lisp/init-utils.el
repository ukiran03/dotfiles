;;; init-utils.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package blackout
  :config
  (dolist (mode '((lisp-interaction-mode . "λ")
                  (python-mode . "Py")
                  (emacs-lisp-mode . "Eλ")
                  (sh-mode . "SH")
                  (js-jsx-mode . "Js +JSX")
                  (js-mode . "Js")
                  (racket-mode . "RKT")
                  (scheme-mode . "λSCM")))
    (blackout (car mode) (cdr mode))))

(use-package orgmdb
  :ensure t
  :config
  (setq orgmdb-omdb-apikey "fd3c38ff")
  (setq orgmdb-poster-folder "~/Notes/Films/Posters")
  (setq orgmdb-fill-property-list
        '(poster country genre runtime director imdb-id imdb-rating)))

;; #[/home/ukiran/.emacs.d/site-lisp/clue/clue.el:L60]
(use-package clue
  :disabled
  :ensure nil
  :defer t
  :vc (:url "https://github.com/AmaiKinono/clue")
  :blackout (clue-mode . " #")
  ;;  :init
  ;;  (add-hook 'find-file-hook #'clue-auto-enable-clue-mode)
  :config
  (setq
   ;; Set like this if you only want auto-enabling citre-mode to work
   ;; for markdown files.  You can also set it to nil, then the
   ;; auto-enabling works for all files.  By default, it works for all
   ;; text-modes.
   clue-auto-enable-modes '(prog-mode)))

;;https://github.com/hlissner/dotfiles/blob/master/config/bspwm/bspwmrc#L31
;;bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
(use-package emacs-everywhere
  :init
  (setq emacs-everywhere-frame-name-format "EEverywhere")
  :config
  (setq emacs-everywhere-major-mode-function 'markdown-mode)
  :bind (:map emacs-everywhere-mode-map
              ("C-c C-c" . emacs-everywhere-finish)))

;; (use-package tinee
;;   :vc (:url "https://codeberg.org/tusharhero/tinee.git")
;;   ;; uncomment to make tinee automatically copy and insert text area contents.
;;   ;; :hook (tinee-before-make-frame-hook . tinee-x11-copy)
;;   ;; :hook (tinee-after-make-frame-hook . (lambda () (insert (or (gui-selection-value) ""))))

;;   ;; default values, change them if you would like to.
;;   :custom
;;   ((tinee-send-text-function 'tinee-x11-write) ; or 'tinee-paste or 'tinee-paste-c-v
;;    (tinee-frame-name "tinee"))
;;   :config
;;   (defun tinee-x11-write (string)
;;     "Simulate key press using xdotool to paste STRING."
;;     (start-process "tinee-x11-write" nil
;;                    "xdotool" "key" "--clearmodifiers" "Shift+Insert" string))
;;   (defun tinee-x11-write (string)
;;     "Simulate key press using xdotool to paste STRING."
;;     (start-process "tinee-write" nil
;;                    "xdotool" "type" "--clearmodifiers" string))
;;   (defun tinee-x11-copy ()
;;     "Copy STRING from X11 window text area."
;;     (start-process "tinee-x11-copy" nil
;;                    "xclip" "-selection" "clipboard" "%f")
;;     (sleep-for 1)))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys))

;; (use-package kiwix
;;   :ensure t
;;   :after org
;;   :commands (kiwix-launch-server kiwix-at-point)
;;   ;; :bind (:map document-prefix ("w" . kiwix-at-point))
;;   :custom ((kiwix-server-type 'kiwix-serve-local)
;;            (kiwix-server-url "http://192.168.1.2:8000")
;;            (kiwix-server-port 8000)
;;            (kiwix-zim-dir (expand-file-name "/home/ukiran/zims")
;;                           ))
;;   :hook (org-load . org-kiwix-setup-link)
;;   :init (require 'org-kiwix)
;;   :config (add-hook 'org-load-hook #'org-kiwix-setup-link))

(use-package kiwix
  :ensure nil
  :commands (kiwix-launch-server kiwix-at-point)
  :config
  (setq kiwix-server-type 'kiwix-server-local)
  (setq kiwix-server-url "http://192.168.1.2:8000")
  (setq kiwix-server-port 8000)
  (setq kiwix-zim-dir (expand-file-name "/home/ukiran/zims")))

;; Smooth scrolling
;; (use-package ultra-scroll
;;   :ensure nil
;;   :init (unless (package-installed-p 'ultra-scroll)
;;           (package-vc-install "https://github.com/jdtsmith/ultra-scroll"))
;;   :hook (after-init . ultra-scroll-mode))

(use-package sinister
  :disabled
  :vc (:url "https://github.com/positron-solutions/sinister")
  :config
  (sinister-stillness-mode 1)
  (sinister-misc-settings)
  (setq sinister-stillness-margin 10))

(use-package keycast)

(use-package zoxide
  :config
  (defun zoxide-dired (&optional other-window)
    (interactive "P")
    (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t)))

(use-package ztree)

(use-package cursory
  ;; :disabled
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :cursor-color success ; will typically be green
           :blink-cursor-interval 1.2)
          (box-no-blink
           :inherit box
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :cursor-color error ; will typically be red
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :inherit bar
           :blink-cursor-mode -1)
          (underscore
           :cursor-color warning ; will typically be yellow
           :cursor-type (hbar . 3)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50)
          (underscore-no-other-window
           :inherit underscore
           :cursor-in-non-selected-windows nil)
          (underscore-thick
           :inherit underscore
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-color unspecified ; use the theme's original
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))
  ;; I am using the default value of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from
  ;; `cursory-presets'.  Alternatively, use the function
  ;; `cursory-set-last-or-fallback' (can be added to the
  ;; `after-init-hook'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  ;; Persist configurations between Emacs sessions.  Also apply the
  ;; :cursor-color again when swithcing to another theme.
  (cursory-mode 1)
  :bind
  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  ("C-c p c" . cursory-set-preset))

;; (use-package leetcode
;;   :load-path ("site-lisp/leetcode")
;;   :config
;;   (setq leetcode-language "go")
;;   :commands (leetcode))  ; Add autoload for the main function if desired


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
