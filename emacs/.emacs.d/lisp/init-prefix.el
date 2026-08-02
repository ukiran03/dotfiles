;;; init-prefix.el --- Hyper Key Prefix Action maps -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

;;; Hyper Key & Keyboard Chord Translation Setup

;; Bind your Window Manager/OS Super modifier combos to Emacs Hyper
(setq x-hyper-keysym 'hyper)

(defun my/map-hyper-chord (key-string)
  "Translate the 3-modifier chord (Ctrl-Alt-Super) to Hyper for a given KEY-STRING."
  (define-key key-translation-map
              (kbd (format "C-M-s-%s" key-string))
              (kbd (format "H-%s" key-string))))

;; Loop to batch-map alphanumeric inputs
(cl-loop for char across "abcdefghijklmnopqrstuvwxyz0123456789,.';"
         do (my/map-hyper-chord (char-to-string char)))

;;; Hyper-Prefix Keymaps (`defvar-keymap`)

;; Files Prefix (H-f)
(defvar-keymap files-hyper-map
  :doc "Files configuration actions map."
  :name "Files"
  "C-g" #'keyboard-quit
  "f"   #'find-file
  "H-f" #'find-file
  "F"    #'view-file
  "r"    #'consult-recent-file
  "m"    #'rename-file
  "M"    #'rename-visited-file
  "w"    #'write-file
  "l"    #'find-library
  "v"    #'find-alternate-file)
(keymap-set global-map "H-f" files-hyper-map)

;; Dired Prefix (H-d)
(defvar-keymap dired-hyper-map
  :doc "Dired file management navigation map."
  :name "Dired"
  "C-g" #'keyboard-quit
  "c"    #'consult-dir
  "d"    #'dired
  "H-d" #'dired
  "j"    #'dired-jump)
(keymap-set global-map "H-d" dired-hyper-map)

;; Buffers Prefix (H-b)
(defvar-keymap buffer-hyper-map
  :doc "Buffer tracking and active buffer actions."
  :name "Buffer"
  "C-g" #'keyboard-quit
  "b"    #'consult-buffer
  "H-b" #'consult-buffer
  "k"    #'kill-buffer
  "i"    #'insert-buffer
  "g"    #'revert-buffer-quick
  "c"    #'clone-buffer
  "r"    #'rename-buffer
  "B"    #'ibuffer
  "K"    #'kill-current-buffer)
(keymap-set global-map "H-b" buffer-hyper-map)

;; Narrowing Prefix (H-n)
(defvar-keymap narrow-hyper-map
  :doc "Buffer narrowing actions."
  :name "Narrow"
  :parent narrow-map
  "C-g" #'keyboard-quit)
(keymap-set global-map "H-n" narrow-hyper-map)

;; Project Prefix (H-p)
(defvar-keymap project-hyper-map
  :doc "Project-wide version control & context lookup."
  :name "Project"
  :parent project-prefix-map
  "C-g"      #'keyboard-quit
  "."        #'project-dired
  "<return>" #'project-dired
  "F"        #'project-view-file
  "<delete>" #'project-forget-project)
(keymap-set global-map "H-p" project-hyper-map)

;; Registers & Bookmarks Prefix (H-r)
(defvar-keymap reg-and-bm-hyper-map
  :doc "Register tracking and bookmark access."
  :name "Reg & Bm"
  :parent ctl-x-r-map
  "C-g" #'keyboard-quit
  "p"   #'point-to-register)
(keymap-set global-map "H-r" reg-and-bm-hyper-map)

;; Version Control Prefix (H-v)
(defvar-keymap vc-hyper-map
  :doc "VC and Git internal operations."
  :name "VC"
  :parent vc-prefix-map
  "C-g" #'keyboard-quit)
(keymap-set global-map "H-v" vc-hyper-map)

;; Window Configurations Prefix (H-w)
(defvar-keymap window-hyper-map
  :doc "Window structural alterations."
  :name "Window"
  :parent window-prefix-map
  "C-g" #'keyboard-quit
  "t"   #'transpose-frame
  "="   #'balance-windows
  "+"   #'balance-windows-area
  "_"   #'shrink-window-if-larger-than-buffer)
(keymap-set global-map "H-w" window-hyper-map)

;; Multi-Window / Multi-Frame Prefixes (H-4 / H-5)
(defvar-keymap other-window-hyper-map
  :doc "Other-window target actions map."
  :name "Other-Window Prefix"
  :parent ctl-x-4-map
  "C-g" #'keyboard-quit
  "l"   #'find-library-other-window
  "H-4" #'other-window-prefix)
(keymap-set global-map "H-4" other-window-hyper-map)

(defvar-keymap other-frame-hyper-map
  :doc "Other-frame target actions map."
  :name "Other-Frame Prefix"
  :parent ctl-x-5-map
  "C-g" #'keyboard-quit)
(keymap-set global-map "H-5" other-frame-hyper-map)

;; Infrastructure Prefixes (H-a d)
(defvar-keymap docker-map
  :doc "Docker Infrastructure management operations."
  :name "Docker"
  "i" #'docker-images
  "c" #'docker-containers
  "v" #'docker-volumes
  "n" #'docker-networks
  "x" #'docker-contexts
  "C" #'docker-compose)
(keymap-set global-map "H-a d" docker-map)

;; AI. Gptel prefixes (H-a g)
(defvar-keymap gptel-map
  :doc "GPTel, a simple, extensible LLM client"
  :name "GPTel"
  "g" #'gptel-menu
  "i" #'gptel
  "s" #'gptel-send)
(keymap-set global-map "H-a g" gptel-map)

;;; Global Shortcuts & Specialized Utility Keymaps

;; Standalone Bindings
(keymap-set global-map "H-s" #'save-buffer)
(keymap-set global-map "H-k" #'kill-current-buffer)
(keymap-set global-map "C-M-S-s-s" #'save-some-buffers)

;; Non-Hyper Context: Avy Maps (C-;)
(defvar-keymap avy-cmd-map
  :doc "Essential structural jump commands via Avy."
  :name "Avy"
  ";" #'avy-goto-char-timer
  "2" #'avy-goto-char-2
  "w" #'avy-goto-word-1
  "g" #'avy-goto-line
  "m" #'avy-move-line
  "l" #'avy-copy-line
  "k" #'avy-kill-whole-line
  "r" #'avy-copy-region
  "K" #'avy-kill-region
  "s" #'avy-kill-ring-save-region
  "h" #'avy-org-goto-heading-timer)
(keymap-set global-map "C-;" avy-cmd-map)

;; Repeating Actions Navigation Maps
(defvar-keymap uk-buffer-repeat-map
  :doc "Frequent sequential buffer cyclic actions."
  :repeat t
  "," #'previous-buffer
  "." #'next-buffer)

(provide 'init-prefix)
;;; init-prefix.el ends here
