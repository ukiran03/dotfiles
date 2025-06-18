;;; init-prefix.el --- Hyper Key Prefix Action maps

;;; Code:
(defvar-keymap files-hyper-map
  :doc "My Hyper prefix map For `Files'."
  :name "Files"
  "C-g" 'keyboard-quit
  "f"    #'find-file
  "H-f"  #'find-file
  "F"  #'view-file
  "r"    'consult-recent-file
  "R"    #'rename-file
  "w"    #'write-file
  "l"    #'find-library
  "v"    #'find-alternate-file)
(keymap-set global-map "H-f" files-hyper-map)

(defvar-keymap dired-hyper-map
  :doc "Hyper prefix map for `Dired'."
  :name "Dired"
  "C-g" 'keyboard-quit
  "c"    'consult-dir
  "d"    #'dired
  "H-d"    #'dired
  "j"    #'dired-jump)
(keymap-set global-map "H-d" dired-hyper-map)

(defvar-keymap saves-hyper-map
  :doc "saves `Buffers'."
  :name "Save"
  "C-g" 'keyboard-quit
  "s"    #'save-buffer
  "H-s"  #'save-buffer
  "a"    #'save-some-buffers)
(keymap-set global-map "H-s" saves-hyper-map)


;; Karthinks: https://karthinks.com/software/it-bears-repeating/

(defvar-keymap buffer-hyper-map
  :doc "buffer actions."
  :name "Buffer"
  "C-g" 'keyboard-quit
  "b"    'consult-buffer
  "H-b"  'consult-buffer
  "k"    #'kill-buffer
  "i"    #'insert-buffer
  "g"    #'revert-buffer-quick
  "c"    #'clone-buffer
  "r"    #'rename-buffer
  "B"    #'ibuffer
  "K"    #'kill-current-buffer)
(keymap-set global-map "H-b" buffer-hyper-map)

(defvar-keymap uk-buffer-repeat-map
  :doc "Freq buffer actions"
  :repeat t
  "," #'previous-buffer
  "<left>" #'previous-buffer
  "." #'next-buffer
  "<right>" #'next-buffer)

(defvar-keymap other-window-hyper-map
  :doc "`other-window-prefix' actions."
  :name "Other-Window Prefix"
  :parent ctl-x-4-map
  "C-g" 'keyboard-quit
  "l"    #'find-library-other-window
  "H-4"  #'other-window-prefix)
(keymap-set global-map "H-4" other-window-hyper-map)

(defvar-keymap other-frame-hyper-map
  :doc "`other-frame-prefix' actions."
  :name "Other-Frame Prefix"
  :parent ctl-x-5-map
  "C-g" 'keyboard-quit)
(keymap-set global-map "H-5" other-frame-hyper-map)


(defvar-keymap narrow-hyper-map
  :doc "Narrow-map hyper actions."
  :name "Narrow"
  :parent narrow-map
  "C-g" 'keyboard-quit)
(keymap-set global-map "H-n" narrow-hyper-map)


(defvar-keymap project-hyper-map
  :doc "project-map hyper actions"
  :name "Project"
  :parent project-prefix-map
  "." 'project-dired
  "<return>" 'project-dired
  "C-g" 'keyboard-quit
  "F" #'project-view-file
  "<delete>" 'project-forget-project)
(keymap-set global-map "H-p" project-hyper-map)

(defvar-keymap reg-and-bm-hyper-map
  :doc "Register and Bookmark hyper-map."
  :name "Reg & Bm"
  :parent ctl-x-r-map
  "C-g" 'keyboard-quit
  "p"    #'point-to-register)
(keymap-set global-map "H-r" reg-and-bm-hyper-map)

;; (defvar-keymap tab-hyper-map
;;   :doc "tab-bar hyper map."
;;   :name "Tabs"
;;   :parent tab-prefix-map
;;   "C-g" 'keyboard-quit)
;; (keymap-set global-map "H-t" tab-hyper-map)

(defvar-keymap vc-hyper-map
  :doc "`vc' hyper map."
  :name "VC"
  :parent vc-prefix-map
  "C-g" 'keyboard-quit)
(keymap-set global-map "H-v" vc-hyper-map)

(defvar-keymap window-hyper-map
  :doc "`window' hyper map."
  :name "window"
  :parent window-prefix-map
  "t" 'transpose-frame
  "=" 'balance-windows
  "+" 'balance-windows-area
  "_" 'shrink-window-if-larger-than-buffer
  "C-g" 'keyboard-quit)
(keymap-set global-map "H-w" window-hyper-map)

;; (keymap-set global-map "H-e" #'eval-last-sexp)
;; (keymap-set global-map "H-z" #'repeat)
;; (define-key global-map "H-e" #'eval-last-sexp)
;; (define-key global-map "H-z" #'repeat)


(provide 'init-prefix)
;;; init-prefix.el ends here.
