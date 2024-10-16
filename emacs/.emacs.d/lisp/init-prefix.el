;;; init-prefix.el --- Hyper Key Prefix Action maps

;;; Code:
(defvar-keymap files-hyper-map
  :doc "My Hyper prefix map For `Files'."
  :name "Files"
  "f"    #'find-file
  "H-f"  #'find-file
  "r"    'consult-recent-file
  "R"    #'rename-file
  "w"    #'write-file
  "l"    #'find-library
  "v"    #'find-alternate-file)
(keymap-set global-map "H-f" files-hyper-map)

(defvar-keymap dired-hyper-map
  :doc "Hyper prefix map for `Dired'."
  :name "Dired"
  "c"    'consult-dir
  "d"    #'dired
  "H-d"    #'dired
  "j"    #'dired-jump)
(keymap-set global-map "H-d" dired-hyper-map)

(defvar-keymap saves-hyper-map
  :doc "saves `Buffers'."
  :name "Save"
  "s"    #'save-buffer
  "H-s"  #'save-buffer
  "a"    #'save-some-buffers)
(keymap-set global-map "H-s" saves-hyper-map)

(defvar-keymap num-window-hyper-map
  :doc "number window actions."
  :name "Win Actions"
  )

(defvar-keymap buffer-hyper-map
  :doc "buffer actions."
  :name "Buffer"
  "b"    'consult-buffer
  "H-b"  'consult-buffer
  "k"    #'kill-buffer
  "i"    #'insert-buffer
  "g"    #'revert-buffer-quick
  "c"    #'clone-buffer
  "r"    #'rename-buffer
  "K"    #'kill-current-buffer)
(keymap-set global-map "H-b" buffer-hyper-map)

(defvar-keymap other-window-hyper-map
  :doc "`other-window-prefix' actions."
  :name "Other-Window Prefix"
  :parent ctl-x-4-map
  "l"    #'find-library-other-window
  "H-4"  #'other-window-prefix)
(keymap-set global-map "H-4" other-window-hyper-map)

(defvar-keymap other-frame-hyper-map
  :doc "`other-frame-prefix' actions."
  :name "Other-Frame Prefix"
  :parent ctl-x-5-map)
(keymap-set global-map "H-5" other-frame-hyper-map)

;; (defvar-keymap narrow-hyper-map
;;   :doc "Narrow-map hyper actions."
;;   :name "Narrow"
;;   :parent narrow-map)
;; (keymap-set global-map "H-n" narrow-hyper-map)

;; (defvar-keymap org-narrow-hyper-map
;;   :doc "Org Narrow Functions map."
;;   :name "Org Narrow"
;;   "b" 'org-narrow-to-block
;;   "e" 'org-narrow-to-element
;;   "s" 'org-narrow-to-subtree)
;; (keymap-set org-mode-map "H-n" org-narrow-hyper-map)


(defvar-keymap narrow-hyper-map
  :doc "Narrow-map hyper actions."
  :name "Narrow"
  :parent narrow-map  ;; Change this if there's a specific parent map you want to use
  "b" 'org-narrow-to-block
  "e" 'org-narrow-to-element
  "s" 'org-narrow-to-subtree)
(keymap-set global-map "H-n" narrow-hyper-map)

;; (defvar-keymap org-narrow-hyper-map
;;   :doc "Org Narrow Functions map."
;;   :name "Org Narrow"
;;   "b" 'org-narrow-to-block
;;   "e" 'org-narrow-to-element
;;   "s" 'org-narrow-to-subtree)
;; (keymap-set org-mode-map "H-n" org-narrow-hyper-map)


(defvar-keymap project-hyper-map
  :doc "project-map hyper actions"
  :name "Project"
  :parent project-prefix-map)
(keymap-set global-map "H-p" project-hyper-map)

(defvar-keymap reg-and-bm-hyper-map
  :doc "Register and Bookmark hyper-map."
  :name "Reg & Bm"
  :parent ctl-x-r-map
  "p"    #'point-to-register)
(keymap-set global-map "H-r" reg-and-bm-hyper-map)

(defvar-keymap tab-hyper-map
  :doc "tab-bar hyper map."
  :name "Tabs"
  :parent tab-prefix-map)
(keymap-set global-map "H-t" tab-hyper-map)

(defvar-keymap vc-hyper-map
  :doc "`vc' hyper map."
  :name "VC"
  :parent vc-prefix-map)
(keymap-set global-map "H-v" vc-hyper-map)

(defvar-keymap window-hyper-map
  :doc "`window' hyper map."
  :name "Window"
  "o"    #'other-window
  "-"    #'fit-window-to-buffer
  "_"    #'shrink-window-if-larger-than-buffer
  "="    #'balance-windows
  "+"    #'balance-windows-area
  "0"    #'delete-windows-on
  "2"    #'split-root-window-below
  "3"    #'split-root-window-right
  "s"    #'window-toggle-side-windows)
(keymap-set global-map "H-w" window-hyper-map)

;; Direct Keys for a while

(keymap-set global-map "H-e" #'eval-last-sexp)

(keymap-set global-map "H-o" #'other-window)
(keymap-set global-map "H-q" #'read-only-mode)
(keymap-set global-map "H-/" 'vundo)
(keymap-set global-map "H-z" #'repeat)
(keymap-set global-map "H-;" #'comment-line)

(keymap-set global-map "H-g" 'magit-status)

(keymap-set global-map "H-0" #'delete-window)
(keymap-set global-map "H-1" #'delete-other-windows)
(keymap-set global-map "H-2" #'split-window-below)
(keymap-set global-map "H-3" #'split-window-right)


(provide 'init-prefix)
;;; init-prefix.el ends here.
