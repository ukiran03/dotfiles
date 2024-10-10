

;; by <https://github.com/leemeichin>
;; Sets the default mode-line. Remove '-default' if you want to test your changes on-the-fly with (eval-buffer)
(setq-default mode-line-format
      '(
        ;; add a noticeable red block that says 'READ ONLY' when the file's, er, read only
        (:eval
         (when (eql buffer-read-only t)
             ;; propertize adds metadata to text, so you can add colours and formatting, amongst other things
             (propertize " READ ONLY " 'face
                         '(:background "color-88" :foreground "white" :weight bold))))
        ;; show the buffer filename, with a green background when unmodified/saved and a red one when modified
        (:eval
         (propertize " %b " 'face
                     (if (buffer-modified-p)
                         '(:background "red" :foreground "white" :weight bold)
                       '(:background "green" :foreground "black" :weight bold))))
        ;; show the current major mode in use (use obsolete format because trailing spaces nice it up)
        (:propertize " %m " face (:background "grey"))
        ;; show the current branch and VCS in use, if there is one
        (:propertize (vc-mode vc-mode) face (:weight normal))
        " "
        ;; show the line number and column number (no 'All', 'Top', 'Bottom', etc.)
        (:propertize " %l:%c " face (:background "black" :foreground "white" :weight light))))
