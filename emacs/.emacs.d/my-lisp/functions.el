(defun my-command (arg)
  (interactive "P")
  (if arg
      (message "Called with prefix argument!")
    (message "Called without prefix argument!")))


(defun my-copy-from-above-command (&optional arg)
  "Copy ..."
  (interactive "P")
  (let ((cc (current-column))
        n
        (string ""))
    (save-excursion
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward " \t\n")
      (move-to-column cc)
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      (if (< cc (current-column))
          (if (= (preceding-char) ?\t)
              (progn
                (setq string (make-string (min n (- (current-column) cc)) ?\s))
                (setq n (- n (min n (- (current-column) cc)))))
            (backward-char 1)))
      (setq string (concat string
                           (buffer-substring
                            (point)
                            (min (line-end-position)
                                 (+ n (point)))))))
    (insert string)))

(defun kill&-copy-from-above-command (&optional arg)
  "Kill n Copy ..."
  (interactive "P")
  (let ((cc (current-column))
        n
        (string ""))
    (save-excursion
      (kill-line)
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward " \t\n")
      (move-to-column cc)
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      (if (< cc (current-column))
          (if (= (preceding-char) ?\t)
              (progn
                (setq string (make-string (min n (- (current-column) cc)) ?\s))
                (setq n (- n (min n (- (current-column) cc)))))
            (backward-char 1)))
      (setq string (concat string
                           (buffer-substring
                            (point)
                            (min (line-end-position)
                                 (+ n (point)))))))
    (insert string)))

(defun uk-insert-line (arg)
  "Insert line above no matter your point
with ARG as prefix Insert line below."
  (interactive "P")
  (if arg
      (progn
        (end-of-line)
        (newline-and-indent))
    (progn
      (forward-line -1)
      (end-of-line)
      (newline-and-indent))))
