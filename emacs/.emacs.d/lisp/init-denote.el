;; init-denote.el --- Better Note-tatking configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; Best Note taking software

(use-package consult-denote
  :bind (("C-c n c f" . consult-denote-find)
         ("C-c n c r" . consult-denote-grep)))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))

  :config
  (setq denote-file-type 'org)
  (setq denote-directory (expand-file-name "~/Documents/Notes"))
  (setq denote-known-keywords '("meta" "system" "emacs" "media"
			                    "programming" "essay" "english" "book"  "film" "music"))
  (denote-rename-buffer-mode 1))

;;(add-to-list 'denote-file-types
;;             '((go
;;                :extension ".go"
;;                :front-matter "package main\n"
;;                :title-key-regexp "^* title\\s-*:"
;;                :title-value-function identity
;;                :title-value-reverse-function denote-trim-whitespace
;;                :keywords-key-regexp "^* tags\\s-*:"
;;                :keywords-value-function denote-format-keywords-for-text-front-matter
;;                :keywords-value-reverse-function denote-extract-keywords-from-front-matter
;;                :identifier-key-regexp "^* identifier\\s-*:"
;;                :identifier-value-function denote-format-string-for-org-front-matter
;;                :identifier-value-reverse-function denote-trim-whitespace
;;                :link denote-org-link-format
;;                :link-in-context-regexp denote-org-link-in-context-regexp)))

(defun uk-denote-leet-go-file ()
  "blah"
  (declare (interactive-only t))
  (interactive)
  (let* ((denote-directory "~/prog/algo/leetcode")
         (denote-file-type 'go)
         (denote-prompts-with-history-as-completion
          ;; no history for titles
          '(denote-signature-prompt denote-files-matching-regexp-prompt denote-query-link-prompt))
         (denote-known-keywords '("array" "string" "hash" "dynamic" "math" "sorting" "greedy" "dfs"
                                  "binarysearch" "database" "matrix" "bit manipulation" "tree" "bfs"
                                  "two pointers" "prefix sum" "heap" "simulation" "binary tree" "graph"
                                  "counting" "stack" "sliding window" "design" "enumeration"
                                  "backtracking" "union find" "number theory" "linked list" "ordered set"
                                  "monotonic stack" "segment tree" "trie" "combinatorics" "bitmask"
                                  "divide and conquer" "queue" "recursion" "geometry" "binary indexed tree"
                                  "memoization" "hash function" "binary search tree" "shortest path"
                                  "string matching" "topological sort" "rolling hash" "game theory"
                                  "interactive" "data stream" "monotonic queue" "brainteaser"
                                  "doubly-linked list" "randomized" "merge sort" "counting sort"
                                  "iterator" "concurrency" "probability and statistics" "quickselect"
                                  "suffix array" "line sweep" "minimum spanning tree" "bucket sort"
                                  "shell" "reservoir sampling" "strongly connected component" "eulerian circuit"
                                  "radix sort" "rejection sampling" "biconnected component"
                                  ))
         (denote-excluded-keywords-regexp '("test"))
         (denote-infer-keywords nil))
    (call-interactively #'denote)))

(use-package denote-silo
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories (list denote-directory "~/prog/algo/leetcode/")))

(use-package denote-org)
(use-package denote-journal)
(use-package denote-sequence)
(use-package denote-search)

(defun my-denote-create-note-in-any-directory ()
  "Create new Denote note in any directory.
     Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively #'denote)))

;; denote packages: denote, denote-journal, denote-sequence,
;; denote-org, denote-markdown, denote-silo, denote-menu, citar-denote

(provide 'init-denote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-denote.el ends here
