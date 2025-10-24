;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun uk-denote-leet-go-file ()
  "Create a Denote file with Go type in the Leetcode directory."
  (declare (interactive-only t))
  (interactive)
  (require 'denote)
  (let* ((denote-directory "~/prog/algo/leetcode")
         (denote-file-type 'go)
         (denote-prompts (denote-add-prompts '(signature)))
         (denote-prompts-with-history-as-completion
          '(denote-files-matching-regexp-prompt denote-query-link-prompt))
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
    (declare (special denote-directory
                      denote-file-type
                      denote-prompts
                      denote-prompts-with-history-as-completion
                      denote-known-keywords
                      denote-excluded-keywords-regexp
                      denote-infer-keywords))
    (call-interactively #'denote)))

(use-package denote
  :defer t
  :config
  (add-to-list 'denote-file-types
               '(go
                 :extension ".go"
                 :front-matter uk-denote-go-front-matter
                 :title-key-regexp "^* title\\s-*:"
                 :title-value-function identity
                 :title-value-reverse-function denote-trim-whitespace
                 :keywords-key-regexp "^* tags\\s-*:"
                 :keywords-value-function denote-format-keywords-for-text-front-matter
                 :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                 :signature-key-regexp "^* signature\\s-*:"
                 :signature-value-function denote-format-string-for-org-front-matter
                 :signature-value-reverse-function denote-trim-whitespace
                 :identifier-key-regexp "^* identifier\\s-*:"
                 :identifier-value-function denote-format-string-for-org-front-matter
                 :identifier-value-reverse-function denote-trim-whitespace
                 :date-key-regexp "^* date\\s-*:"
                 :date-value-function denote-date-iso-8601
                 :date-value-reverse-function denote-extract-date-from-front-matter
                 :link denote-org-link-format
                 :link-in-context-regexp denote-org-link-in-context-regexp))
  (defvar uk-denote-go-front-matter
    "package main\n"
    "golang file front matter"))

(provide 'init-leet)
