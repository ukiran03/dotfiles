;;; init-quotes.el --- Motivational quotes for the scratch buffer -*- lexical-binding: t -*-

;;; Commentary:
;; Configures a random Sanatana Dharma or Indian philosophical quote to be
;; displayed in the initial *scratch* buffer.

;; Reference: https://martinsos.com/posts/may-recommend-emacs-home-base

;;; Code:

;; Define a literal vector containing nested vectors of Sanatana Dharma &
;; Indian quotes
(defvar my/motivational-quotes
  [["Arise, awake, and stop not till the goal is reached." "Swami Vivekananda"]
   ["You have a right to perform your prescribed duty, but you are not entitled to the fruits of action." "Bhagavad Gita"]
   ["Truth can be stated in a thousand different ways, yet each one can be true." "Swami Vivekananda"]
   ["The mind is everything. What you think you become." "Buddha"]
   ["When meditation is mastered, the mind is unwavering like the flame of a candle in a windless place." "Bhagavad Gita"]
   ["Truth alone triumphs; not falsehood." "Mundaka Upanishad (Satyameva Jayate)"]
   ["The power of concentration is the only key to the treasure-house of knowledge." "Swami Vivekananda"]
   ["A man is made by his belief. As he believes, so he is." "Bhagavad Gita"]
   ["The greatest error is to believe that you are weak." "Swami Vivekananda"]
   ["He who has no attachments can really love others, for his love is pure and divine." "Swami Chinmayananda"]]
  "A vector of vectors containing Sanatana Dharma and Indian philosophical
quotes.")

;; Instantaneous random selection mapped via use-package emacs
(use-package emacs
  :straight (:type built-in)
  :custom
  (initial-scratch-message
   (let* ((len (length my/motivational-quotes))
          (picked (aref my/motivational-quotes (random len))))
     (format ";; \"%s\" - %s\n\n" (aref picked 0) (aref picked 1)))))

(provide 'init-quotes)
;;; init-quotes.el ends here
