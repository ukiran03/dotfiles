;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:

;; ~/.config/emacs/templates

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d")
       :ann "Today's date"
       :doc "Insert today's date")

prog-mode

(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: ")

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(whilelet "(while-let (" p ")" n> r> ")")
(andlet "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(lett "(let* (" p ")" n> r> ")")
(pcaselet "(pcase-let (" p ")" n> r> ")")
(pcaselett "(pcase-let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " --- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)
(package (i header) r n n (i provide))

eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

sh-base-mode

(! & "#!/usr/bin/env bash" n q)
(if "if [ " p " ]" n "  then " p n "fi" q)
(ife "if [ " p " ]" n "  then " p n "else" p n "fi" q)
(cs "case " p " in" n> p " )" n> p n> ";;" n> q n "esac")
(fr "for " p " in " p n "do" n> q n "done")

text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n :doc "UNICODE BOX")
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n :doc "ASCII BOX")
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(table (p (read-number "Rows: ") rows noinsert)
       (p (read-number "Cols: ") cols noinsert)
       "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n
       "|" (* cols "----|") n
       (* rows "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n))

rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

org-mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
;; (src "#+begin_src " q n r n "#+end_src")
(src "#+begin_src " p n r n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: Ushakiran")

go-mode go-ts-mode

(pkg "package " p q n>)
(fpf "fmt.Printf(\"" p "%v\\n\", " q ")")
(lpf "log.Printf(\"" p "%v\\n\", " q ")")
(fpl "fmt.Println(" q ")")
(lpl "log.Println(" q ")")
(rng "for " p ", " p " := range " p " {" n> q n "}" >)
(fr "for " p "{" n> q n "}" >)
(struct "type " p " struct {" n> q n "}")
(inf "type " p " interface {" n> q n "}")
;; (con "const " p " = " q )
(cons "const (" n> p " = " q n ")")
;; (var "var " p " " q)
;; (map "map[" p "]" q)
;; (if "if " p " {" n> n "}" >)
(ifel "if " p " {" n> p n "} else {" > n> p n "}" > q)
(elif "if " p " {" n> p n "} else if " > p " {" n> p n "}" > q)
(ifen "if err != nil {" n> q n "}" >)
(ifer "if err != " p " {" n> q n "}" >)
(sel "select {" n> "case " p ":" n> q n "}" >)
(switch "switch " p " {" n> "case " p ":" q n "}" >)
(fmain "func main() {" n> r> q n "}" >)
(func "func " p " (" p ") " p " {" n> q n "}" >)
;; (init "func init() {" n> q n "}")
(run "func run() {" n> q n "}")
(test "func Test" p " (t *testing.T) { " n> q n "}")

racket-mode

(isl+ "#lang htdp/isl+" n n>)
(bsl "#lang htdp/bsl" n n>)
(isl "#lang htdp/isl" n n>)
(image "(require 2htdp/image)" n>)
(universe "(require 2htdp/universe)" n>)
(image+universe "(require 2htdp/image)" n "(require 2htdp/universe)" n n>)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
