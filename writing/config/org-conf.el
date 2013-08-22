;;(require 'org-install)
(require 'ox-latex)
(setq org-clock-persist 'history)

(defun org-insert-aps-article-header ()
  "Insert header for APS Report"
  (interactive)
  (if (not (bolp)) (newline))
  (insert "#+TITLE: Annual Progress Seminar Report
#+AUTHOR: Sriram Ponnambalam C (10405602) Guide : Prof. Srinivas Aluru
#+EMAIL:
#+DATE:
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:t \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LINK_UP:
#+LINK_HOME:
#+XSLT:
#+LaTeX_CLASS: aps-article
#+LaTeX_CLASS_OPTIONS: [integrals, nointegrals, article, 11pt, a4paper]
#+LATEX_HEADER: \\usepackage{geometry}
#+LATEX_HEADER: \\usepackage{amsmath}
#+LATEX_HEADER: \\usepackage[MnSymbol]{mathspec}
#+LATEX_HEADER: \\usepackage{fontspec}
#+LATEX_HEADER: \\usepackage{xltxtra}
#+LATEX_HEADER: \\setprimaryfont{Minion Pro}
#+LATEX_HEADER: \\setmainfont[Mapping=tex-text]{Minion Pro}
#+LATEX_HEADER: \\setsansfont[Mapping=tex-text]{Myriad Pro}
#+LATEX_HEADER: \\setmathsfont[Set=Greek,Uppercase=Italic,Lowercase=Italic]{Minion Pro}
#+LATEX_HEADER: \\font\\TitleFont=\"Myriad Pro:letterspace=10,+smcp\" at 24 pt
#+LATEX_HEADER: \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
#+LaTeX_HEADER: \\usepackage{amsthm}
#+LaTeX_HEADER: \\newtheorem{theorem}{Theorem}[section]
#+LaTeX_HEADER: \\newtheorem{lemma}[theorem]{Lemma}
#+LATEX_CMD: xelatex
#+LATEX_EXPORT_ON_SAVE: t
#+LATEX_CMD: xelatex
#+LATEX_EXPORT_ON_SAVE: t
"))

(defun is-flag-set? (in-str)
  "Check if LEOS flag is set"
  (let ((leos-string
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (and (re-search-forward
                   (format "^#\\+%s:[ \t]*\\([-/a-zA-Z]+\\)"
                           in-str) nil t)
                  (match-string 1))))))
    (if (and leos-string
             (string= leos-string "t"))
        t
      nil)))

(defun is-leos? ()
  "Check if LEOS flag is set"
  (is-flag-set? "LATEX_EXPORT_ON_SAVE"))

(defun org-insert-leos-option ()
  "Insert the Latex Export on Save option"
  (interactive)
  (if (not (bolp)) (newline))
  (insert "#+LATEX_EXPORT_ON_SAVE: t"))

(defun is-beamer-leos? ()
  "Check if LEOS flag is set"
  (is-flag-set? "LATEX_BEAMER_EXPORT_ON_SAVE"))

(defun org-insert-beamer-leos-option ()
  "Insert the Latex Export on Save option"
  (interactive)
  (if (not (bolp)) (newline))
  (insert "#+LATEX_BEAMER_EXPORT_ON_SAVE: t"))

(defun org-mode-export-on-save-hook ()
  "Org mode to save as latex hook"
  (if (is-leos?)
      (org-latex-export-to-latex))
  (if (is-beamer-leos?)
      (org-beamer-export-to-latex)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'org-mode-export-on-save-hook
                      nil 'make-it-local)))

(defun test-leos ()
  (interactive)
  (message (if (is-leos?) "hello" "no!")))


(setq org-export-latex-listings t)

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
;; updated to org-mode 8.05
(defun my-auto-tex-cmd (inarg)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet -output-directory=%o %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet -output-directory=%o %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet -output-directory=%o %f"))
  ;; LaTeX compilation command
  (setq org-latex-pdf-process (list texcmd)))

(add-hook 'org-export-before-processing-hook 'my-auto-tex-cmd)
;;(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)


;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(defun my-auto-tex-parameters (inarg)
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1"   "fontenc"   t)
              (""     "fixltx2e"  nil)
              (""     "wrapfig"   nil)
              (""     "soul"      t)
              (""     "textcomp"  t)
              (""     "marvosym"  t)
              (""     "wasysym"   t)
              (""     "latexsym"  t)
              (""     "amssymb"   t)
              (""     "hyperref"  nil)))

      (if (string-match "LATEX_LANG: tamil" (buffer-string))
          (setq org-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ("" "xltxtra" t)
                  ("xetex" "hyperref" nil)
                  )))

      ;; Packages to include when xelatex is used
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-latex-default-packages-alist
                '(("" "url" t)
                  ("" "rotating" t)
                  ("american" "babel" t)
                  ("babel" "csquotes" t)
                  ("" "soul" t)
                  ("xetex" "hyperref" nil)
                  ))))

(add-hook 'org-export-before-processing-hook 'my-auto-tex-parameters)

;; Custom article classes
(add-to-list 'org-latex-classes
             '("aps-article"
              "\\documentclass[11pt,article,oneside]{memoir}
              [DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("kamban-book"
              "\\documentclass[11pt,draft,twoside,a4paper]{kamban}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("tamil-article"
              "\\documentclass[11pt,article,oneside]{memoir}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("xelatex-article"
              "\\documentclass[11pt,article,oneside]{memoir}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
