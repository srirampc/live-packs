(require 'org-install)
(require 'org-latex)
(setq org-clock-persist 'history)

(defun is-leos? ()
  "Check if LEOS flag is set"
  (let ((leos-string
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (and (re-search-forward
                   "^#\\+LATEX_EXPORT_ON_SAVE:[ \t]*\\([-/a-zA-Z]+\\)" nil t)
                  (match-string 1))))))
    (if (and leos-string
             (string= leos-string "t"))
        t
      nil)))

(defun org-insert-leos-option ()
  "Insert the Latex Export on Save option"
  (interactive)
  (if (not (bolp)) (newline))
  (insert "#+LATEX_EXPORT_ON_SAVE: t"))

(defun org-mode-export-on-save-hook ()
  "Org mode to save as latex hook"
  (if (is-leos?)
      (org-export-as-latex 3)))

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
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)


;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(defun my-auto-tex-parameters ()
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-export-latex-default-packages-alist
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

      (if (string-match "LATEX_PROJECT: kamban" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(
                  )))

      (if (string-match "LATEX_PROJECT: kamban" (buffer-string))
          (setq org-export-latex-classes
                (cons '("book"
                        "\\documentclass[11pt,draft,twoside,a4paper]{kamban}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\chapter{%s}" . "\\chapter*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes)))

      (if (string-match "LATEX_LANG: tamil" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ("" "xltxtra" t)
                  ("xetex" "hyperref" nil)
                  )))

      (if (string-match "LATEX_LANG: tamil" (buffer-string))
          (setq org-export-latex-classes
                (cons '("article"
                        "\\documentclass[11pt,article,oneside]{memoir}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes)))

      ;; Packages to include when xelatex is used
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ("" "url" t)
                  ("" "rotating" t)
                  ("american" "babel" t)
                  ("babel" "csquotes" t)
                  ("" "soul" t)
                  ("xetex" "hyperref" nil)
                  )))

      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-classes
                (cons '("article"
                        "\\documentclass[11pt,article,oneside]{memoir}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)
