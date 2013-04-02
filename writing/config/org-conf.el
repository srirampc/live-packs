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
