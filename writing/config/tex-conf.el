(live-add-pack-lib "auctex")
(load "auctex")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("acrobat reader" "evince %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "acrobat reader") (output-html "start")))))

(live-add-pack-lib "auto-complete-latex")
(require 'auto-complete-latex)
(setq ac-l-dict-directory
      (concat (live-pack-lib-dir) "auto-complete-latex/" "ac-l-dict/"))
(add-to-list 'ac-modes 'latex-mode)
(add-hook 'latex-mode-hook 'ac-l-setup)

(require 'ac-source-ispell)
