(require 'cc-mode)
;; TODO: update this properly
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (defun-block-intro . 4)
                                   (substatement      . 4)
                                   (label             . 4)
                                   (statement-block-intro . 4)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
    ;; we like auto-newline and hungry-delete
;;  (c-toggle-auto-hungry-state 1)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
