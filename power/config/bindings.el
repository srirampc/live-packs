;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-x\C-g" 'goto-line)

;;(setq frame-title-format '("" "%b @ " (getenv "USERNAME")))
(setq frame-title-format
      '(buffer-file-name
        "%f"
        (dired-directory dired-directory "%b")))
