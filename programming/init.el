;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config

(live-load-config-file "bindings.el")
(live-load-config-file "hg-conf.el")
;;(live-load-config-file "python-conf.el")
(live-load-config-file "cc-conf.el")
(live-load-config-file "ess-conf.el")
(live-load-config-file "haskell-conf.el")
(live-load-config-file "go-conf.el")

;; option for octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
;(shell)
