;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config
(live-load-config-file "bindings.el")
(live-load-config-file "anything-conf.el")
(live-load-config-file "theme-conf.el")
(live-load-config-file "eproject-conf.el")

;; automatically load when changed outside
(global-auto-revert-mode 1)
;; Use shit -> for moving between visible buffers
(windmove-default-keybindings)
;; Prevent the annoying beep on errors
(setq visible-bell t)
