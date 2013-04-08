;;
 (if (equalp (symbol-name system-type) "gnu/linux")
     (set-default-font "Inconsolata-12")
   (set-default-font "Monaco-12"))

;;
(setq custom-theme-directory (live-pack-lib-dir))
;;(load-theme 'blackboard t)
