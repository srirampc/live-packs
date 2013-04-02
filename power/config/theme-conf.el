;;
(if (equalp (symbol-name system-type) "gnu/linux")
    (set-default-font
     "-outline-Inconsolata-normal-r-normal-normal-16-97-96-96-c-*-iso10646-1")
  (set-default-font "Monaco-12"))

;;
(setq custom-theme-directory (live-pack-lib-dir))
;;(load-theme 'blackboard t)
