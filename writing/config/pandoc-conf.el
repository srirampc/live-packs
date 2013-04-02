;; pandoc settings
(require 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(setq pandoc-binary "~/.cabal/bin/pandoc")
