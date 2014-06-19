(live-add-pack-lib "bookmark-plus")

(require 'bookmark+)


(setq project-files-to-delete (list))

(defun project-register-buffer (b)
  (if (and b (bufferp b))
      (let ((f (buffer-file-name b)))
        (if (and f (stringp f)
                 (not (file-directory-p f))
                 (file-exists-p f))
            (progn
              (bmkp-bookmark-a-file f)
              (delete f project-files-to-delete)
              (bmkp-refresh-menu-list))))))

(defun project-unregister-file (f)
  (if (and f (stringp f)
           (not (file-directory-p f))
           (file-exists-p f)
           (bmkp-get-autofile-bookmark f))
      (progn

        (bmkp-delete-bookmarks
         (bookmark-get-position
          (bmkp-get-autofile-bookmark f))))
    (message "bad file %s" (if (stringp f) f "x"))))

(defun project-add-files-to-delete (b)
  (if (and b (bufferp b) )
      (let ((f (buffer-file-name b)))
        (if (and f (stringp f)
             (not (file-directory-p f))
             (file-exists-p f))
            (add-to-list 'project-files-to-delete f)))))


(defun project-kill-emacs-hook ()
 (let (buff-fnames del-fnames )
   (setq buff-fnames (mapcar 'buffer-file-name (buffer-list)))
   (setq del-fnames (remove-if
                     'null (mapcar
                            (lambda (x) (if (member x buff-fnames) nil x))
                            project-files-to-delete)))
  (mapcar 'project-unregister-file del-fnames)))

;(project-kill-emacs-hook)


;;(bmkp-get-autofile-bookmark "c:/dot-emacs/live-packs/power/lib/eproject/eproject.el")
;;(bmkp-get-autofile-bookmark "c:/dot-emacs/live-packs/power/config/theme-conf.el")

(defun project-wcc-hook ()
  (let ((w (selected-window)) (b (window-buffer (selected-window))))
    ;;(message "wcc-hook: %s" (prin1-to-string (list w b)))
    (project-register-buffer b)
    ))

(defun project-find-file-hook ()
  (run-with-idle-timer 0.2 nil 'project-wcc-hook))


(defun project-kill-buffer-hook ()
  (project-add-files-to-delete (current-buffer)))

(add-hook 'find-file-hook 'project-find-file-hook)
(remove-hook 'window-configuration-change-hook 'project-wcc-hook)

(add-hook 'kill-buffer-hook 'project-kill-buffer-hook)
(add-hook 'kill-emacs-hook 'project-kill-emacs-hook)


;;(remove-hook 'find-file-hook 'project-find-file-hook)
;;(remove-hook 'kill-buffer-hook 'project-kill-buffer-hook)

;;(remove-hook 'window-configuration-change-hook 'project-wcc-hook)
(defun project-switch-main ()
  (interactive)
  (bmkp-switch-bookmark-file bookmark-default-file)
  (bookmark-bmenu-list))
(global-set-key [f5] 'project-switch-main)
