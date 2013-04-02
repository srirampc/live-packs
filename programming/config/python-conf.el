;; (live-add-pack-lib "anything-python")

(live-add-pack-lib "python")
(require 'python)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;(live-add-pack-lib "ipython")
;;(require 'ipython)
;;(setq-default py-python-command-args '("--pylab" "--colors=NoColor"))
;; ;; TODO: update auto-complete thingy
;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (define-key py-mode-map (kbd "M-<tab>")
;;                 'anything-ipython-complete)))

;; (add-hook 'ipython-shell-hook
;;           #'(lambda ()
;;               (define-key py-mode-map (kbd "M-<tab>")
;;                 'anything-ipython-complete)))


;; pymacs - use more ropemacs
(live-add-pack-lib "pymacs")
(require 'pymacs)
;; Initialize Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Auto-completion
;; ;;;  Integrates:
;; ;;;    Rope
;; ;;;   all with AutoComplete.el
;; from
;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")
(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))
(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))
(add-hook 'python-mode-hook
      (lambda ()
        (auto-complete-mode 1)
        (set (make-local-variable 'ac-sources)
             (append ac-sources '(ac-source-rope)))
        (set (make-local-variable 'ac-find-function) 'ac-python-find)
        (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;        (set (make-local-variable 'ac-auto-start) nil)
        ))

;; ein
(live-add-pack-lib "websocket")
(live-add-pack-lib "smartrep")
(live-add-pack-lib "nxhtml")
(live-add-pack-lib "ert")
(live-add-pack-lib "pos-tip")
(live-add-pack-lib "emacs-request")
(live-add-pack-lib "emacs-ipython-notebook/lisp")

(require 'ein)
;; auto-complete
(setq ein:use-auto-complete-superpack t)
(setq ein:console-args '("--profile" "default"))
(require 'auto-complete-config nil t)

(when (featurep 'auto-complete-config)
  (ac-config-default)
  ;; (add-to-list 'ac-dictionary-directories
  ;;              (concat (live-pack-lib-dir) "auto-complete/dict"))
  (global-auto-complete-mode t))
(require 'ein-dev)
