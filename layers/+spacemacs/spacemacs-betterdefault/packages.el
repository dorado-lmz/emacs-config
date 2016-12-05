(setq spacemacs-betterdefault-packages
      '(
	company
	hippie-exp
	))


(defun spacemacs-betterdefault/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
	    company-minimum-prefix-length 2
	    company-require-match nil
	    company-dabbrev-ignore-case nil
	    company-dabbrev-downcase nil)
      )
    :config
    (progn
      (spacemacs|diminish company-mode " Ⓐ" " a")

      (let ((map company-active-map))
	(define-key map (kbd "C-/") 'company-search-candidates)
	(define-key map (kbd "C-M-/") 'company-filter-candidates)
	(define-key map (kbd "C-d") 'company-show-doc-buffer))
      )))


(defun spacemacs-betterdefault/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
  (setq hippie-expand-try-functions-list
	'(
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-complete-lisp-symbol-partially
	  tyr-complete-lisp-symbol)))
