
(global-company-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(delete-selection-mode t)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)

(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;;(abbrev-mode t)
;;(define-abbrev-table 'global-abbrev-table '(
;;					    ("8l" "lllllll")
;;					    ))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer. "
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))

(require 'dired-x)

(provide 'init-better-defaults)
