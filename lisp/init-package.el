(when (> emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(require 'cl)

(defvar lmz/packages '(
		       company
		       monokai-theme
		       hungry-delete
		       swiper
		       counsel
		       smartparens
		       js2-mode
		       nodejs-repl
		       popwin
		       web-mode
		       org-pomodoro
		       evil
		       evil-leader
		       window-numbering
		       powerline
		       ) "Default packages")



(defun lmz/packages-installed-p ()
  (loop for pkg in lmz/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (lmz/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lmz/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(global-company-mode t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(global-hungry-delete-mode)

(require 'smartparens-config)
(smartparens-global-mode t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(evil-mode 1)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(global-evil-leader-mode) 
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(window-numbering-mode 1)
(require 'powerline)
(powerline-default-theme)

(setq auto-mode-alist
      (append
       '(
	 ("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode)
	 )
       auto-mode-alist))

(load-theme 'monokai t)
(provide 'init-package)

