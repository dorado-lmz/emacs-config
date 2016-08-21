(when (memq window-system '(mac ns))
     (exec-path-from-shell-initialize))

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(global-hungry-delete-mode)

(require 'smartparens-config)
(smartparens-global-mode t)


(setq auto-mode-alist
      (append
       '(
	 ("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.cl\\'" . lisp-mode)
	 )
       auto-mode-alist))

(require 'js2-refactor)
(add-hook 'js2-mode #'js2-refactor-mode)


;;(require 'evil-surround)
;;(global-evil-surround-mode t)

;;(evilnc-default-hotkeys)
;;(define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
;;(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

;;(which-key-mode t)
;;(setq which-key-side-window-location 'right)
(provide 'init-package)

