(setq spacemacs-editing-packages
      '(
	hungry-delete
	move-text
	(origami :toggle (eq 'origami dotspacemacs-folding-method))
	smartparens
	undo-tree
	))

(defun spacemacs-editing/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (spacemacs|add-toggle hungry-delete
      :mode hungry-delete-mode
      :documentation "Delete consecutive horizontal whitespace with  a single key."
      :evil-leader "td")
    (global-hungry-delete-mode t)
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      )))

(defun spacemacs-editing/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (progn
      (define-key evil-normal-state-map "J" 'move-text-down)
      (define-key evil-normal-state-map "D" 'move-text-up)
      )))

(defun spacemacs-editing/init-origami ()
  (use-package origami
    :defer t
    :init
    (progn
      (global-origami-mode)
      (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
      (define-key evil-normal-state-map "zc" 'origami-close-node)
      (define-key evil-normal-state-map "zo" 'origami-open-node)
      (define-key evil-normal-state-map "zn" 'origami-next-fold)
      (define-key evil-normal-state-map "zp" 'origami-previous-fold)
      (define-key evil-normal-state-map "zm" 'origami-close-all-nodes)
      (define-key evil-normal-state-map "zr" 'origami-open-all-nodes))))

(defun spacemacs-editing/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline sp-up-sexp)
    :init
    (progn
      ;;settings
      (setq sp-show-pair-delay 0.2)
      (setq sp-show-pair-from-inside t)
      (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
				  'smartparens-strict-mode
				'smartparens-mode)
			      '(prog-mode-hook comint-mode-hook))
      ;;toggle
      (spacemacs|add-toggle smartparens
	:mode smartparens-mode
	:documentation "Enable smartparens"
	:evil-leader "tp"))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " Ⓟ" " p")
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
      (show-smartparens-global-mode +1)
      )))

(defun spacemacs-editing/init-undo-tree ()
  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (spacemacs|hide-lighter undo-tree-mode)))
