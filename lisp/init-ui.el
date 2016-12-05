(tool-bar-mode -1)

(scroll-bar-mode -1)

(setq inhibit-splash-screen t)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;(global-linum-mode t)

(setq-default cursor-type 'bar)

(window-numbering-mode 1)

(defun spacemacs/mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property.
and its values are removed."
  (let ((tail plist)
	result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))

    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
	(pop tail)))

    (while (consp tail)
      (push (pop tail) result))
    (nreverse result))
  )

(defun spacemacs/set-default-font (plists)
  "set the font given the passed PLISTS.
PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 vale2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))

  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
	(let* ((font (car plist))
	       (props (cdr plist))
	       (scale (plist-get props :powerline-scale))
	       (font-props (spacemacs/mplist-remove
			    (spacemacs/mplist-remove props :powerline-scale)
			    :powerline-offset))
	       (fontspec (apply 'font-spec :name font font-props)))
	  (set-frame-font fontspec nil t)))))

  )

(spacemacs/set-default-font '("Source Code Pro"
			      :size 16
			      :weight normal
			      :width normal
			      :powerline-scale 1.1))


(load-theme 'sanityinc-solarized-dark t)

;;(setq powerline-default-separator 'wave)
;;(setq spaceline-window-numbers-unicode t)
;;(setq spaceline-workspace-numbers-unicode t)
;;(require 'spaceline-config)
;;(spaceline-spacemacs-theme)

(provide 'init-ui)

