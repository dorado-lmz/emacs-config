(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))


(load-theme 'sanityinc-solarized-dark t)
(global-linum-mode t)
(setq-default cursor-type 'bar)
(global-hl-line-mode t)

(provide 'init-ui)
