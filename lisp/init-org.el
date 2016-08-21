
(with-eval-after-load 'org
 (setq default-org-directory "~/Code/Plan")
 (setq org-agenda-files (list default-org-directory))

 (setq org-src-fontify-natively t)

 (setq org-capture-templates
       '(("t" "Todo" entry (file+headline (concat default-org-directory "/gtd.org") "工作安排")
 	 "* TODO [#B] %?\n  %i\n"
 	 :empty-lines 1)
 	("c" "Chrome" entry (file+headline "~/.emacs.d/gtd.org" "Quicknotes")
                "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
                :empty-lines 1)
 	))
)


(provide 'init-org)
