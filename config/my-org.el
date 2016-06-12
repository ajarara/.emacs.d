(setq org-directory "~/Documents/org/")

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "<f5>") `org-capture)


;; capture templates that work, as of now.
;; for more info, check out http://orgmode.org/manual/Capture-templates.html
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("e" "Emacs" entry (file+datetree "~/Documents/org/emacs.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	)
      )
