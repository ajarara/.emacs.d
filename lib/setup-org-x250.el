;; config that relies on the filesystem. It might be possible to have a cross platform (ie including phone) setup but this works ok for now.

(use-package org
  :init
  (setq org-directory "~/Documents/org/")

  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq my-org-capture-directory "~/Documents/org/capture/")
  
  ;; org-capture is fantastic. if you're anything like me you have ideas
  ;; that come and go like the wind. this allows you to easily capture
  ;; those ideas without worrying about where to save them, whether or
  ;; not context is necessary, the directory structure. It's a massively
  ;; beefed up remember buffer. If you want something that just works
  ;; without any previous configuration, then try M-x remember, the file
  ;; will be saved in your .emacs.d directory, and it's called notes.
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/gtd-capture.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("e" "Emacs" entry (file+datetree "~/Documents/org/emacs.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("k" "KOL" entry (file+datetree "~/Documents/org/kol.org")
           "* %?\nEntered on %U\n %a")
          ("a" "ascension" entry (file+datetree "~/Documents/org/kol-ascension.org")
           "* %?\nEntered on %U\n %a")
          ("r" "track" entry (file+datetree "~/Documents/org/track.org")
           "* %?\nEntered on %U\n")
          ("d" "dose" entry (file+datetree "~/Documents/org/dose.org")
           "* %?\nEntered on %U\n")
          ("g" "grievances" entry (file+datetree "~/Documents/org/grievances.org")
           "* %?\nEntered on %U\n %i")
          ("p" "programming" entry (file+datetree "~/Documents/org/programming.org")
           "* %?\nEntered on %U\n  %i")
          ("l" "laptop" entry (file+datetree "~/Documents/org/laptop.org")
           "* %?\nEntered on %U\n %i")
          ("m" "music" entry (file+datetree "~/Documents/org/music.org")
           "* %?\nEntered on %U\n %i")
          ("u" "uncategorized-mess" entry (file+datetree "~/Documents/org/u-mess.org")
           "* %?\nEntered on %U\n")
          ("h" "recurse" entry (file+datetree "~/Documents/org/recurse.org")
           "* %?\nEntered on %U\n")
          ("c" "coffee" entry (file+datetree "~/Documents/org/coffee.org")
           "* %?\nEntered on %U\n"))))

(provide 'setup-org-x250)
