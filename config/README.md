# Config

This directory holds all the base configuration. Init.el simply loads all files in this directory. Everything stored in the aux directory within this folder is only loaded when called from the config files in this level. This is mostly so that I can easily disable packages without having to hide files (ie evil-leader, when I'm feeling more holy).

One reason I do it this way is that I can create new config files on the fly and have them executed without going into my init file. Which means I can use a keybinding to prompt for any of these, and essentially navigate by section. (I'm referring to "C-, C-c", bound in my-emacs-leader-bindings) (also yes I call them leaders. whatchu gonna do about it)