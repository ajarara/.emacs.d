# .emacs.d

All my config is in one file, built by tangling (extracting source from) myinit.org

To tangle any org file, have at least one source block. A source block has this syntax
#+BEGIN_SRC emacs-lisp tangle: outputfile.el
(kill-emacs) ; not recommended config
#+END_SRC

then press C-c C-v t