This config is used across many different machines, so I homebrewed a 'profile' system. Profiles are minor modes that are capabilities. There really is no reason to have an IRC client on work machines, and no reason to clone org on remote machines. What this gets is faster startup, especially in the case of on demand virtual machines. When elpaca gets version lockfiles I'd probably switch to that.

Otherwise, this is as straightforward as it gets: use-package, straight.

There is some stuff in `lisp/`, nothing polished though.


