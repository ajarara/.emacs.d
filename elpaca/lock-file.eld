((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id ace-window :type git
                       :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id avy :type git :protocol https
                :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (bui :source "elpaca-menu-lock-file" :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id bui :type git :protocol https
                :inherit t :depth treeless :ref
                "4319e1bf3ff94ff0568eed280ac1f980a4b68679"))
 (circe :source "elpaca-menu-lock-file" :recipe
        (:package "circe" :repo "emacs-circe/circe" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id circe :type git :protocol https
                  :inherit t :depth treeless :ref
                  "4bd1e964066fe8985d8112ed9172db3a1ecf58c8"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "MELPA" :id company :type git :protocol
                    https :inherit t :depth treeless :ref
                    "1cc907ac9e46ae4209eb5a341131787e0c678406"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat"
                    . "compat")
                   :tar "31.0.0.2" :host gnu :files
                   ("*" (:exclude ".git")) :source "GNU ELPA" :id
                   compat :type git :protocol https :inherit t :depth
                   treeless :ref
                   "90880f81419577e1d3f68424d2a3adf31e6d663e"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id cond-let :type git :protocol https
             :inherit t :depth treeless :ref
             "3b88187fe067d4ca3dec3ef8a329b0ce18bdb356"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id consult :type git :protocol
                    https :inherit t :depth treeless :ref
                    "b7558f65374b733b749147b2c299b93d7b939483"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :id dash
                 :type git :protocol https :inherit t :depth treeless
                 :ref "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (desktop-environment :source "elpaca-menu-lock-file" :recipe
                      (:package "desktop-environment" :fetcher github
                                :repo
                                "DamienCassou/desktop-environment"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :id
                                desktop-environment :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "1f16fa0fcc1b5b33773a8e334a60133a046e9fc7"))
 (detached :source "elpaca-menu-lock-file" :recipe
           (:package "detached" :fetcher sourcehut :repo
                     "niklaseklund/detached.el" :old-names (dtache)
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id detached :type git :protocol
                     https :inherit t :depth treeless :ref
                     "6b64d4d8064cee781e071e825857b442ea96c3d9"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo
                     "myrjola/diminish.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id diminish :type git :protocol
                     https :inherit t :depth treeless :ref
                     "43d0b9e0d18e4d124e3f7dad9d73275fb678fa50"))
 (direnv :source "elpaca-menu-lock-file" :recipe
         (:package "direnv" :fetcher github :repo
                   "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id direnv :type git :protocol
                   https :inherit t :depth treeless :ref
                   "c1f38f71184f8aa3130898932f2de638beb5ed33"))
 (dumb-jump :source "elpaca-menu-lock-file" :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id dumb-jump :type git
                      :protocol https :inherit t :depth treeless :ref
                      "cf06b4ccdce6a39346c32f05139f9ee8b77ee229"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
                (:package "edit-indirect" :fetcher github :repo
                          "Fanael/edit-indirect" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id edit-indirect :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "82a28d8a85277cfe453af464603ea330eae41c05"))
 (eglot :source "elpaca-menu-lock-file" :recipe
        (:package "eglot" :repo
                  ("https://github.com/emacs-mirror/emacs" . "eglot")
                  :tar "1.24" :host gnu :branch "master" :files
                  ("lisp/progmodes/eglot.el"
                   "doc/emacs/doclicense.texi"
                   "doc/emacs/docstyle.texi" "doc/misc/eglot.texi"
                   "etc/EGLOT-NEWS" (:exclude ".git"))
                  :source "GNU ELPA" :id eglot :type git :protocol
                  https :inherit t :depth treeless :ref
                  "1ca500605d2ac25b99712c908c6b977fd74fccbf"))
 (eldoc :source "elpaca-menu-lock-file" :recipe
        (:package "eldoc" :repo
                  ("https://github.com/emacs-mirror/emacs" . "eldoc")
                  :tar "1.16.0" :host gnu :branch "master" :files
                  ("lisp/emacs-lisp/eldoc.el" (:exclude ".git"))
                  :source "GNU ELPA" :id eldoc :type git :protocol
                  https :inherit t :depth treeless :ref
                  "1ca500605d2ac25b99712c908c6b977fd74fccbf"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "5b0cbb19421ef20c140b46a7b1fb7d04240b53f6" :depth 1
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build
                               (:not elpaca-source elpaca-build-docs)
                               :source "Elpaca extensions" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "5b0cbb19421ef20c140b46a7b1fb7d04240b53f6"))
 (esxml :source "elpaca-menu-lock-file" :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el") :source "MELPA" :id
                  esxml :type git :protocol https :inherit t :depth
                  treeless :ref
                  "6a375888a74b7563eb53176aad81faee8b858189"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo
                          "magnars/expand-region.el" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id expand-region :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (flymake :source "elpaca-menu-lock-file" :recipe
          (:package "flymake" :repo
                    ("https://github.com/emacs-mirror/emacs"
                     . "flymake")
                    :tar "1.4.7" :host gnu :branch "master" :files
                    ("lisp/progmodes/flymake.el"
                     "doc/emacs/doclicense.texi"
                     "doc/emacs/docstyle.texi" "doc/misc/flymake.texi"
                     (:exclude ".git"))
                    :source "GNU ELPA" :id flymake :type git :protocol
                    https :inherit t :depth treeless :ref
                    "1ca500605d2ac25b99712c908c6b977fd74fccbf"))
 (geiser :source "elpaca-menu-lock-file" :recipe
         (:package "geiser" :fetcher codeberg :repo "geiser/geiser"
                   :files ("elisp/*.el" "doc/dir" "doc/geiser.texi")
                   :source "MELPA" :id geiser :type git :protocol
                   https :inherit t :depth treeless :ref
                   "1d7b08e989df3933ea0b4414ebfbdd99c3cc24ed"))
 (geiser-guile :source "elpaca-menu-lock-file" :recipe
               (:package "geiser-guile" :fetcher codeberg :repo
                         "geiser/guile" :files
                         (:defaults ("src" "src/*")) :source "MELPA"
                         :id geiser-guile :type git :protocol https
                         :inherit t :depth treeless :ref
                         "fd732bfb4c7c7881ab50bf58c15c9348bd1840db"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo
                    "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id general :type git :protocol
                    https :inherit t :depth treeless :ref
                    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (ghostel :source "elpaca-menu-lock-file" :recipe
          (:package "ghostel" :fetcher github :repo "dakra/ghostel"
                    :files
                    (:defaults "etc" "src" "vendor" "build.zig"
                               "build.zig.zon" "symbols.map")
                    :source "MELPA" :id ghostel :type git :protocol
                    https :inherit t :depth treeless :ref
                    "9bf8c7a7f624eaba60d46b7b1123d29c39a2f4da"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id git-link :type git :protocol
                     https :inherit t :depth treeless :ref
                     "ca01d013bd575710e2cd47001ee1ef6ee41667cf"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id gptel :type git :protocol https
                  :inherit t :depth treeless :ref
                  "ecc693d69e0f9737fa637abc88ec4ce0c3187b8e"))
 (guix :source "elpaca-menu-lock-file" :recipe
       (:package "guix" :fetcher codeberg :repo "guix/emacs-guix"
                 :files
                 ("elisp/*.el" "doc/*.texi" ("images" "images/*.svg")
                  ("scheme" "scheme/*")
                  (:exclude "scheme/Makefile.am"))
                 :source "MELPA" :id guix :type git :protocol https
                 :inherit t :depth treeless :ref
                 "c70d2fd14e1d88df937affb7cc0972df5b4e7d3d"))
 (jsonrpc :source "elpaca-menu-lock-file" :recipe
          (:package "jsonrpc" :repo
                    ("https://github.com/emacs-mirror/emacs"
                     . "jsonrpc")
                    :tar "1.0.29" :host gnu :branch "master" :files
                    ("lisp/jsonrpc.el" (:exclude ".git")) :source
                    "GNU ELPA" :id jsonrpc :type git :protocol https
                    :inherit t :depth treeless :ref
                    "1ca500605d2ac25b99712c908c6b977fd74fccbf"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :id
                  llama :type git :protocol https :inherit t :depth
                  treeless :ref
                  "cfea618f14bc8317f8e4947fe10000b229b9a447"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("githooks" "githooks/*")
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :id magit :type git :protocol https
                  :inherit t :depth treeless :ref
                  "83ba66c8ab6fcdbd809077ae4db1f7e2ed832655"))
 (magit-popup :source "elpaca-menu-lock-file" :recipe
              (:package "magit-popup" :fetcher github :repo
                        "emacsattic/magit-popup" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-popup :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "d8585fa39f88956963d877b921322530257ba9f5"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :id magit-section :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "83ba66c8ab6fcdbd809077ae4db1f7e2ed832655"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id markdown-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "76cb4ffecfdf95ee769e5cb4608e04202c3c1521"))
 (monokai-theme :source "elpaca-menu-lock-file" :recipe
                (:package "monokai-theme" :repo
                          "oneKelvinSmith/monokai-emacs" :fetcher
                          github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id monokai-theme :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "dacd9d8a8867afea3ed76b15a6c997053ff88093"))
 (nov :source "elpaca-menu-lock-file" :recipe
      (:package "nov" :fetcher git :url
                "https://depp.brause.cc/nov.el.git" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id nov :type git :protocol https
                :inherit t :depth treeless :ref
                "874daf5e4791a6d4f47741422c80e2736e907351"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id orderless :type git
                      :protocol https :inherit t :depth treeless :ref
                      "5806e3f9401606d16962cffae68188c92deb1272"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id paredit :host github :repo
                    "emacsmirror/paredit" :type git :protocol https
                    :inherit t :depth treeless :ref
                    "af075775af91f2dbc63b915d762b4aec092946c4"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo
                     "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id posframe :type git :protocol
                     https :inherit t :depth treeless :ref
                     "435055dd6894fd4e8b21b355d40c0b289211b714"))
 (project :source "elpaca-menu-lock-file" :recipe
          (:package "project" :repo
                    ("https://github.com/emacs-mirror/emacs"
                     . "project")
                    :tar "0.12.0" :host gnu :branch "master" :files
                    ("lisp/progmodes/project.el" (:exclude ".git"))
                    :source "GNU ELPA" :id project :type git :protocol
                    https :inherit t :depth treeless :ref
                    "1ca500605d2ac25b99712c908c6b977fd74fccbf"))
 (sops-file :source "elpaca-menu-lock-file" :recipe
            (:source nil :package "sops-file" :id sops-file :host
                     github :repo "ajarara/sops-file.el" :type git
                     :protocol https :inherit t :depth treeless :ref
                     "e8cde267404c492d721a07b307520ccaac31974c"))
 (srfi :source "elpaca-menu-lock-file" :recipe
       (:package "srfi" :fetcher github :repo
                 "srfi-explorations/emacs-srfi" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id srfi :type git :protocol https
                 :inherit t :depth treeless :ref
                 "6b77411c4191dc26113658bae101a92828427c46"))
 (subed :source "elpaca-menu-lock-file" :recipe
        (:package "subed" :repo "sachac/subed" :tar "1.5.2" :host
                  github :files ("subed/*.el") :source "NonGNU ELPA"
                  :id subed :type git :protocol https :inherit t
                  :depth treeless :ref
                  "be52d53bf2ccb583f881e5663ad5a6fc28221396"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id transient :type git
                      :protocol https :inherit t :depth treeless :ref
                      "03c8ccc6aab24021787aada2be12d64cb1f436e8"))
 (treesit-auto :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-auto" :fetcher github :repo
                         "renzmann/treesit-auto" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id treesit-auto :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "3106c739c2a84bec2cb671997fe074e5dd5dd967"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "MELPA" :id vertico :type git
                    :protocol https :inherit t :depth treeless :ref
                    "a9998a777f1d92348f84d091bb15b87df933a7a2"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id which-key :type git
                      :protocol https :inherit t :depth treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id with-editor :type git :protocol https
             :inherit t :depth treeless :ref
             "5021ef6885381cf5b2852f7a3f67ca8c4be1dca2"))
 (xref :source "elpaca-menu-lock-file" :recipe
       (:package "xref" :repo
                 ("https://github.com/emacs-mirror/emacs" . "xref")
                 :tar "1.7.0" :host gnu :branch "master" :files
                 ("lisp/progmodes/xref.el" (:exclude ".git")) :source
                 "GNU ELPA" :id xref :type git :protocol https
                 :inherit t :depth treeless :ref
                 "1ca500605d2ac25b99712c908c6b977fd74fccbf")))
