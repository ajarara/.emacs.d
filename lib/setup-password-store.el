;; I am migrating to bitwarden -- for the seamless 2fa setup on mobile
;; and for sharing passwords with family members. I cannot in good faith
;; ask my family to manage a gpg keychain.

;; There are still passwords in here, and unfortunately the emacs
;; support for bitwarden ain't great. But ya gotta do what you gotta do.
(use-package password-store :straight t)

;; Additionally, use emacs for pin entry
(use-package pinentry
  :straight t
  :config
  (pinentry-start))

(provide 'setup-password-store)
