;; Circe is described as an IRC client that is an intermediary between
;; erc and rcirc. It features pretty good default options (with one
;; exception that's really not all that important), and default
;; network configuration out of the box. It's written by
;; jorgenschaefer all around good person, who also is the lead on
;; elpy!

(use-package circe
  :straight t
  :config
  (setq circe-network-defaults nil)
  
    (setq circe-network-options
        (let ((server-passwd
               (lambda (server-name)
                 (read-passwd (format "Password for server: %s? " server-name)))))
            `(("ZNC/freenode"
           :tls t
           :host "jarmac.org"
           :port 5013
           :user "alphor/freenode"
           :pass ,server-passwd)
           ("ZNC/mozilla"
            :tls t
            :host "jarmac.org"
            :port 5013
            :user "alphor/mozilla"
            :pass ,server-passwd)
           ("ZNC/snoonet"
            :tls t
            :host "jarmac.org"
            :port 5013
            :user "alphor/snoonet"
            :pass ,server-passwd)
           ("ZNC/gitter"
            :tls t
            :host "jarmac.org"
            :port 5013
            :user "alphor/gitter"
            :pass ,server-passwd)
           ("local/i2p"
            :tls t
            :host "localhost"
            :port 6668)
           ("freenode"
            :tls t
            :host "chat.freenode.net"
            :port 6697))))
    ;; I have no idea why colored nicks are not enabled by default. Much
    ;; prettier! (This is the default option I was complaining about
    ;; earlier)
    (enable-circe-color-nicks)
    
    ;; Unfortunately, swiper calls font-lock-ensure-function which has
    ;; the annoying habit of washing out all the color. I add a
    ;; function to circe's mode hook that sets font-lock-ensure to the
    ;; ignore function.
    (add-hook 'circe-mode-hook
              (lambda ()
                (setq-local font-lock-ensure-function 'ignore)))

    ;; Don't bombard me with leaves if the leaver hasn't spoke in a while.
    (setq circe-reduce-lurker-spam t)

    (defun my-circe-intersect-nicks (buf1 buf2)
      "Does what you think it does. It would make a little sense to remove your own nick from this list, but meh"
      (interactive "b\nb")
      (let ((names1 (with-current-buffer (set-buffer buf1)
                      (circe-channel-nicks)))
            (names2 (with-current-buffer (set-buffer buf2)
                      (circe-channel-nicks))))
        (message (prin1-to-string (-intersection names1 names2))))))

;; Circe-actions is a package that handles events coming in from IRC.
;; Not published to Melpa, so pulling it in "manually".
;; Straight makes things so damn easy.
(straight-use-package
 '(circe-actions :type git :host github :repo "alphor/circe-actions"))
(use-package circe-actions)
;; (use-package circe-znc)

(provide 'setup-circe)
