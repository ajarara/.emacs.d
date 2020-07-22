;; man do I love this package. Lightweight, pretty, and FAST. Enabling
;; ivy mode globally gives most aspects of the minibuffer fuzzy typeahead
;; (like ido-mode) but it further allows you to keep your search session
;; if needed (especially useful when looking in help variables). Swiper
;; is used to search buffers, and uses ivy as a backend. It is possible
;; to use helm as a backend instead.

(use-package ivy
  :straight t
  :demand t
  :diminish ivy-mode
  :config
  (setq ivy-ignore-buffers `("\\` "))
  
  ;; i like completion in the minibuffer, completion in region is obnoxious when you have hl-line-mode active. This must be set before ivy-mode is called.
  (setcdr (assoc 'ivy-completion-in-region ivy-display-functions-alist) nil)

  (ivy-mode t))

(use-package swiper
  :config

  ;; almost required, I use search a lot for navigation, especially in
  ;;   this growing init file. Note that if multiple candidates are in a
  ;;   view moving between them does not recenter the buffer.
  (setq swiper-action-recenter t)

  ;; shadows isearch
  :bind* (("C-s" . swiper)))



(provide 'setup-swiper)
