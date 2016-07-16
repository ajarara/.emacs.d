;; should be loaded LAST
(defvar my-prefix "C-j ")
(defun my/prefix-add-to-map (map key-as-string function-symbol)
  (define-key map (kbd (concat my-prefix key-as-string)) function-symbol))

(defvar my/mode-map
  (let ((my-map (make-keymap)))
    
    ;; top level binds
    (define-key my-map (kbd "C-q") `ace-window)
    (define-key my-map (kbd "C-z") `eshell)
;;    (define-key my-map (kbd "M-h") `switch-to-buffer)
    (define-key my-map (kbd "M-r") `delete-other-windows)
    (define-key my-map (kbd "M-w") `delete-window)
    (define-key my-map (kbd "M-q") `kill-buffer)
    ;; this doesn't work well, it gets rebound to tab. need another keybind. C-m doesn't work either, for the same stupid reason.
    ;;(define-key my-map (kbd "C-i") `my/kill-other-window)
    
    ;; muddling in other keymaps
    (define-key my-map (kbd "C-h SPC") `which-key-show-top-level)
    
    ;; under my/prefix
    (my/prefix-add-to-map my-map "V" `visual-line-mode)
    (my/prefix-add-to-map my-map "v" `split-window-horizontally)
    (my/prefix-add-to-map my-map "h" `split-window-vertically) ; yeah, it doesnt make sense.

    (my/prefix-add-to-map my-map "C-g" `keyboard-quit) 
    (my/prefix-add-to-map my-map "C-j" `ace-window)
    (my/prefix-add-to-map my-map "C-0" `text-scale-adjust)
    (my/prefix-add-to-map my-map "C-r" `org-capture)
    (my/prefix-add-to-map my-map "C-q" `quoted-insert)
    (my/prefix-add-to-map my-map "C-f" `ffap)
    (my/prefix-add-to-map my-map "C-s" `magit-status)
    
    ;; under my/prefix with a custom func
    (my/prefix-add-to-map my-map "C-t" `my/toggle-window-split)
    (my/prefix-add-to-map my-map "C-c" `my/find-emacs-config-file)
    (my/prefix-add-to-map my-map "C-p" `my/find-projects)
    my-map
    ))

;; all homemade functions can be found under this minor mode declaration
(define-minor-mode my/mode
  :diminish
  :global
  :keymap `my/mode-map
  )
;; evaluate it. considering moving to johnw's bind-key so that I can declare these keybinds in use-package configs
(my/mode)

	
(defun my/kill-other-window ()
  (interactive)
  (if (= (count-windows) 2)
      (progn
	(other-window 1)
	(kill-buffer))
    (error "This only works when there are two buffers!")))

		  
;; not mine, found off of emacs-wiki. quickly switches orientation of two buffers.
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; quick way to find configuration files
(defun my/find-emacs-config-file ()
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/config/"))

;; quick way to nav projects
(defun my/find-projects ()
  (interactive)
  (ido-find-file-in-dir "~/Documents/projects/"))
