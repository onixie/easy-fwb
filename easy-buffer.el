;;; easy-buffer.el --- Easy Buffer

;; Copyright (C) 2018 Yc.S

;; Author: Yc.S <onixie@gmail.com>
;; Created: 14 Jul 2010
;; Keywords: 
;; Homepage: https://github.com/onixie/easy-fwb

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-.emacs ()
  (interactive)
  (find-file "~/.emacs"))

(defun switch-to-calender ()
  (interactive)
  (if (or (not (fboundp 'calendar-exit)) 
	  (null (calendar-exit)))
      (calendar)))

(defun kill-other-buffers (&rest buffers-not-to-kill)
  "Kill buffers not listed in arguements. 
If the arguements are nil, all buffers except current buffer will be killed"
  (interactive)
  (let ((buffers-all (buffer-list))
	(buffers-not-to-kill (or buffers-not-to-kill (list (current-buffer))))
	(kill-buffer-query-functions nil))
    (mapc 'kill-buffer
	  (cl-remove-if (lambda (buffer)
			  (memq buffer buffers-not-to-kill))
			buffers-all))))

(defvar easy-buffer-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "<delete>") 'kill-buffer-and-window)
    (define-key map (kbd "<kp-delete>") (kbd "<delete>"))
    (if (not window-system)
	(define-key map (kbd "<deletechar>") (kbd "<delete>")))
    
    (define-key map (kbd "S-<delete>")
      (lambda ()
	(interactive)
	(kill-other-buffers (current-buffer)
			    (get-buffer "*scratch*")
			    (get-buffer "*Messages*")
			    (get-buffer "*ielm*")
			    (get-buffer "*eshell*"))
	(call-interactively 'delete-other-windows)))

    (define-key map (kbd "C-S-M-g") 'revert-buffer)

    (define-key map (kbd "<f10>") nil) ; Conflict with GDB's key binding for gud-step
    (define-key map (kbd "<f1> <f10>") 'menu-bar-open)

    (define-key map (kbd "C-`") 'switch-to-calender)

    (define-key map (kbd "<home>") 'ielm)
    (define-key map (kbd "<kp-home>") 'ielm)

    (define-key map (kbd "<end>") 'switch-to-.emacs)
    (define-key map (kbd "<kp-end>") 'switch-to-.emacs)
    (when (not window-system)
      (define-key map (kbd "<select>") 'switch-to-.emacs))

    (define-key map (kbd "<insert>") 'switch-to-messages)
    (define-key map (kbd "<kp-insert>") 'switch-to-messages)
    (when (not window-system)
      (define-key map (kbd "<insertchar>") 'switch-to-messages))

    (define-key map (kbd "<kp-left>") 'switch-to-scratch)
    (define-key map (kbd "<kp-up>") 'eshell)
    (define-key map (kbd "<kp-begin>") 'eww)
    (define-key map (kbd "<kp-down>") 'erc)

    map))

(define-key Buffer-menu-mode-map (kbd "C-m") 
  (lambda ()
    (interactive)
    (mapc 'call-interactively '(Buffer-menu-this-window delete-other-windows))))

(define-key Buffer-menu-mode-map (kbd "e") (kbd "C-m"))

(define-minor-mode easy-buffer-mode
  "Keymap for manipulating buffer
   \{KEYMAP}"
  :init-value t
  :lighter " Easy-B"
  :keymap easy-buffer-mode-map
  :group 'easy-fwb
  :global t)

(provide 'easy-buffer)
