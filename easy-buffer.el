;;; easy-buffer.el --- Easy Buffer

;; Author: Yc.S <onixie@gmail.com>
;; URL: https://github.com/onixie/easy-fwb
;; Version: 0.0.1

;;; Commentary:

;; Copyright (c) 2018, 2019, Yc.S

;;; Code:

(require 'calendar)
(require 'eshell)
(require 'erc)
(require 'eww)
(require 'centaur-tabs)

(defun easy-buffer-switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun easy-buffer-switch-to-messages ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun easy-buffer-switch-to-.emacs ()
  "Open .emacs file buffer."
  (interactive)
  (find-file "~/.emacs"))

(defun easy-buffer-switch-to-calendar ()
  "Switch to *Calendar* buffer."
  (interactive)
  (if (or (not (fboundp 'calendar-exit))
	  (null (calendar-exit)))
      (calendar)))

(defun easy-buffer-kill-buffers-except (&rest buffers-not-to-kill)
  "Kill buffers not listed in arguement BUFFERS-NOT-TO-KILL.
If the arguements are nil, all buffers except current buffer will be killed"
  (interactive)
  (let ((buffers-all (buffer-list))
	(buffers-not-to-kill (or buffers-not-to-kill (list (current-buffer))))
	(kill-buffer-query-functions nil))
    (mapc 'kill-buffer
	  (cl-remove-if (lambda (buffer)
			  (memq buffer buffers-not-to-kill))
			buffers-all))))

;;; deprecated
;; (defmacro easy-buffer--tabbar-dwim-move (direction)
;;   (let ((dir-text (symbol-name direction)))
;;     `(lambda ()
;;        (interactive)
;;        (if tabbar--buffer-show-groups
;;            (progn
;;              (call-interactively #',(intern (concatenate 'string "tabbar-" dir-text "-group")))
;;              (call-interactively #'tabbar-press-home))
;;          (call-interactively #',(intern (concatenate 'string "centaur-tabbar-" dir-text)))))))

(defvar easy-buffer-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "S-<left>") 'centaur-tabs-backward)
    (define-key map (kbd "S-<right>") 'centaur-tabs-forward)

    (define-key map (kbd "S-<up>") (lambda () (interactive) (setq centaur-tabs--buffer-show-groups t)))
    (define-key map (kbd "S-<down>") (lambda () (interactive) (setq centaur-tabs--buffer-show-groups nil)))

    (define-key map (kbd "<delete>") 'kill-buffer-and-window)
    (define-key map (kbd "<kp-delete>") (kbd "<delete>"))
    (if (not window-system)
	(define-key map (kbd "<deletechar>") (kbd "<delete>")))
    
    (define-key map (kbd "S-<delete>")
      (lambda ()
	(interactive)
	(easy-buffer-kill-buffers-except (current-buffer)
                                         (get-buffer "*scratch*")
                                         (get-buffer "*Messages*")
                                         (get-buffer "*ielm*")
                                         (get-buffer "*eshell*"))
	(call-interactively 'delete-other-windows)))

    (define-key map (kbd "C-S-M-g") 'revert-buffer)

    (define-key map (kbd "<f10>") nil) ; Conflict with GDB's key binding for gud-step
    (define-key map (kbd "<f1> <f10>") 'menu-bar-open)

    (define-key map (kbd "C-`") 'easy-buffer-switch-to-calendar)

    (define-key map (kbd "<home>") 'ielm)
    (define-key map (kbd "<kp-home>") 'ielm)

    (define-key map (kbd "<end>") 'easy-buffer-switch-to-.emacs)
    (define-key map (kbd "<kp-end>") 'easy-buffer-switch-to-.emacs)
    (when (not window-system)
      (define-key map (kbd "<select>") 'easy-buffer-switch-to-.emacs))

    (define-key map (kbd "<insert>") 'easy-buffer-switch-to-messages)
    (define-key map (kbd "<kp-insert>") 'easy-buffer-switch-to-messages)
    (when (not window-system)
      (define-key map (kbd "<insertchar>") 'easy-buffer-switch-to-messages))

    (define-key map (kbd "<kp-left>") 'easy-buffer-switch-to-scratch)
    (define-key map (kbd "<kp-up>") 'eshell)
    (define-key map (kbd "<kp-begin>") 'eww)
    (define-key map (kbd "<kp-down>") 'erc)

    map))

(add-hook 'Buffer-menu-mode-hook
          (lambda ()
            (define-key Buffer-menu-mode-map (kbd "C-m")
              (lambda ()
                (interactive)
                (mapc 'call-interactively '(Buffer-menu-this-window delete-other-windows))))
            (define-key Buffer-menu-mode-map (kbd "e") (kbd "C-m"))
            ))

(defun easy-buffer--pop-up-vertically (buffer alist)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (display-buffer-pop-up-window buffer alist)))

(add-to-list 'display-buffer-alist
             '("*Buffer List*" easy-buffer--pop-up-vertically))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
            ))

(define-minor-mode easy-buffer-mode
  "Keymap for manipulating buffer
   \{KEYMAP}"
  :require 'easy-buffer
  :init-value t
  :lighter " Easy-B"
  :keymap easy-buffer-mode-map
  :group 'easy-fwb
  :global t)

(provide 'easy-buffer)

;;; easy-buffer.el ends here
