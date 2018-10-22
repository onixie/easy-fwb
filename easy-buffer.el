;;; easy-buffer.el --- Easy Buffer

;; Author: Yc.S <onixie@gmail.com>
;; URL: https://github.com/onixie/easy-fwb
;; Version: 0.0.1

;;; Commentary:

;; Copyright (c) 2018, Yc.S

;;; Code:

(require 'calendar)
(require 'eshell)
(require 'erc)
(require 'eww)
(require 'tabbar)

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

;;; Tabbar + buffer
(defvar easy-buffer--max-cycle-header-count 3)
(set-default (make-variable-buffer-local 'cycle-header-count) easy-buffer--max-cycle-header-count)
(set-default (make-variable-buffer-local 'cycle-header-format) header-line-format)
(defun easy-buffer--cycle-tabbar-press-home ()
  (interactive)
  (let* ((tbl-fmt '(:eval (tabbar-line)))
	 (cmd-key (this-command-keys))
	 (up-p (cl-equalp cmd-key (kbd "S-<up>")))
	 (down-p (cl-equalp cmd-key (kbd "S-<down>"))))
    (setq cycle-header-count
	  (cond (up-p (mod (decf cycle-header-count) easy-buffer--max-cycle-header-count))
		(down-p (mod (incf cycle-header-count) easy-buffer--max-cycle-header-count))
		(t easy-buffer--max-cycle-header-count)))
    (cond ((= cycle-header-count (1- easy-buffer--max-cycle-header-count))
	   (unless (cl-equalp header-line-format tbl-fmt)
	     (setq cycle-header-format header-line-format))
	   (setq header-line-format tbl-fmt)
	   (tabbar-press-home))
	  ((= cycle-header-count 0)
	   (unless (cl-equalp cycle-header-format tbl-fmt)
	     (setq header-line-format cycle-header-format))
	   (when (or down-p (cl-equalp cycle-header-format tbl-fmt))
	     (tabbar-press-home)))
	  (t (setq header-line-format tbl-fmt)
	     (when (or up-p (cl-equalp cycle-header-format tbl-fmt))
	       (tabbar-press-home))))))

(defmacro easy-buffer--tabbar-dwim-move (direction)
  (let ((dir-text (symbol-name direction)))
    `(lambda ()
       (interactive)
       (if tabbar--buffer-show-groups
           (progn
             (call-interactively #',(intern (concatenate 'string "tabbar-" dir-text "-group")))
             (call-interactively #'tabbar-press-home))
         (call-interactively #',(intern (concatenate 'string "tabbar-" dir-text)))))))

(defvar easy-buffer-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "S-<left>") (easy-buffer--tabbar-dwim-move backward))
    (define-key map (kbd "S-<right>") (easy-buffer--tabbar-dwim-move forward))

    (define-key map (kbd "S-<up>") 'easy-buffer--cycle-tabbar-press-home)
    (define-key map (kbd "S-<down>") 'easy-buffer--cycle-tabbar-press-home)

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

(define-key Buffer-menu-mode-map (kbd "C-m")
  (lambda ()
    (interactive)
    (mapc 'call-interactively '(Buffer-menu-this-window delete-other-windows))))

(define-key Buffer-menu-mode-map (kbd "e") (kbd "C-m"))

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
