;;; easy-window.el --- Easy Window

;; Copyright (C) 2018 Yc.S

;; Author: Yc.S <onixie@gmail.com>
;; Created: 14 Jul 2010
;; Keywords: 
;; Homepage: https://github.com/onixie/easy-fwb

;;;;;;;;;;;;;;;;  Easy Buffer and Window  ;;;;;;;;;;;;;;;;

(require 'cl)
(require 'windmove)

(defun symbolize (&rest nameparts)
  "Make a symbol by concating the strings/symbols as its name"
  (intern (apply #'concat 
		 (mapcar #'(lambda (name)
			     (cond ((symbolp name) (symbol-name name))
				   ((numberp name) (number-to-string name))
				   ((stringp name) name)
				   (t (error "can't be glue together"))))
			 nameparts))))

(defmacro kill-window-along-direction (direction &optional keymap)
  "Define buffer killing function for direction"
  `(progn 
     (if (keymapp ,keymap)
	 (let ((key (concat "<" ,(symbol-name direction) ">")))
	   (define-key ,keymap (read-kbd-macro key)
	     ',(symbolize "kill-" direction "-window"))))
     (defun ,(symbolize "kill-" direction "-window") ()
       (interactive)
       (save-selected-window
	 (if (not (null (condition-case err 
			    (,((lambda (direction)
				 (symbolize "windmove-" direction)) direction))
			  (error nil))))
	     ;; Do not kill buffer as such simple way, 
	     ;; or you might lose origninal window.
	     ;; (kill-buffer-and-window)
	     (delete-window))))))

(defmacro windmove-diagonal (hori vert)
  `(progn
     (defun ,(symbolize "windmove-" hori "-" vert) ()
       (interactive)
       (condition-case nil
	   (windmove-do-window-select ',hori)
	 (error
	  (mapc #'windmove-do-window-select '(,vert ,hori))
	  (return nil)))
       (windmove-do-window-select ',vert))
     ',(symbolize "windmove-" hori "-" vert)))

(defun other-window-by-name (name &optional win)
  "Move cursor to the window with buffer named like NAME by other-window"
  (interactive "sName:")
  (let* ((current (or win (selected-window)))
	 (next (next-window current nil t)) ;; minibuffer cannot use other-window
	 (count 1)
	 (run t))
    (while run
      (if (string-match name (buffer-name (window-buffer next)))
	  (progn
	    (other-window count t)
	    (setq run nil))
	(progn 
	  (setq count (1+ count))
	  (setq next (next-window next nil t))))
      (if (eq next current)
	  (setq run nil)))))

(defvar easy-window-mode-map
  (let ((map (make-sparse-keymap)))
    (kill-window-along-direction left map)
    (kill-window-along-direction right map)
    (kill-window-along-direction up map)
    (kill-window-along-direction down map)

    (define-key map (kbd "<C-left>") 'windmove-left)
    (define-key map (kbd "<C-right>") 'windmove-right)
    (define-key map (kbd "<C-up>") 'windmove-up)
    (define-key map (kbd "<C-down>") 'windmove-down)

    (define-key map (kbd "<C-kp-left>") 'windmove-left)
    (define-key map (kbd "<C-kp-right>") 'windmove-right)
    (define-key map (kbd "<C-kp-up>") 'windmove-up)
    (define-key map (kbd "<C-kp-down>") 'windmove-down)

    (define-key map (kbd "<C-kp-home>") (windmove-diagonal left up))
    (define-key map (kbd "<C-kp-prior>") (windmove-diagonal right up))
    (define-key map (kbd "<C-kp-end>") (windmove-diagonal left down))
    (define-key map (kbd "<C-kp-next>") (windmove-diagonal right down))

    (define-key map (kbd "C-x C-b") 'windmove-list-buffer)

    map))

(defun windmove-list-buffer ()
  (interactive)
  (call-interactively 'list-buffers)
  (other-window-by-name "*Buffer List*"))

(define-key Buffer-menu-mode-map (kbd "C-m") 
  (lambda ()
    (interactive)
    (mapc 'call-interactively '(Buffer-menu-this-window delete-other-windows))))

(define-key Buffer-menu-mode-map (kbd "e") (kbd "C-m"))

(define-minor-mode easy-window-mode
  "Keymap for manipulating window
   \{KEYMAP}"
  :init-value t
  :lighter " Easy-W"
  :keymap easy-window-mode-map
  :group 'easy-fwb
  :global t)

(provide 'easy-window)
