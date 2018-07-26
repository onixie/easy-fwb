;;; easy-window.el --- Easy Window

;; Author: Yc.S <onixie@gmail.com>
;; URL: https://github.com/onixie/easy-fwb
;; Version: 0.0.1

;;; Commentary:

;; Copyright (c) 2018, Yc.S

;;; Code:

(eval-when-compile (require 'cl))
(require 'windmove)

(eval-when-compile
  (defun symbolize (&rest name-parts)
    "Create a symbol with name formed by concatenating the rest arguments NAME-PARTS."
    (intern (apply #'concat
		   (mapcar (lambda (name)
			     (cond ((symbolp name) (symbol-name name))
				   ((numberp name) (number-to-string name))
				   ((stringp name) name)
				   (t (error "Fail to symbolize due to invalid part: %s" name))))
			   name-parts)))))

(defmacro define-kill-window-along-direction (direction &optional keymap)
  "Define kill-<DIRECTION>-window function and key when an optional KEYMAP is given."
  (let ((func-name (symbolize "kill-" direction "-window")))
    `(progn
       (if (keymapp ,keymap)
	   (let ((key (concat "<" ,(symbol-name direction) ">")))
	     (define-key ,keymap (read-kbd-macro key)
	       ',func-name)))
       (defun ,func-name ()
	 (interactive)
	 (save-selected-window
	   (if (not (null (condition-case err
			      (,((lambda (direction)
				   (symbolize "windmove-" direction)) direction))
			    (error nil))))
	       ;; Do not kill buffer as such simple way,
	       ;; or you might lose origninal window.
	       ;; (kill-buffer-and-window)
	       (delete-window)))))))

(defmacro windmove-diagonal (hori vert)
  "Macro to define windmove-<HORI>-<VERT> function."
  (let ((func-name (symbolize "windmove-" hori "-" vert)))
    `(progn
       (defun ,func-name ()
	 (interactive)
	 (condition-case nil
	     (windmove-do-window-select ',hori)
	   (error
	    (mapc #'windmove-do-window-select '(,vert ,hori))
	    (return nil)))
	 (windmove-do-window-select ',vert))
       ',func-name)))

(defun other-window-by-name (name &optional win)
  "Move cursor to the window with buffer named NAME, from optional argument WIN or current selected window."
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

(defun windmove-list-buffer ()
  "Move to *Buffer List* buffer."
  (interactive)
  (call-interactively 'list-buffers)
  (other-window-by-name "*Buffer List*"))

(defvar easy-window-mode-map
  (let ((map (make-sparse-keymap)))
    (define-kill-window-along-direction left map)
    (define-kill-window-along-direction right map)
    (define-kill-window-along-direction up map)
    (define-kill-window-along-direction down map)

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

(define-key Buffer-menu-mode-map (kbd "C-m")
  (lambda ()
    (interactive)
    (mapc 'call-interactively '(Buffer-menu-this-window delete-other-windows))))

(define-key Buffer-menu-mode-map (kbd "e") (kbd "C-m"))

(define-minor-mode easy-window-mode
  "Keymap for manipulating window
   \{KEYMAP}"
  :require 'easy-window
  :init-value t
  :lighter " Easy-W"
  :keymap easy-window-mode-map
  :group 'easy-fwb
  :global t)

(provide 'easy-window)

;;; easy-window.el ends here
