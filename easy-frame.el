;;; easy-frame.el --- Easy Frame

;; Copyright (C) 2018 Yc.S

;; Author: Yc.S <onixie@gmail.com>
;; Created: 14 Jul 2010
;; Keywords: 
;; Homepage: https://github.com/onixie/easy-fwb

(defcustom default-unfillness 0.85
  "The default ratio of the workarea to fill when unfill it"
  :type 'number
  :group 'easy-frame)

(defcustom auto-smart-fill t
  "Smartly fill new frame automatically"
  :type 'boolean
  :group 'easy-frame)

(defcustom first-frame-x-workarea 'smart
  "Let first frame fill the whole workarea"
  :type 'symbol
  :group 'easy-frame)

(defcustom precise-1 2
  "Internal use for set frame position and size precisely"
  :type 'integer
  :group 'easy-frame)

(defcustom precise-2 0.10
  "Internal use for set frame position and size precisely"
  :type 'number
  :group 'easy-frame)

(defsubst easy-frame--desktop (&optional frame)
  "Return the 0-based index of desktop the frame posed in."
  (condition-case nil
      (or (x-window-property "_NET_WM_DESKTOP" nil "AnyPropertyType" 
			     (string-to-number (frame-parameter frame 'outer-window-id)) nil t)
	  (error "_NET_WORKAREA not supported"))
    (error nil 0)))

(defsubst easy-frame--workarea (&optional frame)
  "Return the allowed workarea (x y width height) from Window Manager in current desktop."
  (condition-case nil
      (let ((desk-ind (easy-frame--desktop frame))
	    (workareas (or (x-window-property "_NET_WORKAREA" nil "AnyPropertyType" 0 nil t)
			   (error "_NET_WORKAREA not supported"))))
	(cl-subseq workareas (* 4 desk-ind) (* 4 (1+ desk-ind))))
    (error nil (list 0 0 (display-pixel-width) (display-pixel-height)))))

(defsubst easy-frame--extents (&optional frame)
  "Return the border (left right top bottom) added by Window Manager"
  (condition-case nil
      (or (x-window-property "_NET_FRAME_EXTENTS" nil "AnyPropertyType" 
			     (string-to-number (frame-parameter frame 'outer-window-id)) nil t)
	  (error "_NET_FRAME_EXTENTS not supported"))
    (error nil (list 0 0 0 0))))

;;; Kludge: I have to set multiple times to make the resizing precisely.
(defmacro easy-frame--ensure-do (frame &rest body)
  "Get rid of the effect from wm"
  `(progn
     (dotimes (i precise-1)
       (x-send-client-message nil 0 ,frame "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_FULLSCREEN" 0))
       (x-send-client-message nil 0 ,frame "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 ,frame "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
     (dotimes (i precise-1)
       ,@body)))

(defsubst easy-frame--resize (width height &optional frame)
  "Resize the frame in pixel size."
  (easy-frame--ensure-do
   frame
   (let ((flags (cond ((and (integerp height) (integerp width)) #x0001FC0A)
		      ((integerp width) #x0001F40A)
		      ((integerp height) #x0001F80A)
		      (t #x0001F000))))
     (x-send-client-message nil 0 frame "_NET_MOVERESIZE_WINDOW" 32 `(,flags 0 0 ,(or width 0) ,(or height 0))))))

(defun frame-fill-workarea (&optional frame dir ratio)
  (interactive)
  (easy-frame--ensure-do
   frame
   (let* ((extents (easy-frame--extents frame))
	  (left-extent (aref extents 0))
	  (right-extent (aref extents 1))
	  (top-extent (aref extents 2))
	  (bottom-extent (aref extents 3))
	  (workarea (easy-frame--workarea frame))
	  (left (aref workarea 0))
	  (top (aref workarea 1))
	  (width (aref workarea 2))
	  (height (aref workarea 3)))
     (let ((ratio (if (and (numberp ratio) (<= ratio 1))
		      ratio
		    0.5)))
       (if (>= ratio 1)
	   (frame-fill-workarea frame)
	 (modify-frame-parameters
	  frame 
	  `((ffw-state . ,(cl-case dir
			    ((upper down left right 
				    upper-left upper-right down-left down-right)
			     dir)
			    (t
			     'fill)))))
	 (cl-multiple-value-bind (x y w h) 
	     (cl-case dir
	       ((upper)
		(cl-values left
			   top
			   (- width left-extent right-extent) 
			   (- (* height ratio) top-extent bottom-extent)))
	       ((down)
		(cl-values left
			   (+ top (* height (- 1 ratio)))
			   (- width left-extent right-extent)
			   (- (* height ratio) top-extent bottom-extent)))
	       ((left)
		(cl-values left
			   top
			   (- (* width ratio) left-extent right-extent)
			   (- height top-extent bottom-extent)))
	       ((right)
		(cl-values (+ left (* width (- 1 ratio)) )
			   top
			   (- (* width ratio) left-extent right-extent)
			   (- height top-extent bottom-extent)))
	       ((upper-left)
		(cl-values left
			   top
			   (- (* width ratio) left-extent right-extent)
			   (- (* height ratio) top-extent bottom-extent)))
	       ((upper-right)
		(cl-values (+ left (* width (- 1 ratio)))
			   top
			   (- (* width ratio) left-extent right-extent)
			   (- (* height ratio) top-extent bottom-extent)))
	       ((down-left)
		(cl-values left
			   (+ top (* height (- 1 ratio)))
			   (- (* width ratio) left-extent right-extent)
			   (- (* height ratio) top-extent bottom-extent)))
	       ((down-right)
		(cl-values (+ left (* width (- 1 ratio)))
			   (+ top (* height (- 1 ratio)))
			   (- (* width ratio) left-extent right-extent)
			   (- (* height ratio) top-extent bottom-extent)))
	       (t 
		(cl-values left
			   top
			   (- width left-extent right-extent)
			   (- height top-extent bottom-extent))))

	   (easy-frame--resize (floor w) (floor h) frame)
	   (set-frame-position (or frame (selected-frame)) (floor x) (floor y))))))
   ))

(defun frame-unfill-workarea (&optional frame ratio)
  (interactive)
  (easy-frame--ensure-do
   frame
   (let* ((extents (easy-frame--extents frame))
	  (left-extent (aref extents 0))
	  (right-extent (aref extents 1))
	  (top-extent (aref extents 2))
	  (bottom-extent (aref extents 3))
	  (workarea (easy-frame--workarea frame))
	  (left (aref workarea 0))
	  (top (aref workarea 1))
	  (width (aref workarea 2))
	  (height (aref workarea 3)))
     (let ((ratio (if (and (numberp ratio) (<= ratio 1))
		      ratio
		    default-unfillness)))
       (if (>= ratio 1)
	   (frame-fill-workarea frame)
	 (modify-frame-parameters frame '((ffw-state . unfill)))
	 (cl-multiple-value-bind (x y w h)
	     (cl-values (+ left (/ (* width (- 1 ratio)) 2))
			(+ top (/ (* height (- 1 ratio)) 2))
			(- (* width ratio) left-extent right-extent)
			(- (* height ratio) top-extent bottom-extent))
	   (easy-frame--resize (floor w) (floor h) frame)
	   (set-frame-position (or frame (selected-frame)) (floor x) (floor y))))))))

(defun frame-toggle-unfill (&optional frame)
  (interactive)
  (if (cl-equalp (frame-parameter frame 'ffw-state) 'unfill)
      (frame-fill-workarea frame)
    (frame-unfill-workarea)))

(defun frame-smart-fill-workarea (&optional frame)
  (interactive)
  (cl-flet ((collect-ffw-state (frame)
			       (let* ((di (easy-frame--desktop frame))
				      (fl (cl-remove-if #'(lambda (fr)
							    (/= di (easy-frame--desktop fr)))
							(frame-list))))
				 (cl-loop for fr in fl
					  when (not (cl-equalp frame fr))
					  collect (frame-parameter fr 'ffw-state))))
	    (remove-states (s1 s2)
			   (cl-loop for s in s1
				    when (not (member s s2))
				    collect s))
	    (contain-states (s1 s2)
			    (cl-subsetp s2 s1 :test #'cl-equalp)))
    (let ((ffw-states (collect-ffw-state (or frame (selected-frame)))))

      (cond ((null ffw-states) (frame-fill-workarea frame))
	    ((member 'fill ffw-states) (frame-unfill-workarea frame))
	    (t
	     (cl-flet ((remove-mb (dirs s1 &rest ss)
				  (if (member s1 ffw-states)
				      (remove-states dirs (cl-list* s1 ss))
				    dirs)))
	       (let* ((dirs '(upper down left right upper-left upper-right down-left down-right))
		      (dirs (remove-mb dirs 'upper 'upper-left 'upper-right 'left 'right))
		      (dirs (remove-mb dirs 'down 'down-left 'down-right 'left 'right))	
		      (dirs (remove-mb dirs 'left 'upper-left 'down-left 'upper 'down))
		      (dirs (remove-mb dirs 'right 'upper-right 'down-right 'upper 'down))
		      (dirs (remove-mb dirs 'upper-left 'upper 'left))
		      (dirs (remove-mb dirs 'upper-right 'upper 'right))
		      (dirs (remove-mb dirs 'down-right 'down 'right))
		      (dirs (remove-mb dirs 'down-left 'down 'left)))
		 (cond ((null dirs) (frame-unfill-workarea frame))
		       (t
			(frame-fill-workarea frame (cl-first (remove-states dirs ffw-states))))))))))))

(when auto-smart-fill
  (add-to-list 'after-make-frame-functions 
	       #'(lambda (fr)
		   ;; Make sure it start after all other actions
		   (when auto-smart-fill
		     (run-at-time precise-2 nil #'frame-smart-fill-workarea fr)))))

(defmacro define-frame-fill-workarea-key (map key dir)
  (require 'cl)
  (cl-flet ((name-glue (&rest nameparts)
		       (intern (apply #'concat 
				      (mapcar #'(lambda (name)
						  (cond ((symbolp name) (symbol-name name))
							((numberp name) (number-to-string name))
							((stringp name) name)
							(t (error "can't be glue together"))))
					      nameparts)))))

    `(progn
       (unless (fboundp ',(name-glue "frame-fill-" dir "-workarea"))
	 (defun ,(name-glue "frame-fill-" dir "-workarea") ()
	   (interactive)
	   (frame-fill-workarea nil ',dir)))
       (define-key ,map ,key ',(name-glue "frame-fill-" dir "-workarea")))))

(defvar easy-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-frame-fill-workarea-key map (kbd "M-<up>") upper)
    (define-key map (kbd "<M-kp-up>") (kbd "M-<up>"))
    (define-frame-fill-workarea-key map (kbd "M-<down>") down)
    (define-key map (kbd "<M-kp-down>") (kbd "M-<down>"))
    (define-frame-fill-workarea-key map (kbd "M-<left>") left)
    (define-key map (kbd "<M-kp-left>") (kbd "M-<left>"))
    (define-frame-fill-workarea-key map (kbd "M-<right>") right)
    (define-key map (kbd "<M-kp-right>") (kbd "M-<right>"))
    (define-frame-fill-workarea-key map (kbd "<M-kp-home>") upper-left)
    (define-frame-fill-workarea-key map (kbd "<M-kp-end>") down-left)
    (define-frame-fill-workarea-key map (kbd "<M-kp-next>") down-right)
    (define-frame-fill-workarea-key map (kbd "<M-kp-prior>") upper-right)
    (define-key map (kbd "<M-kp-begin>") #'frame-toggle-unfill)
    (define-key map (kbd "<M-kp-enter>") #'frame-smart-fill-workarea)
    (define-key map (kbd "<M-kp-add>") (kbd "C-x 5 2"))
    (define-key map (kbd "<M-kp-subtract>") (kbd "C-x 5 0"))
    (define-key map (kbd "C-x 9") #'frame-fill-workarea)
    (define-key map (kbd "C-x 7") #'frame-unfill-workarea)
    map))

(define-minor-mode easy-frame-mode
  "Keymap for manipulating frame
   \{KEYMAP}"
  :init-value t
  :lighter " Easy-F"
  :keymap easy-frame-mode-map
  :group 'easy-fwb
  :global t
  :set (run-at-time precise-2 nil
		    (lambda nil
		      (cl-case first-frame-x-workarea
			((fill) (frame-fill-workarea))
			((unfill) (frame-unfill-workarea))
			((upper) (frame-fill-workarea nil 'upper))
			((left) (frame-fill-workarea nil 'left))
			((smart) (frame-smart-fill-workarea))))))

(add-hook 'window-setup-hook 'easy-frame-mode)

(provide 'easy-frame)
