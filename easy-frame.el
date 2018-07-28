;;; easy-frame.el --- Easy Frame

;; Author: Yc.S <onixie@gmail.com>
;; URL: https://github.com/onixie/easy-fwb
;; Version: 0.0.1

;;; Commentary:

;; Copyright (c) 2018, Yc.S

;;; Code:

(eval-when-compile (require 'cl))

(defcustom easy-frame-default-ratio 0.85
  "The default ratio of the workarea to take up for centered-fill mode."
  :type 'number
  :group 'easy-frame)

(defcustom easy-frame-auto-smart-fill t
  "Fill new frame automatically and smartly."
  :type 'boolean
  :group 'easy-frame)

(defcustom easy-frame-initial-fill 'smart
  "Default fill method for first frame."
  :type 'symbol
  :group 'easy-frame)

(defcustom easy-frame--precise-1 2
  "Precision parameter 1 to adjust frame position and size.
This parameter controls the times to execute BODY in easy-frame--ensure-do"
  :type 'integer
  :group 'easy-frame)

(defcustom easy-frame--precise-2 0.10
  "Precision parameter 2 to adjust frame position and size.
Delay in seconds for starting adjust the first frame."
  :type 'number
  :group 'easy-frame)

(defsubst easy-frame--desktop (&optional frame)
  "Return 0-based index of the desktop in which FRAME or the currently selected frame is placed."
  (condition-case nil
      (or (x-window-property "_NET_WM_DESKTOP" nil "AnyPropertyType"
			     (string-to-number (frame-parameter frame 'outer-window-id)) nil t)
	  (error "_NET_WM_DESKTOP not supported"))
    (error nil 0)))

(defsubst easy-frame--workarea (&optional frame)
  "Return the workarea (x y width height) in the desktop in which FRAME or the currently selected frame is."
  (condition-case nil
      (let ((desk-ind (easy-frame--desktop frame))
	    (workareas (or (x-window-property "_NET_WORKAREA" nil "AnyPropertyType" 0 nil t)
			   (error "_NET_WORKAREA not supported"))))
	(cl-subseq workareas (* 4 desk-ind) (* 4 (1+ desk-ind))))
    (error nil (list 0 0 (display-pixel-width) (display-pixel-height)))))

(defsubst easy-frame--extents (&optional frame)
  "Return the border (left right top bottom) of FRAME or the currently selected frame."
  (condition-case nil
      (or (x-window-property "_NET_FRAME_EXTENTS" nil "AnyPropertyType"
			     (string-to-number (frame-parameter frame 'outer-window-id)) nil t)
	  (error "_NET_FRAME_EXTENTS not supported"))
    (error nil (list 0 0 0 0))))

;;; Kludge: I have to set multiple times to make the resizing precisely.
(defmacro easy-frame--ensure-do (frame &rest body)
  "Macro to ensure WM messages work on FRAME by running BODY multiple times."
  `(let ((,@frame))
     (dotimes (i easy-frame--precise-1)
       (x-send-client-message nil 0 ,(car frame) "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_FULLSCREEN" 0))
       (x-send-client-message nil 0 ,(car frame) "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 ,(car frame) "_NET_WM_STATE" 32 '(0 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
     (dotimes (i easy-frame--precise-1)
       ,@body)))

(defsubst easy-frame--resize (width height &optional frame)
  "Resize to WIDTH * HEIGHT in pixel size for FRAME or the current selected frame."
  (easy-frame--ensure-do (frame frame)
			 (let ((flags (cond ((and (integerp height) (integerp width)) #x0001FC0A)
					    ((integerp width) #x0001F40A)
					    ((integerp height) #x0001F80A)
					    (t #x0001F000))))
			   (x-send-client-message nil 0 frame "_NET_MOVERESIZE_WINDOW" 32 `(,flags 0 0 ,(or width 0) ,(or height 0))))))

(defun easy-frame-fill-workarea (&optional frame dir ratio)
  "Resize FRAME in direction DIR so that it takes up RATIO or EASY-FRAME-DEFAULT-RATIO (center) or 1/2 (non-center) part of the workarea."
  (interactive)
  (easy-frame--ensure-do (frame frame)
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
					  (cl-case dir
					    ((center) easy-frame-default-ratio)
					    (t 0.5)))))
			     (if (>= ratio 1)
				 (easy-frame-fill-workarea frame)
			       (modify-frame-parameters frame
							`((ffw-state . ,(cl-case dir
									  ((upper down left right upper-left upper-right down-left down-right center) dir)
									  (t 'fill)))))
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
				     ((center)
				      (cl-values (+ left (/ (* width (- 1 ratio)) 2))
						 (+ top (/ (* height (- 1 ratio)) 2))
						 (- (* width ratio) left-extent right-extent)
						 (- (* height ratio) top-extent bottom-extent)))
				     (t
				      (cl-values left
						 top
						 (- width left-extent right-extent)
						 (- height top-extent bottom-extent))))
				 (easy-frame--resize (floor w) (floor h) frame)
				 (set-frame-position (or frame (selected-frame)) (floor x) (floor y))))))))

(defun easy-frame-toggle-fill-workarea (&optional frame)
  "Toggle fill workarea with FRAME or the current selected frame between full-fill and centered-fill mode."
  (interactive)
  (if (cl-equalp (frame-parameter frame 'ffw-state) 'center)
      (easy-frame-fill-workarea frame)
    (easy-frame-fill-workarea frame 'center)))

(defun easy-frame-smart-fill-workarea (&optional frame)
  "Resize FRAME or the current selected frame so that it fills workarea smartly."
  (interactive)
  (cl-flet ((collect-ffw-state (frame)
			       (let* ((di (easy-frame--desktop frame))
				      (fl (cl-remove-if (lambda (fr) (/= di (easy-frame--desktop fr)))
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
      (cond ((null ffw-states) (easy-frame-fill-workarea frame))
	    ((member 'fill ffw-states) (easy-frame-fill-workarea frame 'center))
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
		 (cond ((null dirs) (easy-frame-fill-workarea frame 'center))
		       (t (easy-frame-fill-workarea frame (cl-first (remove-states dirs ffw-states))))))))))))

(defmacro easy-frame--define-fill-workarea-key (map key dir)
  "In MAP, define KEY for each frame-fill-<DIR>-workarea function."
  (cl-flet ((symbolize (&rest name-parts)
		       (intern (apply #'concat
				      (mapcar (lambda (name)
						(cond ((symbolp name) (symbol-name name))
						      ((numberp name) (number-to-string name))
						      ((stringp name) name)
						      (t (error "Fail to symbolize due to invalid part: %s" name))))
					      name-parts)))))
    (let ((func-name (symbolize "frame-fill-" dir "-workarea")))
      `(progn
	 (unless (fboundp ',func-name)
	   (defun ,func-name ()
	     (interactive)
	     (easy-frame-fill-workarea nil ',dir)))
	 (define-key ,map ,key ',func-name)))))

(when easy-frame-auto-smart-fill
  (add-to-list 'after-make-frame-functions
	       (lambda (fr)
		 ;; Make sure it start after all other actions
		 (when easy-frame-auto-smart-fill
		   (run-at-time easy-frame--precise-2 nil #'easy-frame-smart-fill-workarea fr)))))

(defvar easy-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-frame--define-fill-workarea-key map (kbd "M-<up>") upper)
    (define-key map (kbd "<M-kp-up>") (kbd "M-<up>"))

    (easy-frame--define-fill-workarea-key map (kbd "M-<down>") down)
    (define-key map (kbd "<M-kp-down>") (kbd "M-<down>"))

    (easy-frame--define-fill-workarea-key map (kbd "M-<left>") left)
    (define-key map (kbd "<M-kp-left>") (kbd "M-<left>"))

    (easy-frame--define-fill-workarea-key map (kbd "M-<right>") right)
    (define-key map (kbd "<M-kp-right>") (kbd "M-<right>"))

    (easy-frame--define-fill-workarea-key map (kbd "<M-kp-home>") upper-left)
    (easy-frame--define-fill-workarea-key map (kbd "<M-kp-end>") down-left)
    (easy-frame--define-fill-workarea-key map (kbd "<M-kp-next>") down-right)
    (easy-frame--define-fill-workarea-key map (kbd "<M-kp-prior>") upper-right)

    (define-key map (kbd "<M-kp-begin>") #'easy-frame-toggle-fill-workarea)
    (define-key map (kbd "<M-kp-enter>") #'easy-frame-smart-fill-workarea)

    (define-key map (kbd "<M-kp-add>") (kbd "C-x 5 2"))
    (define-key map (kbd "<M-kp-subtract>") (kbd "C-x 5 0"))

    (define-key map (kbd "C-x 9") #'easy-frame-fill-workarea)
    (define-key map (kbd "C-x 7") (lambda () (interactive) (easy-frame-fill-workarea nil 'center)))

    map))

(define-minor-mode easy-frame-mode
  "Keymap for manipulating frame
   \{KEYMAP}"
  :require 'easy-frame
  :init-value t
  :lighter " Easy-F"
  :keymap easy-frame-mode-map
  :group 'easy-fwb
  :global t
  :set (run-at-time easy-frame--precise-2 nil
		    (lambda ()
		      (cl-case easy-frame-initial-fill
			((fill) (easy-frame-fill-workarea))
			((center) (easy-frame-fill-workarea nil 'center))
			((upper) (easy-frame-fill-workarea nil 'upper))
			((left) (easy-frame-fill-workarea nil 'left))
			((smart) (easy-frame-smart-fill-workarea))))))

(add-hook 'window-setup-hook 'easy-frame-mode)

(provide 'easy-frame)

;;; easy-frame.el ends here
