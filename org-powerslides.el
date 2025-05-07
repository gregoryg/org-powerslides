;;; org-powerslides.el --- Minimalist Org Mode functions for presentations
;;
;; Copyright (C) 2023,2024 by Gregory Grubbs
;;
;; Author: Gregory Grubbs
;; Version: 0.1
;; Package-Requires: ((org "9"))
;; URL: https://github.com/gregoryg/org-powerslides

(defgroup org-powerslides nil
  "Use Org Mode as frictionless presentation."
  :group 'org-structure)

(defvar org-powerslides--image-buffers nil
  "List of image buffers opened/displayed during a presentation.")

(defcustom org-powerslides-same-level-only nil
  "If non-nil, restrict slides to the current outline level only.
When nil, every heading becomes a slide.  When t, only
the first level you start on is used—its children are shown
inside that same slide instead of becoming separate slides."
  :type 'boolean
  :group 'org-powerslides)

(defcustom org-powerslides-image-window-placement 'right
  "If defined, a cons cell indicating image window placement (left, right, above, below).  Default is to use other-window."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'org-powerslides)

(defcustom org-powerslides-presentation-frame-font ""
  "Preferred font for presentation.
See documentation for `set-frame-font'.
Example value: \"Hack-24\"."
  :type 'string
  :group 'org-powerslides)

(defcustom org-powerslides-post-image-load-hook nil
  "Normal hook that runs after an image for a slide has first been loaded."
  :type 'hook
  :group 'org-powerslides)

(setq org-powerslides--saved-frame-font nil)

(defun org-powerslides/show-random-slide-same-level ()
  "Show a random subtree at the same level as current point, or level 1 if outside the first heading.  This function may be most useful for a future extension of this package to handle flashcards using a similar Org Mode structure."
  (interactive)
  (let ((current-level (if (org-at-heading-p)
                           (org-current-level)
                         1)))
    (widen)
    (org-overview)
    (goto-char (point-min))
    (let ((same-level-headings '()))
      (while (re-search-forward (format "^\\*\\{%d\\} " current-level) nil t)
        (push (point) same-level-headings))
      (if (null same-level-headings)
          (user-error "No headings at current level: cannot pick a slide.")
        (goto-char (nth (random (length same-level-headings)) same-level-headings))
        (org-reveal t)
        (org-show-entry)
        (outline-show-children)
        (org-narrow-to-subtree)))))


;; new helper
(defun org-powerslides--move-to-slide (direction &optional no-narrow)
  "Move to the next or previous slide and display it.
DIRECTION is either 'next or 'prev.  If NO-NARROW is non-nil,
do not narrow the buffer to the subtree."
  (catch 'out-of-bounds
    (let* ((move-fn
            (pcase (list direction org-powerslides-same-level-only)
              (`(next t) (lambda () (org-forward-heading-same-level 1)))
              (`(next ,_) #'outline-next-heading)
              (`(prev t) (lambda () (org-backward-heading-same-level 1)))
              (`(prev ,_) #'outline-next-heading)
              (_ (error "Bad direction/same-level flag: %S" (list direction org-powerslides-same-level-only)))
              ))
           (orig (point)))
      (widen)
      ;; try the move, bail out if off-heading or off-buffer
      (funcall move-fn)
      (unless (and (< (point) (point-max))
                   (> (point) (point-min))
                   (org-at-heading-p))
        (goto-char orig)
        (message (if (eq direction 'next)
                     "End of outline reached."
                   "Top of outline reached."))
        (throw 'out-of-bounds nil))
      ;; —if we get here, move was good—continue with reveal, narrow, image, etc.
      ;; show just this subtree
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)
      (when (not org-powerslides-same-level-only)
        ;; when we're *not* restricting to same level, show everything under it
        (org-show-all))
      (unless no-narrow
        (org-narrow-to-subtree))
      ;; maybe display an image on the side
      (let ((img (org-entry-get nil "image")))
        (when img
          (org-powerslides--display-image
           (org-powerslides--select-random-image img)))))))

;; now redefine the public commands
(defun org-powerslides/show-next-slide (&optional no-narrow)
  "Advance to the next slide."
  (interactive "P")
  (org-powerslides--move-to-slide 'next no-narrow))

(defun org-powerslides/show-previous-slide (&optional no-narrow)
  "Go back to the previous slide."
  (interactive "P")
  (org-powerslides--move-to-slide 'prev no-narrow))

(defun org-powerslides--display-image (imgpath)
  "Load and size IMGPATH in other-window and remember the buffer."
  (let* ((current-window (selected-window))
         (buffer (find-file-noselect imgpath)))
    ;; record image buffer in internal var
    (unless (memq buffer org-powerslides--image-buffers)
      (push buffer org-powerslides--image-buffers))
    (when (and (boundp 'org-powerslides-image-window-placement) (one-window-p t)) ;; only one window, so split
      (split-window nil nil org-powerslides-image-window-placement))
    (display-buffer buffer)
    (org-powerslides-right-size-image-window (get-buffer-window buffer))
    (with-current-buffer buffer
      (image-transform-fit-to-window)
      (run-hooks 'org-powerslides-post-image-load-hook))
    (select-window current-window)))

(defun org-powerslides--select-random-image (imgpath)
  (let* ((s (s-split "|" imgpath))
         (lens (length s)))
    (nth (random lens) s)))

(defun org-powerslides-right-size-image-window (window )
  "Do eet"
  (let* ((current-width (window-pixel-width window))
         (desired-width (floor (* .45 (frame-pixel-width))))
         (width-diff (- desired-width current-width)))
    (unless (zerop width-diff)
      (window-resize window width-diff t t t))))

(defun org-powerslides-start-presentation ()
  (interactive)
  (window-divider-mode -1)
  (fringe-mode -1)
  (org-powerslides--hide-mode-line)
  (when (fboundp 'org-tidy-mode)
    (org-tidy-mode 1))
  ;; frame font
  (when (and (boundp 'org-powerslides-presentation-frame-font) (> (length org-powerslides-presentation-frame-font) 0))
    ;; save original frame font
    (when (not
           (s-equals? org-powerslides--saved-frame-font org-powerslides-presentation-frame-font))
      ;; assume current frame, global value
      (setq org-powerslides--saved-frame-font (frame-parameter nil 'font)) )
    (set-frame-font org-powerslides-presentation-frame-font))
  ;; opacity/transparency
  (when (fboundp 'set-opacity) (set-opacity 100)))

(defun org-powerslides--cleanup-images ()
  "Kill all image buffers opened by org-powerslides."
  (dolist (buf org-powerslides--image-buffers)
    (when (buffer-live-p buf)
      ;; close any window showing it
      (dolist (win (get-buffer-window-list buf nil t))
        (delete-window win))
      (kill-buffer buf)))
  (setq org-powerslides--image-buffers nil))

(defun org-powerslides-end-presentation ()
  (interactive)
  (window-divider-mode 1)
  (fringe-mode 1)
  (org-powerslides--hide-mode-line -1)
  (when (fboundp 'org-tidy-mode)
    (org-tidy-mode -1))
  ;; restore frame font
  (when (boundp 'org-powerslides--saved-frame-font)
    (set-frame-font org-powerslides--saved-frame-font)
    (setq org-powerslides--saved-frame-font nil))
  ;; opacity/transparency
  (when (fboundp 'set-opacity) (set-opacity 90)))

(defun org-num-level-2-only (numbering)
  "Custom numbering function.
NUMBERING is a list of numbers."
  (let* ((l2num (second numbering))
         (numstr (if l2num (number-to-string l2num) "")))
    (concat numstr " ")))

(defun org-powerslides--hide-mode-line (&optional value)
  "If a global mode-line hider is available, call the global function.
If VALUE is a negative number, disable hiding."

  (let ((hide-function
         (cond ((fboundp 'global-mask-mode-line-mode) 'global-mask-mode-line-mode)
               ((fboundp 'global-hide-mode-line-mode) 'global-hide-mode-line-mode)
               (nil)))
        (enable-disable (if (and (numberp value) (< value 0))
                            -1
                          1)))
    (when hide-function
      (when (fboundp 'doom-moodline-mode)
        (doom-modeline-mode -1))
      (funcall hide-function enable-disable))))

;; fun random image flipper, written by Bojack Opus 2024-04-22
(defun org-powerslides--random-image-effects ()
  "Fun random image flipper, written by Bojack Claude Opus 2024-04-22.
Add to `org-powerslides-post-image-load-hook'."
  (let ((random-effect (random 4)))
    (cond ((= random-effect 0)
           (image-rotate 180))
          ((= random-effect 1)
           (image-flip-horizontal))
          ((= random-effect 2)
           (let ((bojack-img (expand-file-name "~/Pictures/bojack.jpg")))
             (when (file-exists-p bojack-img)
               (find-image bojack-img))))
          (t nil))))

;;(add-hook 'org-powerslides-post-image-load-hook 'org-powerslides--random-image-effects)


;; the key combo is set in org-mode-map since this should only be used in org mode
;; TODO first assure org is loaded
(global-unset-key (kbd "s-]"))
(global-unset-key (kbd "s-["))
(define-key org-mode-map (kbd "s-]") 'org-powerslides/show-next-slide)
(define-key org-mode-map (kbd "s-[") 'org-powerslides/show-previous-slide)
(provide 'org-powerslides)
;;; org-powerslides.el ends here
