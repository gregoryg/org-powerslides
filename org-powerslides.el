;;; org-powerslides.el --- Minimalist Org Mode functions for presentations
;;
;; Copyright (C) 2023,2024 by Gregory Grubbs
;;
;; Author: Gregory Grubbs
;; Version: 0.1
;; Package-Requires: ((org "7"))
;; URL: https://github.com/gregoryg/org-powerslides

(defgroup org-powerslides nil
  "Use Org Mode as frictionless presentation."
  :group 'org-structure)

(defcustom org-powerslides-level-limit 0
  "If non-zero, a number indicating what level outline to display as a slide - child levels will be displayed in the slide.  The default is to use every level of an outline as a separate slide.

N.B. Currently, setting this variable to non-zero simply keeps the org heading level you begin with."
  :type 'integer
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
  "Show a random subtree at the same level as current point, or level 1 if outside the first heading."
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
      (goto-char (nth (random (length same-level-headings)) same-level-headings))
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)
      (org-narrow-to-subtree))))


(defun org-powerslides/show-next-slide (&optional NO-NARROW)
  "Show next subtree as slide with optional image, keeping other entries closed. Narrow to the subtree unless NO-NARROW is true."
  (interactive)
  (widen)
  (let ((pos (point)))
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (outline-show-children))
      (if (not (zerop org-powerslides-level-limit))
          ;; todo: signal if no movement (end of preso)
          (org-forward-heading-same-level 1)
        (outline-next-heading))
      (unless (and (bolp) (org-at-heading-p))
        (org-up-heading-safe)
        (outline-hide-subtree)
        (message "End of outline reached."))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)
      (when (not (zerop org-powerslides-level-limit))
        (org-show-all))
      (unless NO-NARROW (org-narrow-to-subtree))
      (let ((imgpath (org-entry-get nil "image")))
        (when imgpath
          (org-powerslides--display-image (org-powerslides--select-random-image imgpath)))))))

(defun org-powerslides/show-previous-slide (&optional NO-NARROW)
  "Show previous subtree as a slide with optional image, keeping other entries closed. Narrow to the subtree unless NO-NARROW is true."
  (interactive)
  (widen)
  (let ((pos (point)))
    (if (not (zerop org-powerslides-level-limit))
        ;; todo signal if no movement (top of preso)
        (org-backward-heading-same-level 1)
      (outline-previous-heading))
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (message "Top of outline reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)
    (unless NO-NARROW (org-narrow-to-subtree))
    (let ((imgpath (org-entry-get nil "image")))
      (when imgpath
        (org-powerslides--display-image (org-powerslides--select-random-image imgpath))))))

(defun org-powerslides--display-image (imgpath)
  "Load and size IMGPATH in other-window."
  (let ((current-window (selected-window))
        (buffer (find-file-noselect imgpath)))
    (when (and (boundp 'org-powerslides-image-window-placement) (not (window-parent))) ;; only one window, so split
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
(add-hook 'org-powerslides-post-image-load-hook
          (lambda ()
            (let ((random-effect (random 4)))
              (cond ((= random-effect 0)
                     (image-rotate 180))
                    ((= random-effect 1)
                     (image-flip-horizontal))
                    ((= random-effect 2)
                     (let ((bojack-img (expand-file-name "~/Pictures/bojack.jpg")))
                       (when (file-exists-p bojack-img)
                         (find-image bojack-img))))
                    (t nil)))))



;; the key combo is set in org-mode-map since this works only in org mode
;; TODO first assure org is loaded
(global-unset-key (kbd "s-]"))
(global-unset-key (kbd "s-["))
(define-key org-mode-map (kbd "s-]") 'org-powerslides/show-next-slide)
(define-key org-mode-map (kbd "s-[") 'org-powerslides/show-previous-slide)
(provide 'org-powerslides)
;;; org-powerslides.el ends here
