;;; org-powerslides.el --- Minimalist Org Mode functions for presentations
;;
;; Copyright (C) 2023 by Gregory Grubbs
;;
;; Author: Gregory Grubbs
;; Version: 0.1
;; Package-Requires: ((org "7"))
;; URL: https://github.com/gregoryg/org-powerslides

(defgroup org-powerslides nil
  "Use Org Mode as frictionless presentation."
  :group 'org-structure)

(defcustom org-powerslides-level-limit 0
  "If non-zero, a number indicating what level outline to display as a slide - child levels will be displayed in the slide.  The default is to use every level of an outline as a separate slide."
  :type 'integer
  :group 'org-powerslides)

(defcustom org-powerslides-image-window nil
  "If defined, a cons cell indicating image window position (left, right, above, below).  Default is to use other-window."
  :type 'symbol
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
        (org-powerslides--display-image (org-powerslides--select-random-image imgpath))))))

(defun org-powerslides--display-image (imgpath)
  "Load and size IMGPATH in other-window."
  (let ((current-window (selected-window))
        (buffer (find-file-noselect imgpath)))
    (display-buffer buffer)
    (org-powerslides-right-size-image-window (get-buffer-window buffer))
    (with-current-buffer buffer
      (image-transform-fit-to-window))
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
  (let ((hide-function (or 'global-mask-mode-line-mode 'global-hide-mode-line-mode)))
    (when hide-function (apply hide-function '(1))))
  (when (fboundp 'org-tidy-mode)
    (org-tidy-mode 1))
  (when (fboundp 'set-opacity) (set-opacity 100)))

(defun org-powerslides-end-presentation ()
  (interactive)
  (window-divider-mode 1)
  (fringe-mode 1)
  (let ((hide-function (or 'global-mask-mode-line-mode 'global-hide-mode-line-mode)))
    (when hide-function (apply hide-function '(-1))))
  (when (fboundp 'org-tidy-mode)
    (org-tidy-mode -1))
  (when (fboundp 'set-opacity) (set-opacity 90)))

(defun org-num-level-2-only (numbering)
  "Custom numbering function.
NUMBERING is a list of numbers."
  (let* ((l2num (second numbering))
         (numstr (if l2num (number-to-string l2num) "")))
    (concat numstr " ")))

;; the key combo is set in org-mode-map since this works only in org mode
;; TODO first assure org is loaded
(global-unset-key (kbd "s-]"))
(global-unset-key (kbd "s-["))
(define-key org-mode-map (kbd "s-]") 'org-powerslides/show-next-slide)
(define-key org-mode-map (kbd "s-[") 'org-powerslides/show-previous-slide)
(provide 'org-powerslides)
;;; org-powerslides.el ends here
