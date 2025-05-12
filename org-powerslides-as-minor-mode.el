;;; org-powerslides.el --- Minimalist Org Mode presentations as a minor mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023,2024 by Gregory Grubbs
;; Author: Gregory Grubbs
;; Version: 0.2
;; Package-Requires: ((org "9"))
;; URL: https://github.com/gregoryg/org-powerslides

;;; Commentary:

;; Provides focused, minimal slideshows in Org Mode using subtree narrowing
;; and optional image display. As a minor mode, toggles all presentation extras
;; (font, hiding, window tweaks) on/off—navigation is always available in org-mode.

;;; Code:

(require 'org)

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
  "Image window placement, as understood by `split-window'.
Choices: left, right, above, below."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" above)
                 (const :tag "Bottom" below))
  :group 'org-powerslides)

(defcustom org-powerslides-presentation-frame-font ""
  "Preferred font for presentation. See documentation for `set-frame-font'.
Example value: \"Hack-24\"."
  :type 'string
  :group 'org-powerslides)

(defcustom org-powerslides-post-image-load-hook nil
  "Normal hook that runs after an image for a slide has first been loaded."
  :type 'hook
  :group 'org-powerslides)

(defvar org-powerslides--saved-frame-font nil
  "Internal: frame font before enabling presentation mode.")

(defvar org-powerslides-minor-mode-map (make-sparse-keymap)
  "Keymap for `org-powerslides-minor-mode` (not used for navigation).")

(defun org-powerslides--reset-image-buffers ()
  (dolist (buf org-powerslides--image-buffers)
    (when (buffer-live-p buf)
      (dolist (win (get-buffer-window-list buf nil t))
        (delete-window win))
      (kill-buffer buf)))
  (setq org-powerslides--image-buffers nil))

(defun org-powerslides--hide-mode-line (&optional value)
  "Hide mode-line using available package; show if VALUE is negative."
  (let ((hide-fn (cond ((fboundp 'global-mask-mode-line-mode) 'global-mask-mode-line-mode)
                       ((fboundp 'global-hide-mode-line-mode) 'global-hide-mode-line-mode)
                       (nil))))
    (when hide-fn
      (when (fboundp 'doom-modeline-mode)
        (doom-modeline-mode (if (and (numberp value) (< value 0)) 1 -1)))
      (funcall hide-fn (if (and (numberp value) (< value 0)) -1 1)))))

(defun org-powerslides--display-image (imgpath)
  "Load and size IMGPATH in other-window and remember the buffer."
  (when (and imgpath (not (string-empty-p imgpath)))
    (let* ((current-window (selected-window))
           (buffer (find-file-noselect imgpath)))
      (unless (memq buffer org-powerslides--image-buffers)
        (push buffer org-powerslides--image-buffers))
      (when (and org-powerslides-image-window-placement (one-window-p t))
        (ignore-errors
          (split-window nil nil org-powerslides-image-window-placement)))
      (display-buffer buffer)
      (when-let* ((win (get-buffer-window buffer)))
        (org-powerslides-right-size-image-window win))
      (with-current-buffer buffer
        (when (fboundp 'image-transform-fit-to-window)
          (ignore-errors (image-transform-fit-to-window)))
        (run-hooks 'org-powerslides-post-image-load-hook))
      (select-window current-window))))

(defun org-powerslides-right-size-image-window (window)
  "Resize the image window to occupy about 45% of the frame width."
  (let* ((current-width (window-pixel-width window))
         (desired-width (floor (* 0.45 (frame-pixel-width))))
         (width-diff (- desired-width current-width)))
    (unless (zerop width-diff)
      (window-resize window width-diff t t t))))

(defun org-powerslides--select-random-image (imgpath)
  "Split IMGPATH on | and pick one at random."
  (let ((parts (split-string imgpath "|" t)))
    (when (> (length parts) 0)
      (nth (random (length parts)) parts))))

(defun org-powerslides--reveal-slide (&optional no-narrow)
  "Reveal (and optionally narrow to) the current heading."
  (org-overview)
  (org-reveal t)
  (org-show-entry)
  (outline-show-children)
  (when (not org-powerslides-same-level-only)
    (org-show-all))
  (unless no-narrow
    (org-narrow-to-subtree))
  (let ((img (org-entry-get nil "image")))
    (when (and img (not (string-empty-p img)))
      (org-powerslides--display-image
       (org-powerslides--select-random-image img)))))

(defun org-powerslides-next-slide (&optional no-narrow)
  "Advance to the next slide."
  (interactive "P")
  (org-powerslides--move-to-slide 'next no-narrow))

(defun org-powerslides-previous-slide (&optional no-narrow)
  "Go back to the previous slide."
  (interactive "P")
  (org-powerslides--move-to-slide 'prev no-narrow))

(defun org-powerslides--move-to-slide (direction &optional no-narrow)
  "Move to next/previous slide and reveal it."
  (catch 'out-of-bounds
    (let* ((move-fn
            (pcase (list direction org-powerslides-same-level-only)
              (`(next t)  (lambda () (org-forward-heading-same-level 1)))
              (`(next ,_) #'outline-next-heading)
              (`(prev t)  (lambda () (org-backward-heading-same-level 1)))
              (`(prev ,_) #'outline-previous-heading)
              (_ (error "Bad direction/same-level flag"))))
           (orig (point)))
      (widen)
      (funcall move-fn)
      (unless (and (< (point) (point-max))
                   (> (point) (point-min))
                   (org-at-heading-p))
        (goto-char orig)
        (message (if (eq direction 'next)
                     "End of outline reached."
                   "Top of outline reached."))
        (throw 'out-of-bounds nil))
      (org-powerslides--reveal-slide no-narrow))))

(defun org-powerslides-show-random-slide-same-level ()
  "Random slide at current heading level (or top-level if not at heading)."
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
        (org-powerslides--reveal-slide)))))

;;;###autoload
(define-minor-mode org-powerslides-minor-mode
  "Toggle minimal Org presentation polish in this buffer."
  :init-value nil
  :lighter " PowerSlides"
  :group 'org-powerslides
  (if org-powerslides-minor-mode
      (progn
        (window-divider-mode -1)
        (fringe-mode -1)
        (org-powerslides--hide-mode-line)
        (when (fboundp 'org-tidy-mode)
          (org-tidy-mode 1))
        (when (and (stringp org-powerslides-presentation-frame-font)
                   (> (length org-powerslides-presentation-frame-font) 0))
          (setq org-powerslides--saved-frame-font (frame-parameter nil 'font))
          (set-frame-font org-powerslides-presentation-frame-font))
        (when (fboundp 'set-opacity) (set-opacity 100)))
    ;; Cleanup/restore
    (window-divider-mode 1)
    (fringe-mode 1)
    (org-powerslides--hide-mode-line -1)
    (when (fboundp 'org-tidy-mode)
      (org-tidy-mode -1))
    (when org-powerslides--saved-frame-font
      (set-frame-font org-powerslides--saved-frame-font)
      (setq org-powerslides--saved-frame-font nil))
    (when (fboundp 'set-opacity) (set-opacity 90))
    (org-powerslides--reset-image-buffers)))

(make-obsolete 'org-powerslides-start-presentation 'org-powerslides-minor-mode "0.2")
(make-obsolete 'org-powerslides-end-presentation   'org-powerslides-minor-mode "0.2")

(defun org-powerslides-start-presentation ()
  "Obsolete wrapper for enabling org-powerslides-minor-mode."
  (interactive)
  (org-powerslides-minor-mode 1))

(defun org-powerslides-end-presentation ()
  "Obsolete wrapper for disabling org-powerslides-minor-mode."
  (interactive)
  (org-powerslides-minor-mode 0))

;; Navigation keybindings: always available in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-]") 'org-powerslides-next-slide)
  (define-key org-mode-map (kbd "s-[") 'org-powerslides-previous-slide))

(provide 'org-powerslides)
;;; org-powerslides.el ends here
