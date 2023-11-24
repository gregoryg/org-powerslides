;;; org-powerslides.el --- Minimalist Org Mode functions for presentations
;;
;; Copyright (C) 2023 by Gregory Grubbs
;;
;; Author: Ric Lister
;; Version: 0.1
;; Package-Requires: ((org "7"))
;; URL: https://github.com/gregoryg/org-powerslides

(defgroup org-powerslides nil
  "Use Org Mode as frictionless presentation."
  :group 'org-structure)

(defcustom org-powerslides-level-limit nil
  "If specified, a number indicating what level outline to display as a slide - child levels will be displayed in the slide.  The default is to use every level of an outline as a separate slide."
  :type 'integer
  :group 'org-powerslides)

(defcustom org-powerslides-image-window nil
  "If defined, a cons cell indicating image window position (left, right, above, below).  Default is to use other-window."
  :type 'string
  :group 'org-powerslides)


(defun org-powerslides/show-next-slide (&optional NO-NARROW)
  "Show next subtree as slide with optional image, keeping other entries closed. Narrow to the subtree unless NO-NARROW is true."
  (interactive)
  (widen)
  (let ((pos (point)))
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (outline-show-children))
      (outline-next-heading)
      (unless (and (bolp) (org-at-heading-p))
        (org-up-heading-safe)
        (outline-hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)
      (unless NO-NARROW (org-narrow-to-subtree))
      (let ((imgpath (org-entry-get nil "image")))
        (when imgpath
          (let ((current-window (selected-window))
                (buffer (find-file-noselect imgpath)))
            (display-buffer buffer)
            (select-window current-window)))))))

(defun org-powerslides/show-previous-slide (&optional NO-NARROW)
  "Show previous subtree as a slide with optional image, keeping other entries closed. Narrow to the subtree unless NO-NARROW is true."
  (interactive)
  (widen)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)
    (unless NO-NARROW (org-narrow-to-subtree))))

(add-to-list 'org-speed-commands
             '("]" ded/org-show-next-heading-tidily))
(add-to-list 'org-speed-commands
             '("[" ded/org-show-previous-heading-tidily))
(add-to-list 'org-speed-commands
             '("s" save-buffer))
(add-to-list 'org-speed-commands
             '("d" org-toggle-narrow-to-subtree))

;; TODO set global keys
;;  or ... the key combo should actually be in org-mode-map since this works only in org mode
;; (bind-key "s-]" 'org-powerslides/show-next-slide)
;; (bind-key "s-[" 'org-powerslides/show-previous-slide)

(provide 'org-powerslides)
;;; org-powerslides.el ends here
