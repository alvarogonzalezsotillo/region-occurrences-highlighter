;;; package --- Summary

;;; Commentary:

;;; Code:


(defvar region-occurrences-highlighter--previous-region nil)
(make-variable-buffer-local 'region-occurrences-highlighter--previous-region)

(defgroup region-occurrences-highlighter--group nil
  "Region occurrences highlighter.")


(defface region-occurrences-highlighter--face
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Face for occurrences of current region."
  :group 'region-occurrences-highlighter--group)


(defcustom region-occurrences-highlighter--max-size 300
  "Maximum length of region of which highlight occurrences."
  :type 'integer
  :group 'region-occurrences-highlighter--group)

(defcustom region-occurrences-highlighter--min-size 3
  "Minimum length of region to highlight occurrences."
  :type 'integer
  :group 'region-occurrences-highlighter--group)

(defun region-occurrences-highlighter--accept (begin end)
  "Accept to highlight occurrences if BEGIN and END are between limits."
  (and
   (not (eq begin end))
   (>= (abs (- begin end)) region-occurrences-highlighter--min-size)
   (<= (abs (- begin end)) region-occurrences-highlighter--max-size)))

(defun region-occurrences-highlighter--change-hook ()
  "Called each time the region is changed."

  ;;; REMOVE PREVIOUS HIGHLIGHTED REGION
  (when region-occurrences-highlighter--previous-region
    (unhighlight-regexp region-occurrences-highlighter--previous-region)
    (setq region-occurrences-highlighter--previous-region nil))

  (when region-occurrences-highlighter-mode 
    

    ;;; HIGHLIGHT THE CURRENT REGION
    (when (region-active-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (when (region-occurrences-highlighter--accept begin end)
          (let ((str (regexp-quote (buffer-substring begin end))))
            (setq region-occurrences-highlighter--previous-region str)
            (highlight-regexp str 'region-occurrences-highlighter--face)))))))

  ;;;###autoload
(define-minor-mode region-occurrences-highlighter-mode
  "Highlight the current region and its occurrences, a la Visual Code"
  :group region-occurrences-highlighter--group

  (add-hook 'post-command-hook #'region-occurrences-highlighter--change-hook))


(provide 'region-occurrences-highlighter)

;;; region-occurrences-highlighter ends here

(add-hook 'prog-mode-hook 'region-occurrences-highlighter-mode)
