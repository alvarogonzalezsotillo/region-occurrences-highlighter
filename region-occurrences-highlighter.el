;;; region-occurrences-highlighter.el --- Mark occurrences of current region (selection).  -*- lexical-binding: t; -*-

;; Author: Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; URL: https://github.com/alvarogonzalezsotillo/region-occurrences-highlighter
;; Package-Requires: ((emacs "24"))
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; GNU General Public License v3.0. See COPYING for details.

;;; Commentary:

;; This minor mode marks all the occurrences of the current region (selection)
;;
;; Quick start:
;; Enable region-occurrences-highlighter-mode, via alt-X or using hooks
;;
;; More information at https://github.com/alvarogonzalezsotillo/region-occurrences-highlighter

;;; Code:

(require 'hi-lock)

(defvar region-occurrences-highlighter--previous-region nil)
(make-variable-buffer-local 'region-occurrences-highlighter--previous-region)

(defgroup region-occurrences-highlighter-group nil
  "Region occurrences highlighter."
  :group 'convenience)


(defface region-occurrences-highlighter-face
  '((t (:inverse-video t)))
  "Face for occurrences of current region.")


(defcustom region-occurrences-highlighter-max-size 300
  "Maximum length of region of which highlight occurrences."
  :type 'integer)

(defcustom region-occurrences-highlighter-min-size 3
  "Minimum length of region to highlight occurrences."
  :type 'integer)

(defcustom region-occurrences-highlighter-ignore-regex "[[:space:]\n]+"
  "Ignore selection if matches this regex. Set it to empty string to maintaint compatibility with previous versions."
  :type 'string)
(defun region-occurrences-highlighter--ignore(str)
  "Check if STR matches the ignore regex."
  (or
   (not region-occurrences-highlighter-ignore-regex)
   (string=
    ""
    (replace-regexp-in-string region-occurrences-highlighter-ignore-regex "" str))))


(defun region-occurrences-highlighter--accept (begin end)
  "Accept to highlight occurrences if BEGIN and END are between limits, and the selection doesn't match ignore regex."
(message "accept:")
  (and
   (not (eq begin end))
   (>= (abs (- begin end)) region-occurrences-highlighter-min-size)
   (<= (abs (- begin end)) region-occurrences-highlighter-max-size)
   (let ((str (buffer-substring-no-properties begin end)))
     (not (region-occurrences-highlighter--ignore str)))))
;;;###autoload
(define-minor-mode region-occurrences-highlighter-mode
  "Highlight the current region and its occurrences, a la Visual Code"
  :group region-occurrences-highlighter-group

  (remove-hook 'post-command-hook #'region-occurrences-highlighter--change-hook t)
  (when region-occurrences-highlighter-mode
    (add-hook 'post-command-hook #'region-occurrences-highlighter--change-hook t)))

(defun region-occurrences-highlighter--change-hook ()
  "Called each time the region is changed."

  ;;; REMOVE PREVIOUS HIGHLIGHTED REGION
  (when region-occurrences-highlighter--previous-region
    (unhighlight-regexp region-occurrences-highlighter--previous-region)
    (setq region-occurrences-highlighter--previous-region nil))

  (when region-occurrences-highlighter-mode

    ;;; HIGHLIGHT THE CURRENT REGION
    (when (and (region-active-p)
               (not deactivate-mark))
      (let ((begin (region-beginning))
            (end (region-end)))
        (when (region-occurrences-highlighter--accept begin end)
          (let ((str (regexp-quote (buffer-substring-no-properties begin end))))
            (setq region-occurrences-highlighter--previous-region str)
            (highlight-regexp str 'region-occurrences-highlighter-face)))))))



(provide 'region-occurrences-highlighter)


;;; Some tabs and spaces, to test region-occurrences-highlighter-ignore-regex. Visible with witespace-mode.
						
						
						
						
						
						
						
						
						
						

              
              
              
              
              
              


;;; region-occurrences-highlighter.el ends here
