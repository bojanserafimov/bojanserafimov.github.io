;;; rcp-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Bojan Serafimov
;;
;; Author: Bojan Serafimov <bojan.serafimov7@gmail.com>
;; Maintainer: Bojan Serafimov <bojan.serafimov7@gmail.com>
;; Created: March 11, 2023
;; Modified: March 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bojan/rcp-mode
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defconst rcp-keywords
  '(("^>" . font-lock-keyword-face)
    ; (":.*\\.jpg" . font-lock-reference-face)
    ("^#.*$" . font-lock-builtin-face)
    ; ("^==.*==$" . font-lock-builtin-face)
    ("^>.*?\\(\\.\\)\\(.*\\)" 2 font-lock-comment-face)
    (".*" . font-lock-comment-face)
    ("\\[" . font-lock-comment-face)
    ("\\]" . font-lock-comment-face)
    ("^\\[.*$" . font-lock-keyword-face)
    ("\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]"
     (1 '(face font-lock-keyword-face) t)
     (2 '(face font-lock-comment-face) t))))

(define-derived-mode rcp-mode fundamental-mode "Recipe"
  "Major mode for editing Recip code"
  (setq-local font-lock-defaults '(rcp-keywords))
  (setq-default truncate-lines nil))

(defun rcp--title-to-html (title)
  "Convert TITLE content to html."
  (concat "<h1>" title "</h1>"))

(defun rcp--comment-to-html (comment)
  "Convert COMMENT content to html."
  (concat "<p class=\"faded\">" comment "</p>"))

(defun rcp--split-at-full-stop (string)
  "Split STRING at the first full stop."
  (let ((index (string-match "\\." string)))
    (if index
        (cons (substring string 0 (1+ index))
              (substring string (1+ index)))
      (cons string nil))))

(defun rcp--command-to-html (command)
  "Convert COMMAND content to html."
  (let* ((parsed (rcp--split-at-full-stop command))
         (faded-span (if (string-empty-p (cdr parsed)) ""
                       (concat "<span class=\"faded\">"
                               (concat (cdr parsed) "</span>")))))
    (concat
     "<p style=\"clear: both\">"
     (car parsed)
     faded-span
     "</p>")))

(defun rcp--subtitle-to-html (subtitle)
  "Convert SUBTITLE content to html."
  (concat
   "<div style=\"clear: both;\"></div>"
   "<div class=\"line\"><h3><span>"
   subtitle
   "</span></h3></div>"))

(defun rcp--generate-html-output-path ()
  "Replace .rcp with .html in the current path."
  (let ((file-name (buffer-file-name)))
    (if (string-suffix-p ".rcp" file-name)
        (concat (projectile-project-root)
                (file-name-base file-name)
                ".html")
      (error "Current file is not a .rcp file"))))

(defconst rcp--head "\
<head>
  <meta charset=\"utf-8\">
  <!-- Improves font size on mobile -->
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>Bojan Serafimov - %s</title>
  <link rel=\"stylesheet\" href=\"style.css\"/>
</head>
")

(defconst rcp--image "\
<img alt=\"%s\" src=\"%s\" width=\"128\" height=\"128\"
     style=\"float: left; margin-right: 20px; margin-top: 30px;\"/>
")

(defun parse-link-line (line)
  "Hi LINE."
  (if (string-match "\\[\\(.*\\)\\]:\\s-*\\(.*\\)" line)
      (list (match-string 1 line)
            (match-string 2 line))
    nil))

(defun rcp-buffer-to-html ()
  "Convert this buffer to html."
  (interactive)
  (save-excursion
    (let ((out-buffer (generate-new-buffer "html-output"))
          (out-path (rcp--generate-html-output-path))
          (title nil)
          (image nil))

      ; Insert body and extract title and image path
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (html
                (cond ((string-prefix-p "# " line)
                       (setq title (string-remove-prefix "# " line))
                       (rcp--title-to-html title))
                      ((string-prefix-p ":" line)
                       (setq image (string-remove-prefix ":" line))
                       "")
                      ((string-prefix-p "> " line)
                       (rcp--command-to-html line))
                      ((and (string-prefix-p "== " line)
                            (string-suffix-p "" line))
                       (rcp--subtitle-to-html
                        (string-remove-prefix "== " (string-remove-suffix " ==" line))))
                      ((string-match "\\[\\(.*\\)\\]:\\s-*\\(.*\\)" line)
                       (let ((link-identifier (match-string 1 line))
                             (link-url (string-trim (match-string 2 line))))
                         ; TODO test this
                         (with-current-buffer out-buffer
                           (goto-char (point-min))
                           (while (search-forward-regexp
                                   (concat "\\[" link-identifier "\\]\\*") nil t)
                             (replace-match "hiii" nil nil)))
                       ""))
                      ((string-empty-p line) "")
                      (t (rcp--comment-to-html line)))))
          (with-current-buffer out-buffer
            (insert html)
            (insert "\n"))
        (forward-line)))

      ; Insert image
      (with-current-buffer out-buffer
        (goto-char (point-min))
        (insert (format rcp--image title image)))

      ; Wrap in <body> tag
      (with-current-buffer out-buffer
        (indent-rigidly (point-min) (point-max) 2)
        (goto-char (point-min))
        (insert "<body>")
        (insert "\n")
        (goto-char (point-max))
        (insert "</body>")
        (insert "\n"))

      ; Insert header and image at the top
      (with-current-buffer out-buffer
        (goto-char (point-min))
        (insert (format rcp--head title)))

      ; Wrap everything in <html> tag
      (with-current-buffer out-buffer
        (indent-rigidly (point-min) (point-max) 2)
        (goto-char (point-min))
        (insert "<!DOCTYPE html>")
        (insert "\n")
        (insert "<html lang=\"en\">")
        (insert "\n")
        (goto-char (point-max))
        (insert "</html>")
        (insert "\n"))

      ; Remove extra whitespace
      (with-current-buffer out-buffer
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n")))

      ; Write to file
      (with-current-buffer out-buffer
        (write-region (point-min) (point-max) out-path)))))

(evil-define-key 'normal rcp-mode-map (kbd "SPC l h") #'rcp-buffer-to-html)

(provide 'rcp-mode)
;;; rcp-mode.el ends here
