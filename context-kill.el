;;; context-kill.el --- provide context kill line function -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>

;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a single function that similar to
;; `paredit-kill', but it work with not Lisp languages.

;; Usage:
;;
;; (add-hook 'prog-mode-hook
;;   '(lambda () (define-key global-map [remap kill-line] 'context-kill)))
;;
;;; Code:

(require 'subr-x)

(defconst context-kill-multi-comment-open  6291467)
(defconst context-kill-multi-comment-close 6291468)

;;;###autoload
(defun context-kill ()
  "Kill something; a.k.a, `kill-line' DWIM."
  (interactive)
  (let ((pos (point))
        (ppss (syntax-ppss)))
    (if-let ((end (context-kill--compute-boundary ppss)))
        (prog1 nil
          (cl-typecase end
            (number (kill-region
                     pos
                     (- end (context-kill--offset))))
            (list
             (when-let ((no-offset (plist-get end :no-offset)))
               (kill-region pos (plist-get end :end)))))
          (funcall indent-line-function))
      (call-interactively 'kill-line))))

(defun context-kill--get-current-char (&optional pos)
  "Return syntax-char or nil.
If you specified POS argument, check text property of the point."
  (when-let ((c (car (get-text-property (or pos (point)) 'syntax-table))))
    (cl-case c
      (1  ?\s) ; space
      (2  ?w)  ; word
      (3  ?_)  ; symbol
      (4  ?.)  ; punctuation
      (5  ?\() ; open paren
      (6  ?\)) ; close paren
      (7  ?\") ; string quote
      (8  ?\\) ; escape
      (9  ?$)  ; pair
      (10 ?')  ; expression quote
      (11 ?<)  ; comment start
      (12 ?>)  ; comment end
      (13 ?/)  ; char quote
      (14 ?@)  ; inherit from parent
      (15 ?|)  ; generic string fence
      (16 ?!)  ; generic comment fence
      (t
       (cond ((eq c context-kill-multi-comment-open)
              context-kill-multi-comment-open)
             ((eq c context-kill-multi-comment-close)
              context-kill-multi-comment-close)
             (t nil))))))

(defun context-kill--multicomment-open-p ()
  (eq (context-kill--get-current-char)
      context-kill-multi-comment-open))

(defun context-kill--multicomment-close-p ()
  (eq (context-kill--get-current-char)
      context-kill-multi-comment-close))

(defun context-kill--offset ()
  "Return offset value."
  1)

(defun context-kill--skip (syntax-char)
  "Skip multi-comment."
  (while (eq ?\n (char-before (context-kill--find-end syntax-char))) ;?>
    nil))

(defun context-kill--compute-boundary (ppss)
  "Return end point of current context of PPSS."
  (save-excursion
    (cond
     ((string-match "^\\s-+$" (thing-at-point 'line))
      nil)
     ;; Comment
     ((context-kill--multicomment-open-p)
      (goto-char (1+ (point)))
      (context-kill--compute-comment-end (syntax-ppss))
      (re-search-forward "\\s>" nil t)
      (when (save-excursion
              (goto-char (1- (point)))
              (context-kill--multicomment-close-p))
        (list :no-offset t :end (point))))
     ((or (nth 4 ppss)
          (eq ?< (char-syntax (char-after (point)))))
      (context-kill--compute-comment-end ppss))
     ;; String
     ((nth 3 ppss)
      (when-let ((syntax (context-kill--get-syntax ppss 8)))
        (context-kill--find-end syntax)))
     ;; Parenthesis
     ((eq ?\( (char-syntax (char-after (point))))
      (context-kill--parenthesis ppss))
     ((nth 1 ppss)
      (when-let ((syntax (context-kill--get-syntax ppss 1)))
        (context-kill--parenthesis ppss -1))))))

(defun context-kill--parenthesis (ppss &optional offset)
  ""
  (let* ((depth (+ (nth 0 ppss) (or offset 0)))
         (previous-pos 0))
    (catch 'exit
      (while (not (eq depth (nth 0 (syntax-ppss (context-kill--find-end ?\))))))
        (when (eq previous-pos (point))
          (throw 'exit t))
        (setq previous-pos (point)))
      (list :no-offset t :end (if offset (1- (point)) (point))))))

(defun context-kill--get-syntax (ppss n)
  "Return char syntax from PPSS.
The argument N is used to check Nth element of PPSS."
  (when-let ((dest (nth n ppss)))
    (save-excursion
      (goto-char dest)
      (or (context-kill--get-current-char)
          (char-syntax (char-after (point)))))))

(defun context-kill--find-end (syntax-char)
  "Return point of pair of SYNTAX-CHAR."
  (re-search-forward (format "\\s%c" syntax-char) nil t)
  (point))

(defun context-kill--compute-comment-end (ppss)
  "Compute point of context end from PPSS."
  (when-let ((syntax (context-kill--get-syntax ppss 8)))
    (cond
     ;; multiline comment
     ((eq syntax context-kill-multi-comment-open)
      (let* ((nest-level (nth 4 ppss))
             (previous-pos 0))
        (catch 'exit
          ;; Skip ordinal ?>
          (context-kill--skip ?>)
          ;; Skip nested comment
          (while (and (not (eq nest-level (nth 4 (syntax-ppss (1- (point))))))
                      (not (eq previous-pos (point))))
            (context-kill--skip ?>)
            (when (eq previous-pos (point))
              (throw 'exit t))
            (setq previous-pos (point))))
        ;; Skip ordinal ?> til multiline comment's ?>
        (context-kill--skip ?>)
        (goto-char (1- (point)))
        (when (eq context-kill-multi-comment-close
                  (context-kill--get-current-char))
          (point))))
     ((eq ?< syntax)
      (context-kill--find-end ?>))
     (t
      (context-kill--find-end syntax)))))

(provide 'context-kill)
;;; context-kill.el ends here
