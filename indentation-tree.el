;;; indentation-tree.el --- show tree structures of current indentation

;; Copyright (C) 2014 Florian Knupfer, 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer

;;; Commentary:

;; This is based on version 2.1.5 of indent-guide.el from zk_phi.
;; URL: http://hins11.yu-yake.com/

;;; Code:

(defgroup indentation-tree nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indentation-tree-char "|"
  "character used as vertical line"
  :group 'indentation-tree)

(define-minor-mode indentation-tree-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter nil
  :global nil
  (if indentation-tree-mode
      (progn
        (add-hook 'pre-command-hook 'indentation-tree-remove nil t)
        (add-hook 'post-command-hook 'indentation-tree-show nil t))
    (remove-hook 'pre-command-hook 'indentation-tree-remove t)
    (remove-hook 'post-command-hook 'indentation-tree-show t)))

(defvar indentation-tree-manually-p nil)

(defun indentation-tree-draw-manually ()
  (interactive)
  (if indentation-tree-mode
      (progn
            (remove-hook 'pre-command-hook 'indentation-tree-remove t)
            (remove-hook 'post-command-hook 'indentation-tree-show t)
            (setq indentation-tree-manually-p t)
            (indentation-tree-show)
            (setq indentation-tree-manually-p nil)
            (add-hook 'pre-command-hook 'indentation-tree-remove nil t)
            (add-hook 'post-command-hook 'indentation-tree-show nil t))
                (setq indentation-tree-manually-p t)
                (indentation-tree-show)
                (setq indentation-tree-manually-p nil))
  (sit-for 120)
  (indentation-tree-remove))

(defface indentation-tree-branch-face
  '((t (:foreground "#644")))
  "Face used for branches."
  :group 'indentation-tree)

(defface indentation-tree-leave-face
  '((t (:foreground "#262")))
  "Face used for leaves."
  :group 'indentation-tree)

(defvar indentation-tree-is-a-leave nil)

(defun indentation-tree--active-overlays ()
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indentation-tree) ov))
         (overlays-in (point-min) (point-max)))))

(defun indentation-tree--beginning-of-level (&optional origin)
  ;; origin <- indent column of current line
  (unless origin
    (back-to-indentation)
    (if (not (eolp))
        (setq origin (current-column))
      (let ((forward (save-excursion
                       (while (and (forward-line 1)
                                   (not (eobp))
                                   (progn (back-to-indentation) t)
                                   (eolp)))
                       (if (eobp) 0 (current-column))))
            (backward (save-excursion
                        (while (and (zerop (forward-line -1))
                                    (progn (back-to-indentation) t)
                                    (eolp)))
                        (if (bobp) 0 (current-column)))))
        (setq origin (max forward backward)))))
  (cond ((zerop origin)
         (point))
        ((= (forward-line -1) -1)
         nil)
        ((progn
           (back-to-indentation)
           (and (not (eolp))
                (< (current-column) origin)))
         (point))
        (t
         (indentation-tree--beginning-of-level origin))))

;; * generate guides

(defun indentation-tree--make-overlay (line col &optional is-leave is-branch)
  "draw line at (line, col)"
  (when indentation-tree-manually-p
    (sit-for 0.05))
  
  (let ((original-pos (point))
        diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      (setq indentation-tree-overlay-protected nil)
      (dolist (ov (overlays-at (point)))
        (if (not is-branch)
            (delete-overlay ov)
          (setq indentation-tree-overlay-protected t)))
      (unless indentation-tree-overlay-protected
        ;; calculate difference from the actual col
        (setq diff (- (current-column) col))
        ;; make overlay or not
        (cond ((eolp) ; blank line (with no or less indent)
               (setq string (concat (make-string (- diff) ?\s)
                                    indentation-tree-char)
                     prop 'before-string
                     ov (and (not (= (point) original-pos))
                             (make-overlay (point) (point)))))
              ((not (zerop diff)) ; looking back tab
               (setq string (concat (make-string (- tab-width diff) ?\s)
                                    indentation-tree-char
                                    (make-string (1- diff) ?\s))
                     prop 'display
                     ov (and (not (= (point) (1- original-pos)))
                             (make-overlay (point) (1- (point))))))
              ((looking-at "\t") ; looking at tab
               (setq string (concat indentation-tree-char
                                    (make-string (1- tab-width) ?\s))
                     prop 'display
                     ov (and (not (= (point) original-pos))
                             (make-overlay (point) (+ 1 (point))))))
              (t ; no problem
               (setq string indentation-tree-char
                     prop 'display
                     ov (and (not (= (point) original-pos))
                             (make-overlay (point) (+ 1 (point)))))))
        (when ov
          (overlay-put ov 'category 'indentation-tree)
          (overlay-put ov prop
                       (if is-leave
                           (propertize string 'face 'indentation-tree-leave-face)
                         (propertize string 'face 'indentation-tree-branch-face))))))))

;;(defvar old-indent nil)
(defun indentation-tree-recursion (&optional is-recursed)
  (when (not is-recursed)
    (setq line-col-save line-col)
    (setq indentation-tree-branch-indent-save indentation-tree-branch-indent)
    (setq indentation-tree-branch-line-save indentation-tree-branch-line)
    (indentation-tree-show t)
    (setq line-col line-col-save)
    (setq indentation-tree-branch-indent indentation-tree-branch-indent-save)
    (setq indentation-tree-branch-line indentation-tree-branch-line-save)))

(defun indentation-tree-show (&optional is-recursed)
  (unless (active-minibuffer-window)
    (setq line-col nil)
    (setq indentation-tree-branch-indent nil)
    (setq indentation-tree-branch-line nil)
    (let ((win-start (max (- (window-start) 1000) 0))
          (win-end (+ (window-end) 1000))
          line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (if (not (indentation-tree--beginning-of-level))
            (setq line-col 0
                  line-start 1)
          (setq line-col (current-column)
                line-start (max (+ 1 (line-number-at-pos))
                                (line-number-at-pos win-start)))))
      ;; decide line-end
      (save-excursion
        (back-to-indentation)
        (when (equal (current-column) 0)
          (forward-line 1)
          (back-to-indentation))
        
        ;; Don't bug on comments.
        (unless (= (current-column) 0)
          (while (and (progn (back-to-indentation)
                             (or (< line-col (current-column)) (eolp)))
                      (forward-line 1)
                      (not (eobp))
                      (<= (point) win-end)))
          
          (when (re-search-backward "[^ \n\t}]" nil t)
            (when (not (eobp)) (forward-char 1))
            (goto-char (re-search-backward "[^ \n\t}]" nil t)))
          
          (setq line-end (line-number-at-pos))
          (goto-char (point-min))
          (forward-line (1- line-start))
          (back-to-indentation)
          
          (while (and (setq old-indent (current-column))
                      (progn (or (< line-col old-indent) (eolp)))
                      (forward-line 1)
                      (not (eobp))
                      (<= (point) win-end))
            (back-to-indentation)
            (setq current-indent (current-column))
            (when (> current-indent old-indent)
              (when (not indentation-tree-branch-indent) (setq indentation-tree-branch-indent old-indent))
              (when (>= indentation-tree-branch-indent old-indent)
                (when (> indentation-tree-branch-indent old-indent) (setq indentation-tree-branch-indent old-indent))
                (setq indentation-tree-branch-line (line-number-at-pos))
                (setq indentation-tree-char "─")
                (dotimes (tmp (- old-indent line-col 1))
                  (indentation-tree--make-overlay (- indentation-tree-branch-line 1) (+ tmp line-col 1)))
                
                (setq indentation-tree-char "├") 
                (indentation-tree--make-overlay (- indentation-tree-branch-line 1) line-col))
              
              (indentation-tree-recursion is-recursed)))
          
          (when (re-search-backward "[^ \n\t}]" nil t)
            (when (not (eobp)) (forward-char 1))
            (goto-char (re-search-backward "[^ \n\t}]" nil t))
            (back-to-indentation))
          (setq indentation-tree-is-a-leave t)
          (when (and indentation-tree-branch-line (not (= indentation-tree-branch-indent (current-column))))
            (setq line-end (- indentation-tree-branch-line 1))
            (setq indentation-tree-is-a-leave nil))
          
          (setq indentation-tree-char "│")          
          (when (and indentation-tree-is-a-leave indentation-tree-branch-line)
            (dotimes (tmp (- indentation-tree-branch-line line-start))
              (indentation-tree--make-overlay (+ line-start tmp) line-col nil t))
            (setq indentation-tree-char "└")          
            (indentation-tree--make-overlay (- indentation-tree-branch-line 1) line-col nil)
            (setq indentation-tree-char "│")
            (dotimes (tmp (- line-end indentation-tree-branch-line))
              (indentation-tree--make-overlay (+ indentation-tree-branch-line tmp) line-col t t)))
          (when (and indentation-tree-is-a-leave (not indentation-tree-branch-line))
            (dotimes (tmp (- line-end line-start))
              (indentation-tree--make-overlay (+ line-start tmp) line-col indentation-tree-is-a-leave t)))
          (when (not indentation-tree-is-a-leave)
            (dotimes (tmp (- line-end line-start))
              (indentation-tree--make-overlay (+ line-start tmp) line-col indentation-tree-is-a-leave t)))
          
          (goto-char (point-min))
          (forward-line (- line-end 1))
          (back-to-indentation)

          (setq indentation-tree-char "─")
          (dotimes (tmp (- (current-column) line-col 1))
            (indentation-tree--make-overlay line-end (+ 1 tmp line-col) indentation-tree-is-a-leave))
          
          (setq indentation-tree-char "└")
          (indentation-tree--make-overlay line-end line-col indentation-tree-is-a-leave))))))

(defun indentation-tree-remove ()
  (dolist (ov (indentation-tree--active-overlays))
    (delete-overlay ov)))

(provide 'indentation-tree)

;;; indentation-tree.el ends here 
