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

(defcustom indentation-tree-show-entire-tree nil
  "Show always full tree, ignore level of point."
  :group 'indentation-tree
  :type 'boolean)

(defcustom indentation-tree-scope 1
  "Range in which nodes outside the display are considered.
Defines how many screens will used above and after the current display.
Screen means here quantity of all chars currently displayed.
Using a negative value will set the range to infinite.
Greater values are more accurate but consume a lot more cpu cycles."
  :group 'indentation-tree
  :type 'integer)

(defcustom indentation-tree-draw-speed 0.1
  "Time in seconds to draw one char.

This speed is only considered, if indentation-tree-draw-slow is non-nil."
  :group 'indentation-tree
  :type 'float)

(defcustom indentation-tree-draw-slow nil
  "Draws tree slowly.

This is interesting for debugging and to understand,
how this prog works. And its quite funny."
  :group 'indentation-tree
  :type 'boolean)

(defface indentation-tree-branch-face
  '((t (:foreground "#644" :weight bold)))
  "Face used for branches."
  :group 'indentation-tree)

(defface indentation-tree-leave-face
  '((t (:foreground "#262" :weight semi-bold)))
  "Face used for leaves."
  :group 'indentation-tree)

(defface indentation-tree-root-branch-face
  '((t (:foreground "#844" :weight ultra-bold)))
  "Face used for branches."
  :group 'indentation-tree)

(defface indentation-tree-root-leave-face
  '((t (:foreground "#282" :weight bold)))
  "Face used for leaves."
  :group 'indentation-tree)

(defcustom indentation-tree-horizontal-branch "─"
  "Character used for horizontal branches."
  :group 'indentation-tree)

(defcustom indentation-tree-vertical-branch "│"
  "Character used for vertical branches."
  :group 'indentation-tree)

(defcustom indentation-tree-edge-branch "└"
  "Character used for edges of branches."
  :group 'indentation-tree)

(defcustom indentation-tree-split-branch "├"
  "Character used for splitting branches."
  :group 'indentation-tree)

(defcustom indentation-tree-horizontal-leave "─"
  "Character used for horizontal leaves."
  :group 'indentation-tree)

(defcustom indentation-tree-vertical-leave "│"
  "Character used for vertical leaves."
  :group 'indentation-tree)

(defcustom indentation-tree-edge-leave "└"
  "Character used for edges of leaves."
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

(defvar indentation-tree-char "")
(defvar indentation-tree-is-a-leave nil)

(defun indentation-tree-draw-all-trees ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and 
            (re-search-forward "^[^ \n\t]" nil t)
            (re-search-forward "^[ \t\n]" nil t))
      (indentation-tree-show))
    (sit-for 120))
  (indentation-tree-remove))

(defun indentation-tree-slow-motion ()
  (interactive)
  (if indentation-tree-draw-slow
      (setq indentation-tree-draw-slow nil)
    (setq indentation-tree-draw-slow t)))

(defun indentation-tree-draw-tree ()
  (interactive)
  (indentation-tree-show)
  (sit-for 120)
  (indentation-tree-remove))

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

(defvar indentation-tree-accumulate-draws 0)

(defun indentation-tree--make-overlay (line col &optional is-leave is-branch is-root)
  "draw line at (line, col)"
  
  (let ((original-pos (point))
        diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      (setq indentation-tree-overlay-protected nil)
      (dolist (ov (overlays-at (point)))
        (if (and is-branch (eq (overlay-get ov 'category) 'indentation-tree))
            (setq indentation-tree-overlay-protected t)))
            
            (unless indentation-tree-overlay-protected
              ;; calculate difference from the actual col
              (when indentation-tree-draw-slow
                (setq indentation-tree-accumulate-draws (+ indentation-tree-accumulate-draws indentation-tree-draw-speed))
                (when (> indentation-tree-accumulate-draws 0.03)
                  (sit-for indentation-tree-accumulate-draws)
                  (setq indentation-tree-accumulate-draws 0)))
              
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
                             (if is-root
                                 (if is-leave
                                     (propertize string 'face 'indentation-tree-root-leave-face)
                                   (propertize string 'face 'indentation-tree-root-branch-face))
                               (if is-leave
                                   (propertize string 'face 'indentation-tree-leave-face)
                                 (propertize string 'face 'indentation-tree-branch-face)))))))))

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
    (setq old-indent 0)
    (setq line-col nil)
    (setq indentation-tree-branch-indent nil)
    (setq indentation-tree-branch-line nil)
    
    (let ((win-start (max (- (window-start)
                             (* (- (window-end) (window-start)) indentation-tree-scope))
                          (point-min)))
          (win-end (min (+ (window-end)
                           (* (- (window-end) (window-start)) indentation-tree-scope))
                        (point-max)))
          line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (when (and indentation-tree-show-entire-tree (not is-recursed))
          (when (not (eobp)) (forward-char 1))
          (re-search-backward "^[^ \n\t]" nil t))
        (when (< indentation-tree-scope 0)
          (setq win-start (point-min))
          (setq win-end (point-max)))        
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
          (re-search-forward "[^ \n\t]" nil t)
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
          
          (while (and (if (not (eolp)) (setq old-indent (current-column)) t)
                      (progn (or (< line-col old-indent) (eolp)))
                      (forward-line 1)
                      (not (eobp))
                      (<= (point) win-end))
            (back-to-indentation)
            (setq current-indent (current-column))
            (when (and (> current-indent old-indent) (not (eolp)))
              (when (or (not indentation-tree-branch-indent) (>= indentation-tree-branch-indent old-indent))
                (indentation-tree-draw-horizontal-branches))
              (indentation-tree-recursion is-recursed)))        
          
          (when (re-search-backward "[^ \n\t}]" nil t)
            (when (not (eobp)) (forward-char 1))
            (back-to-indentation))
          
          (if (and indentation-tree-branch-line (not (= indentation-tree-branch-indent (current-column))))
              (progn (setq line-end (- indentation-tree-branch-line 1))
                     (setq indentation-tree-is-a-leave nil))
            (setq indentation-tree-is-a-leave t))          
          
          (indentation-tree-draw-vertical-branches-and-leaves))))))

(defun indentation-tree-draw-horizontal-branches ()
  (setq indentation-tree-branch-indent old-indent)
  (setq indentation-tree-char indentation-tree-split-branch)
  (when indentation-tree-branch-line 
    (indentation-tree--make-overlay (- indentation-tree-branch-line 1) line-col nil nil (not is-recursed)))
  (setq indentation-tree-branch-line (line-number-at-pos))
  (setq indentation-tree-char indentation-tree-horizontal-branch)
  (dotimes (tmp (- old-indent line-col 1))
    (indentation-tree--make-overlay (- indentation-tree-branch-line 1) (+ tmp line-col 1) nil nil (not is-recursed))))



(defun indentation-tree-draw-vertical-branches-and-leaves ()
  (if (and indentation-tree-is-a-leave indentation-tree-branch-line)
      (progn 
        (setq indentation-tree-char indentation-tree-vertical-branch)          
        (dotimes (tmp (- indentation-tree-branch-line line-start 1))
          (indentation-tree--make-overlay (+ line-start tmp) line-col nil t (not is-recursed)))
        (setq indentation-tree-char indentation-tree-edge-branch)          
        (indentation-tree--make-overlay (- indentation-tree-branch-line 1) line-col nil nil (not is-recursed))
        (setq indentation-tree-char indentation-tree-vertical-leave)          
        (dotimes (tmp (- line-end indentation-tree-branch-line))
          (indentation-tree--make-overlay (+ indentation-tree-branch-line tmp) line-col t t (not is-recursed))))
    (if indentation-tree-is-a-leave
        (progn
          (setq indentation-tree-char indentation-tree-vertical-leave)          
          (dotimes (tmp (- line-end line-start))
            (indentation-tree--make-overlay (+ line-start tmp) line-col indentation-tree-is-a-leave t (not is-recursed))))
      (setq indentation-tree-char indentation-tree-vertical-branch)          
      (dotimes (tmp (- line-end line-start))
        (indentation-tree--make-overlay (+ line-start tmp) line-col indentation-tree-is-a-leave t (not is-recursed)))))

  (goto-char (point-min))
  (forward-line (- line-end 1))
  (back-to-indentation)

  (if indentation-tree-is-a-leave
      (setq indentation-tree-char indentation-tree-horizontal-leave)
    (setq indentation-tree-char indentation-tree-horizontal-branch))
  (dotimes (tmp (- (current-column) line-col 1))
    (indentation-tree--make-overlay line-end (+ 1 tmp line-col) indentation-tree-is-a-leave nil (not is-recursed)))

  (if indentation-tree-is-a-leave
      (setq indentation-tree-char indentation-tree-edge-leave)
    (setq indentation-tree-char indentation-tree-edge-branch))
  (indentation-tree--make-overlay line-end line-col indentation-tree-is-a-leave nil (not is-recursed)))

(defun indentation-tree-remove ()
  (dolist (ov (indentation-tree--active-overlays))
    (delete-overlay ov)))

(provide 'indentation-tree)

;;; indentation-tree.el ends here 
