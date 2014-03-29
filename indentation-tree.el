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

(defconst indentation-tree-version "2.1.5")

;; * customs

(defgroup indentation-tree nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indentation-tree-char "|"
  "character used as vertical line"
  :group 'indentation-tree)

(defcustom indentation-tree-inhibit-modes '(dired-mode)
  "list of major-modes in which indentation-tree should be turned off"
  :group 'indentation-tree)

;; * minor-mode

(define-minor-mode indentation-tree-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indentation-tree-mode
      (progn
        (add-hook 'pre-command-hook 'indentation-tree-remove nil t)
        (add-hook 'post-command-hook 'indentation-tree-show nil t))
    (remove-hook 'pre-command-hook 'indentation-tree-remove t)
    (remove-hook 'post-command-hook 'indentation-tree-show t)))

(define-globalized-minor-mode indentation-tree-global-mode
  indentation-tree-mode
  (lambda ()
    (unless (memq major-mode indentation-tree-inhibit-modes)
      (indentation-tree-mode 1))))

;; * variables / faces

(make-face 'indentation-tree-face)
(set-face-attribute 'indentation-tree-face nil
                    :foreground "#535353")

(make-face 'indentation-tree-face-rec-level-1)
(set-face-attribute 'indentation-tree-face-rec-level-1 nil
                    :foreground "#ff5353")

;; * utilities

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

(defun indentation-tree--make-overlay (line col &optional rec-level)
  "draw line at (line, col)"
  ;;(sit-for 0.05) ;; for debugging
  (let ((original-pos (point))
        diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
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
                           (make-overlay (point) (1+ (point))))))
            (t ; no problem
             (setq string indentation-tree-char
                   prop 'display
                   ov (and (not (= (point) original-pos))
                           (make-overlay (point) (+ 1 (point)))))))
      (when ov
        (overlay-put ov 'category 'indentation-tree)
        (overlay-put ov prop
                     (if (equal rec-level nil)
                         (propertize string 'face 'indentation-tree-face)
                       (propertize string 'face 'indentation-tree-face-rec-level-1)))))))

(defun indentation-tree-recursion ()
  (when (not recursed)
    (setq horizontal-length-save horizontal-length)
    (setq horizontal-position-save horizontal-position)
    (setq the-fork-indent-save the-fork-indent)
    (setq the-last-fork-save the-last-fork)

    (indentation-tree-show t)
    
    (setq the-last-fork the-last-fork-save)
    (setq the-fork-indent the-fork-indent-save)
    (setq horizontal-position horizontal-position-save)
    (setq horizontal-length horizontal-length-save))
  )

(defun indentation-tree-show (&optional recursed)
  ;; (unless (or (indentation-tree--active-overlays)
  ;; (active-minibuffer-window))
  (let ((win-start (max (- (window-start) 1000) 0))
        (win-end (+ (window-end) 1000))
        line-col line-start line-end)
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
      (unless (equal (current-column) 0) 
        (while (and (progn (back-to-indentation)
                           (or (< line-col (current-column)) (eolp)))
                    (forward-line 1)
                    (not (eobp))
                    (<= (point) win-end)))
        (setq the-fork-indent nil)
        (setq the-last-fork nil)
        (when (re-search-backward "[^ \n\t]" nil t)
          (when (not (eobp)) (forward-char 1))
          (goto-char (re-search-backward "[^ \n\t]" nil t)))
        (back-to-indentation)
        (setq line-end (line-number-at-pos))
        (back-to-indentation)
        (setq horizontal-length (- (current-column) line-col))
        (setq horizontal-position line-col)
        (goto-char (point-min))
        (forward-line (1- line-start))
        (back-to-indentation)
        (setq current-indent (current-column))
        ;;        
        (while (and (progn (back-to-indentation)
                           (or (< line-col (current-column)) (eolp)))
                    (setq old-indent (current-column))
                    (forward-line 1)
                    (not (eobp))
                    (<= (point) win-end))
          
          (back-to-indentation)
          (setq current-indent (current-column))
          (setq indentation-tree-char "~")
          (when (> current-indent old-indent)
            (when (not the-fork-indent) (setq the-fork-indent old-indent))
            (when (equal the-fork-indent old-indent)
              (setq the-last-fork (line-number-at-pos))
              (dotimes (tmp (- old-indent line-col 1))
                (indentation-tree--make-overlay (- (line-number-at-pos) 1) (+ tmp line-col 1) recursed)))
            (indentation-tree-recursion)))
        (setq indentation-tree-char "_")
        ;; (when (not the-fork-indent) (message (format "%s" (+ old-indent (* the-fork-indent 1000)))))
        (when (and the-last-fork (not (equal the-fork-indent old-indent)))
          (setq line-end (- the-last-fork 1))
          (setq horizontal-length (- horizontal-length the-fork-indent)))
        (dotimes (tmp (- horizontal-length 1))
          (indentation-tree--make-overlay line-end (+ 1 horizontal-position tmp) recursed))
          ;; draw line
          (setq indentation-tree-char "|")
          (when (and the-last-fork (not (equal the-fork-indent old-indent))
                     (setq line-end (- the-last-fork 1))))
          (dotimes (tmp (- line-end line-start))
            (indentation-tree--make-overlay (+ line-start tmp) line-col recursed))
          (setq indentation-tree-char "\\")
          (indentation-tree--make-overlay line-end line-col recursed)))))

;;)
(defun indentation-tree-remove ()
  (dolist (ov (indentation-tree--active-overlays))
    (delete-overlay ov)))

;; * provide

(provide 'indentation-tree)

;;; indentation-tree.el ends here 
