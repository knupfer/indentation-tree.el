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

(defcustom indentation-tree-narrow-on-tree nil
  "Shows parts of the buffer which aren't in current tree in other face.

This is combinable with indentation-tree-show-entire-tree. It is most useful
for navigation purposes."
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

(defcustom indentation-tree-draws-per-second 10
  "Amount of chars drawn in one second.

This speed is only considered, if indentation-tree-draw-slow is non-nil."
  :group 'indentation-tree
  :type 'integer)

(defface indentation-tree-narrow-face
  '((t (:foreground "#444" :weight bold)))
  "Face used for narrowing.

There are quite a lot of thinkable solutions. When you want this effect not
so intrusive, you could let this face only inherit from default, which would
just remove syntax-highlighting. If you want to preserve syntax-highlighting
you can use the weight ultralight."
  :group 'indentation-tree)

(defface indentation-tree-branch-face
  '((t (:foreground "#333" :weight bold)))
  "Face used for branches."
  :group 'indentation-tree)

(defface indentation-tree-leave-face
  '((t (:foreground "#222" :weight semi-bold)))
  "Face used for leaves."
  :group 'indentation-tree)

(defface indentation-tree-root-branch-face
  '((t (:foreground "#337" :weight ultra-bold)))
  "Face used for root branches."
  :group 'indentation-tree)

(defface indentation-tree-root-leave-face
  '((t (:foreground "#226" :weight bold)))
  "Face used for root leaves."
  :group 'indentation-tree)

(defface indentation-tree-child-branch-face
  '((t (:foreground "#644" :weight bold)))
  "Face used for branches."
  :group 'indentation-tree)

(defface indentation-tree-child-leave-face
  '((t (:foreground "#262" :weight semi-bold)))
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

;;;###autoload
(define-minor-mode indentation-tree-mode
  "Visualize tree structure of indentation.

This mode doesn't analyse the structure of your code, but of your indentation.
Therefor, if you mess up your indentation, you will most certainly a messed up
tree.

A huge amount of things is customizable, including options to raise performance
eaunder loss of precision.

There is one bug, which is fixable, but not so easy to do so: if there are
multiple branches or leaves which cross an empty line, distances from column
0 will be add up. This is not the case if it is only one branch/leave or
if the empty line is as well indented.

Following interactive commands are supported:
M-x:
   indentation-tree-mode: activates or disables this mode
   indentation-tree-draw-tree: draws tree with root one level above point
   indentation-tree-draw-all-trees: draws all trees in the buffer
                                    this is usefull, if you use beforehand
                                    text-scale-decrease a lot or you activate
                                    indentation-tree-slow-motion
   indentation-tree-slow-motion: waits after each drawn character
                                 this is not only funning, but also interesting
                                 to understand the functioning of this programm
                                 and to understand the tree structure better
Faces and other stuff can be modified with customize-group."
  :init-value nil
  :lighter " ⵄ"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<left>")
              'indentation-tree-move-to-parent)
            (define-key map (kbd "S-<right>")
              'indentation-tree-move-to-child)
            (define-key map (kbd "S-<up>")
              'indentation-tree-move-to-older-brother)
            (define-key map (kbd "S-<down>")
              'indentation-tree-move-to-younger-brother)
            (define-key map (kbd "C-S-<left>")
              'indentation-tree-move-to-root)
            (define-key map (kbd "C-S-<right>")
              'indentation-tree-move-to-leave)
            (define-key map (kbd "C-S-<up>")
              'indentation-tree-move-to-oldest-brother)
            (define-key map (kbd "C-S-<down>")
              'indentation-tree-move-to-youngest-brother)
            map)
  :global nil
  (setq indentation-tree-narrow nil)
  (let ((fun1 '(lambda (a b c)
                 (indentation-tree-remove)
                 (indentation-tree-show)))
        (fun2 '(lambda () (when (not (= indentation-tree-old-line
                                        (line-number-at-pos)))
                            (indentation-tree-remove)
                            (indentation-tree-show)
                            (setq indentation-tree-old-line (line-number-at-pos)
                                  indentation-tree-changed-p nil)))))
    (if indentation-tree-mode
        (progn
          (add-hook 'after-change-functions fun1 nil t)
          (add-hook 'post-command-hook fun2 nil t))
      (remove-hook 'after-change-functions fun1 t)
      (remove-hook 'post-command-hook fun2 t))))

(defvar indentation-tree-char nil)
(defvar indentation-tree-is-a-leave nil)
(defvar indentation-tree-debug nil)
(defvar indentation-tree-draw-slow nil)
(defvar indentation-tree-accumulate-draws 0)
(defvar indentation-tree-old-line 0)

(defun indentation-tree-draw-all-trees ()
  (interactive)
  (setq ov-before (make-overlay (point-min) (point-max)))
  (overlay-put ov-before
               'face 'indentation-tree-narrow-face)
  (overlay-put ov-before
               'category 'indentation-tree-narrow)
  (save-excursion
    (goto-char (point-min))
    (if indentation-tree-narrow-on-tree
        (progn
          (setq indentation-tree-narrow-on-tree nil)
          (while (and (re-search-forward "^[^ \n\t]" nil t)
                      (re-search-forward "^[ \t\n]" nil t))
            (backward-char 2)
            (indentation-tree-show))
          (setq indentation-tree-narrow-on-tree t))
      (while (and (re-search-forward "^[^ \n\t]" nil t)
                  (re-search-forward "^[ \t\n]" nil t))
        (backward-char 2)
        (indentation-tree-show)))
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
  (delq nil (mapcar
             (lambda (ov)
               (and (or (eq (overlay-get ov 'category)
                            'indentation-tree)
                        (eq (overlay-get ov 'category)
                            'indentation-tree-narrow))
                    ov))
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
        ((progn (back-to-indentation)
                (and (not (eolp)) (< (current-column) origin)))
         (point))
        (t
         (indentation-tree--beginning-of-level origin))))

(defun indentation-tree--make-overlay (line col &optional
                                            is-leave is-branch is-root)
  "draw line at (line, col)"
  (let ((original-pos (point)) diff string ov prop)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      (setq indentation-tree-warn nil)
      (setq indentation-tree-overlay-protected nil)
      (dolist (ov (overlays-at (point)))
        (if (and is-branch (eq (overlay-get ov 'category) 'indentation-tree))
            (setq indentation-tree-overlay-protected t)
          (setq indentation-tree-warn t)))
      (unless indentation-tree-overlay-protected
        ;; calculate difference from the actual col
        (when indentation-tree-draw-slow
          (setq indentation-tree-accumulate-draws
                (+ indentation-tree-accumulate-draws
                   (/ 1.0 indentation-tree-draws-per-second)))
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
          (overlay-put
           ov 'category 'indentation-tree)
          (overlay-put
           ov prop
           (if (and indentation-tree-warn indentation-tree-debug)
               (propertize string 'face 'warning)
             (if (and is-root (not indentation-tree-on-root-level))
                 (if is-leave
                     (propertize string 'face
                                 'indentation-tree-root-leave-face)
                   (propertize string 'face
                               'indentation-tree-root-branch-face))
               (if is-leave
                   (if (or indentation-tree-on-root-level
                           (and indentation-tree-child-begin
                                (or
                                 (and (> line indentation-tree-child-begin)
                                      (< line indentation-tree-child-end))
                                 (and (> line indentation-tree-child-begin)
                                      (= indentation-tree-child-begin
                                         indentation-tree-child-end)))))
                       (propertize string 'face
                                   'indentation-tree-child-leave-face)
                     (propertize string 'face
                                 'indentation-tree-leave-face))
                 (if (or indentation-tree-on-root-level
                         (and indentation-tree-child-begin
                              (or
                               (and (> line indentation-tree-child-begin)
                                    (< line indentation-tree-child-end))
                               (and (> line indentation-tree-child-begin)
                                    (= indentation-tree-child-begin
                                       indentation-tree-child-end)))))
                     (propertize string 'face
                                 'indentation-tree-child-branch-face)
                   (propertize string 'face
                               'indentation-tree-branch-face)))))))))))

(defun indentation-tree-recursion (&optional is-recursed moving moving-line)
  (when (not is-recursed)
    (setq line-col-save line-col)
    (setq indentation-tree-branch-indent-save indentation-tree-branch-indent)
    (setq indentation-tree-branch-line-save indentation-tree-branch-line)
    (indentation-tree-show t moving moving-line)
    (setq line-col line-col-save)
    (setq indentation-tree-branch-indent indentation-tree-branch-indent-save)
    (setq indentation-tree-branch-line indentation-tree-branch-line-save)))

(defun indentation-tree-show (&optional is-recursed moving moving-line)
  (unless (active-minibuffer-window)
    (when (not is-recursed)
      (setq indentation-tree-child-begin nil)
      (setq indentation-tree-child-end nil)
      (setq indentation-tree-original-line (line-number-at-pos))
      (setq indentation-tree-on-root-level nil)
      (save-excursion
        (back-to-indentation)
        (setq foo-bar (current-column))
        (forward-line 1)
        (back-to-indentation)
        (when (and (> foo-bar 0) (> (current-column) foo-bar))
          (setq indentation-tree-child-begin (- (line-number-at-pos) 1))
          (forward-line -1)
          (indentation-tree-move-to-younger-brother nil t)
          (setq indentation-tree-child-end (line-number-at-pos)))))
    (setq old-indent 0)
    (setq line-col nil)
    (setq indentation-tree-branch-indent nil)
    (setq indentation-tree-branch-line nil)
    (let ((win-start (max (- (window-start)
                             (* (- (window-end) (window-start))
                                indentation-tree-scope))
                          (point-min)))
          (win-end (min (+ (window-end)
                           (* (- (window-end) (window-start))
                              indentation-tree-scope))
                        (point-max)))
          line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (when (and indentation-tree-show-entire-tree
                   (not is-recursed)
                   (not moving))
          (when (not (eobp)) (forward-char 1))
          (re-search-backward "^[^ \n\t]" nil t))
        (when (or (< indentation-tree-scope 0) moving)
          (setq win-start (point-min))
          (setq win-end (point-max)))
        (if (not (indentation-tree--beginning-of-level))
            (setq line-col 0
                  line-start 1)
          (setq line-col (current-column)
                line-start (max (+ 1 (line-number-at-pos))
                                (line-number-at-pos win-start)))))
      (save-excursion
        (back-to-indentation)
        (when (equal (current-column) 0)
          (forward-line 1)
          (re-search-forward "[^ \n\t]" nil t)
          (setq indentation-tree-on-root-level t)
          (back-to-indentation))
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
              (when (or (not indentation-tree-branch-indent)
                        (>= indentation-tree-branch-indent old-indent))
                (setq indentation-tree-branch-indent old-indent)
                (if moving
                    (indentation-tree-move-save-split)
                  (indentation-tree-draw-horizontal-branches))
                (setq indentation-tree-branch-line (line-number-at-pos)))
              (indentation-tree-recursion is-recursed moving moving-line)))
          (when (re-search-backward "[^ \n\t}]" nil t)
            (when (not (eobp)) (forward-char 1))
            (back-to-indentation))
          (if (and indentation-tree-branch-line
                   (not (>= indentation-tree-branch-indent (current-column))))
              (progn (setq line-end (- indentation-tree-branch-line 1))
                     (setq indentation-tree-is-a-leave nil))
            (setq indentation-tree-is-a-leave t))
          (if moving
              (indentation-tree-move-save-edge)
            (indentation-tree-draw-vertical-branches-and-leaves)))))))

(defun indentation-tree-move-save-split ()
  (when is-recursed
    (progn
      (when (and (< (line-number-at-pos) moving-line)
                 (or (not indentation-tree-lesser)
                     (> (line-number-at-pos)
                        indentation-tree-lesser)))
        (setq indentation-tree-lesser (line-number-at-pos)))
      (when (and (> (- (line-number-at-pos) 1) moving-line)
                 (or (not indentation-tree-greater)
                     (< (- (line-number-at-pos) 1)
                        indentation-tree-greater)))
        (setq indentation-tree-greater
              (- (line-number-at-pos) 1))))))

(defun indentation-tree-move-save-edge ()
  (when is-recursed
    (progn
      (when (and (< line-end moving-line)
                 (or (not indentation-tree-lesser)
                     (> line-end
                        indentation-tree-lesser)))
        (setq indentation-tree-lesser line-end))
      (when (and (> line-end moving-line)
                 (or (not indentation-tree-greater)
                     (< line-end
                        indentation-tree-greater)))
        (setq indentation-tree-greater line-end)))))

(defun indentation-tree-draw-horizontal-branches ()
  (setq indentation-tree-char indentation-tree-split-branch)
  (when indentation-tree-branch-line
    (indentation-tree--make-overlay (- indentation-tree-branch-line 1)
                                    line-col nil nil (not is-recursed)))
  (setq indentation-tree-char indentation-tree-horizontal-branch)
  (dotimes (tmp (- old-indent line-col 1))
    (indentation-tree--make-overlay (- (line-number-at-pos) 1)
                                    (+ tmp line-col 1) nil nil
                                    (not is-recursed))))

(defun indentation-tree-draw-vertical-branches-and-leaves ()
  (if (and indentation-tree-is-a-leave indentation-tree-branch-line)
      (progn
        (setq indentation-tree-char indentation-tree-vertical-branch)
        (dotimes (tmp (- indentation-tree-branch-line line-start 1))
          (indentation-tree--make-overlay (+ line-start tmp)
                                          line-col nil t (not is-recursed)))
        (setq indentation-tree-char indentation-tree-edge-branch)
        (indentation-tree--make-overlay (- indentation-tree-branch-line 1)
                                        line-col nil nil (not is-recursed))
        (setq indentation-tree-char indentation-tree-vertical-leave)
        (dotimes (tmp (- line-end indentation-tree-branch-line))
          (indentation-tree--make-overlay (+ indentation-tree-branch-line tmp)
                                          line-col t t (not is-recursed))))
    (if indentation-tree-is-a-leave
        (progn
          (setq indentation-tree-char indentation-tree-vertical-leave)
          (dotimes (tmp (- line-end line-start))
            (indentation-tree--make-overlay (+ line-start tmp)
                                            line-col
                                            indentation-tree-is-a-leave
                                            t (not is-recursed))))
      (setq indentation-tree-char indentation-tree-vertical-branch)
      (dotimes (tmp (- line-end line-start))
        (indentation-tree--make-overlay (+ line-start tmp)
                                        line-col indentation-tree-is-a-leave
                                        t (not is-recursed)))))
  (goto-char (point-min))
  (forward-line (- line-end 1))
  (back-to-indentation)
  (if (not indentation-tree-is-a-leave)
      (setq indentation-tree-char indentation-tree-edge-branch)
    (setq indentation-tree-char indentation-tree-horizontal-leave)
    (dotimes (tmp (- (current-column) line-col 1))
      (indentation-tree--make-overlay line-end
                                      (+ 1 tmp line-col)
                                      indentation-tree-is-a-leave nil
                                      (not is-recursed)))
    (setq indentation-tree-char indentation-tree-edge-leave))
  (indentation-tree--make-overlay line-end
                                  line-col indentation-tree-is-a-leave
                                  nil (not is-recursed))
  (when indentation-tree-narrow-on-tree
    (indentation-tree--narrow-window))
  (when (not is-recursed)
    (setq indentation-tree-on-root-level nil)))

(defun indentation-tree--narrow-window ()
    (when (or (not indentation-tree-narrow)
            (and indentation-tree-narrow
                 (< indentation-tree-narrow line-end)))
      (setq indentation-tree-narrow line-end))
    (when (not is-recursed)
      (save-excursion
        (goto-char (point-min))
        (forward-line (- line-start 2))
        (setq ov-before (make-overlay (point-min) (point)))
        (overlay-put ov-before
                     'face 'indentation-tree-narrow-face)
        (overlay-put ov-before
                     'category 'indentation-tree-narrow)
        (goto-char (point-min))
        (forward-line indentation-tree-narrow)
        (setq ov-after (make-overlay (point) (point-max)))
        (overlay-put ov-after
                     'face 'indentation-tree-narrow-face)
        (overlay-put ov-after
                     'category 'indentation-tree-narrow)
        (setq indentation-tree-narrow nil))))

(defun indentation-tree-move-to-parent (arg &optional silent)
  (interactive "P")
  (when (and arg (> arg 1))
    (indentation-tree-move-to-parent (- arg 1) t))
  (setq indentation-tree-position (point))
  (if
      (save-excursion
        (back-to-indentation)
        (setq indentation-tree-current-indentation (current-column))
        (while (and (> (current-column) 0)
                    (> (line-number-at-pos) 1)
                    (or (eolp)
                        (>= (current-column)
                            indentation-tree-current-indentation)))
          (forward-line -1)
          (back-to-indentation))
        (if (= (current-column) indentation-tree-current-indentation)
            (progn
              (when (not silent) (message "There is no parent."))
              nil)
          (setq indentation-tree-position (point))
          t))
      (progn
        (goto-char indentation-tree-position)
        t)
    nil))

(defun indentation-tree-move-to-child (arg &optional silent)
  (interactive "P")
  (when (and arg (> arg 1))
    (indentation-tree-move-to-child (- arg 1) t))
  (save-excursion
    (setq indentation-tree-lesser nil)
    (setq indentation-tree-greater nil)
    (back-to-indentation)
    (forward-char 1)
    (re-search-backward "[^ \n\t]")
    (back-to-indentation)
    (setq indentation-tree-greater-recursed nil)
    (if (> (current-column) 0)
        (indentation-tree-show nil t (line-number-at-pos))
      (when (re-search-forward "^[ \n\t]" nil t)
        (indentation-tree-move-to-younger-brother nil t)
        (indentation-tree-move-to-older-brother nil t)
        (setq indentation-tree-greater (line-number-at-pos)))))
  (if (not indentation-tree-greater)
      (progn
        (when (not silent) (message "There is no child."))
        nil)
    (goto-char (point-min))
    (forward-line (- indentation-tree-greater 1))
    (back-to-indentation)
    t))

(defun indentation-tree-move-to-older-brother (arg &optional silent)
  (interactive "P")
  (when (and arg (> arg 1))
    (indentation-tree-move-to-older-brother (- arg 1) t))
  (save-excursion
    (setq indentation-tree-lesser nil)
    (setq indentation-tree-greater nil)
    (back-to-indentation)
    (forward-char 1)
    (re-search-backward "[^ \n\t]")
    (back-to-indentation)
    (if (> (current-column) 0)
        (indentation-tree-show t t (line-number-at-pos))
      (when (and (re-search-backward "^[ \n\t]" nil t)
                 (re-search-backward "^[^ \n\t]" nil t))
        (setq indentation-tree-lesser (+ 1 (line-number-at-pos))))))
  (if (not indentation-tree-lesser)
      (progn
        (when (not silent) (message "There is no older brother."))
        nil)
    (goto-char (point-min))
    (forward-line (- indentation-tree-lesser 2))
    (back-to-indentation)
    (when (= (current-column) 0)
      (set-window-start nil (point))
      (redraw-display))
    t))

(defun indentation-tree-move-to-younger-brother (arg &optional silent)
  (interactive "P")
  (when (and arg (> arg 1))
    (indentation-tree-move-to-younger-brother (- arg 1) t))
  (save-excursion
    (setq indentation-tree-lesser nil)
    (setq indentation-tree-greater nil)
    (back-to-indentation)
    (forward-char 1)
    (re-search-backward "[^ \n\t]")
    (back-to-indentation)
    (if (> (current-column) 0)
        (indentation-tree-show t t (line-number-at-pos))
      (forward-line 1)
      (when (and (re-search-forward "^[^ \n\t]" nil t)
                 (re-search-forward "^[ \n\t]" nil t))
        (setq indentation-tree-greater (- (line-number-at-pos) 1)))))
  (if (not indentation-tree-greater)
      (progn
        (when (not silent) (message "There is no younger brother."))
        nil)
    (goto-char (point-min))
    (forward-line (- indentation-tree-greater 1))
    (back-to-indentation)
    (when (= (current-column) 0)
      (set-window-start nil (point))
      (redraw-display))
    t))

(defun indentation-tree-move-to-youngest-brother ()
  (interactive)
  (if (indentation-tree-move-to-younger-brother nil)
      (progn
        (while (indentation-tree-move-to-younger-brother nil t))
        t)
    nil))

(defun indentation-tree-move-to-oldest-brother ()
  (interactive)
  (if (indentation-tree-move-to-older-brother nil)
      (progn
        (while (indentation-tree-move-to-older-brother nil t))
        t)
    nil))

(defun indentation-tree-move-to-root ()
  (interactive)
  (if (indentation-tree-move-to-parent nil)
      (progn
        (while (indentation-tree-move-to-parent nil t))
        (set-window-start nil (point))
        (redraw-display)
        t)
    nil))

(defun indentation-tree-move-to-leave ()
  (interactive)
  (if (indentation-tree-move-to-child nil)
      (progn
        (while (indentation-tree-move-to-child nil t))
        t)
    nil))

(defun indentation-tree-remove ()
  (dolist (ov (indentation-tree--active-overlays))
    (delete-overlay ov)))

(provide 'indentation-tree)

;;; indentation-tree.el ends here
