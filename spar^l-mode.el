;;; spar^l-mode.el --- display ^L better  -*- lexical-binding: t -*-
;;
;; Author: Joe Wreschnig
;; Package-Version: 20170610
;; Package-Requires: ((emacs "24.4"))
;; Keywords: display, faces

;; Copyright 2017 Joe Wreschnig
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Confused by all the ^Ls you see in official Emacs code?
;; Disappointed that you installed a package named "pretty" that
;; isn't?  Then Spar^L Mode is for you!
;;
;; Spar^L Mode replaces the form feed (Control-L, or ^L) character in
;; Emacs buffers with something much cuter.
;;
;; To use it, add the following to your Emacs initialization:
;;
;;     (spar^l-mode t)

;;; Code:

(defconst spar^l-mode-tail '("︵‿✧" "︵‿☆")
  "Strings to pick from to conclude sparkling.")

(defconst spar^l-mode-tail-tty '("  ~  *")
  "Strings to pick from to conclude sparkling, on TTYs.")

(defconst spar^l-mode-body '("．☆ · ゜✧ ｡ " "．· ° · ✧ ")
  "Strings to pick from while sparkling.")

(defconst spar^l-mode-body-tty '(". · ° * " ". * ° · ")
  "Strings to pick from while sparkling, on TTYs.")

(defface spar^l-mode '((t :inherit (escape-glyph)))
  "The face to use for the sparkles."
  :group 'Spar^L)

(defgroup Spar^L nil
  "Sparkle instead of displaying Control-L (`^L') characters."
  :prefix "spar^L-" :group 'convenience)

(defcustom spar^l-mode-compatibility-mode nil
  "Restrict sparkles to common glyphs for compatibility.")

(defun spar^l-mode--tty-p ()
  "Return t if glyphs should be restricted to a TTY-friendly set."
  (or spar^l-mode-compatibility-mode
      (not (display-graphic-p))))

(defun spar^l-mode--choose (seq)
  "Pick a random item from SEQ."
  (nth (random (length seq)) seq))

(defun spar^l-mode--random-string (body tail width)
  "Build a random string from elements of BODY, ending with TAIL.

The string will be no longer than WIDTH columns."
  (let ((s ""))
    (while (<= (length s) width)
      (setq s (concat s (spar^l-mode--choose body))))
    (truncate-string-to-width
     s width nil nil (spar^l-mode--choose tail))))

(defun spar^l-mode--string (width)
  "Create a random sparkle text no longer than WIDTH columns."
  (interactive)
  (random (number-to-string width))
  (let* ((tail (if (spar^l-mode--tty-p) spar^l-mode-tail-tty
                  spar^l-mode-tail))
         (body (if (spar^l-mode--tty-p) spar^l-mode-body-tty
                 spar^l-mode-body)))
    (mapcar (lambda (c) (make-glyph-code c 'spar^l-mode))
            (concat "\n"
                    (spar^l-mode--random-string body tail (1- width))
                    "\n"))))

;;;###autoload
(define-minor-mode spar^l-mode
  "Display of Control-l (`^L') characters as sparkles."
  nil nil nil
  :global t :group 'Spar^L
  :lighter " ･｡ﾟ✧"
  (if spar^l-mode
      (add-hook 'window-configuration-change-hook #'spar^l-mode-refresh)
    (remove-hook 'window-configuration-change-hook #'spar^l-mode-refresh))
  (walk-windows
   (lambda (window)
     (let ((display-table (or (window-display-table window)
                              (set-window-display-table
                               window (make-display-table)))))
       (aset display-table ?\014
             (and spar^l-mode
                  (vconcat (spar^l-mode--string
                            (window-body-width window)))))))))
   
(defun spar^l-mode-refresh ()
  "Re-enable spar^L mode if it's on."
  (when spar^l-mode
    (spar^l-mode t)))

(provide 'spar^l-mode)

;;; spar^l-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
