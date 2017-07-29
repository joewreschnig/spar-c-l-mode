;;; spar^l-mode.el --- . ☆ · ﾟ ✧ ｡ ⸼ ⸰ ° · ✧ ︵‿✧   -*- lexical-binding: t -*-
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
;; isn't?  Then Spar^L mode is for you!
;;
;; Spar^L mode replaces the form feed (Control-L, or ^L) character in
;; Emacs buffers with something much cuter.
;;
;; To use it, add the following to your Emacs initialization:
;;
;;     (spar^l-mode t)

;;; Code:

(defconst spar^l-mode-tail '("︵‿✧" "︵‿☆")
  "Strings to pick from to conclude sparkling.")

(defconst spar^l-mode-tail-compat '("  ~  *")
  "Strings to pick from to conclude sparkling with wide compatibility.")

(defconst spar^l-mode-body '(". ☆ · ﾟ ✧ ｡ " "⸼ ⸰ ° · ✧ ")
  "Strings to pick from while sparkling.")

(defconst spar^l-mode-body-compat '(". · ° * " ". * ° · ")
  "Strings to pick from while sparkling with wide compatibility.")

(defface spar^l-mode '((t :inherit (escape-glyph)))
  "The face to use for the sparkles."
  :group 'Spar^L)

(defgroup Spar^L nil
  "Sparkle instead of displaying Control-L (`^L') characters."
  :prefix "spar^L-" :group 'convenience)

(defcustom spar^l-mode-compatibility-mode nil
  "Restrict sparkles to common glyphs for compatibility.

This is always enabled in console modes.  Setting this also
enables it for graphical displays, which may be needed if your
monospace fonts don't have the required glyphs.")

(defun spar^l-mode--compat-p ()
  "Return t if glyphs should be restricted to a widely-compatible set."
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
  (let* ((tail (if (spar^l-mode--compat-p) spar^l-mode-tail-compat
                  spar^l-mode-tail))
         (body (if (spar^l-mode--compat-p) spar^l-mode-body-compat
                 spar^l-mode-body)))
    (mapcar (lambda (c) (make-glyph-code c 'spar^l-mode))
            (concat "\n"
                    (spar^l-mode--random-string body tail (1- width))
                    "\n"))))

;;;###autoload
(define-minor-mode spar^l-mode
  "Display Control-l (`^L') characters as sparkles."
  :global t :group 'Spar^L
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
                            (window-body-width window)))))))
   'skip-minibuffer 'visible))
   
(defun spar^l-mode-refresh ()
  "Re-enable Spar^L mode if it's on."
  (when spar^l-mode
    (spar^l-mode t)))

(provide 'spar^l-mode)

;;; spar^l-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
