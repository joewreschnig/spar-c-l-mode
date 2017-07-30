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

(require 'subr-x)

(defconst spar^l-mode-tail " ︵‿★"
  "String to conclude sparkling.")

(defconst spar^l-mode-tail-compat "  ~ *"
  "Strings to conclude sparkling with restricted glyphs.")

(defconst spar^l-mode-body ". ☆ · ﾟ ✧ ｡ ⸼ ⸰ ° · ✧"
  "String to use for sparkling.")

(defconst spar^l-mode-body-compat ". · ° * . ° * ·"
  "Strings to use for sparkling with restricted glyphs.")

(defface spar^l-mode '((t :inherit (escape-glyph)))
  "The face to use for sparkling."
  :group 'Spar^L)

(defgroup Spar^L nil
  "Sparkle instead of displaying Control-L (`^L') characters."
  :prefix "spar^L-" :group 'convenience)

(defcustom spar^l-mode-compatibility-mode nil
  "Restrict sparkles to common glyphs for compatibility.

This is always enabled in text-only terminals.  Setting this also
enables it for graphical displays, which may be needed if your
default fonts don't have the required glyphs.")

(defun spar^l-mode--compat-p (&optional display)
  "Return non-nil if DISPLAY should use a restricted glyph set."
  (or spar^l-mode-compatibility-mode
      (not (display-graphic-p display))))

(defun spar^l-mode--string (width)
  "Create a sparkle no longer than WIDTH columns."
  (interactive)
  (let* ((tail (if (spar^l-mode--compat-p) spar^l-mode-tail-compat
                  spar^l-mode-tail))
         (body (if (spar^l-mode--compat-p) spar^l-mode-body-compat
                 spar^l-mode-body))
         (bodies (string-join
                  (make-list (1+ (/ width (length body))) body) " ")))
    (mapcar (lambda (c) (make-glyph-code c 'spar^l-mode))
            (concat "\n"
                    (truncate-string-to-width bodies width nil nil tail)
                    "\n"))))

;;;###autoload
(define-minor-mode spar^l-mode
  "Display Control-l (`^L') characters as sparkles."
  :global t :group 'Spar^L
  (if spar^l-mode
      (add-hook 'window-size-change-functions #'spar^l-mode-refresh)
    (remove-hook 'window-size-change-functions #'spar^l-mode-refresh))
  (spar^l-mode-refresh))
   
(defun spar^l-mode-refresh (&optional _)
  "Re-enable Spar^L mode if it's on."
  (walk-windows
   (lambda (window)
     (let ((display-table (window-display-table window)))
       (when (or display-table spar^l-mode)
         (unless display-table
           (setq display-table (make-display-table)))
         (aset display-table ?\C-l
               (and spar^l-mode
                    (vconcat (spar^l-mode--string
                              (window-body-width window)))))
         (set-window-display-table window display-table))))

   'skip-minibuffer 'visible))

(provide 'spar^l-mode)

;;; spar^l-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
