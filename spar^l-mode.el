;;; spar^l-mode.el --- . ☆ · ﹡ ⸼ ✧ ⁀‿⁐⁓✨ -*- lexical-binding: t -*-
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
;;     (require ’spar^l-mode)
;;     (spar^l-mode)

;;; Code:

(require 'subr-x)

(defconst spar^l-mode-tail "⁀‿⁐⁓✨"
  "String to conclude sparkling.")

(defconst spar^l-mode-tail-compat "  ~ *"
  "Strings to conclude sparkling with restricted glyphs.")

(defconst spar^l-mode-body "⸰ . ☆ · ﹡ ⸼ ✧ ⁕ ° "
  "String to use for sparkling.")

(defconst spar^l-mode-body-compat ". · ° * . ° * · "
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

(defvar spar^l-mode--inhibit-refresh nil
  "Inhibit sparkling refresh if set.

This is used to prevent infinite loops when creating a new window to test
the width of a string for a window.")

(defun spar^l-mode--pixel-width (string frame)
  "Calculate the width of a STRING in a given FRAME."
  (with-selected-frame frame
    (with-temp-buffer
      (insert (propertize string 'face 'spar^l-mode))
      (save-window-excursion
        (set-window-dedicated-p nil nil)
        (set-window-buffer nil (current-buffer))
        (car (window-text-pixel-size))))))

(defun spar^l-mode--string-for-window (window)
  "Create a sparkle to fit within WINDOW."
  (interactive)
  (if (spar^l-mode--compat-p)
      (let ((width (window-body-width window)) (s))
        (while (< (string-width s) width)
          (setq s (concat s spar^l-mode-body-compat)))
        (truncate-string-to-width s width nil nil spar^l-mode-tail-compat))

    (let* ((spar^l-mode--inhibit-refresh t)
           (frame (window-frame window))
           (body-width (spar^l-mode--pixel-width spar^l-mode-body frame))
           (tail-width (spar^l-mode--pixel-width spar^l-mode-tail frame))
           (avail-width (- (window-body-width window t) tail-width))
           (s ""))
      (dotimes (_ (ceiling (/ (float avail-width) body-width)))
        (setq s (concat s spar^l-mode-body)))
      (while (>= (spar^l-mode--pixel-width s frame) avail-width)
        ;; This could be searched more efficiently, but I'm not sure
        ;; `s' is ever over-long enough to justify the overhead.
        (setq s (substring s 0 -1)))
      (concat s spar^l-mode-tail))))

;;;###autoload
(define-minor-mode spar^l-mode
  "Display Control-L (`^L') characters as sparkles."
  :global t :group 'Spar^L
  (if spar^l-mode
      (add-hook 'window-size-change-functions #'spar^l-mode-refresh)
    (remove-hook 'window-size-change-functions #'spar^l-mode-refresh))
  (spar^l-mode-refresh))

(defun spar^l-mode-refresh (&optional frame)
  "Refresh sparkles, either all or those in FRAME."
  (walk-windows
   (lambda (window)
     (let ((display-table (window-display-table window)))
       (when (or display-table spar^l-mode)
         (unless display-table
           (setq display-table (make-display-table)))
         (aset display-table ?\C-l
               (and spar^l-mode
                    (vconcat
                     (mapcar (lambda (c) (make-glyph-code c 'spar^l-mode))
                             (format
                              "\n%s\n"
                              (spar^l-mode--string-for-window window))))))
         (set-window-display-table window display-table))))
     'skip-minibuffer (or frame 0)))

(provide 'spar^l-mode)

;;; spar^l-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
