;;; spar^l-mode.el --- . ☆ · ﹡ ⸼ ✧ ⁀‿⁐⁓✨ -*- lexical-binding: t -*-
;;
;; Author: Joe Wreschnig
;; Package-Version: 20170805
;; Package-Requires: ((emacs "25"))
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
;;
;; There are three ways I've seen other Emacs Lisp programs implement
;; "fancy ^L":
;;
;; 1) Display tables (pretty-print-c-l): This is the easiest way to do
;;    it globally, and also the only way to support strings as wide as
;;    a window, since it's the one that's set per-window rather than
;;    per-buffer.  However, you also can't easily disable it for
;;    specific modes or ^Ls that are not at the start of a line (which
;;    Emacs doesn't treat as page breaks), and there are bad
;;    interactions between mouse scrolling and multi-line display
;;    table elements (though cursor motion works fine).
;;
;; 2) Fontification / properties (e.g. https://github.com/wasamasa/form-feed)
;;    This lets you trigger it on a per-buffer basis, but can
;;    interfere with other major modes, and you're limited in what you
;;    can render at exactly window-width. All cursor and scrolling
;;    motion I've tested works fine with multi-line 'display
;;    properties, unlike...
;;
;; 3) Overlays (e.g. https://www.emacswiki.org/emacs/OverlayControlL)
;;    which are basically the same as fontification in terms of
;;    features.  You're less likely to mess up another mode's
;;    fontification, but more likely to run into bad performance.
;;    multi-line overlays have bad interactions with cursor motion.
;;
;; Since each of these alone has significant downsides, this package
;; uses a hybrid approach.  It uses font-lock tied to a minor mode to
;; change `page-delimiter's to a private Unicode codepoint.  Then it
;; maps that private codepoint to a window-specific string via the
;; display table.

;;;###autoload
(define-minor-mode spar^l-mode
  "Toggle display of page breaks (`^L') as sparkles.
With a prefix argument ARG, enable Spar^L mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

You can customize this minor mode, see option `spar^l-mode'."
  :global t :group 'Spar^L
  :require 'spar^l-mode
  (remove-hook 'after-change-major-mode-hook #'spar^l-mode--handle-buffer)
  (remove-hook 'window-size-change-functions #'spar^l-mode-refresh-frame)
  (remove-hook 'after-setting-font-hook #'spar^l-mode-refresh-frame)
  (when spar^l-mode
    (add-hook 'after-change-major-mode-hook #'spar^l-mode--handle-buffer)
    (add-hook 'window-size-change-functions #'spar^l-mode-refresh-frame)
    (add-hook 'after-setting-font-hook #'spar^l-mode-refresh-frame))
  (mapc #'spar^l-mode-refresh-frame (frame-list))
  (mapc #'spar^l-mode--handle-buffer (buffer-list)))

(defgroup Spar^L nil
  "Sparkle instead of page breaks (`^L')."
  :prefix "spar^L-" :group 'convenience)

(defface spar^l-mode '((t :inherit (escape-glyph)))
  "The face to use for sparkling."
  :group 'Spar^L)

(defcustom spar^l-mode-tail "⁀‿⁐⁓✨"
  "String to conclude sparkling."
  :type 'string)

(defcustom spar^l-mode-body "⸰ . ☆ · ﹡ ⸼ ✧ ⁕ ° "
  "String to repeat for sparkling."
  :type 'string)

(defcustom spar^l-mode-compatibility-mode nil
  "Restrict sparkles to common glyphs for compatibility.

This is always enabled in text-only terminals.  Setting this also
enables it for graphical displays, which may be needed if your
default fonts don't have the required glyphs or you encounter
performance problems with Emacs's font rendering."
  :type 'boolean)

(defcustom spar^l-mode-tail-compat "  ~ *"
  "String to conclude sparkling with restricted glyphs."
  :type 'string)

(defcustom spar^l-mode-body-compat ". · ° * . ° * · "
  "String to use for sparkling with restricted glyphs."
  :type 'string)

(defcustom spar^l-mode-exclude-major-modes
  '()
  "Disable sparkling major modes matching these patterns.

This is necessary for modes that also have unusual fontification
behaviors or where you want to see literal `^L'.

Sparkling is always disabled in modes without fontification."
  :type '(repeat regexp))



(require 'subr-x)
(require 'seq)

(defun spar^l-mode--compat-p ()
  "Return non-nil if the selected frame should restrict sparkle glyphs."
  (or spar^l-mode-compatibility-mode (not (display-graphic-p))))

(defvar spar^l-mode--inhibit-refresh nil
  "Inhibit sparkling refresh if set.

This is used to prevent infinite loops when creating a new window to test
the width of a string for a window.")

(defun spar^l-mode--pixel-width (string)
  "Calculate the width of a STRING in the selected frame and window."
  (with-temp-buffer
    (insert (propertize string 'face 'spar^l-mode))
    (save-window-excursion
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil (current-buffer))
      (car (window-text-pixel-size)))))

(defun spar^l-mode--string-for-window (window)
  "Create a sparkle to fit within WINDOW."
  (with-selected-frame (window-frame window)
    (if (spar^l-mode--compat-p)
        (let ((width (window-body-width window)) (s ""))
          (while (< (string-width s) width)
            (setq s (concat s spar^l-mode-body-compat)))
          (truncate-string-to-width
           s (1- width) nil nil spar^l-mode-tail-compat))

      (let* ((spar^l-mode--inhibit-refresh t)
             (body-width (spar^l-mode--pixel-width spar^l-mode-body))
             (tail-width (spar^l-mode--pixel-width spar^l-mode-tail))
             (avail-width (- (window-body-width window t) tail-width))
             (s ""))
        (dotimes (_ (ceiling (/ (float avail-width) body-width)))
          (setq s (concat s spar^l-mode-body)))
        (while (>= (spar^l-mode--pixel-width s) avail-width)
          ;; This could be searched more efficiently, but I'm not sure
          ;; `s' is ever over-long enough to justify the overhead.
          (setq s (substring s 0 -1)))
        (concat s spar^l-mode-tail)))))

(defun spar^l-mode--keyword ()
  "Generate a font-lock keyword for page breaks."
  `((,page-delimiter
     (0 `(face spar^l-mode
               display "\uE0C0"
               yank-handler (nil ,(substring-no-properties
                                   (match-string 0))))))))

(defun spar^l-mode-refresh-window (&optional window)
  "Refresh the sparkles in WINDOW."
  (setq window (or window (selected-window)))
  (unless (window-minibuffer-p window)
    ;; Calculating pixel-accurate string width is expensive, don't do
    ;; it unless the window width has changed.
    (let ((display-table (window-display-table window))
          (width (when spar^l-mode (window-body-width window t))))
      (when (not (eql width (window-parameter window 'spar^l-mode-width)))
        (unless display-table
          (setq display-table (make-display-table)))
        (aset display-table ?\uE0C0
              (when spar^l-mode
                (vconcat
                 (mapcar (lambda (c) (make-glyph-code c 'spar^l-mode))
                         (spar^l-mode--string-for-window window)))))
        (set-window-display-table window display-table)
        (set-window-parameter window 'spar^l-mode-width width)))))

(defun spar^l-mode-refresh-frame (&optional frame)
  "Refresh the sparkles for all windows in FRAME."
  (mapc #'spar^l-mode-refresh-window
        (window-list (or frame (selected-frame)) 'skip-minibuffer)))

(defun spar^l-mode--excluded (buffer)
  "Return non-nil if sparkling should be ignored in BUFFER."
  (or (not (font-lock-specified-p t))
      (let ((mode-name
             (symbol-name (buffer-local-value 'major-mode buffer))))
        (seq-some (lambda (re) (string-match-p re mode-name))
                  spar^l-mode-exclude-major-modes))))

(defun spar^l-mode--handle-buffer (&optional buffer)
  "Fontify BUFFER to match the sparkle settings."
  (setq buffer (or buffer (current-buffer)))
  (unless (spar^l-mode--excluded buffer)
    (with-current-buffer buffer
      (if spar^l-mode
          (font-lock-add-keywords nil (spar^l-mode--keyword) 'append)
        (font-lock-remove-keywords nil (spar^l-mode--keyword))
        (save-mark-and-excursion
         (goto-char 0)
         (with-silent-modifications
           (while (re-search-forward page-delimiter nil t)
             (remove-list-of-text-properties
              (match-beginning 0) (match-end 0)
              '(display face yank-handler))))))
      (font-lock-flush))))

(provide 'spar^l-mode)

;;; spar^l-mode.el ends here



;; Local Variables:
;; sentence-end-double-space: t
;; End:
