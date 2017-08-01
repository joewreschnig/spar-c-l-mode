# Spar^l Mode

Confused by all the ^Ls you see in official Emacs code?
Disappointed that you installed a package named "pretty" that
isn't?  Then Spar^L mode is for you!

Spar^L mode replaces the form feed (Control-L, or ^L) character in
Emacs buffers with something much cuter.

⸰ . ☆ · ﹡ ⸼ ✶ ⁕ ° ⸰ . ☆ · ﹡ ⸼ ✶ ⁕ ° ⸰ . ☆ · ﹡ ⸼ ✶ ⁕ ° ⸰ . ☆ · ﹡ ⸼ ✶ ⁀‿⁐⁓✨

To use it, add the following to your Emacs initialization after
installing the package:

    (require ’spar^l-mode)
    (spar^l-mode)

## Troubleshooting

**If you lack some of the characters used for sparkles** I recommend
installing the [Symbola][] font.

**If resizing windows or activating the minibuffer is slow**, you have
hit what seems to be a pathological case in Emacs’s font handling. I
haven’t been able to fully diagnose the problem, but it happens when
Emacs has to check too many fallback fonts in order to render the
string (which we do more than usual, to fit the string to the window).

You can probably avoid this problem by restricting which fonts are
checked for which scripts. In particular, this triggers when Symbola
is registered for the whole `unicode` script but it doesn’t have the
right glyph. Instead, only use Symbola for the scripts you want it
to cover:

    ;; Instead of this:
    (set-fontset-font t ’unicode "Symbola 14" nil ’prepend)

    ;; Try this:
    (set-fontset-font t 'symbol "Symbola 14" nil ’prepend)

The default sparkles are restricted to characters from the `latin` and
`symbol` scripts.

[Symbola]: http://users.teilar.gr/~g1951d/

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the [GNU General Public License][] as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

[GNU General Public License]: https://www.gnu.org/licenses/gpl-3.0.en.html
