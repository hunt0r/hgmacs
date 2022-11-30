HGMacs --- based on [Emacs Prelude](https://github.com/bbatsov/prelude)
=============

Prelude is a great Emacs distribution.
This is a fork where I make my favorite customizations.

## Install

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `macOS`, `Solaris`, etc), you already have a recent version of Emacs installed, as well as `git` & `curl` just type in your favorite shell:

```shellsession
curl -L https://raw.githubusercontent.com/hunt0r/hgmacs/master/utils/installer.sh | sh
```

And start Emacs.

Prelude's installer will back up any existing `.emacs` file or `.emacs.d` since it will unpack Prelude's code in `.emacs.d`.
But I recommend creating a temporary local copy since I have modified Prelude's installer.
