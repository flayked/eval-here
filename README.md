# eval-here
Eval within current lexical environment for Emacs Lisp

The version in this directory has been tested on emacs-27.0.91. It should also
work on the latest git version of emacs (as of 2020-06-09). For older versions
of emacs, see the emacs-24.5 and emacs-26.3 directories. The cl--sm-macroexpand
function distributed with emacs has changed significantly between the versions,
so eval-here will not work correctly unless you use the appropriate version for
your version of GNU Emacs.
