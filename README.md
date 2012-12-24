# Elisp Pastebin

This is a demo of an Emacs pastebin server using
[simple-httpd](https://github.com/skeeto/emacs-http-server)
servlets. Supports language syntax highlighting, expiration, diffs,
and two different backend databases.

## Quick Start

To try it out right away as a standalone server, without any extra
setup, run this from the repository:

    emacs -nw -Q -L . -l pastebin-standalone.el

This will automatically fetch and load the dependencies, start the
server on port 8000, and attempt to point your default browser at
it. Requires Emacs version >= 24.

## Manual Start

Add your Git repository clone to your `load-path`, start the
web-server in Emacs (`httpd-start`), load `pastebin.el`, and visit
[http://localhost:8080/pastebin/](http://localhost:8080/pastebin/).
