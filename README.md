# Elisp Pastebin

This is a demo of an Emacs pastebin server using
[simple-httpd](https://github.com/skeeto/emacs-http-server)
servlets. Supports language syntax highlighting, expiration, diffs,
and two different backend databases.

Add your Git repository clone to your `load-path`, start the
web-server in Emacs (`httpd-start`), load `pastebin.el`, and visit
[http://localhost:8080/pastebin/](http://localhost:8080/pastebin/).
