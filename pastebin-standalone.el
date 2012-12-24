;;; pastebin-standalone.el --- run a standalone pastebin server

;;; Usage:

;;   emacs -nw -Q -L . --script pastebin-standalone.el

;; On unix platforms there's also --daemon to daemonize the server.

;;; Settings:

(setq httpd-port 8000)

;; Uncomment to use a flat-file database in /tmp/pastebin. Unlike the
;; default, this will be persistent between runs.
;(eval-after-load 'pastebin
;  '(setq pastebin-db (make-db-flat-file "/tmp/pastebin")))

;;; Version Check:

(when (< emacs-major-version 24)
  (princ "error: requires Emacs version >= 24\n" t)
  (kill-emacs 1))

;; Dependencies:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not (package-installed-p 'simple-httpd))
  (package-refresh-contents)
  (package-install 'simple-httpd))

;; Start the Server:

(require 'pastebin)
(httpd-start)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))
(switch-to-buffer "*httpd*")

;; Start a Browser:

(browse-url (format "http://localhost:%d/pastebin/" httpd-port))
