;;; pastebin.el --- pastebin example with simple-httpd

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/emacs-pastebin
;; Version: 1.0
;; Package-Requires: ((simple-httpd "1.4.0"))

;;; Commentary:

;; This package provides a pastebin web service using the simple-httpd
;; package. The backend here is kept minimal, basically a simple
;; database and file server, with the bulk of the work being done by
;; the client. Three servlets are created,

;; * /pastebin/     -- serves static files from the data root
;; * /pastebin/get  -- serves a paste in JSON form
;; * /pastebin/post -- accepts a new paste in JSON form

;; Dynamic insertion of a paste into the page and syntax highlighting
;; are done after load by JavaScript. Syntax highlighting could have
;; easily been done by Emacs (htmlize) but I honestly don't trust the
;; various programming modes to securely handle arbitrary data.

;; Multiple backend databases are supported. See pastebin-db.el. To
;; change to a new database rebind `pastebin-db' to a new database
;; object. For example,

;;     (setq pastebin-db (make-db-flat-file "/tmp/pastebin"))

;;; Code:

(require 'cl)
(require 'simple-httpd)
(require 'pastebin-db)

(defvar pastebin-data-root (file-name-directory load-file-name))

;; DB setup

(defvar pastebin-db (make-db-hash-table)
  "The current pastebin database.")

(defun pastebin-get (id)
  "Get a pastebin entry from the database."
  (pastebin-db-get pastebin-db (pastebin-sanitize-id id)))

(defun pastebin-put (id entry)
  "Put a new pastebin entry in the database."
  (pastebin-db-put pastebin-db (pastebin-sanitize-id id) entry))

;; IDs

(defvar pastebin-id-digits
  "0123456789abcedfghijklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ-_")

(defun pastebin-sanitize-id (id)
  "Remove invalid characters from ID."
  (replace-regexp-in-string (format "[^%s]+" pastebin-id-digits) "" id))

(defun pastebin-id-valid-p (id)
  "Return T if the given ID is valid and unique."
  (not (or (pastebin-get id)
           (string-match-p "^[_-]\\|[_-]$\\|_-\\|-_" id))))

(defun* pastebin-make-id (&optional (min-length 4))
  "Generate a new, unique pastebin ID."
  (flet ((make-digit ()
           (aref pastebin-id-digits (random (length pastebin-id-digits)))))
    (loop for length upfrom min-length
          for digits = (loop repeat length collect (make-digit))
          for id = (coerce digits 'string)
          when (pastebin-id-valid-p id) return id)))

;; Servlets

(defun httpd/pastebin (proc path args request)
  "Serve up various static files from the data root."
  (if (equal path "/pastebin")
      (httpd-redirect proc "/pastebin/")
    (let* ((file (substring path (length "/pastebin/")))
           (httpd-root pastebin-data-root)
           (path (httpd-gen-path file))
           (default (expand-file-name "index.html" pastebin-data-root)))
      (if (file-exists-p path)
          (httpd-send-file proc path request)
        (httpd-send-file proc default request)))))

(defservlet pastebin/get text/json (path args)
  "Serves a raw entry from the database."
  (let ((entry (pastebin-get (file-name-nondirectory path))))
    (cond
     ((null entry) (httpd-send-header t "text/plain" 404))
     ((assoc "raw" args)
      (insert (db-entry-content entry))
      (httpd-send-header t "text/plain" 200))
     ((assoc "download" args)
      (insert (db-entry-content entry))
      (httpd-send-header t "application/download" 200))
     (t (insert (db-entry-to-json entry))))))

(defservlet pastebin/post text/plain (path query request)
  "Adds the paste entry to the database."
  (let ((id (pastebin-make-id)))
    (pastebin-put id (db-entry-from-json (cadr (assoc "Content" request))))
    (insert id)))

(provide 'pastebin)

;;; pastebin.el ends here
