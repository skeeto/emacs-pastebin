;;; pastebin.el --- pastebin example with simple-httpd

(require 'cl)
(require 'url-util)
(require 'simple-httpd)

(defvar pastebin-data-root (file-name-directory load-file-name))

(defvar pastebin-db (make-hash-table))

(defun pastebin-get-file (file)
  (with-temp-buffer
    (insert-file-literally (expand-file-name file pastebin-data-root))
    (buffer-string)))

(defservlet pastebin text/html (path)
  (let* ((id (string-to-number (file-name-nondirectory path) 16))
         (content (url-insert-entities-in-string (gethash id pastebin-db ""))))
    (insert (format (pastebin-get-file "paste.html") content))))

(defun httpd/pastebin/post (proc path query request)
  (let* ((id (random most-positive-fixnum))
         (content (cadr (assoc "Content" request)))
         (decode (url-unhex-string (substitute ?  ?+ content) t)))
    (puthash id (substring decode 2) pastebin-db)
    (httpd-redirect proc (format "/pastebin/%x" id))))
