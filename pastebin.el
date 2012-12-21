;;; pastebin.el --- pastebin example with simple-httpd

(require 'cl)
(require 'url-util)
(require 'simple-httpd)

(defvar pastebin-data-root (file-name-directory load-file-name))

(defvar pastebin-db (make-hash-table :test 'equal))

(defvar pastebin-id-digits
  "0123456789abcedfghijklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ-.")

(defun pastebin-id-valid-p (id)
  (not (or (gethash id pastebin-db)
           (string-match-p "^[.-]\\|[.-]$\\|\\.-\\|-\\." id))))

(defun* pastebin-make-id (&optional (min-length 4))
  (loop with base = (length pastebin-id-digits)
        for length upfrom min-length
        for digits = (loop repeat length
                           collect (aref pastebin-id-digits (random base)))
        for id = (coerce digits 'string)
        when (pastebin-id-valid-p id) return id))

(defun* pastebin-make-id (&optional (min-length 4))
  (flet ((make-digit ()
           (aref pastebin-id-digits (random (length pastebin-id-digits)))))
    (loop for length upfrom min-length
          for digits = (loop repeat length collect (make-digit))
          for id = (coerce digits 'string)
          when (pastebin-id-valid-p id) return id)))

(defun pastebin-get-file (file)
  (with-temp-buffer
    (insert-file-literally (expand-file-name file pastebin-data-root))
    (buffer-string)))

(defservlet pastebin text/html (path)
  (let* ((id (file-name-nondirectory path))
         (content (url-insert-entities-in-string (gethash id pastebin-db ""))))
    (insert (format (pastebin-get-file "paste.html") content))))

(defun httpd/pastebin/post (proc path query request)
  (let* ((id (pastebin-make-id))
         (content (cadr (assoc "Content" request)))
         (decode (url-unhex-string (substitute ?  ?+ content) t)))
    (puthash id (substring decode 2) pastebin-db)
    (httpd-redirect proc (format "/pastebin/%s" id))))
