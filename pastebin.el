;;; pastebin.el --- pastebin example with simple-httpd

(require 'cl)
(require 'url-util)
(require 'simple-httpd)
(require 'pastebin-db)

(defvar pastebin-data-root (file-name-directory load-file-name))

;; DB setup

(defvar pastebin-db (make-instance 'db-hash-table)
  "The current pastebin database.")

(defun pastebin-get (id)
  "Get a pastebin entry from the database."
  (pastebin-db-get pastebin-db-get id))

(defun pastebin-put (id entry)
  "Put a new pastebin entry in the database."
  (pastebin-db-put pastebin-db-get id entry))

;; IDs

(defvar pastebin-id-digits
  "0123456789abcedfghijklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ-.")

(defun pastebin-id-valid-p (id)
  "Return T if the given ID is valid and unique."
  (not (or (pastebin-get id)
           (string-match-p "^[.-]\\|[.-]$\\|\\.-\\|-\\." id))))

(defun* pastebin-make-id (&optional (min-length 4))
  "Generate a new, unique pastebin ID."
  (flet ((make-digit ()
           (aref pastebin-id-digits (random (length pastebin-id-digits)))))
    (loop for length upfrom min-length
          for digits = (loop repeat length collect (make-digit))
          for id = (coerce digits 'string)
          when (pastebin-id-valid-p id) return id)))

;; Servlets

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
