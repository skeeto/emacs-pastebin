;;; pastebin.el --- pastebin example with simple-httpd

(require 'cl)
(require 'json)
(require 'url-util)
(require 'simple-httpd)
(require 'pastebin-db)

(defvar pastebin-data-root (file-name-directory load-file-name))

;; DB setup

(defvar pastebin-db (make-instance 'db-hash-table)
  "The current pastebin database.")

(defun pastebin-get (id)
  "Get a pastebin entry from the database."
  (pastebin-db-get pastebin-db id))

(defun pastebin-put (id entry)
  "Put a new pastebin entry in the database."
  (pastebin-db-put pastebin-db id entry))

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

(defvar pastebin-static
  (delete-if (lambda (file) (eql (aref file 0) ?.))
             (directory-files pastebin-data-root))
  "Static files served up by the root servlet.")

(defun httpd/pastebin (proc path args request)
  "Serve up various static files from the data root."
  (if (equal path "/pastebin")
      (httpd-redirect proc "/pastebin/")
    (let ((file (file-name-nondirectory path)))
      (flet ((expand (name) (expand-file-name name pastebin-data-root)))
        (if (member file pastebin-static)
            (httpd-send-file proc (expand file) request)
          (httpd-send-file proc (expand "index.html") request))))))

(defun httpd/pastebin/get (proc path &rest rest)
  "Serves a raw entry from the database."
  (let* ((id (file-name-nondirectory path))
         (entry (pastebin-get id)))
    (if (null entry)
        (httpd-send-header proc "text/plain" 404)
      (with-httpd-buffer proc "text/json"
        (insert (json-encode `((content . ,(db-entry-content entry))
                               (type . ,(db-entry-type entry)))))))))

(defun httpd/pastebin/post (proc path query request)
  "Post a new entry into the database."
  (let* ((id (pastebin-make-id))
         (content (cadr (assoc "Content" request)))
         (decode (url-unhex-string (substitute ?  ?+ content) t)))
    (pastebin-put id (make-db-entry :content (substring decode 2)))
    (httpd-redirect proc (format "/pastebin/%s" id))))
