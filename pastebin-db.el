;;; pastebin-db.el --- backend databases for the pastebin package

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This is a pluggable database API with two implementations:

;; * hash-table (no on-disk storage)
;; * flat-file

;; New implementations only need to create a class that implements the
;; two generic functions, `pastebin-db-get' and `pastebin-db-put'.

;;; Code:

(require 'json)
(require 'eieio)

(defstruct db-entry
  "Represents a single, immutable database entry."
  (content "" :read-only t)
  (language "" :read-only t)
  (expiration 0 :read-only t)
  (title "" :read-only t)
  (parent nil :read-only t)
  (time 0 :read-only t))

(defun db-entry-to-json (entry)
  "Encode a DB entry into a JSON string."
  (json-encode `((content . ,(db-entry-content entry))
                 (language . ,(db-entry-language entry))
                 (expiration . ,(db-entry-expiration entry))
                 (title . ,(db-entry-title entry))
                 (parent . ,(db-entry-parent entry))
                 (time . ,(db-entry-time entry)))))

(defun db-entry-from-json (string)
  "Turn a JSON expression into a DB entry."
  (let ((json (json-read-from-string string)))
    (macrolet ((ref (slot) `(cdr (assoc (quote ,slot) json))))
      (make-db-entry :content (ref content)
                     :language (ref language)
                     :expiration (+ (float-time) (ref expiration))
                     :title (ref title)
                     :parent (ref parent)
                     :time (float-time)))))

(defun db-entry-alive-p (entry)
  "Return T if the entry has not expired."
  (< (float-time) (db-entry-expiration entry)))

(defgeneric pastebin-db-get (db id)
  "Get paste entry ID from database DB. Returns NIL if ID doesn't
exist. The ID passed to this method should already be sanitized.")

(defgeneric pastebin-db-put (db id entry)
  "Put a paste entry into database DB, returning ENTRY. The ID
passed to this method should already be sanitized.")

;; Hash table database

(defclass db-hash-table ()
  ((table :initform (make-hash-table :test 'equal)))
  (:documentation "Database stored in a native hash table."))

(defun make-db-hash-table ()
  "Create a new empty db-hash-table."
  (make-instance 'db-hash-table))

(defmethod pastebin-db-get ((db db-hash-table) id)
  (let ((entry (gethash id (slot-value db 'table))))
    (when entry
      (if (db-entry-alive-p entry)
          entry
        (remhash id (slot-value db 'table))))))

(defmethod pastebin-db-put ((db db-hash-table) id entry)
  (puthash id entry (slot-value db 'table)))

;; Flat-file database

(defclass db-flat-file ()
  ((directory :initarg :directory))
  (:documentation "Entries are stored as individual files in a directory."))

(defun make-db-flat-file (root-directory)
  "Make a new flat-file database using ROOT-DIRECTORY as storage."
  (make-instance 'db-flat-file :directory root-directory))

(defun db-flat-file--resolve (id)
  "Convert an ID into a path."
  (concat (substring id 0 2) "/" id))

(defmethod pastebin-db-get ((db db-flat-file) id)
  (let* ((root (slot-value db 'directory))
         (file (expand-file-name (db-flat-file--resolve id) root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (let ((entry (read (current-buffer))))
          (if (db-entry-alive-p entry)
              entry
            (delete-file file)))))))

(defmethod pastebin-db-put ((db db-flat-file) id entry)
  (let* ((root (slot-value db 'directory))
         (file (expand-file-name (db-flat-file--resolve id) root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (prin1 entry (current-buffer)))))

;; SQLite (this is messy, but it works!)

(defvar sqlite3-program-name "sqlite3"
  "The location of the sqlite3 command line program.")

(defclass db-sqlite ()
  ((file :initarg :file))
  (:documentation "Requires the sqlite3 command line program."))

(defun db-sqlite--parse-csv ()
  "Split the CSV line in the buffer into a list."
  (flet ((unquote (string)
           (cond
            ((string= string "\"\"") "")
            ((eql (aref string 0) ?\")
             (substring (replace-regexp-in-string "\"\"" "\"" string) 1 -1))
            (string))))
    (loop with re = "\"\\(?:[^\"]\\|\"\"\\)*\"\\|[^,]+\\|\"\""
          while (re-search-forward re nil t)
          collect (unquote (match-string 0)))))

(defun db-sqlite--quote (content)
  "Quote CONTENT for insertion."
  (format "'%s'" (replace-regexp-in-string "'" "''" content)))

(defun sqlite-eval (db &rest statement)
  "Evaluate a STATEMENT in DB and return the result as a list."
  (declare (indent defun))
  (let ((file (slot-value db 'file)))
    (with-temp-buffer
      (dolist (part statement)
        (if (null part)
            (insert "'' ")
          (insert part " ")))
      (insert ";")
      (call-process-region (point-min) (point-max) sqlite3-program-name
                           t t nil "-csv" file)
      (when (> (buffer-size) 0)
        (delete-char -1)
        (goto-char (point-min))
        (db-sqlite--parse-csv)))))

(defun make-db-sqlite (db-file)
  "Make a new database connection to DB-FILE."
  (let ((db (make-instance 'db-sqlite :file (expand-file-name db-file))))
    (prog1 db
      (sqlite-eval db
        "create table if not exists pastebin ("
        "id text," "content blob," "language text," "expiration real,"
        "title blob," "parent text," "time real,"
        "PRIMARY KEY (id))"))))

(defmethod pastebin-db-get ((db db-sqlite) id)
  (let ((list (sqlite-eval db
                "select * from pastebin where id = " (format "'%s'" id))))
    (when list
      (destructuring-bind (id content language expiration title parent time)
          list
        (make-db-entry :content content :language language
                       :expiration (string-to-number expiration)
                       :title title
                       :parent (if (string= parent "") nil parent)
                       :time (string-to-number time))))))

(defmethod pastebin-db-put ((db db-sqlite) id entry)
  (setq a entry)
  (prog1 entry
    (sqlite-eval db
      "insert into pastebin values ("
      (db-sqlite--quote id) ","
      (db-sqlite--quote (db-entry-content entry)) ","
      (db-sqlite--quote (db-entry-language entry)) ","
      (number-to-string (db-entry-expiration entry)) ","
      (db-sqlite--quote (db-entry-title entry)) ","
      (db-sqlite--quote (or (db-entry-parent entry) "")) ","
      (number-to-string (db-entry-time entry)) ")")))

(provide 'pastebin-db)

;;; pastebin-db.el ends here
