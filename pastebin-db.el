(require 'json)
(require 'eieio)

(defstruct db-entry
  content language expiration)

(defun db-entry-to-json (entry)
  "Encode a DB entry into a JSON string."
  (json-encode `((content . ,(db-entry-content entry))
                 (language . ,(db-entry-language entry))
                 (expiration . ,(db-entry-expiration entry)))))

(defun db-entry-from-json (string)
  "Turn a JSON expression into a DB entry."
  (let ((json (json-read-from-string string)))
    (macrolet ((ref (slot) `(cdr (assoc (quote ,slot) json))))
      (make-db-entry :content (ref content)
                     :language (ref language)
                     :expiration (+ (float-time) (ref expiration))))))

(defgeneric pastebin-db-get (db id)
  "Get paste entry ID from database DB. Returns NIL if ID doesn't exist.")

(defgeneric pastebin-db-put (db id entry)
  "Put a paste entry into database DB, returning ENTRY.")

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
      (if (< (float-time) (db-entry-expiration entry))
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

(defun db-flat-file--clean (id)
  (replace-regexp-in-string "[/~]\\|\\.\\." "" id))

(defmethod pastebin-db-get ((db db-flat-file) id)
  (let* ((root (slot-value db 'directory))
         (file (expand-file-name (db-flat-file--clean id) root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (read (current-buffer))))))

(defmethod pastebin-db-put ((db db-flat-file) id entry)
  (let* ((root (slot-value db 'directory))
         (file (expand-file-name (db-flat-file--clean id) root)))
    (with-temp-file file
      (prin1 entry (current-buffer)))))

(provide 'pastebin-db)
