(require 'json)
(require 'eieio)

(defstruct db-entry
  content language)

(defun db-entry-to-json (entry)
  "Encode a DB entry into a JSON string."
  (json-encode `((content . ,(db-entry-content entry))
                 (language . ,(db-entry-language entry)))))

(defun db-entry-from-json (string)
  "Turn a JSON expression into a DB entry."
  (let ((json (json-read-from-string string)))
    (make-db-entry :content (cdr (assoc 'content json))
                   :language (cdr (assoc 'language json)) )))

(defgeneric pastebin-db-get (db id)
  "Get paste entry ID from database DB. Returns NIL if ID doesn't exist.")

(defgeneric pastebin-db-put (db id entry)
  "Put a paste entry into database DB, returning ENTRY.")

;; Hash table database

(defclass db-hash-table ()
  ((table :initform (make-hash-table :test 'equal)))
  (:documentation "Database stored in a native hash table."))

(defmethod pastebin-db-get ((db db-hash-table) id)
  (gethash id (slot-value db 'table)))

(defmethod pastebin-db-put ((db db-hash-table) id entry)
  (puthash id entry (slot-value db 'table)))
