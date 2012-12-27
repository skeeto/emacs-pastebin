;;; pastebin-test.el --- unit tests for the pastebin package

;;   emacs -batch -L . -l pastebin-test.el -f ert-run-tests-batch

(require 'cl)
(require 'ert)
(require 'pastebin)

(defun pastebin-test-exercise-db (db)
  "Attempt to store and retrieve tricky values to and from a database."
  (macrolet ((entry (content)
               (make-db-entry :content content
                              :expiration (ffloor (+ (float-time) 60)))))
    (let ((data `(("foo" ,(entry "bar"))
                  ("foO" ,(entry "naïveté"))
                  ("fo0" ,(entry "a\n\"b\"\n'c'\n")))))
      (loop for (id entry) in data
            do (pastebin-db-put db id entry))
      (loop for (id entry) in data
           do (should (equal (pastebin-db-get db id) entry))))))

(ert-deftest pastebin-db-hash-table ()
  "Test the hash table database."
  (pastebin-test-exercise-db (make-db-hash-table)))

(ert-deftest pastebin-db-flat-file ()
  "Test the flat file database."
  (let ((directory (make-temp-file "pastebin-" t)))
    (unwind-protect
        (pastebin-test-exercise-db (make-db-flat-file directory))
      (delete-directory directory t))))

(ert-deftest pastebin-db-sqlite ()
  "Test the SQLite database."
  (when (file-exists-p sqlite3-program-name)
    (let ((db-file (make-temp-file "pastebin-")))
      (unwind-protect
          (pastebin-test-exercise-db (make-db-sqlite db-file))
        (delete-file db-file)))))

(ert-deftest pastebin-test-json ()
  "Test the JSON translation functions."
  (flet ((float-time () 0.0))
    (let ((entry (make-db-entry :content "\"naïveté\n\"" :expiration 0.0)))
      (should (equal (db-entry-from-json (db-entry-to-json entry)) entry)))))

;;; pastebin-test.el ends here
