(require 'widget)

;;; Anki Connect

(defconst anki-connect-default-version 6
  "Version of anki connect API to use if none specified.")

(cl-defun anki-connect--action-format (action &optional params (version anki-connect-default-version))
  "Format ACTION, PARAMS, and VERSION to be submitted to anki connect."
  (let (anki-action-data)
    (when version
      (push `(version . ,version) anki-action-data))
    (when params
      (push `(params . ,params) anki-action-data))
    (push `(action . ,action) anki-action-data)
    anki-action-data))

(defun anki-connect-action (action &optional params)
  (let ((url-request-data (json-encode (anki-connect--action-format action params)))
        (url-request-method "POST")
        reply)

    (with-current-buffer (url-retrieve-synchronously "http://localhost:8765" t t 10)
      (goto-char url-http-end-of-headers)
      (setq reply (json-read)))

    (let-alist reply
      (when .error (error .error))
      .result)))

(defun anki-connect-model-names ()
  "Gets the complete list of model names for the current user."
  (let ((json-array-type 'list))
    (anki-connect-action "modelNames")))

(defun anki-connect-model-field-names (model-name)
  "Gets the complete list of field names for MODEL-NAME."
  (let ((json-array-type 'list))
    (anki-connect-action "modelFieldNames" `(("modelName" . ,model-name)))))

(defun anki-get-note (note-id)
  (aref (anki-connect-action "notesInfo" `(("notes" . (,note-id)))) 0))

;;; Forms

(defun anki-form-note (&optional note-type note-id)
  "Anki widget"
  (interactive
   (list (completing-read "Note Type: " (anki-connect-model-names))))
  ;; Setup
  (switch-to-buffer "*Anki Note*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Create a new card \n\n")

  (let ((fields (mapcar 'list (anki-connect-model-field-names note-type))))
    (mapcar 'anki--create-field fields))
  ;; Populate
  (use-local-map widget-keymap)
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let ((note-front (widget-value front-widget))
                                 (note-back (widget-value back-widget)))
                             (message
                              (int-to-string (anki-connect-action
                                              "addNote"
                                              `(("note" .
                                                 (("deckName" . "Test")
                                                  ("modelName" . "Basic")
                                                  ("fields" .
                                                   (("Front" . ,note-front)
                                                    ("Back" . ,note-back)))
                                                  ("tags" . ,(vconcat nil))))))))))
                 "Submit")
  (widget-setup))

(defun anki--create-field (field-data)
  (let ((field-name (car field-data))
        (field-value (cdr field-data)))
    (widget-create 'editable-field
                   :format (concat field-name ":\n%v")
                   :value (or field-value ""))))
