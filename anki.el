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
(defun anki-create-note ()
  )

;;; Forms

(defun anki-form-note ()
  "Anki widget"
  (interactive)
  (switch-to-buffer "*Anki Add Note*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Create a new card \n\n")
  (setq-local
   front-widget (widget-create 'editable-field
                               :format "Front:\n%v"))
  (setq-local
   back-widget
   (widget-create 'editable-field
                  :format "Back:\n%v"))
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
