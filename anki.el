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
  (let ((url-request-data
         (encode-coding-string
          (json-encode (anki-connect--action-format action params))
          'utf-8))
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

(defun anki-connect-deck-names ()
  "Gets the complete list of deck names for the current user."
  (let ((json-array-type 'list))
    (anki-connect-action "deckNames")))

(defun anki-connect-model-field-names (model-name)
  "Gets the complete list of field names for MODEL-NAME."
  (let ((json-array-type 'list))
    (anki-connect-action "modelFieldNames" `(("modelName" . ,model-name)))))

(defun anki-connect-note-info (note-id)
  (aref (anki-connect-action "notesInfo" `(("notes" . (,note-id)))) 0))

(defun anki-connect-add-note (deck-name model-name fields &optional tags allow-duplicate audio)
  (let ((json-null))
    (anki-connect-action
     "addNote"
     `((note .
             ((deckName . ,deck-name)
              (modelName . ,model-name)
              ;; TODO Add check that fields actually exist in model
              (fields . ,fields)
              (options . ((allowDuplicate . ,(or allow-duplicate :json-false))))
              (tags . ,(vconcat tags))))))))

(defun anki-connect-update-note (note-id &optional fields tags)
  (let ((json-null))
    (anki-connect-action
     "updateNoteFields"
     `((note .
             ((id . ,note-id)
              (fields . ,fields)))))
    ;; TODO: Handle tags (need to get current, then add/remove as
    ;;       needed
    ))

;;; Forms

(defun anki-create-note (deck note-type)
  (interactive
   (list (completing-read "Deck: " (anki-connect-deck-names))
         (completing-read "Note Type: " (anki-connect-model-names))))
  (anki-edit--note deck note-type))

(defun anki-edit-note (note-id)
  (anki-edit--note nil nil note-id))

(defun anki-edit--note (&optional deck note-type note-id)
  ;; Setup
  (when (get-buffer "*Anki Note*")
    (kill-buffer "*Anki Note*"))
  (switch-to-buffer "*Anki Note*")
  (org-mode)
  (defvar-local anki-field-widgets nil)
  (defvar-local anki-deck-name deck)
  (defvar-local anki-note-type note-type)
  (defvar-local anki-note-id note-id)

  (setq anki-deck-name deck
        anki-note-type note-type
        anki-note-id note-id)

  (let ((fields
	 (or
          (and note-id (anki--note-field-data (anki-connect-note-info note-id)))
          (mapcar 'list (anki-connect-model-field-names note-type)))))
    (setq anki-field-widgets (mapcar 'anki--create-field fields))
    ;; Populate
    ;; (use-local-map widget-keymap)
    ;; (widget-create 'push-button
    ;;                :notify (lambda (&rest ignore)
    ;;                          (let ((fields (mapcar
    ;;                                         (lambda (widget)
    ;;                                           (cons (widget-get widget :anki-field-name)
    ;;                                                 (anki-convert-org-to-html (widget-value widget))))
    ;;                                         anki-field-widgets)))
    ;;                            (if anki-note-id
    ;;                                (anki-connect-update-note anki-note-id fields)
    ;;                              (setq anki-note-id
    ;;                                    (anki-connect-add-note anki-deck-name anki-note-type fields)))
    ;;                            (anki-form-note anki-deck-name anki-note-type)))
    ;;                "Submit")
    (om-insert 1 anki-field-widgets)))

(defun anki-edit-submit ()
  ;; TODO: Possibly split the field generation into separate function
  (let ((fields (mapcar (lambda (field)
			  (cons (anki--om-to-string (om-get-property :title field))
				(anki-convert-org-to-html (anki--om-to-string (om-headline-get-section field)))))
			(om-get-headlines))))
    (if anki-note-id
	(anki-connect-update-note anki-note-id fields)
      (setq anki-note-id
	    (anki-connect-add-note anki-deck-name anki-note-type fields)))
    (anki-edit--note anki-deck-name anki-note-type)))

(defun anki-form-note (&optional deck note-type note-id)
  "Anki widget"
  (interactive
   (list (completing-read "Deck: " (anki-connect-deck-names))
         (completing-read "Note Type: " (anki-connect-model-names))))

  (anki-edit-mode)
  ;; (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Create a new card \n\n")


  (widget-setup)
  (widget-forward 1))

(defun anki--note-field-data (note-data)
  "Transform NOTE-DATA to an alist of field name and org field value."
  (mapcar (lambda (field)
            (cons
             (symbol-name (car field))
             (anki-convert-html-to-org (alist-get 'value (cdr field)))))
          (alist-get 'fields note-data)))

(defun anki--create-field (field-data)
  (let ((field-name (car field-data))
        (field-value (cdr field-data)))
    (->>
     (anki--om-parse-object-string (or field-value "\n"))
     (om-build-headline :title (list field-name)
			:post-blank (and (not field-value) 1)))))

(defun anki-convert-org-to-html (org)
  "Convert ORG to html string."
  (if (not (string= "" org))
      (with-temp-buffer
        (insert org)
        (mark-whole-buffer)
        (org-html-convert-region-to-html)
        ;; Remove paragraph tags
        (goto-char (point-min))
        (when (search-forward "<p>" nil t)
          (replace-match "")
          (when (looking-at "$")
            (delete-char 1))
          (goto-char (point-max))
          (when (search-backward "</p>" nil t)
            (replace-match "")))
        (buffer-string))
    ""))

(defun anki-convert-html-to-org (html)
  (when html
    (let ((html-temp-file (make-temp-file "html" nil ".html" html)))
      (substring (shell-command-to-string
                  (format "pandoc -f html -t org %s" html-temp-file))
                 nil -1))))
;;; Helper functions

(defun anki--om-parse-object-string (string)
  (with-temp-buffer
    (insert string)
    (om-parse-section-at 1)))

(defun anki--om-to-string (node)
  "Same as `om-to-string' but doesn't apply `om--clean' or
`om--blank' both of which seem to duplicate data."
  ;; TODO: Look into why `om-to-string' is causing duplicated data.
  (->> node
       (org-element-interpret-data)
       (substring-no-properties)))
