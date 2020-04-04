;;; -*- lexical-binding: t; -*-


(require 'widget)

;;; Anki Connect

(defconst anki-connect-default-version 6
  "Version of anki connect API to use if none specified.")

(defvar anki-media-directory nil
  "Path to anki-media-directory")

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
  ;; TODO: create function for multiple notes and then call it from here
  (let* ((note-info
	  (aref (anki-connect-action "notesInfo" `(("notes" . (,note-id)))) 0))
	 ;; Get info for first card since anki stores deck at card level
	 (card-info (anki-connect-card-info (elt (alist-get 'cards note-info) 0))))
    (append note-info `((deckName . ,(alist-get 'deckName card-info))))))

(defun anki-connect-card-info (card-id)
  (aref (anki-connect-action "cardsInfo" `(("cards" . (,card-id)))) 0))

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

(defun anki-connect-store-media-file (file)
  (let ((data (base64-encode-string
	       (with-temp-buffer
		 (insert-file-contents file)
		 (buffer-string))))
	(filename (file-name-nondirectory file)))
    (anki-connect-action "storeMediaFile" `((filename . ,filename) (data . ,data)))))

;;; Forms

(defun anki-create-note (deck note-type)
  (interactive
   (list (completing-read "Deck: " (anki-connect-deck-names))
         (completing-read "Note Type: " (anki-connect-model-names))))
  (anki-edit--note deck note-type))

(defun anki-edit-note (note-id)
  ;; TODO: Find a way to make sure card isn't currently being browsed
  ;; as then we won't be able to edit.  Should also check when
  ;; submitting.
  (anki-edit--note nil nil (anki-connect-note-info note-id)))

(defun anki-edit--note (&optional deck note-type note-info)
  ;; Setup
  (when (get-buffer "*Anki Note*")
    (kill-buffer "*Anki Note*"))
  (switch-to-buffer "*Anki Note*")
  (org-mode)
  (defvar-local anki-deck-name deck)
  (defvar-local anki-note-type note-type)
  (defvar-local anki-note-id note-id)

  (mapc (lambda (var) (put var 'permanent-local t))
	'(anki-deck-name anki-note-type anki-note-id))

  (setq anki-deck-name (or deck (alist-get 'deckName note-info))
        anki-note-type (or note-type (alist-get 'modelName note-info))
        anki-note-id (alist-get 'noteId note-info))

  (let ((fields
	 (or
          (and note-info (anki--note-field-data note-info))
          (mapcar 'list (anki-connect-model-field-names note-type)))))
    (om-insert 1 (mapcar 'anki--create-field fields))
    (anki-edit-mode)))

(defun anki-edit-submit ()
  (interactive)
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

(defun anki--note-field-data (note-data)
  "Transform NOTE-DATA to an alist of field name and org field value."
  (mapcar (lambda (field)
            (cons
             (symbol-name (car field))
             (anki-convert-html-to-org (alist-get 'value (cdr field)))))
          (alist-get 'fields note-data)))

(defun anki--create-field (field-data)
  (let* ((field-name (car field-data))
	 (field-value (cdr field-data))
	 (field-value (if (equal field-value "") nil field-value)))
    (->>
     (anki--om-parse-object-string (or field-value "\n"))
     (om-build-headline :title (list field-name)
			:post-blank (and (not field-value) 1)))))

(defun anki-edit-next-field ()
  (interactive)
  (let ((start-point (point)))
    (if (not (org-at-heading-p))
	(progn (org-next-visible-heading 1)
	       (if (eq start-point (point))
		   (progn (goto-char (point-min))
			  (anki-edit-next-field))
		 (forward-line 1)))
      (forward-line 1))))

(defun anki-edit-previous-field ()
  (interactive)
  ;; TODO: Handle going from first field to last
  (when (not (org-at-heading-p))
    (org-next-visible-heading -1))
  (forward-line -1))

(defun anki-convert-org-to-html (org)
  "Convert ORG to html string."
  (if (not (string= "" org))
      (with-temp-buffer
        (insert org)
        (mark-whole-buffer)
        (org-export-replace-region-by 'anki)
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
      (thread-last
	  (substring (shell-command-to-string
		      (format "pandoc -f html -t org %s" html-temp-file))
		     nil -1)
	(anki--html-convert-links)
	(anki--html-clean-underscores)))))

(defun anki--html-convert-links (org)
  "Convert links in ORG to reference file in `anki-media-directory'.

Includes handling converting sound tags to org links"
  (thread-last org
    ;; Fix sound references
    (replace-regexp-in-string
     "\\[sound:\\(.+?\\)\\]"
     (lambda (match)
       (format "[[file:%s]]" (replace-regexp-in-string "\\\\_" "_" (match-string 1 match)))))
    ;; Add media directory
    (replace-regexp-in-string "\\[file:\\(.+?\\)\\]" (format "[file:%s/\\1]" (or anki-media-directory "~")))))

(defun anki--html-clean-underscores (org)
  "Replace escaped underscores with latex like syntax \\under{}"
  (replace-regexp-in-string "\\\\_" "\\\\under{}" org))

;;; Org Export Backend

(defun anki-link (link desc info)
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (file-name (file-name-nondirectory raw-path))
	 (file-type (downcase (file-name-extension raw-path t)))
	 (image-formats '(".jpeg" ".jpg" ".png" ".gif" ".svg"))
	 (sound-formats '(".mp3"))
	 (supported-formats (append image-formats sound-formats)))
    (when (and (string= "file" type)
	       (member file-type supported-formats))
      ;; TODO: Store these function calls and call them during card
      ;; creation to avoid side effects here
      (unless (equal (file-name-directory raw-path) anki-media-directory)
	(anki-connect-store-media-file raw-path))
      (cond
       ((member file-type image-formats)
	;; TODO: Handle SVG
	(org-html-close-tag "img" (org-html--make-attribute-string
				   (list :src file-name
					 :alt file-name))
			    info))
       ((member file-type sound-formats)
	(format "[sound:%s]" file-name))))))

(org-export-define-derived-backend 'anki 'html
  :translate-alist '((link . anki-link)))

(defun anki-link-filter (data _backend info)
  (setq anki-test-var (list data info)))

;;; Anki Edit Mode

(defvar anki-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map
     org-mode-map)
    (define-key map (kbd "M-n") #'anki-edit-next-field)
    (define-key map (kbd "<tab>") #'anki-edit-next-field)
    (define-key map (kbd "M-p") #'anki-edit-previous-field)
    (define-key map (kbd "<backtab>") #'anki-edit-previous-field)
    (define-key map (kbd "S-TAB") #'anki-edit-previous-field)
    (define-key map (kbd "<S-tab>") #'anki-edit-previous-field)
    (define-key map (kbd "C-c C-c") #'anki-edit-submit)
    map)
  "Keymap for Anki Edit mode")

(define-derived-mode anki-edit-mode org-mode "AnkiEdit"
  (org-show-all)
  (forward-line))

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
