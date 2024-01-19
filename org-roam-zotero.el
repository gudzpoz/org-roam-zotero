;;; org-roam-zotero.el --- Org-roam Extension to Connect to Zotero -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024 gudzpoz

;; author: gudzpoz
;; Keywords: org-mode, zotero
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-roam "2.1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  Org-roam-zotero provides a way to create Org-roam nodes on Zotero entries,
;;  allowing for quick jump between Org-roam and Zotero.
;;
;;; Code:
;;;; Dependencies

(require 'bindat)
(require 'json)
(require 'org)
(require 'org-roam)

(defgroup org-roam-zotero nil
  "Zotero connector for Org-roam."
  :group 'org-roam
  :prefix "org-roam-zotero")

(defconst org-roam-zotero--bindat-spec
  (bindat-type
    (tr-id uint 32)
    (len uint 32)
    (msg str len))
  "Zotero LibreOffice integration wire format.")

(defconst org-roam-zotero--bindat-peek-bindat-spec
  (bindat-type
    (tr-id uint 32)
    (len uint 32))
  "Header part of a wire format frame.")

(defconst org-roam-zotero--plugin-version 1)

(defun org-roam-zotero--payload-pack (tr-id msg &optional err)
  "Packs a Zotero LibreOffice integration payload into a binary unibyte string.

   Parameters:
   - tr-id :: The transaction id.
   - msg :: The message string or object, ignored when err is non-nil.
   - err :: The optional error message.
  "
  (let* ((payload (if err (concat "ERR:" err) (if msg (string-to-unibyte (json-serialize msg)) "null")))
         (len (length payload)))
    (bindat-pack org-roam-zotero--bindat-spec
                 `((tr-id . ,tr-id)
                   (len . ,len)
                   (msg . ,payload)))))

(defun org-roam-zotero--payload-unpack (payload)
  "Unpacks a Zotero LibreOffice integration payload from a binary unibyte string."
  (bindat-unpack org-roam-zotero--bindat-spec payload))

(defun org-roam-zotero--pack-command (command)
  "Packs a Zotero integration command, attaching relevant API info."
  (org-roam-zotero--payload-pack 0
                              `((command . ,command)
                                (templateVersion . ,org-roam-zotero--plugin-version))))

(defvar org-roam-zotero--connection nil)
(defvar org-roam-zotero--buffer "")
(defun org-roam-zotero--get-connection ()
  "Returns the current Zotero connection, or opens a new one if necessary."
  (when (or (not org-roam-zotero--connection) (eq (process-status org-roam-zotero--connection) 'closed))
    (setq org-roam-zotero--buffer "")
    (setq org-roam-zotero--connection (open-network-stream "org-roam-zotero" nil "127.0.0.1" 23116
                                                           :type 'plain :coding 'no-conversion))
    (set-process-filter
     org-roam-zotero--connection
     (lambda (_ output)
       "Callback function that calls `org-roam-zotero--process-frame' when a full frame is received."
       (setq org-roam-zotero--buffer (concat org-roam-zotero--buffer output))
       (when (> (length org-roam-zotero--buffer) 8)
         (let* ((header (bindat-unpack org-roam-zotero--bindat-peek-bindat-spec
                                       (substring org-roam-zotero--buffer 0 8)))
                (len (+ (alist-get 'len header) 8))
                (frame (if (>= (length org-roam-zotero--buffer) len) (substring org-roam-zotero--buffer 0 len))))
           (when frame
             (setq org-roam-zotero--buffer (substring org-roam-zotero--buffer len))
             (org-roam-zotero--process-frame frame)))))))
  org-roam-zotero--connection)

(defun org-roam-zotero--send (payload)
  "Sends a payload to the Zotero LibreOffice integration."
  (let ((connection (org-roam-zotero--get-connection)))
    (process-send-string connection payload)))

(defconst org-roam-zotero--api-version 3)
(defconst org-roam-zotero--document-id #x0BADCAFE
  "A random document id that hopefully won't collide with those from the LibreOffice extension.")

;; This data is never changed but nevertheless not using `defconst'.
;; We might consider making it dynamic in the future if the wire protocol changes.
(defvar org-roam-zotero--field-data "ITEM CSL_CITATION {\"citationItems\":[],\"properties\":{}}"
  "Fake field data to make Zotero happy.")
;; Initial document data, copied from Zotero log.
;; Note that with the current Zotero implementation, it must begins with `<', no spaces allowed.
(defvar org-roam-zotero--document-data "<data data-version=\"3\" zotero-version=\"6.0.30\">
    <session id=\"session\"/>
    <style id=\"http://www.zotero.org/styles/apa\" locale=\"en-US\" hasBibliography=\"1\" bibliographyStyleHasBeenSet=\"0\"/>
    <prefs>
      <pref name=\"fieldType\" value=\"ReferenceMark\"/>
      <pref name=\"delayCitationUpdates\" value=\"true\"/>
    </prefs>
  </data>")

(defun org-roam-zotero--display-alert (msg icon buttons)
  "Displays an alert from the Zotero LibreOffice integration."
  (interactive)
  (let ((label (upcase (pcase icon (0 "stop") (1 "note") (2 "warn") (_ "hint"))))
        (options (pcase buttons
                   (1 '((t . 1) (:false . 0) (nil . 0)))
                   (2 '((t . 1) (:false . 0) (nil . 0)))
                   (3 '((t . 2) (:false . 1) (nil . 0)))
                   (_ '((t . 1) (:false . 1) (nil . 1)))))
        (inhibit-quit t))
    (let ((selected (with-local-quit
                      (if (yes-or-no-p (concat (propertize label 'face '(bold default)) " " msg)) t :false))))
      (setq quit-flag nil)
      (alist-get selected options))))

(defcustom org-roam-zotero-uri-format :uri
  "Custom URI format for the `ROAM_REFS' attached to org-roam nodes.

   - `:uri' (default) :: the default format, `https://zotero.org/users/user_id/items/item_id'.
   - `:zotero' :: the app link format, `zotero://select/library/items/item_id'.
   - a string like `\"user_name\"' :: web link format, `https://zotero.org/user_name/items/item_id'."
  :type '(choice ((symbol :tag "Either `:uri' or `:zotero'")
                  (string :tag "Username")))
  :set (lambda (_ value) (if (or (and (stringp value) (string-match-p "^\\w+$" value))
                                 (eq value :uri) (eq value :zotero)) value :uri))
  :group 'org-roam-zotero)

(defun org-roam-zotero--uri-to-id (uri)
  "Extracts the Zotero ID from a Zotero URI."
  (when (or (string-match "^https?://zotero.org/users/\\w+?/items/\\(\\w+\\)$" uri)
            (string-match "^https?://zotero.org/\\w+?/items/\\(\\w+\\)$" uri)
            (string-match "^zotero://select/library/items/\\(\\w+\\)$" uri))
    `(,(match-string 1 uri) . ,uri)))

(defun org-roam-zotero--id-to-uri (id-url)
  "Returns links according to `org-roam-zotero-uri-format'."
  (let ((id (car id-url)) (url (cdr id-url)) (type org-roam-zotero-uri-format))
    (cond ((eq type :zotero) (concat "zotero://select/library/items/" id))
          ((stringp type) (concat "https://zotero.org/" type "/items/" id))
          (t url)))
  )

(defun org-roam-zotero--authors-to-string (authors)
  "Converts a list of authors to a string."
  (mapconcat (lambda (author)
               (let ((given (gethash "given" author))
                     (family (gethash "family" author)))
                 (concat "#+subauthor: "
                         (format (if (or (= 0 (length given)) (= 0 (length family)))
                                     "%s%s" "%s, %s")
                                 given family) "\n")))
             authors))

(defvar org-roam-zotero--ignored-fields
  ["id" "title" "title-short" "abstract" "author" "citation-key" "issued" "note" "source" "journalAbbreviation"]
  "A list of fields to ignore when converting a Zotero item to an Org-roam template.")

(defvar org-roam-zotero--field-name-mapping
  '(("container-title" . "from"))
  "A list of field name mappings for converting Zotero fields to Org-roam fields.")

(defcustom org-roam-zotero-filename-format "%<%Y%m%d%H%M%S>-${slug}.org"
  "The filename format for Org-roam templates."
  :type '(string)
  :group 'org-roam-zotero)

(defun org-roam-zotero--item-to-template (ref title item)
  "Converts a Zotero item to a Org-roam template."
  (let* ((short-title (gethash "title-short" item title))
         (doi (gethash "DOI" item))
         (doi-link (if doi (concat "[[https://doi.org/" doi "]]")))
         (abstract-content (gethash "abstract" item))
         (abstract (if abstract-content
                       (concat "\n\n* Abstract: " short-title "\n"
                               doi-link
                               "\n#+begin_quote\n"
                               (with-temp-buffer
                                 (insert abstract-content)
                                 (fill-region (point-min) (point-max))
                                 (buffer-substring (point-min) (point-max)))
                               "\n#+end_quote\n\n* Notes\n\n%?")
                     (concat "\n" doi-link "\n* Notes\n\n")))
         (authors (org-roam-zotero--authors-to-string (gethash "author" item)))
         (issued (mapconcat (apply-partially 'format "%s")
                            (elt (gethash "date-parts" (gethash "issued" item)) 0) "/")))
    `("d" title plain ,abstract
      :unnarrowed t
      :target (file+head ,org-roam-zotero-filename-format
                         ,(concat
                           ":PROPERTIES:\n"
                           ":ROAM_REFS: " ref "\n"
                           (mapconcat (lambda (k)
                                        (unless (seq-contains-p org-roam-zotero--ignored-fields k)
                                          (let ((name (concat
                                                       ":"
                                                       (replace-regexp-in-string
                                                        "-" "_"
                                                        (upcase (alist-get k org-roam-zotero--field-name-mapping
                                                                           k nil 'equal)))
                                                       ":")))
                                            (format "%s%s%s\n"
                                                    name
                                                    (make-string (max 1 (- 11 (length name))) ? )
                                                    (gethash k item)))))
                                      (hash-table-keys item))
                           ":END:\n"
                           "#+title: " title "\n"
                           (if (not (string-equal authors "")) authors)
                           (if (not (string-equal issued ""))
                               (concat "#+date: " issued "\n"))
                           "\n"
                           )))))

(defun org-roam-zotero--visit-node-id (node-id)
  "Visits an existing node by its node ID."
  (let ((node (car (org-roam-db-query `[:select [file pos title] :from nodes
                                                :where id := ,node-id]))))
    (when node
      (pcase-let* ((`(,file ,pos ,title) node)
                   (node (org-roam-node-create :id node-id
                                               :file file
                                               :point pos
                                               :title title)))
        (org-roam-node-visit node)
        ))))

(defun org-roam-zotero--remove-url-type (url)
  "Truncates URIs like `https://localhost' to `//localhost'."
  (let ((i (string-match "://" url)))
    (if i (substring url (1+ i)) url)))

(defun org-roam-zotero--handle-code (code)
  "Handles citation info injected by Field_setCode calls from the Zotero LibreOffice integration."
  (let* ((json (substring code (seq-position code ?{)))
         (info (json-parse-string json))
         (item (elt (gethash "citationItems" info) 0))
         (uri (elt (gethash "uris" item) 0))
         (id-url (org-roam-zotero--uri-to-id uri))
         (ref (org-roam-zotero--id-to-uri id-url))
         (data (gethash "itemData" item))
         (title (gethash "title" data (gethash "title-short" data "<unknown title>")))
         (template (org-roam-zotero--item-to-template ref title data))
         (node-id (car (org-roam-db-query `[:select [node-id] :from refs
                                                    :where ref := ,(org-roam-zotero--remove-url-type ref)])))
         (inhibit-quit t))
    (message "%s" node-id)
    (with-local-quit
      (if node-id
          (org-roam-zotero--visit-node-id (car node-id))
        (org-roam-node-find nil title nil nil :templates `(,template))))
    (setq quit-flag nil)))

(defun org-roam-zotero--process-frame (frame)
  "Process a frame from the Zotero LibreOffice integration."
  (interactive)
  (let* ((json (org-roam-zotero--payload-unpack frame))
         (tr-id (alist-get 'tr-id json))
         (payload (json-parse-string (alist-get 'msg json)))
         (command (elt payload 0))
         (params (elt payload 1))
         (result (pcase command
                   ("Application_getActiveDocument" `[,org-roam-zotero--api-version ,org-roam-zotero--document-id])
                   ("Document_displayAlert" (apply 'org-roam-zotero--display-alert
                                                   (coerce (seq-subseq params 1 4) 'list)))
                   ("Document_activate" :null)
                   ("Document_canInsertField" t)
                   ("Document_setDocumentData" (setq org-roam-zotero--document-data (elt params 1)) :null)
                   ("Document_getDocumentData" org-roam-zotero--document-data)
                   ("Document_cursorInField" `[0 ,org-roam-zotero--field-data 0])
                   ("Document_insertField" 'err)
                   ("Document_insertText" 'err)
                   ("Document_getFields" `[[0] [,org-roam-zotero--field-data] [0] [-1]])
                   ("Document_convert" 'err)
                   ("Document_convertPlaceholdersToFields" 'err)
                   ("Document_importDocument" 'err)
                   ("Document_exportDocument" 'err)
                   ("Document_setBibliographyStyle" :null)
                   ("Document_complete" :null)
                   ("Field_delete" :null)
                   ("Field_select" :null)
                   ("Field_removeCode" :null)
                   ("Field_setText" :null)
                   ("Field_getText" "")
                   ("Field_setCode" (org-roam-zotero--handle-code (elt params 2)) :null)
                   ("Field_convert" :null)
                   (_ 'err))))
    (message "%d: %s (%s)" tr-id command params)
    (org-roam-zotero--send (org-roam-zotero--payload-pack tr-id result
                                                          (if (eq result 'err) "unknown command")))))

;;;###autoload
(defun org-roam-zotero-find-node ()
  "Selects a Zotero entry with Zotero prompt and opens or creates an Org-roam node for it."
  (interactive)
  (org-roam-zotero--send (org-roam-zotero--pack-command "addEditCitation")))

;;;###autoload
(defun org-roam-open-in-zotero ()
  "Opens the current entry in Zotero."
  (interactive)
  (let ((id-url (car (remq nil (mapcar org-roam-zotero--uri-to-id
                                     (org-entry-get-multivalued-property (point-min) "ROAM_REFS"))))))
    (when id-url
      (browse-url (concat "zotero://select/library/items/" (car id-url))))))
