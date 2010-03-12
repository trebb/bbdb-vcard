;;; bbdb-vcard.el --- import vCards (RFC2426) into BBDB

;; Copyright (c) 2010 Bert Burgemeister

;; Author: Bert Burgemeister <trebbu@googlemail.com>
;; Keywords: data calendar mail news

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 
;; Purpose
;; -------
;; 
;; Data import from VCARDs version 3.0 as defined in RFC2425 and
;; RFC2426 into The Insidious Big Brother Database (BBDB).
;; 
;; Usage
;; -----
;;
;; On a file, a buffer or a region containing one or more vcards, use
;; `bbdb-vcard-import-file', `bbdb-vcard-import-buffer', or
;; `bbdb-vcard-import-region' respectively to import them into BBDB.
;;
;; There are a few customization variables grouped under `bbdb-vcard'.
;;
;; Installation
;; ------------
;;
;; Put this file into your `load-path' and add the following line to
;; your Emacs initialization file:
;;
;;   (require 'bbdb-vcard)
;;
;; Implementation
;; --------------
;;
;; An existing BBDB entry is extended by new information from a vcard
;; 
;;   (a) if name and company and an email address match
;;   (b) or if name and company match
;;   (c) or if name and an email address match
;;   (d) or if name and birthday match
;;   (e) or if name and a phone number match.
;;
;; Otherwise, a fresh BBDB entry is created.
;;
;; When `bbdb-vcard-try-merge' is set to nil, there is always a fresh
;; entry created.
;;
;; In cases (c), (d), and (e), if the vcard has ORG defined, this ORG
;; would overwrite an existing Company in BBDB.
;;
;; For vcard types that have more or less direct counterparts in BBDB,
;; labels and parameters are translated and structured values
;; (lastname; firstname; additional names; prefixes etc.) are
;; converted appropriately with the risk of some (hopefully
;; unessential) information loss.  For labels of the vcard types ADR
;; and TEL, translation is defined in bbdb-vcard-translation-table.
;; 
;; If there is a REV entry, it is stored in BBDB's creation-date in
;; newly created BBDB records, or discarded for existing ones.
;;
;; Vcard type prefixes (A.ADR:..., B.ADR:... etc.) are stripped off
;; and discarded from the following types: N, FN, NICKNAME, ORG (first
;; entry), ADR, TEL, EMAIL, URL, BDAY (first entry), NOTE.
;;
;; All remaining vcard types that don't match the regexp in
;; `bbdb-vcard-skip' and that have a non-empty value are stored
;; unaltered in the BBDB Notes alist where, for instance,
;; `TZ;VALUE=text:-05:00' is stored as `(tz\;value=text . "-05:00")'.
;; From the BBDB data fields AKA, Phones, Addresses, Net Addresses,
;; and Notes, duplicates are removed, respectively.
;;
;; Vcards found inside other vcards (as values of type AGENT) are
;; imported as well.
;;
;;
;; Handling of the individual types defined in RFC2426 (assuming
;; default label translation and no vcard type exclusion):
;; "
;; |-------------+-------------------+------------------------|
;; | TYPE FROM   | VCARD             | STORAGE IN BBDB        |
;; | VCARD       | PARAMETERS        |                        |
;; |             |                   |                        |
;; |-------------+-------------------+------------------------|
;; | VERSION     |                   | -                      |
;; |-------------+-------------------+------------------------|
;; | N           |                   | First entry:           |
;; |             |                   | Firstname              |
;; |             |                   | Lastname               |
;; |             |                   |                        |
;; |             |                   | Rest:                  |
;; |             |                   | AKAs (append)          |
;; |-------------+-------------------+------------------------|
;; | FN          |                   | AKAs (append)          |
;; | NICKNAME    |                   | AKAs (append)          |
;; |-------------+-------------------+------------------------|
;; | ORG         |                   | First entry:           |
;; |             |                   | Company                |
;; |             |                   |                        |
;; |             |                   | Rest:                  |
;; |             |                   | Notes<org              |
;; |             |                   | (repeatedly)           |
;; |-------------+-------------------+------------------------|
;; | ADR         | ;TYPE=x,HOME,y    | Addresses<Home         |
;; |             | ;TYPE=x;TYPE=HOME | Addresses<Home         |
;; |             | ;TYPE=x,WORK,y    | Addresses<Office       |
;; |             | ;TYPE=x;TYPE=WORK | Addresses<Office       |
;; |             | ;TYPE=x,y,z       | Addresses<x,y,z        |
;; |             | ;TYPE=x;TYPE=y    | Addresses<x,y          |
;; |             | (none)            | Addresses<Office       |
;; |-------------+-------------------+------------------------|
;; | TEL         | ;TYPE=x,HOME,y    | Phones<Home (append)   |
;; |             | ;TYPE=x;TYPE=HOME | Phones<Home (append)   |
;; |             | ;TYPE=x,WORK,y    | Phones<Office (append) |
;; |             | ;TYPE=x;TYPE=WORK | Phones<Office (append) |
;; |             | ;TYPE=x,CELL,y    | Phones<Mobile (append) |
;; |             | ;TYPE=x;TYPE=CELL | Phones<Mobile (append) |
;; |             | ;TYPE=x,y,z       | Phones<x,y,z (append)  |
;; |             | ;TYPE=x;TYPE=y    | Phones<x,y (append)    |
;; |             | (none)            | Phones<Office (append) |
;; |-------------+-------------------+------------------------|
;; | EMAIL       | ;TYPE=x,y,z       | Net-Addresses (append) |
;; | URL         |                   | Notes<www              |
;; | BDAY        |                   | Notes<anniversary      |
;; | NOTE        |                   | Notes<notes (append)   |
;; | REV         |                   | Notes<creation-date    |
;; | CATEGORIES  |                   | Notes<categories       |
;; | SORT-STRING |                   | Notes<sort-string      |
;; | KEY         |                   | Notes<key              |
;; | GEO         |                   | Notes<geo              |
;; | TZ          |                   | Notes<tz               |
;; | PHOTO       |                   | Notes<photo            |
;; | LABEL       |                   | Notes<label            |
;; | LOGO        |                   | Notes<logo             |
;; | SOUND       |                   | Notes<sound            |
;; | TITLE       |                   | Notes<title            |
;; | ROLE        |                   | Notes<role             |
;; | AGENT       |                   | Notes<agent            |
;; | MAILER      |                   | Notes<mailer           |
;; | UID         |                   | Notes<uid              |
;; | PRODID      |                   | Notes<prodid           |
;; | CLASS       |                   | Notes<class            |
;; | X-foo       |                   | Notes<x-foo            |
;; |-------------+-------------------+------------------------|
;; | anyJunK     | ;a=x;b=y          | Notes<anyjunk;a=x;b=y  |
;; |-------------+-------------------+------------------------|
;; "


;;; History:
;; 


;;; Code:

(require 'bbdb)
(require 'cl)

;;;; User Variables

(defgroup bbdb-vcard nil
  "Customizations for vcards"
  :group 'bbdb)

(defcustom bbdb-vcard-skip
  "X-GSM-"
  "Regexp describing vcard entry types that are to be discarded.
Example: `X-GSM-\\|X-MS-'."
  :group 'bbdb-vcard
  :type 'regexp)

(defcustom bbdb-vcard-skip-valueless
  t
  "Skip vcard entry types with an empty value.
Nil means insert empty types into BBDB."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-translation-table
  '(("CELL\\|CAR" . "Mobile")
    ("WORK" . "Office")
    ("HOME" . "Home")  ; translates e.g. "dom,home,postal,parcel" to Home
    ("^$" . "Office")) ; acts as a default for parameterless ADR or TEL
  "Label translation.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (VCARD-LABEL-REGEXP . BBDB-LABEL).  One entry
should map a default BBDB label to the empty string (`\"^$\"') which
corresponds to unlabelled vcard entries."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-try-merge
  t
  "Try to merge vcards into existing BBDB entries.
Nil means create a fresh bbdb entry each time a vcard is read."
  :group 'bbdb-vcard
  :type 'boolean)

;;;; User Functions

(defun bbdb-vcard-import-region (begin end)
  "Import the vcards between BEGIN and END into BBDB.
Existing BBDB entries may be altered."
  (interactive "r")
  (bbdb-vcard-iterate-vcards (buffer-substring-no-properties begin end)
                             'bbdb-vcard-process-vcard))

(defun bbdb-vcard-import-buffer (vcard-buffer)
  "Import vcards from VCARD-BUFFER into BBDB.
Existing BBDB entries may be altered."
  (interactive "bVcard buffer: ")
  (set-buffer vcard-buffer)
  (bbdb-vcard-import-region (point-min) (point-max)))

(defun bbdb-vcard-import-file (vcard-file)
  "Import vcards from VCARD-FILE into BBDB.
If VCARD-FILE is a wildcard, import each matching file.  Existing BBDB
entries may be altered."
  (interactive "FVcard file (or wildcard): ")
  (dolist (vcard-file (file-expand-wildcards vcard-file))
    (with-temp-buffer
      (insert-file-contents vcard-file)
      (bbdb-vcard-import-region (point-min) (point-max)))))

(defun bbdb-vcard-iterate-vcards (vcards vcard-processor)
  "Apply VCARD-PROCESSOR successively to each vcard in string VCARDS."
  (with-temp-buffer
    (insert vcards)
    (goto-char (point-min))
    ;; Change CR into CRLF if necessary, dealing with inconsitent line
    ;; endings.
    (while (re-search-forward "[^\r]\\(\n\\)" nil t)
      (replace-match "\r\n" nil nil nil 1))
    (goto-char (point-min))
    (while (re-search-forward "\r\n\\( \\|\t\\)" nil t)
      (replace-match "")) ; Unfold folded lines.
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([[:alnum:]-]*\\.\\)?*BEGIN:VCARD\\([\r\n[:print:][:cntrl:]]*?\\)\\(^\\([[:alnum:]-]*\\.\\)?END:VCARD\\)"
            nil t)
      (funcall vcard-processor (match-string 2)))))

(defun bbdb-vcard-process-vcard (entry)
  "Store the vcard ENTRY in BBDB.
\(ENTRY is expected to have BEGIN:VCARD and END:VCARD delimiters
stripped off.) Extend existing BBDB entries where possible."
  (with-temp-buffer
    (insert entry)
    (unless
        (string=
         (cdr (assoc "value"
                     (car (bbdb-vcard-entries-of-type "version")))) "3.0")
      (display-warning '(bbdb-vcard xy) "Not a version 3.0 vcard."))
    (let* ((raw-name
            (cdr (assoc "value" (car (bbdb-vcard-entries-of-type "N" t)))))
           ;; Name suitable for storing in BBDB:
           (name
            (bbdb-vcard-unescape-strings (bbdb-vcard-convert-name raw-name)))
           ;; Name to search for in BBDB now:
           (name-to-search-for
            (when raw-name (if (stringp raw-name)
                               raw-name
                             (concat (nth 1 raw-name) ;given name
                                     " .*"
                                     (nth 0 raw-name))))) ; family name
           ;; Additional names from prefixed types like A.N, B.N etc.:
           (other-names
            (mapcar
             (lambda (n)
               (mapconcat 'identity (bbdb-vcard-convert-name
                                     (cdr (assoc "value" n)))
                          " "))
             (bbdb-vcard-entries-of-type "N")))
           (vcard-formatted-names
            (bbdb-vcard-unescape-strings
             (mapcar (lambda (fn) (cdr (assoc "value" fn)))
                     (bbdb-vcard-entries-of-type "FN"))))
           (vcard-nicknames
            (bbdb-vcard-unescape-strings
             (bbdb-vcard-split-structured-text
              (cdr (assoc "value"
                          (car (bbdb-vcard-entries-of-type "NICKNAME"))))
              "," t)))
           ;; Company suitable for storing in BBDB:
           (vcard-org
            (bbdb-vcard-unescape-strings
             (bbdb-vcard-convert-org
              (cdr (assoc "value"
                          (car (bbdb-vcard-entries-of-type "ORG" t)))))))
           ;; Company to search for in BBDB now:
           (org-to-search-for vcard-org) ; sorry
           ;; Email suitable for storing in BBDB:
           (vcard-email
            (mapcar (lambda (email) (cdr (assoc "value" email)))
                    (bbdb-vcard-entries-of-type "EMAIL")))
           ;; Email to search for in BBDB now:
           (email-to-search-for
            (when vcard-email
              (concat "\\(" (mapconcat 'identity vcard-email "\\)\\|\\(")
                      "\\)")))
           ;; Phone numbers suitable for storing in BBDB:
           (vcard-tels
            (mapcar (lambda (tel)
                      (vector (bbdb-vcard-translate
                               (or (cdr (assoc "type" tel)) ""))
                              (cdr (assoc "value" tel))))
                    (bbdb-vcard-entries-of-type "TEL")))
           ;; Phone numbers to search for in BBDB now:
           (tel-to-search-for
            (when vcard-tels
              (concat "\\("
                      (mapconcat (lambda (x) (elt x 1)) vcard-tels "\\)\\|\\(")
                      "\\)")))
           ;; Addresses
           (vcard-adrs
            (mapcar 'bbdb-vcard-convert-adr
                    (bbdb-vcard-entries-of-type "ADR")))
           (vcard-url
            (cdr (assoc "value" (car (bbdb-vcard-entries-of-type "URL" t)))))
           (vcard-notes (bbdb-vcard-entries-of-type "NOTE"))
           (raw-bday
            (cdr (assoc "value" (car (bbdb-vcard-entries-of-type "BDAY" t)))))
           ;; Birthday suitable for storing in BBDB (usable by org-mode):
           (vcard-bday (when raw-bday (concat raw-bday " birthday")))
           ;; Birthday to search for in BBDB now:
           (bday-to-search-for vcard-bday)
           (vcard-rev
            (cdr (assoc "value" (car (bbdb-vcard-entries-of-type "REV")))))
           ;; The BBDB record to change:
           (record-freshness-info "BBDB record changed:") ; default user info
           (bbdb-record
            (or
             ;; Try to find an existing one ...
             ;; (a) try company and net and name:
             (car (and bbdb-vcard-try-merge
                       name-to-search-for
                       (bbdb-search
                        (and email-to-search-for
                             (bbdb-search
                              (and org-to-search-for
                                   (bbdb-search (bbdb-records)
                                                nil org-to-search-for))
                              nil nil email-to-search-for))
                        name-to-search-for)))
             ;; (b) try company and name:
             (car (and bbdb-vcard-try-merge
                       name-to-search-for
                       (bbdb-search
                        (and org-to-search-for
                             (bbdb-search (bbdb-records)
                                          nil org-to-search-for))
                        name-to-search-for)))
             ;; (c) try net and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       name-to-search-for
                       (bbdb-search
                        (and email-to-search-for
                             (bbdb-search (bbdb-records)
                                          nil nil email-to-search-for))
                        name-to-search-for)))
             ;; (d) try birthday and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       name-to-search-for
                       (bbdb-search
                        (and bday-to-search-for
                             (bbdb-search (bbdb-records)
                                          nil nil nil bday-to-search-for))
                        name-to-search-for)))
             ;; (e) try phone and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       name-to-search-for
                       (bbdb-search
                        (and tel-to-search-for
                             (bbdb-search (bbdb-records)
                                          nil nil nil nil tel-to-search-for))
                        name-to-search-for)))
             ;; No existing record found; make a fresh one:
             (let ((fresh-record (make-vector bbdb-record-length nil)))
               (bbdb-record-set-cache fresh-record
                                      (make-vector bbdb-cache-length nil))
               (if vcard-rev            ; For fresh records,
                   (bbdb-record-putprop ; set creation-date from vcard-rev
                    fresh-record 'creation-date
                    (replace-regexp-in-string "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\).*"
                                              "\\1" vcard-rev))
                 (bbdb-invoke-hook 'bbdb-create-hook fresh-record))
               (setq record-freshness-info "BBDB record added:") ; for user information
               fresh-record)))
           (bbdb-akas (bbdb-record-aka bbdb-record))
           (bbdb-addresses (bbdb-record-addresses bbdb-record))
           (bbdb-phones (bbdb-record-phones bbdb-record))
           (bbdb-nets (bbdb-record-net bbdb-record))
           (bbdb-raw-notes (bbdb-record-raw-notes bbdb-record))
           notes
           other-vcard-type)
      (when name ; which should be the case as N is mandatory in vcard
        (bbdb-record-set-firstname bbdb-record (car name))
        (bbdb-record-set-lastname bbdb-record (cadr name)))
      (bbdb-record-set-aka
       bbdb-record
       (remove (concat (bbdb-record-firstname bbdb-record)
                       " " (bbdb-record-lastname bbdb-record))
               (reduce (lambda (x y) (union x y :test 'string=))
                       (list vcard-nicknames
                             other-names
                             vcard-formatted-names
                             bbdb-akas))))
      (when vcard-org (bbdb-record-set-company bbdb-record vcard-org))
      (bbdb-record-set-net bbdb-record
                           (union vcard-email bbdb-nets :test 'string=))
      (bbdb-record-set-addresses bbdb-record
                                 (union vcard-adrs bbdb-addresses :test 'equal))
      (bbdb-record-set-phones bbdb-record
                              (union vcard-tels bbdb-phones :test 'equal))
      ;; prepare bbdb's notes:
      (when vcard-url (push (cons 'www vcard-url) bbdb-raw-notes))
      (when vcard-notes
        ;; Put vcard NOTEs under key 'notes (append if necessary).
        (unless (assoc 'notes bbdb-raw-notes)
          (push (cons 'notes "") bbdb-raw-notes))
        (setf (cdr (assoc 'notes bbdb-raw-notes))
              (bbdb-vcard-merge-notes
               (cdr (assoc 'notes bbdb-raw-notes))
               (bbdb-vcard-unescape-strings
                (mapconcat (lambda (element) (cdr (assoc "value" element)))
                           (nreverse vcard-notes)
                           ";\n")))))
      (when vcard-bday
        (unless (assoc 'anniversary bbdb-raw-notes)
          (push (cons 'anniversary "") bbdb-raw-notes))
        (setf (cdr (assoc 'anniversary bbdb-raw-notes))
              vcard-bday))              ; for consumption by org-mode
      (while (setq other-vcard-type (bbdb-vcard-other-entry))
        (when (string-match "^\\([[:alnum:]-]*\\.\\)?AGENT"
                            (symbol-name (car other-vcard-type)))
          ;; Notice other vcards inside the current one.
          (bbdb-vcard-iterate-vcards (cdr other-vcard-type)
                                     'bbdb-vcard-process-vcard))
        (unless (or (and bbdb-vcard-skip
                         (string-match bbdb-vcard-skip
                                       (symbol-name (car other-vcard-type))))
                    (and bbdb-vcard-skip-valueless
                         (zerop (length (cdr other-vcard-type)))))
          (push other-vcard-type bbdb-raw-notes)))
      (bbdb-record-set-raw-notes
       bbdb-record
       (remove-duplicates bbdb-raw-notes :test 'equal :from-end t))
      (bbdb-change-record bbdb-record t)
      ;; Tell the user what we've done.
      (message "%s %s %s -- %s"
               record-freshness-info
               (bbdb-record-firstname bbdb-record)
               (bbdb-record-lastname bbdb-record)
               (replace-regexp-in-string
                "\n" "; " (or (bbdb-record-company bbdb-record) "-"))))))

(defun bbdb-vcard-unescape-strings (escaped-strings)
  "Unescape escaped `;', `,', `\\', and newlines in ESCAPED-STRINGS.
ESCAPED-STRINGS may be a string or a sequence of strings."
  (flet ((unescape (x) (replace-regexp-in-string
                        "\\([\\\\]\\)\\([,;\\]\\)" ""
                        (replace-regexp-in-string "\\\\n" "\n" x)
                        nil nil 1)))
    (if (stringp escaped-strings)
        (unescape escaped-strings)
      (mapcar 'unescape
              escaped-strings))))

(defun bbdb-vcard-convert-name (vcard-name)
  "Convert VCARD-NAME (type N) into (FIRSTNAME LASTNAME)."
  (if (stringp vcard-name)              ; unstructured N
      (bbdb-divide-name vcard-name)
    (let ((vcard-name
           (mapcar (lambda (x)
                     (mapconcat 'identity
                                (bbdb-vcard-split-structured-text x "," t)
                                " "))
                   vcard-name))) ; flatten comma-separated substructure
      (list (concat (nth 3 vcard-name)  ; honorific prefixes
                    (when (nth 3 vcard-name) " ")
                    (nth 1 vcard-name)  ; given name
                    (when (nth 2 vcard-name) " ")
                    (nth 2 vcard-name)) ; additional names
            (concat (nth 0 vcard-name)  ; family name
                    (when (nth 4 vcard-name) " ")
                    (nth 4 vcard-name)))))) ; honorific suffixes

(defun bbdb-vcard-convert-org (vcard-org)
  "Convert VCARD-ORG (type ORG), which may be a list, into a string."
  (if (or (null vcard-org)
          (stringp vcard-org)) ; unstructured, probably non-standard ORG
      vcard-org                ; Company, unit 1, unit 2...
    (mapconcat 'identity vcard-org "\n")))

(defun bbdb-vcard-convert-adr (vcard-adr)
  "Convert VCARD-ADR into BBDB format.
Turn a vcard element of type ADR into (TYPE STREETS CITY STATE ZIP
COUNTRY)."
  (let ((adr-type (or (cdr (assoc "type" vcard-adr)) ""))
        (adr-value (mapcar      ; flatten comma-separated substructure
                    (lambda (x)
                      (mapconcat 'identity
                                 (bbdb-vcard-split-structured-text x "," t)
                                 ", "))
                    (cdr (assoc "value" vcard-adr)))))
    (vector (bbdb-vcard-translate adr-type)
            ;; Postbox, Extended, Streets
            (remove-if (lambda (x) (zerop (length x)))
                       (subseq adr-value 0 3))
            (elt adr-value 3)           ; City
            (elt adr-value 4)           ; State
            (elt adr-value 5)           ; Zip
            (elt adr-value 6))))        ; Country

(defun bbdb-vcard-merge-notes (bbdb-notes vcard-notes)
  "Merge string VCARD-NOTES into string BBDB-NOTES.
If VCARD-NOTES is already in BBDB-NOTES, return BBDB-NOTES.  Otherwise
append VCARD-NOTES."
  (with-temp-buffer
    (insert bbdb-notes)
    (unless (search-backward vcard-notes nil t)
      (goto-char (point-max))
      (unless (zerop (buffer-size)) (insert ";\n"))
      (insert vcard-notes))
    (buffer-string)))

(defun bbdb-vcard-entries-of-type (type &optional one-is-enough-p)
  "From current buffer read and delete the vcard entries of TYPE.
The current buffer is supposed to contain a single vcard.  If
ONE-IS-ENOUGH-P is t, read and delete only the first entry of TYPE."
  (goto-char (point-min))
  (let (values parameters read-enough)
    (while
        (and
         (not read-enough)
         (re-search-forward
          (concat
           "^\\([[:alnum:]-]*\\.\\)?\\(" type "\\)\\(;.*\\)?:\\(.*\\)\r$")
          nil t))
      (goto-char (match-end 2))
      (setq parameters nil)
      (push (cons "value" (bbdb-vcard-split-structured-text
                           (match-string 4) ";")) parameters)
      (while (re-search-forward "\\([^;:=]+\\)=\\([^;:]+\\)"
                                (line-end-position) t)
        (let* ((parameter-key (downcase (match-string 1)))
               (parameter-value (downcase (match-string 2)))
               (parameter-sibling (assoc parameter-key parameters)))
          (if parameter-sibling         ; i.e., pair with equal key
              ;; collect vcard parameter list `;a=x;a=y;a=z'
              ;; into vcard value list `;a=x,y,z'; becoming ("a" . "x,y,z")
              (setf (cdr parameter-sibling)
                    (concat (cdr parameter-sibling) "," parameter-value))
            ;; vcard parameter pair `;key=value;' with new key
            (push (cons parameter-key parameter-value) parameters))))
      (push parameters values)
      (delete-region (line-end-position 0) (line-end-position))
      (when one-is-enough-p (setq read-enough t)))
    values))

(defun bbdb-vcard-other-entry ()
  "From current buffer read and delete the topmost vcard entry.
Buffer is supposed to contain a single vcard.  Return (TYPE . ENTRY)."
  (goto-char (point-min))
  (when (re-search-forward "^\\([[:graph:]]*?\\):\\(.*\\)\r$" nil t)
    (let ((type (match-string 1))
          (value (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (cons (intern (downcase type)) (bbdb-vcard-unescape-strings value)))))

(defun bbdb-vcard-split-structured-text
  (text separator &optional return-always-list-p)
  "Split TEXT at unescaped occurrences of SEPARATOR; return parts in a list.
Return text unchanged if there aren't any separators and RETURN-ALWAYS-LIST-P
is nil."
  (when (stringp text)
    (let ((string-elements
           (split-string
            (replace-regexp-in-string
             (concat "\\\\\r" separator) (concat "\\\\" separator)
             (replace-regexp-in-string separator (concat "\r" separator) text))
            (concat "\r" separator))))
      (if (and (null return-always-list-p)
               (= 1 (length string-elements)))
          (car string-elements)
        string-elements))))

(defun bbdb-vcard-translate (vcard-label)
  "Translate VCARD-LABEL into its bbdb counterpart.
Translations are defined in `bbdb-vcard-translation-table'."
  (upcase-initials
   (or (assoc-default vcard-label bbdb-vcard-translation-table 'string-match)
       vcard-label)))

(provide 'bbdb-vcard)

;;; bbdb-vcard.el ends here
