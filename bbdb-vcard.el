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

;; The exporter functionality is based on code from
;; bbdb-vcard-export.el by Jim Hourihan and Alex Schroeder.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; 
;; Purpose
;; -------
;; 
;; Import and export of vCards (version 3.0) as defined in RFC 2425
;; and RFC 2426 to/from The Insidious Big Brother Database (BBDB).
;; 
;;
;; Usage
;; -----
;;
;; vCard Import
;;
;; On a file, a buffer or a region containing one or more vCards, use
;; `bbdb-vcard-import-file', `bbdb-vcard-import-buffer', or
;; `bbdb-vcard-import-region' respectively to import them into BBDB.
;;
;; vCard Export
;;
;; In buffer *BBDB*, press v to export the record under point. Press
;; *v to export all records in buffer into one vCard file.
;; Press * C-u v to export them into one file each.
;;
;; There are a few customization variables grouped under `bbdb-vcard'.
;;
;;
;; Installation
;; ------------
;;
;; Put this file into your `load-path' and add the following line to
;; your Emacs initialization file:
;;
;;   (require 'bbdb-vcard)
;;
;;
;; Implementation
;; --------------
;;
;; vCard Import
;;
;; An existing BBDB entry is extended by new information from a vCard
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
;; In cases (c), (d), and (e), if the vCard has ORG defined, this ORG
;; would overwrite an existing Company in BBDB.
;;
;; For vCard types that have more or less direct counterparts in BBDB,
;; labels and parameters are translated and structured values
;; (lastname; firstname; additional names; prefixes etc.) are
;; converted appropriately with the risk of some (hopefully
;; unessential) information loss.  For labels of the vCard types ADR
;; and TEL, parameter translation is defined in
;; `bbdb-vcard-import-translation-table'.
;; 
;; If there is a REV entry, it is stored in BBDB's creation-date in
;; newly created BBDB records, or discarded for existing ones.
;;
;; VCard type prefixes (A.ADR:..., B.ADR:... etc.) are stripped off
;; and discarded from the following types: N, FN, NICKNAME, ORG (first
;; entry), ADR, TEL, EMAIL, URL, BDAY (first entry), NOTE.
;;
;; VCard types that are prefixed `X-BBDB-' are stored in BBDB without
;; the prefix.
;;
;; All remaining vCard types that don't match the regexp in
;; `bbdb-vcard-skip' and that have a non-empty value are stored
;; unaltered in the BBDB Notes alist where, for instance,
;; `TZ;VALUE=text:-05:00' is stored as `(tz\;value=text . "-05:00")'.
;; From the BBDB data fields AKA, Phones, Addresses, Net Addresses,
;; and Notes, duplicates are removed, respectively.
;;
;; VCards found inside other vCards (as values of type AGENT) are
;; imported as well.
;;
;;
;; Handling of the individual types defined in RFC2426 (assuming
;; default label translation and no vCard type exclusion):
;; "
;; |-------------+-------------------+---------------------------|
;; | TYPE FROM   | VCARD             | STORAGE IN BBDB           |
;; | VCARD       | PARAMETERS        |                           |
;; |             |                   |                           |
;; |-------------+-------------------+---------------------------|
;; | VERSION     |                   | -                         |
;; |-------------+-------------------+---------------------------|
;; | N           |                   | First entry:              |
;; |             |                   | Firstname                 |
;; |             |                   | Lastname                  |
;; |             |                   |                           |
;; |             |                   | Rest:                     |
;; |             |                   | AKAs (append)             |
;; |-------------+-------------------+---------------------------|
;; | FN          |                   | AKAs (append)             |
;; | NICKNAME    |                   | AKAs (append)             |
;; |-------------+-------------------+---------------------------|
;; | ORG         |                   | First entry:              |
;; |             |                   | Company                   |
;; |             |                   |                           |
;; |             |                   | Rest:                     |
;; |             |                   | Notes<org                 |
;; |             |                   | (repeatedly)              |
;; |-------------+-------------------+---------------------------|
;; | ADR         | ;TYPE=x,HOME,y    | Addresses<Home            |
;; |             | ;TYPE=x;TYPE=HOME | Addresses<Home            |
;; |             | ;TYPE=x,WORK,y    | Addresses<Office          |
;; |             | ;TYPE=x;TYPE=WORK | Addresses<Office          |
;; |             | ;TYPE=x,y,z       | Addresses<x,y,z           |
;; |             | ;TYPE=x;TYPE=y    | Addresses<x,y             |
;; |             | (none)            | Addresses<Office          |
;; |-------------+-------------------+---------------------------|
;; | TEL         | ;TYPE=x,HOME,y    | Phones<Home (append)      |
;; |             | ;TYPE=x;TYPE=HOME | Phones<Home (append)      |
;; |             | ;TYPE=x,WORK,y    | Phones<Office (append)    |
;; |             | ;TYPE=x;TYPE=WORK | Phones<Office (append)    |
;; |             | ;TYPE=x,CELL,y    | Phones<Mobile (append)    |
;; |             | ;TYPE=x;TYPE=CELL | Phones<Mobile (append)    |
;; |             | ;TYPE=x,y,z       | Phones<x,y,z (append)     |
;; |             | ;TYPE=x;TYPE=y    | Phones<x,y (append)       |
;; |             | (none)            | Phones<Office (append)    |
;; |-------------+-------------------+---------------------------|
;; | EMAIL       | ;TYPE=x,y,z       | Net-Addresses (append)    |
;; | URL         |                   | Notes<www                 |
;; | BDAY        |                   | Notes<anniversary         |
;; | NOTE        |                   | Notes<notes (append)      |
;; | REV         |                   | Notes<creation-date       |
;; | CATEGORIES  |                   | Notes<mail-alias (append) |
;; | SORT-STRING |                   | Notes<sort-string         |
;; | KEY         |                   | Notes<key                 |
;; | GEO         |                   | Notes<geo                 |
;; | TZ          |                   | Notes<tz                  |
;; | PHOTO       |                   | Notes<photo               |
;; | LABEL       |                   | Notes<label               |
;; | LOGO        |                   | Notes<logo                |
;; | SOUND       |                   | Notes<sound               |
;; | TITLE       |                   | Notes<title               |
;; | ROLE        |                   | Notes<role                |
;; | AGENT       |                   | Notes<agent               |
;; | MAILER      |                   | Notes<mailer              |
;; | UID         |                   | Notes<uid                 |
;; | PRODID      |                   | Notes<prodid              |
;; | CLASS       |                   | Notes<class               |
;; | X-foo       |                   | Notes<x-foo               |
;; | X-BBDB-bar  |                   | Notes<bar                 |
;; |-------------+-------------------+---------------------------|
;; | anyJunK     | ;a=x;b=y          | Notes<anyjunk;a=x;b=y     |
;; |-------------+-------------------+---------------------------|
;; "
;;
;; vCard Export
;;
;; (TODO)

;;; History:
;; 


;;; Code:

(require 'bbdb)
(require 'cl)

;;;; User Variables

(defgroup bbdb-vcard nil
  "Customizations for vCards"
  :group 'bbdb)

(defcustom bbdb-vcard-skip "X-GSM-"
  "Regexp describing vCard entry types that are to be discarded during import.
Example: `X-GSM-\\|X-MS-'."
  :group 'bbdb-vcard
  :type 'regexp)

(defcustom bbdb-vcard-skip-valueless t
  "Skip vCard entry types with an empty value.
Nil means insert empty types into BBDB."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-import-translation-table
  '(("CELL\\|CAR" . "Mobile")
    ("WORK" . "Office")
    ("HOME" . "Home")  ; translates e.g. "dom,home,postal,parcel" to "Home"
    ("^$" . "Office")) ; acts as a default for parameterless ADR or TEL
  "Label translation on vCard import.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (VCARD-LABEL-REGEXP . BBDB-LABEL).  One entry
should map a default BBDB label to the empty string (`\"^$\"') which
corresponds to unlabelled vCard entries."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-try-merge t
  "Try to merge vCards into existing BBDB entries.
Nil means create a fresh bbdb entry each time a vCard is read."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-type-canonicalizer 'upcase
  "Function to apply to vCard type names on export.
Most reasonable choices are `upcase' and `downcase'."
  :group 'bbdb-vcard
  :type 'function)

(defcustom bbdb-vcard-default-dir "~/exported-vcards/"
  "Default storage directory for exported vCards.
Nil means current directory."
  :group 'bbdb-vcard
  :type '(choice directory (const :tag "Current directory" nil)))

(defcustom bbdb-vcard-export-translation-table
  '(("Mobile" . "CELL")
    ("Office" . "WORK"))
  "Label translation on vCard export.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (BBDB-LABEL-REGEXP . VCARD-LABEL).  One entry
should map a default BBDB label to the empty string (`\"^$\"') which
corresponds to unlabelled vCard entries."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-x-type-candidates
  '(attribution
    finger-host
    gnus-score
    mark-char
    mail-name
    face
    tex-name
    aka)                                ; not sure what this is for
  "List of translatable BBDB user field names.
On export to a vCard, they are transformed into vCard-compliant
extended types by prepending `X-BBDB-'.  On (re-)import, this prefix
is removed again."
  :group 'bbdb-vcard
  :type '(repeat symbol))



;;;; User Functions

;;;###autoload
(defun bbdb-vcard-import-region (begin end)
  "Import the vCards between BEGIN and END into BBDB.
Existing BBDB entries may be altered."
  (interactive "r")
  (bbdb-vcard-iterate-vcards (buffer-substring-no-properties begin end)
                             'bbdb-vcard-process-vcard))

;;;###autoload
(defun bbdb-vcard-import-buffer (vcard-buffer)
  "Import vCards from VCARD-BUFFER into BBDB.
Existing BBDB entries may be altered."
  (interactive (list (current-buffer)))
  (set-buffer vcard-buffer)
  (bbdb-vcard-import-region (point-min) (point-max)))

;;;###autoload
(defun bbdb-vcard-import-file (vcard-file)
  "Import vCards from VCARD-FILE into BBDB.
If VCARD-FILE is a wildcard, import each matching file.  Existing BBDB
entries may be altered."
  (interactive "FvCard file (or wildcard): ")
  (dolist (vcard-file (file-expand-wildcards vcard-file))
    (with-temp-buffer
      (insert-file-contents vcard-file)
      (bbdb-vcard-import-region (point-min) (point-max)))))

;;;###autoload
(defun bbdb-vcard-export
  (filename-or-directory all-records-p one-file-per-record-p)
  "From Buffer *BBDB*, write one or more record(s) as vCard(s) to file(s).
\\<bbdb-mode-map>\
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-vcard-export]\"\
is used instead of simply \"\\[bbdb-vcard-export]\", then export all\
records currently
in the *BBDB* buffer.  If used with prefix argument, store records
in individual files."
  (interactive
   (let ((default-filename              ; argument filename-or-directory
           (bbdb-vcard-make-file-name (bbdb-current-record nil)))
         (all-records-p (bbdb-do-all-records-p)))
     (list
      (if all-records-p
          (if current-prefix-arg
              (read-directory-name "Write vCard files to directory: "
                                   bbdb-vcard-default-dir nil 42)
            (read-file-name
             "Write vCards to file: "
             bbdb-vcard-default-dir
             nil nil
             (format-time-string "%Y-%m-%dT%H:%M.vcf" (current-time))))
        (read-file-name "Write current record to vCard file: "
                        bbdb-vcard-default-dir nil nil default-filename))
      all-records-p           ; argument all-records-p
      current-prefix-arg)))             ; argument one-file-per-record-p
  (if all-records-p
      (let ((records (progn (set-buffer bbdb-buffer-name)
                            (mapcar 'car bbdb-records)))
            used-up-basenames)          ; keep them unique
        (if one-file-per-record-p
            (progn
              (dolist (record records)
                (with-temp-buffer
                  (let ((basename
                         (bbdb-vcard-make-file-name record
                                                    used-up-basenames)))
                    (insert (bbdb-vcard-from record))
                    (write-region nil nil
                                  (concat filename-or-directory basename)
                                  nil nil nil t)
                    (push basename used-up-basenames))))
              (message "Wrote %d vCards to %s"
                       (length used-up-basenames) filename-or-directory))
          (with-temp-buffer     ; all visible BBDB records in one file
            (dolist (record records)
              (insert (bbdb-vcard-from record)))
            (write-region nil nil filename-or-directory nil nil nil t))))
    (let ((vcard (bbdb-vcard-from (bbdb-current-record nil)))) ; current record
      (with-temp-buffer
        (insert vcard)
        (write-region nil nil filename-or-directory nil nil nil t)))))

;;;###autoload (define-key bbdb-mode-map [(v)] 'bbdb-vcard-export)
(define-key bbdb-mode-map [(v)] 'bbdb-vcard-export)



(defun bbdb-vcard-iterate-vcards (vcards vcard-processor)
  "Apply VCARD-PROCESSOR successively to each vCard in string VCARDS."
  (with-temp-buffer
    (insert vcards)
    (goto-char (point-min))
    ;; Change CR into CRLF if necessary, dealing with inconsistent line
    ;; endings.
    (while (re-search-forward "[^\r]\\(\n\\)" nil t)
      (replace-match "\r\n" nil nil nil 1))
    (bbdb-vcard-unfold-lines)
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([[:alnum:]-]*\\.\\)?*BEGIN:VCARD\\([\r\n[:print:][:cntrl:]]*?\\)\\(^\\([[:alnum:]-]*\\.\\)?END:VCARD\\)"
            nil t)
      (funcall vcard-processor (match-string 2)))))

(defun bbdb-vcard-process-vcard (entry)
  "Store the vCard ENTRY in BBDB.
\(ENTRY is expected to have BEGIN:VCARD and END:VCARD delimiters
stripped off.) Extend existing BBDB entries where possible."
  (with-temp-buffer
    (insert entry)
    (unless
        (string=
         (cdr (assoc "value"
                     (car (bbdb-vcard-entries-of-type "version")))) "3.0")
      (display-warning '(bbdb-vcard xy) "Not a version 3.0 vCard."))
    (let* ((raw-name
            (cdr (assoc "value" (car (bbdb-vcard-entries-of-type "N" t)))))
           ;; Name suitable for storing in BBDB:
           (name
            (bbdb-vcard-unescape-strings (bbdb-vcard-unvcardize-name raw-name)))
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
               (bbdb-join (bbdb-vcard-unvcardize-name (cdr (assoc "value" n)))
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
             (bbdb-vcard-unvcardize-org
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
              (concat "\\(" (bbdb-join vcard-email "\\)\\|\\(") "\\)")))
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
            (mapcar 'bbdb-vcard-unvcardize-adr
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
           (vcard-categories (bbdb-vcard-entries-of-type "CATEGORIES"))
           ;; The BBDB record to change:
           (record-freshness-info "BBDB record changed:") ; default user info
           (bbdb-record
            (or
             ;; Try to find an existing one ...
             ;; (a) try company and net and name:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for
                        org-to-search-for email-to-search-for)))
             ;; (b) try company and name:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records) name-to-search-for org-to-search-for)))
             ;; (c) try net and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil email-to-search-for)))
             ;; (d) try birthday and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil nil bday-to-search-for)))
             ;; (e) try phone and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil nil nil tel-to-search-for)))
             ;; No existing record found; make a fresh one:
             (let ((fresh-record (make-vector bbdb-record-length nil)))
               (bbdb-record-set-cache fresh-record
                                      (make-vector bbdb-cache-length nil))
               (if vcard-rev            ; For fresh records,
                   (bbdb-record-putprop ; set creation-date from vcard-rev
                    fresh-record 'creation-date
                    (replace-regexp-in-string
                     "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\).*" "\\1"
                     vcard-rev))
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
      (when name ; which should be the case as N is mandatory in vCard
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
      (bbdb-record-set-net
       bbdb-record (union vcard-email bbdb-nets :test 'string=))
      (bbdb-record-set-addresses
       bbdb-record (union vcard-adrs bbdb-addresses :test 'equal))
      (bbdb-record-set-phones bbdb-record
                              (union vcard-tels bbdb-phones :test 'equal))
      ;; prepare bbdb's notes:
      (when vcard-url (push (cons 'www vcard-url) bbdb-raw-notes))
      (when vcard-notes
        ;; Put vCard NOTEs under key 'notes (append if necessary).
        (unless (assq 'notes bbdb-raw-notes)
          (push (cons 'notes "") bbdb-raw-notes))
        (setf (cdr (assq 'notes bbdb-raw-notes))
              (bbdb-vcard-merge-strings
               (cdr (assq 'notes bbdb-raw-notes))
               (bbdb-vcard-unescape-strings
                (mapconcat (lambda (element) (cdr (assoc "value" element)))
                           (nreverse vcard-notes)
                           ";\n"))
               ";\n")))
      (when vcard-bday
        (unless (assq 'anniversary bbdb-raw-notes)
          (push (cons 'anniversary "") bbdb-raw-notes))
        (setf (cdr (assq 'anniversary bbdb-raw-notes))
              vcard-bday))              ; for consumption by org-mode
      (when vcard-categories
        ;; Put vCard CATEGORIES under key 'mail-alias (append if necessary).
        (unless (assq 'mail-alias bbdb-raw-notes)
          (push (cons 'mail-alias "") bbdb-raw-notes))
        (setf (cdr (assq 'mail-alias bbdb-raw-notes))
              (bbdb-vcard-merge-strings
               (cdr (assq 'mail-alias bbdb-raw-notes))
               (bbdb-vcard-unescape-strings
                (mapconcat (lambda (element) (cdr (assoc "value" element)))
                           (nreverse vcard-categories)
                           ","))
               ",")))
      (while (setq other-vcard-type (bbdb-vcard-other-entry))
        (when (string-match "^\\([[:alnum:]-]*\\.\\)?AGENT"
                            (symbol-name (car other-vcard-type)))
          ;; Notice other vCards inside the current one.
          (bbdb-vcard-iterate-vcards (cdr other-vcard-type)
                                     'bbdb-vcard-process-vcard))
        (unless (or (and bbdb-vcard-skip
                         (string-match bbdb-vcard-skip
                                       (symbol-name (car other-vcard-type))))
                    (and bbdb-vcard-skip-valueless
                         (zerop (length (cdr other-vcard-type)))))
          (push (bbdb-vcard-remove-x-bbdb other-vcard-type) bbdb-raw-notes)))
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

(defun bbdb-vcard-remove-x-bbdb (vcard-element)
  "Remove the `X-BBDB-' prefix from the type part of VCARD-ELEMENT if any."
  (cons (intern (replace-regexp-in-string
                 "^X-BBDB-" "" (symbol-name (car vcard-element))))
        (cdr vcard-element)))

(defun bbdb-vcard-prepend-x-bbdb-maybe (bbdb-fieldname)
  "If BBDB-FIELDNAME is in `bbdb-vcard-x-type-candidates', prepend `X-BBDB'."
  (if (member bbdb-fieldname bbdb-vcard-x-type-candidates)
      ;; lowercase more consistent here
      (intern (concat "x-bbdb-" (symbol-name bbdb-fieldname)))
    bbdb-fieldname))

(defun bbdb-vcard-fold-line (long-line)
  "Insert after every 75th position in LONG-LINE a newline and a space."
  (with-temp-buffer (insert long-line)
                    (goto-char (point-min))
                    (while (< (goto-char (+ (point) 75))
                              (point-max))
                      (insert "\n "))
                    (insert "\n")
                    (buffer-string)))

(defun bbdb-vcard-insert-vcard-element (type &rest values)
  "Insert a vCard element comprising TYPE, `:', VALUES into current buffer.
Take care of TYPE canonicalization, line folding, and closing newline.
Do nothing if TYPE is non-nil and VALUES are empty.  Insert just a
newline if TYPE is nil."
  (if type
      (let ((value (bbdb-join values "")))
        (unless (zerop (length value))
          (insert (bbdb-vcard-fold-line
                   (concat (bbdb-vcard-canonicalize-vcard-type type) ":"
                           value)))))
    (insert (bbdb-vcard-fold-line ""))))

(defun bbdb-vcard-from (record)
  "Return BBDB RECORD as a vCard."
  (with-temp-buffer
    (let ((name (bbdb-record-name record))
          (first-name (bbdb-record-firstname record))
          (last-name (bbdb-record-lastname record))
          (aka (bbdb-record-aka record))
          (company (bbdb-record-company record))
          (net (bbdb-record-net record))
          (phones (bbdb-record-phones record))
          (addresses (bbdb-record-addresses record))
          (www (bbdb-get-field record 'www))
          (notes
           (bbdb-vcard-split-structured-text (bbdb-record-notes record)
                                             ";\n" t))
          (anniversary
           (replace-regexp-in-string
            "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\) birthday" "\\1"
            (bbdb-get-field record 'anniversary)))
          (creation-date (bbdb-get-field record 'creation-date))
          (mail-aliases (bbdb-record-getprop record
                                             bbdb-define-all-aliases-field))
          (raw-notes (copy-alist (bbdb-record-raw-notes record))))
      (bbdb-vcard-insert-vcard-element "BEGIN" "VCARD")
      (bbdb-vcard-insert-vcard-element "VERSION" "3.0")
      (bbdb-vcard-insert-vcard-element "FN" (bbdb-vcard-escape-strings name))
      (bbdb-vcard-insert-vcard-element
       "N" (bbdb-vcard-escape-strings last-name)
       ";" (bbdb-vcard-escape-strings first-name)
       ";;;") ; Additional Names, Honorific Prefixes, Honorific Suffixes
      (bbdb-vcard-insert-vcard-element
       "NICKNAME" (bbdb-join (bbdb-vcard-escape-strings aka) ","))
      (bbdb-vcard-insert-vcard-element
       "ORG" (bbdb-vcard-escape-strings company))
      (dolist (mail net)
        (bbdb-vcard-insert-vcard-element
         "EMAIL;TYPE=INTERNET" (bbdb-vcard-escape-strings mail)))
      (dolist (phone phones)
        (bbdb-vcard-insert-vcard-element
         (concat
          "TEL;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-phone-location phone) t)))
         (bbdb-vcard-escape-strings (bbdb-phone-string phone))))
      (dolist (address addresses)
        (bbdb-vcard-insert-vcard-element
         (concat 
          "ADR;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-address-location address) t)))
         ";;"                           ; no Postbox, no Extended
         (bbdb-join (bbdb-vcard-escape-strings (bbdb-address-streets address))
                    ",")
         ";" (bbdb-vcard-escape-strings (bbdb-address-city address))
         ";" (bbdb-vcard-escape-strings (bbdb-address-state address))
         ";" (bbdb-vcard-escape-strings (bbdb-address-zip address))
         ";" (bbdb-vcard-escape-strings (bbdb-address-country address))))
      (bbdb-vcard-insert-vcard-element "URL" www)
      (dolist (note notes)
        (bbdb-vcard-insert-vcard-element
         "NOTE" (bbdb-vcard-escape-strings note)))
      (bbdb-vcard-insert-vcard-element "BDAY" anniversary)
      (bbdb-vcard-insert-vcard-element "REV" creation-date)
      (bbdb-vcard-insert-vcard-element
       "CATEGORIES" 
       (bbdb-join (bbdb-vcard-escape-strings
                   (bbdb-vcard-split-structured-text mail-aliases "," t)) ","))
      ;; prune raw-notes...
      (dolist (key '(www notes anniversary mail-alias creation-date timestamp))
        (setq raw-notes (assq-delete-all key raw-notes)))
      ;; ... and output what's left
      (dolist (raw-note raw-notes)
        (bbdb-vcard-insert-vcard-element
         (symbol-name (bbdb-vcard-prepend-x-bbdb-maybe (car raw-note)))
         (bbdb-vcard-escape-strings (cdr raw-note))))
      (bbdb-vcard-insert-vcard-element "END" "VCARD")
      (bbdb-vcard-insert-vcard-element nil)) ; newline
    (buffer-string)))

(defun bbdb-vcard-unvcardize-name (vcard-name)
  "Convert VCARD-NAME (type N) into (FIRSTNAME LASTNAME)."
  (if (stringp vcard-name)              ; unstructured N
      (bbdb-divide-name vcard-name)
    (let ((vcard-name
           (mapcar (lambda (x)
                     (bbdb-join (bbdb-vcard-split-structured-text x "," t)
                                " "))
                   vcard-name))) ; flatten comma-separated substructure
      (list (concat (nth 3 vcard-name)  ; honorific prefixes
                    (unless (zerop (length (nth 3 vcard-name))) " ")
                    (nth 1 vcard-name)  ; given name
                    (unless (zerop (length (nth 2 vcard-name))) " ")
                    (nth 2 vcard-name)) ; additional names
            (concat (nth 0 vcard-name)  ; family name
                    (unless (zerop (length (nth 4 vcard-name))) " ")
                    (nth 4 vcard-name)))))) ; honorific suffixes

(defun bbdb-vcard-unvcardize-org (vcard-org)
  "Convert VCARD-ORG (type ORG), which may be a list, into a string."
  (if (or (null vcard-org)
          (stringp vcard-org)) ; unstructured, probably non-standard ORG
      vcard-org                ; Company, unit 1, unit 2...
    (bbdb-join vcard-org "\n")))

(defun bbdb-vcard-unvcardize-adr (vcard-adr)
  "Convert VCARD-ADR into BBDB format.
Turn a vCard element of type ADR into (TYPE STREETS CITY STATE ZIP
COUNTRY)."
  (let ((adr-type (or (cdr (assoc "type" vcard-adr)) ""))
        (adr-value
         (mapcar                ; flatten comma-separated substructure
          (lambda (x)
            (bbdb-join (bbdb-vcard-split-structured-text x "," t) ", "))
          (cdr (assoc "value" vcard-adr)))))
    (vector (bbdb-vcard-translate adr-type)
            ;; Postbox, Extended, Streets
            (remove-if (lambda (x) (zerop (length x)))
                       (subseq adr-value 0 3))
            (or (elt adr-value 3) "")    ; City
            (or (elt adr-value 4) "")    ; State
            (or (elt adr-value 5) "")    ; Zip
            (or (elt adr-value 6) "")))) ; Country

(defun bbdb-vcard-canonicalize-vcard-type (&rest strings)
  "Concatenate STRINGS and apply `bbdb-vcard-type-canonicalizer' to them."
  (funcall bbdb-vcard-type-canonicalizer (bbdb-join strings "")))

(defun bbdb-vcard-entries-of-type (type &optional one-is-enough-p)
  "From current buffer read and delete the vCard entries of TYPE.
The current buffer is supposed to contain a single vCard.  If
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
              ;; collect vCard parameter list `;a=x;a=y;a=z'
              ;; into vCard value list `;a=x,y,z'; becoming ("a" . "x,y,z")
              (setf (cdr parameter-sibling)
                    (concat (cdr parameter-sibling) "," parameter-value))
            ;; vCard parameter pair `;key=value;' with new key
            (push (cons parameter-key parameter-value) parameters))))
      (push parameters values)
      (delete-region (line-end-position 0) (line-end-position))
      (when one-is-enough-p (setq read-enough t)))
    values))

(defun bbdb-vcard-other-entry ()
  "From current buffer read and delete the topmost vCard entry.
Buffer is supposed to contain a single vCard.  Return (TYPE . ENTRY)."
  (goto-char (point-min))
  (when (re-search-forward "^\\([[:graph:]]*?\\):\\(.*\\)\r$" nil t)
    (let ((type (match-string 1))
          (value (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (cons (intern (downcase type)) (bbdb-vcard-unescape-strings value)))))

(defun bbdb-vcard-merge-strings (old-string new-string separator)
  "Merge string NEW-STRING into string OLD-STRING.
If NEW-STRING is already in OLD-STRING, return OLD-STRING.  Otherwise
append SEPARATOR and NEW-STRING."
  (with-temp-buffer
    (insert old-string)
    (unless (search-backward new-string nil t)
      (goto-char (point-max))
      (unless (zerop (buffer-size)) (insert separator))
      (insert new-string))
    (buffer-string)))

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

(defun bbdb-vcard-unfold-lines ()
  "Unfold folded vCard lines in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\r\n\\( \\|\t\\)" nil t) (replace-match "")))

(defun bbdb-vcard-unescape-strings (escaped-strings)
  "Unescape escaped `;', `,', `\\', and newlines in ESCAPED-STRINGS.
ESCAPED-STRINGS may be a string or a sequence of strings."
  (flet ((unescape (x) (replace-regexp-in-string
                        "\\([\\\\]\\)\\([,;\\]\\)" ""
                        (replace-regexp-in-string "\\\\n" "\n" x)
                        nil nil 1)))
    (bbdb-vcard-process-strings 'unescape escaped-strings)))

(defun bbdb-vcard-escape-strings (unescaped-strings)
  "Escape `;', `,', `\\', and newlines in UNESCAPED-STRINGS.
UNESCAPED-STRINGS may be a string or a sequence of strings."
  (flet ((escape (x) (replace-regexp-in-string
                      "\n" "\\\\n" (replace-regexp-in-string
                                    "\\(\\)[,;\\]" "\\\\" (or x "")
                                    nil nil 1))))
    (bbdb-vcard-process-strings 'escape unescaped-strings)))

(defun bbdb-vcard-process-strings (string-processor strings)
  "Apply STRING-PROCESSOR to STRINGS.
STRINGS may be a string or a sequence of strings."
  (if (stringp strings)
      (funcall string-processor strings)
    (mapcar string-processor strings)))

(defun bbdb-vcard-translate (label &optional exportp)
  "Translate LABEL from vCard to BBDB or, if EXPORTP is non-nil, vice versa.
Translations are defined in `bbdb-vcard-import-translation-table' and
`bbdb-vcard-export-translation-table' respectively."
  (when label
    (capitalize
     (or (assoc-default label
                        (if exportp
                            bbdb-vcard-export-translation-table
                          bbdb-vcard-import-translation-table) 'string-match)
         label))))

(defmacro bbdb-vcard-search-intersection
  (records &optional name company net notes phone)
  "Search RECORDS for records that match each non-nil argument."
  (let*
      ((phone-search
        (if phone `(when ,phone (bbdb-search ,records nil nil nil nil ,phone))
          records))
       (notes-search
        (if notes `(when ,notes (bbdb-search ,phone-search nil nil nil ,notes))
          phone-search))
       (net-search
        (if net `(when ,net (bbdb-search ,notes-search nil nil ,net))
          notes-search))
       (company-search
        (if company `(when ,company (bbdb-search ,net-search nil ,company))
          net-search))
       (name-search
        (if name `(when ,name (bbdb-search ,company-search ,name))
          company-search)))
    name-search))

(defun bbdb-vcard-make-file-name (bbdb-record &optional used-up-basenames)
  "Come up with a vCard filename given a BBDB-RECORD.
Make it unique against the list USED-UP-BASENAMES."
  (let ((name (bbdb-record-name bbdb-record))
        (aka (car (bbdb-record-aka bbdb-record)))
        (unique-number 0)
        filename)
    (while (member
            (setq filename
                  (concat
                   (replace-regexp-in-string
                    "[[:blank:]]+" "_"
                    (or (unless (zerop (length name)) name)
                        (unless (zerop (length aka)) aka)
                        "bbdb-record"))
                   (unless (zerop unique-number)
                     (concat "-" (number-to-string unique-number)))
                   ".vcf"))
            used-up-basenames)
      (incf unique-number))
    filename))



(provide 'bbdb-vcard)

;;; bbdb-vcard.el ends here
