;;; Tests for the vcard import from bbdb-vcard.el
;;
;; Before proceeding, you should probably save your production bbdb file.
;;
;; To run the tests, eval this file.
;; In case of failure, find test results in buffer `bbdb-vcard-test-result'.
;;
;; For the sake of minimality, not all test cases are rfc compliant.


(require 'bbdb-vcard)

(defun bbdb-vcard-test
  (vcard bbdb-entry search-name
         &optional search-company search-net check-creation-date-p)
  "Import VCARD and search for it in bbdb by SEARCH-NAME,
SEARCH-COMPANY, (perhaps later) SEARCH-NET.  If search result
disagrees with BBDB-ENTRY, talk about it in buffer
bbdb-vcard-test-result. timestamp and, if CHECK-CREATION-DATE-P is
nil, creation-date are not taken into account."
  (bbdb-vcard-iterate-vcards vcard 'bbdb-vcard-process-vcard)
  (let* ((search-company (or search-company ""))
         (bbdb-search-result
          (car (bbdb-search (bbdb-search (bbdb-records) search-name)
                            nil search-company))))
    (setf (cdr (assoc 'timestamp (elt bbdb-search-result 7))) "2010-03-04"
          (cdr (assoc 'timestamp (elt bbdb-entry 7))) "2010-03-04")
    (unless check-creation-date-p
      (setf (cdr (assoc 'creation-date (elt bbdb-search-result 7))) "2010-03-04"
            (cdr (assoc 'creation-date (elt bbdb-entry 7))) "2010-03-04"))
    (unless (equal (subseq bbdb-search-result 0 8)
                   (subseq bbdb-entry 0 8))
      (princ "\nTest failed:\n" (get-buffer-create "bbdb-vcard-test-result"))
      (prin1 vcard (get-buffer-create "bbdb-vcard-test-result"))
      (princ "\nwas stored as\n" (get-buffer-create "bbdb-vcard-test-result"))
      (prin1 (subseq bbdb-search-result 0 8)
             (get-buffer-create "bbdb-vcard-test-result"))
      (princ "\nbut was expected as\n" (get-buffer-create "bbdb-vcard-test-result"))
      (prin1 bbdb-entry (get-buffer-create "bbdb-vcard-test-result")))))


;;; Try not to mess up our real BBDB:
(when bbdb-buffer
  (save-buffer bbdb-buffer)
  (kill-buffer bbdb-buffer))
(when (get-buffer "test-bbdb") (kill-buffer "test-bbdb"))
(setq bbdb-file "/tmp/test-bbdb")
(when (file-exists-p bbdb-file) (delete-file bbdb-file))
(when (get-buffer "bbdb-vcard-test-result") (kill-buffer "bbdb-vcard-test-result"))



;;; The Tests


(bbdb-vcard-test
 "
** A vcard without any type parameters.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:First1 Last1
N:Last1;First1
NICKNAME:Firsty1
PHOTO:The Alphabet:
 abcdefghijklmnop
 qrstuvwsyz
BDAY:1999-12-05
ADR:Box111;Room 111;First Street,First Corner;Cityone;First State;11111;Country
LABEL:Label 1
TEL:+11111111
EMAIL:first1@provider1
MAILER:Wanderlust1
TZ:+01:00
GEO:37.386013;-122.082932
TITLE:Director\\, Research and Development
ROLE:Programmer
LOGO:encoded logo #1
AGENT:CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com
ORG:Company1;Unit1;Subunit1
CATEGORIES:category1
NOTE:This vcard uses every type defined in rfc2426.
PRODID:-//ONLINE DIRECTORY//NONSGML Version 1//EN
REV:1995-10-31T22:27:10Z
SORT-STRING:aaa000
SOUND:Audible1
UID:111-111-111-111
URL:http://first1.host1.org
CLASS:CONFIDENTIAL
KEY:The Key No 1
X-foo:extended type 1
END:VCARD
"
 ["First1" "Last1"
  ("Firsty1")
  "Company1
Unit1
Subunit1"
  (["Office" "+11111111"])
  (["Office"
    ("Box111" "Room 111" "First Street, First Corner")
    "Cityone"
    "First State"
    "11111"
    "Country"])
  ("first1@provider1")
  ((x-foo . "extended type 1")
   (key . "The Key No 1")
   (class . "CONFIDENTIAL")
   (uid . "111-111-111-111")
   (sound . "Audible1")
   (sort-string . "aaa000")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (agent . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo . "encoded logo #1")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "+01:00")
   (mailer . "Wanderlust1")
   (label . "Label 1")
   (photo . "The Alphabet:abcdefghijklmnopqrstuvwsyz")
   (mail-alias . "category1")
   (anniversary . "1999-12-05 birthday")
   (notes . "This vcard uses every type defined in rfc2426.")
   (www . "http://first1.host1.org")
   (creation-date . "1995-10-31") (timestamp . "2010-03-04"))]
 "First1 Last1"
 nil nil t)


(bbdb-vcard-test
 "
** The following is made of examples from rfc2426.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Mr. John Q. Public\\, Esq.
N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
NICKNAME:Robbie
PHOTO;VALUE=uri:http://www.abc.com/pub/photos
 /jqpublic.gif
BDAY:1996-04-15
ADR;TYPE=dom,home,postal,parcel:;;123 Main
  Street;Any Town;CA;91921-1234
LABEL;TYPE=dom,home,postal,parcel:Mr.John Q. Public\\, Esq.\\n
 Mail Drop: TNE QB\\n123 Main Street\\nAny Town\\, CA  91921-1234
 \\nU.S.A.
TEL;TYPE=work,voice,pref,msg:+1-213-555-1234
EMAIL;TYPE=internet:jqpublic@xyz.dom1.com
EMAIL;TYPE=internet:jdoe@isp.net
MAILER:PigeonMail 2.1
TZ:-05:00
GEO:37.386013;-122.082932
TITLE:Director\\, Research and Development
ROLE:Programmer
LOGO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
AGENT;VALUE=uri:
 CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com
ORG:ABC\\, Inc.;North American Division;Marketing
CATEGORIES:TRAVEL AGENT
NOTE:This fax number is operational 0800 to 1715
  EST\\, Mon-Fri.
PRODID:-//ONLINE DIRECTORY//NONSGML Version 1//EN
REV:1995-10-31T22:27:10Z
SOUND;TYPE=BASIC;ENCODING=b:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
UID:19950401-080045-40000F192713-0052
URL:http://www.swbyps.restaurant.french/~chezchic.html
CLASS:PUBLIC
KEY;ENCODING=b:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQA
 wdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENbW11bmljYX
 Rpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
 ZW1zMRwwGgYDVQQDExNyb290Y2EubmV0c2NhcGUuY29tMB4XDTk3MDYwNj
 E5NDc1OVoXDTk3MTIwMzE5NDc1OVowgYkxCzAJBgNVBAYTAlVTMSYwJAYD
 VQQKEx1OZXRzY2FwZSBDb21tdW5pY2F0aW9ucyBDb3JwLjEYMBYGA1UEAx
 MPVGltb3RoeSBBIEhvd2VzMSEwHwYJKoZIhvcNAQkBFhJob3dlc0BuZXRz
 Y2FwZS5jb20xFTATBgoJkiaJk/IsZAEBEwVob3dlczBcMA0GCSqGSIb3DQ
 EBAQUAA0sAMEgCQQC0JZf6wkg8pLMXHHCUvMfL5H6zjSk4vTTXZpYyrdN2
 dXcoX49LKiOmgeJSzoiFKHtLOIboyludF90CgqcxtwKnAgMBAAGjNjA0MB
 EGCWCGSAGG+EIBAQQEAwIAoDAfBgNVHSMEGDAWgBT84FToB/GV3jr3mcau
 +hUMbsQukjANBgkqhkiG9w0BAQQFAAOBgQBexv7o7mi3PLXadkmNP9LcIP
 mx93HGp0Kgyx1jIVMyNgsemeAwBM+MSlhMfcpbTrONwNjZYW8vJDSoi//y
 rZlVt9bJbs7MNYZVsyF1unsqaln4/vy6Uawfg8VUMk1U7jt8LYpo4YULU7
 UZHPYVUaSgVttImOHZIKi4hlPXBOhcUQ==
END:VCARD
"
 ["Dr. John Philip Paul" "Stevenson Jr. M.D. A.C.P."
  ("Mr. John Q. Public, Esq." "Robbie")
  "ABC, Inc.
North American Division
Marketing"
  (["Office" "+1-213-555-1234"])
  (["Home"
    ("123 Main Street")
    "Any Town"
    "CA"
    "91921-1234"
    nil])
  ("jdoe@isp.net" "jqpublic@xyz.dom1.com")
  ((key\;encoding=b
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0ZW1zMRwwGgYDVQQDExNyb290Y2EubmV0c2NhcGUuY29tMB4XDTk3MDYwNjE5NDc1OVoXDTk3MTIwMzE5NDc1OVowgYkxCzAJBgNVBAYTAlVTMSYwJAYDVQQKEx1OZXRzY2FwZSBDb21tdW5pY2F0aW9ucyBDb3JwLjEYMBYGA1UEAxMPVGltb3RoeSBBIEhvd2VzMSEwHwYJKoZIhvcNAQkBFhJob3dlc0BuZXRzY2FwZS5jb20xFTATBgoJkiaJk/IsZAEBEwVob3dlczBcMA0GCSqGSIb3DQEBAQUAA0sAMEgCQQC0JZf6wkg8pLMXHHCUvMfL5H6zjSk4vTTXZpYyrdN2dXcoX49LKiOmgeJSzoiFKHtLOIboyludF90CgqcxtwKnAgMBAAGjNjA0MBEGCWCGSAGG+EIBAQQEAwIAoDAfBgNVHSMEGDAWgBT84FToB/GV3jr3mcau+hUMbsQukjANBgkqhkiG9w0BAQQFAAOBgQBexv7o7mi3PLXadkmNP9LcIPmx93HGp0Kgyx1jIVMyNgsemeAwBM+MSlhMfcpbTrONwNjZYW8vJDSoi//yrZlVt9bJbs7MNYZVsyF1unsqaln4/vy6Uawfg8VUMk1U7jt8LYpo4YULU7UZHPYVUaSgVttImOHZIKi4hlPXBOhcUQ==")
   (class . "PUBLIC")
   (uid . "19950401-080045-40000F192713-0052")
   (sound\;type=basic\;encoding=b
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (agent\;value=uri . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo\;encoding=b\;type=jpeg
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "-05:00")
   (mailer . "PigeonMail 2.1")
   (label\;type=dom\,home\,postal\,parcel
    . "Mr.John Q. Public, Esq.\nMail Drop: TNE QB\n123 Main Street\nAny Town, CA  91921-1234\nU.S.A.")
   (photo\;value=uri . "http://www.abc.com/pub/photos/jqpublic.gif")
   (mail-alias . "TRAVEL AGENT")
   (anniversary . "1996-04-15 birthday")
   (notes . "This fax number is operational 0800 to 1715 EST, Mon-Fri.")
   (www . "http://www.swbyps.restaurant.french/~chezchic.html")
   (creation-date . "1995-10-31") (timestamp . "2010-03-04"))]
 "John"
 nil nil t)


(bbdb-vcard-test
 "
** Exactly the same as before.
   Re-reading it shouldn't duplicate anything.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Mr. John Q. Public\\, Esq.
N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
NICKNAME:Robbie
PHOTO;VALUE=uri:http://www.abc.com/pub/photos
 /jqpublic.gif
BDAY:1996-04-15
ADR;TYPE=dom,home,postal,parcel:;;123 Main
  Street;Any Town;CA;91921-1234
LABEL;TYPE=dom,home,postal,parcel:Mr.John Q. Public\\, Esq.\\n
 Mail Drop: TNE QB\\n123 Main Street\\nAny Town\\, CA  91921-1234
 \\nU.S.A.
TEL;TYPE=work,voice,pref,msg:+1-213-555-1234
EMAIL;TYPE=internet:jqpublic@xyz.dom1.com
EMAIL;TYPE=internet:jdoe@isp.net
MAILER:PigeonMail 2.1
TZ:-05:00
GEO:37.386013;-122.082932
TITLE:Director\\, Research and Development
ROLE:Programmer
LOGO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
AGENT;VALUE=uri:
 CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com
ORG:ABC\\, Inc.;North American Division;Marketing
CATEGORIES:TRAVEL AGENT
NOTE:This fax number is operational 0800 to 1715
  EST\\, Mon-Fri.
PRODID:-//ONLINE DIRECTORY//NONSGML Version 1//EN
REV:1995-10-31T22:27:10Z
SOUND;TYPE=BASIC;ENCODING=b:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
UID:19950401-080045-40000F192713-0052
URL:http://www.swbyps.restaurant.french/~chezchic.html
CLASS:PUBLIC
KEY;ENCODING=b:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQA
 wdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENbW11bmljYX
 Rpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
 ZW1zMRwwGgYDVQQDExNyb290Y2EubmV0c2NhcGUuY29tMB4XDTk3MDYwNj
 E5NDc1OVoXDTk3MTIwMzE5NDc1OVowgYkxCzAJBgNVBAYTAlVTMSYwJAYD
 VQQKEx1OZXRzY2FwZSBDb21tdW5pY2F0aW9ucyBDb3JwLjEYMBYGA1UEAx
 MPVGltb3RoeSBBIEhvd2VzMSEwHwYJKoZIhvcNAQkBFhJob3dlc0BuZXRz
 Y2FwZS5jb20xFTATBgoJkiaJk/IsZAEBEwVob3dlczBcMA0GCSqGSIb3DQ
 EBAQUAA0sAMEgCQQC0JZf6wkg8pLMXHHCUvMfL5H6zjSk4vTTXZpYyrdN2
 dXcoX49LKiOmgeJSzoiFKHtLOIboyludF90CgqcxtwKnAgMBAAGjNjA0MB
 EGCWCGSAGG+EIBAQQEAwIAoDAfBgNVHSMEGDAWgBT84FToB/GV3jr3mcau
 +hUMbsQukjANBgkqhkiG9w0BAQQFAAOBgQBexv7o7mi3PLXadkmNP9LcIP
 mx93HGp0Kgyx1jIVMyNgsemeAwBM+MSlhMfcpbTrONwNjZYW8vJDSoi//y
 rZlVt9bJbs7MNYZVsyF1unsqaln4/vy6Uawfg8VUMk1U7jt8LYpo4YULU7
 UZHPYVUaSgVttImOHZIKi4hlPXBOhcUQ==
END:VCARD
"
 ["Dr. John Philip Paul" "Stevenson Jr. M.D. A.C.P."
  ("Mr. John Q. Public, Esq." "Robbie")
  "ABC, Inc.
North American Division
Marketing"
  (["Office" "+1-213-555-1234"])
  (["Home"
    ("123 Main Street")
    "Any Town"
    "CA"
    "91921-1234"
    nil])
  ("jdoe@isp.net" "jqpublic@xyz.dom1.com")
  ((key\;encoding=b
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0ZW1zMRwwGgYDVQQDExNyb290Y2EubmV0c2NhcGUuY29tMB4XDTk3MDYwNjE5NDc1OVoXDTk3MTIwMzE5NDc1OVowgYkxCzAJBgNVBAYTAlVTMSYwJAYDVQQKEx1OZXRzY2FwZSBDb21tdW5pY2F0aW9ucyBDb3JwLjEYMBYGA1UEAxMPVGltb3RoeSBBIEhvd2VzMSEwHwYJKoZIhvcNAQkBFhJob3dlc0BuZXRzY2FwZS5jb20xFTATBgoJkiaJk/IsZAEBEwVob3dlczBcMA0GCSqGSIb3DQEBAQUAA0sAMEgCQQC0JZf6wkg8pLMXHHCUvMfL5H6zjSk4vTTXZpYyrdN2dXcoX49LKiOmgeJSzoiFKHtLOIboyludF90CgqcxtwKnAgMBAAGjNjA0MBEGCWCGSAGG+EIBAQQEAwIAoDAfBgNVHSMEGDAWgBT84FToB/GV3jr3mcau+hUMbsQukjANBgkqhkiG9w0BAQQFAAOBgQBexv7o7mi3PLXadkmNP9LcIPmx93HGp0Kgyx1jIVMyNgsemeAwBM+MSlhMfcpbTrONwNjZYW8vJDSoi//yrZlVt9bJbs7MNYZVsyF1unsqaln4/vy6Uawfg8VUMk1U7jt8LYpo4YULU7UZHPYVUaSgVttImOHZIKi4hlPXBOhcUQ==")
   (class . "PUBLIC")
   (uid . "19950401-080045-40000F192713-0052")
   (sound\;type=basic\;encoding=b
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (agent\;value=uri . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo\;encoding=b\;type=jpeg
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "-05:00")
   (mailer . "PigeonMail 2.1")
   (label\;type=dom\,home\,postal\,parcel
    . "Mr.John Q. Public, Esq.\nMail Drop: TNE QB\n123 Main Street\nAny Town, CA  91921-1234\nU.S.A.")
   (photo\;value=uri . "http://www.abc.com/pub/photos/jqpublic.gif")
   (www . "http://www.swbyps.restaurant.french/~chezchic.html")
   (mail-alias . "TRAVEL AGENT")
   (anniversary . "1996-04-15 birthday")
   (notes . "This fax number is operational 0800 to 1715 EST, Mon-Fri.")
   (creation-date . "1995-10-31") (timestamp . "2010-03-04"))]
 "John"
 nil nil t)


(bbdb-vcard-test 
 "
** Re-use of existing BBDB entries. 
*** N, ORG, EMAIL
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
ORG:OrgA;UnitA
EMAIL:userA@hostA.example.com
END:vcard
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitA"
  nil
  nil
  ("userA@hostA.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** The same again; shouldn't change the previous one.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
ORG:OrgA;UnitA
EMAIL:userA@hostA.example.com
END:VCARD
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitA"
  nil
  nil
  ("userA@hostA.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** Same N, same ORG, different EMAIL, which should be added to the previous
    one.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
ORG:OrgA;UnitA
EMAIL:personA@example.com
END:VCARD
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitA"
  nil
  nil
  ("userA@hostA.example.com" "personA@example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** Same N, same ORG, no EMAIL; shouldn't change anything.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
ORG:OrgA;UnitA
END:VCARD
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitA"
  nil
  nil
  ("userA@hostA.example.com" "personA@example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** Same N, same EMAIL, no ORG; shouldn't change anything.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
EMAIL:userA@hostA.example.com
END:VCARD
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitA"
  nil
  nil
  ("userA@hostA.example.com" "personA@example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** Same N, same EMAIL, different ORG by which Company of the previous one
    should be replaced.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA;FirstA
ORG:OrgA;UnitB
EMAIL:userA@hostA.example.com
END:VCARD
"
 ["FirstA" "FamilyA"
  nil
  "OrgA
UnitB"
  nil
  nil
  ("userA@hostA.example.com" "personA@example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA FamilyA")


(bbdb-vcard-test
 "
*** Different N, same EMAIL, same ORG; should go into a fresh entry.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyA1;FirstA1
ORG:OrgA;UnitB
EMAIL:userA@hostA.example.com
END:VCARD
"
 ["FirstA1" "FamilyA1"
  nil
  "OrgA
UnitB"
  nil
  nil
  ("userA@hostA.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstA1 FamilyA1")



(bbdb-vcard-test
 "
** AKA has various sources; duplicates are being discarded.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyB;FirstB
A.N:PseudonymB;FirstB
FN:The FirstB of FamilyB
A.FN:FirstB1 FamilyB1
B.FN:FirstB2 FamilyB2
C.FN:FirstB FamilyB
NICKNAME:Bee,Effy Bee,FirstB FamilyB
END:VCARD
"
 ["FirstB" "FamilyB"
  ("The FirstB of FamilyB"
   "FirstB1 FamilyB1"
   "FirstB2 FamilyB2"
   "FirstB PseudonymB"
   "Bee"
   "Effy Bee")
  nil
  nil
  nil
  nil
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstB FamilyB")


(bbdb-vcard-test
 "
** Additional ORGs go to Notes, org.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyC;FirstC
ORG:OrgC1
ORG:OrgC2
END:vcard
"
 ["FirstC" "FamilyC"
  nil
  "OrgC1"
  nil
  nil
  nil
  ((org . "OrgC2")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstC FamilyC")


(bbdb-vcard-test
 "
*** ... but only if they are unique
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyC;FirstC
ORG:OrgC1
ORG:OrgC2
ORG:OrgC3
ORG:OrgC3
ORG:OrgC4
END:VCARD
"
 ["FirstC" "FamilyC"
  nil
  "OrgC1"
  nil
  nil
  nil
  ((org . "OrgC4")
   (org . "OrgC3")
   (org . "OrgC2")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstC FamilyC")


(bbdb-vcard-test
 "
** Prefixes are discarded.
------------------------------------------------------------
X.BEGIN:VCARD
X.VERSION:3.0
X.N:FamilyD;FirstD
X.ORG:OrgD;UnitD
X.EMAIL:userD@hostD.example.com
X.END:VCARD
"
 ["FirstD" "FamilyD"
  nil
  "OrgD
UnitD"
  nil
  nil
  ("userD@hostD.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstD FamilyD")


(bbdb-vcard-test
 "
*** Same as before, don't change anything.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyD;FirstD
ORG:OrgD;UnitD
EMAIL:userD@hostD.example.com
END:VCARD
"
 ["FirstD" "FamilyD"
  nil
  "OrgD
UnitD"
  nil
  nil
  ("userD@hostD.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstD FamilyD")


(bbdb-vcard-test
 "
*** Same as before, don't change anything.
------------------------------------------------------------
Y.BEGIN:VCARD
Y.VERSION:3.0
Y.N:FamilyD;FirstD
Y.ORG:OrgD;UnitD
Y.EMAIL:userD@hostD.example.com
Y.END:VCARD
"
 ["FirstD" "FamilyD"
  nil
  "OrgD
UnitD"
  nil
  nil
  ("userD@hostD.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstD FamilyD")


(bbdb-vcard-test
 "
** Case Insensitivity
------------------------------------------------------------
BEGIN:Vcard
Version:3.0
n:FamilyE;FirstE
Org:OrgE;UnitE
email:userE@hostE.example.com
end:vcard
"
 ["FirstE" "FamilyE"
  nil
  "OrgE
UnitE"
  nil
  nil
  ("userE@hostE.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstE FamilyE")


(bbdb-vcard-test
 "
** Non-ASCII Content
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Franz Rübezahl
N:Rübezahl;Franz
NICKNAME:Fränzchen,Rübe
ADR:Postschließfach 17;Zimmer Zwölf;Einbahnstraße;Ödstadt;;75480;
ORG:Rübe AG
END:VCARD
"
 ["Franz" "Rübezahl"
  ("Fränzchen" "Rübe")
  "Rübe AG"
  nil
  (["Office"
    ("Postschließfach 17" "Zimmer Zwölf" "Einbahnstraße")
    "Ödstadt"
    ""
    "75480"
    ""])
  nil
  ((creation-date . "2010-03-06") (timestamp . "2010-03-06")) ]
 "Rübe")


(bbdb-vcard-test
 "
*** Multiple, structured ADR
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyF;FirstF
ORG:OrgF;UnitF
ADR;TYPE=dom,home,postal,parcel:Box111,LHS;Room 111,or not;First Street,First Corner;Cityone;First State;11111,22222;Country
ADR;TYPE=intl,work,postal,parcel:Box222,RHS;Room 22,or something;Second Street,First Corner;Citytwo;Second State;222,33333;Country
ADR;TYPE=dom,work,postal,parcel:;;Second Street,First Corner;Citytwo;;222,33333;
ADR;TYPE=intl;TYPE=home;TYPE=parcel:;;Third Street,First Corner;Citythree;;222,33333;
END:VCARD
"
 ["FirstF" "FamilyF"
  nil
  "OrgF
UnitF"
  nil
  (["Home"
    ("Third Street, First Corner")
    "Citythree"
    ""
    "222, 33333"
    ""]
   ["Office"
    ("Second Street, First Corner")
    "Citytwo"
    ""
    "222, 33333"
    ""]
   ["Office"
    ("Box222, RHS" "Room 22, or something" "Second Street, First Corner")
    "Citytwo"
    "Second State"
    "222, 33333"
    "Country"]
   ["Home"
    ("Box111, LHS" "Room 111, or not" "First Street, First Corner")
    "Cityone"
    "First State"
    "11111, 22222"
    "Country"])
  nil
  ((creation-date . "2010-03-06") (timestamp . "2010-03-06")) ]
 "FirstF FamilyF")


(bbdb-vcard-test
 "
*** Skip types from bbdb-vcard-skip
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyH;FirstH
ORG:OrgH;UnitH
EMAIL:userH@hostH.example.com
X-GSM-FOO:Blah
X-GSM-BAR:Blahblah
END:VCARD
"
 ["FirstH" "FamilyH"
  nil
  "OrgH
UnitH"
  nil
  nil
  ("userH@hostH.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstH FamilyH")


(bbdb-vcard-test
 "
*** Skip empty types.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyG;FirstG
ORG:OrgG;UnitG
EMAIL:userG@hostG.example.com
ROLE:
TITLE:
GEO:
END:VCARD
"
 ["FirstG" "FamilyG"
  nil
  "OrgG
UnitG"
  nil
  nil
  ("userG@hostG.example.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstG FamilyG")


(bbdb-vcard-test
 "
*** Remove X-BBDB- prefixes
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyN;FirstN
ORG:OrgN;UnitN
EMAIL:userN@hostN.example.com
X-BBDB-MARK-CHAR:b
X-BBDB-TEX-NAME:{\\\\em FirstM FamilyM}
END:VCARD
"
 ["FirstN" "FamilyN"
  nil
  "OrgN
UnitN"
  nil
  nil
  ("userN@hostN.example.com")
  ((tex-name . "{\\em FirstM FamilyM}")
   (mark-char . "b")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstN FamilyN")


(bbdb-vcard-test
 "
** Merging of vcard NOTEs
*** A vcard with two NOTEs.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyI;FirstI
ORG:OrgI
NOTE:Note No. 1a
NOTE:Note No. 1b
END:VCARD
"
 ["FirstI" "FamilyI"
  nil
  "OrgI"
  nil
  nil
  nil
  ((notes . "Note No. 1a;\nNote No. 1b")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstI FamilyI")


(bbdb-vcard-test
 "
*** Same as before, but a different NOTE.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyI;FirstI
ORG:OrgI
NOTE:Note No. 2
END:VCARD
"
 ["FirstI" "FamilyI"
  nil
  "OrgI"
  nil
  nil
  nil
  ((notes . "Note No. 1a;\nNote No. 1b;\nNote No. 2")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstI FamilyI")


(bbdb-vcard-test
 "
*** Same as before, but a NOTE we've seen already
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyI;FirstI
ORG:OrgI
NOTE:Note No. 1b
END:VCARD
"
 ["FirstI" "FamilyI"
  nil
  "OrgI"
  nil
  nil
  nil
  ((notes . "Note No. 1a;\nNote No. 1b;\nNote No. 2")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstI FamilyI")



(bbdb-vcard-test
 "
** Merging of vcard CATEGORIES
*** A vcard with two CATEGORIES.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyM;FirstM
ORG:OrgI
CATEGORIES:Category 1a,Category 1b
CATEGORIES:Category 2a,Category 2b
END:VCARD
"
 ["FirstM" "FamilyM"
  nil
  "OrgI"
  nil
  nil
  nil
  ((mail-alias . "Category 1a,Category 1b,Category 2a,Category 2b")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstM FamilyM")


(bbdb-vcard-test
 "
*** Same as before, but a different CATEGORIES.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyM;FirstM
ORG:OrgI
CATEGORIES:Category 3
END:VCARD
"
 ["FirstM" "FamilyM"
  nil
  "OrgI"
  nil
  nil
  nil
  ((mail-alias . "Category 1a,Category 1b,Category 2a,Category 2b,Category 3")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstM FamilyM")


(bbdb-vcard-test
 "
*** Same as before, but a CATEGORIES we've seen already
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyM;FirstM
ORG:OrgI
CATEGORIES:Category 2b
END:VCARD
"
 ["FirstM" "FamilyM"
  nil
  "OrgI"
  nil
  nil
  nil
  ((mail-alias . "Category 1a,Category 1b,Category 2a,Category 2b,Category 3")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstM FamilyM")


(bbdb-vcard-test
 "
** A vcard with two other vcards inside; we check the outer one
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:OuterfirstA OuterlastA
N:OuterlastA OuterfirstA
AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastA\\;InnerfirstA\\nFN:InnerfirstA InnerlastA\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerA@hostA.com\\nEND:VCARD\\n
B.AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastB\\;InnerfirstB\\nFN:InnerfirstB InnerlastB\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerB@hostB.com\\nEND:VCARD\\n
NOTE:A note
END:VCARD
"
 ["OuterlastA" "OuterfirstA"
  ("OuterfirstA OuterlastA")
  nil
  nil
  nil
  nil
  ((b\.agent . "BEGIN:VCARD
VERSION:3.0
N:InnerlastB;InnerfirstB
FN:InnerfirstB InnerlastB
TEL:+1-919-555-1234
EMAIL;TYPE=INTERNET:InnerB@hostB.com
END:VCARD
")
   (agent . "BEGIN:VCARD
VERSION:3.0
N:InnerlastA;InnerfirstA
FN:InnerfirstA InnerlastA
TEL:+1-919-555-1234
EMAIL;TYPE=INTERNET:InnerA@hostA.com
END:VCARD
")
   (notes . "A note")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "OuterfirstA OuterlastA")


(bbdb-vcard-test
 "
** A vcard with two other vcards inside; we check the first inner one
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:OuterfirstA OuterlastA
N:OuterlastA OuterfirstA
AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastA\\;InnerfirstA\\nFN:InnerfirstA InnerlastA\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerA@hostA.com\\nEND:VCARD\\n
B.AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastB\\;InnerfirstB\\nFN:InnerfirstB InnerlastB\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerB@hostB.com\\nEND:VCARD\\n
NOTE:A note
END:VCARD
"
 ["InnerfirstA" "InnerlastA"
  nil
  nil
  (["Office" "+1-919-555-1234"])
  nil
  ("InnerA@hostA.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "InnerfirstA InnerlastA")


(bbdb-vcard-test
 "
** A vcard with two other vcards inside; we check the second inner one
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:OuterfirstA OuterlastA
N:OuterlastA OuterfirstA
AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastA\\;InnerfirstA\\nFN:InnerfirstA InnerlastA\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerA@hostA.com\\nEND:VCARD\\n
B.AGENT:BEGIN:VCARD\\nVERSION:3.0\\nN:InnerlastB\\;InnerfirstB\\nFN:InnerfirstB InnerlastB\\nTEL:+1-919-555-
 1234\\nEMAIL\\;TYPE=INTERNET:InnerB@hostB.com\\nEND:VCARD\\n
NOTE:A note
END:VCARD
"
 ["InnerfirstB" "InnerlastB"
  nil
  nil
  (["Office" "+1-919-555-1234"])
  nil
  ("InnerB@hostB.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "InnerfirstB InnerlastB")


(bbdb-vcard-test
 "
** Treatment of REV
*** Store REV as creation-date in new records...
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyJ;FirstJ
ORG:OrgJ
REV:1997-03-27T22:27:10Z
END:VCARD
"
 ["FirstJ" "FamilyJ"
  nil
  "OrgJ"
  nil
  nil
  nil
  ((creation-date . "1997-03-27") (timestamp . "2010-03-04")) ]
 "FirstJ FamilyJ"
 nil nil t)


(bbdb-vcard-test
 "
*** ...but not in existing records
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyJ;FirstJ
ORG:OrgJ
REV:1977-12-03T22:27:10Z
END:VCARD
"
 ["FirstJ" "FamilyJ"
  nil
  "OrgJ"
  nil
  nil
  nil
  ((creation-date . "1997-03-27") (timestamp . "2010-03-04")) ]
 "FirstJ FamilyJ"
 nil nil t)



(bbdb-vcard-test
 "
** Matching BDAY and N induce merge
*** Storing a new person
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyK;FirstK
ORG:CompanyK
BDAY:1927-03-27
END:VCARD
"
 ["FirstK" "FamilyK"
  nil
  "CompanyK"
  nil
  nil
  nil
  ((anniversary . "1927-03-27 birthday")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstK FamilyK")


(bbdb-vcard-test
 "
*** Not quite the same person: BDAY differs.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyK;FirstK
ORG:CompanyK2
BDAY:1937-04-28
END:VCARD
"
 ["FirstK" "FamilyK"
  nil
  "CompanyK2"
  nil
  nil
  nil
  ((anniversary . "1937-04-28 birthday")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstK FamilyK"
 "CompanyK2")


(bbdb-vcard-test
 "
*** Known person due to matching BDAY. Different ORG, though.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyK;FirstK
ORG:CompanyK1
BDAY:1927-03-27
END:VCARD
"
 ["FirstK" "FamilyK"
  nil
  "CompanyK1"
  nil
  nil
  nil
  ((anniversary . "1927-03-27 birthday")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "FirstK FamilyK"
 "CompanyK1")



(bbdb-vcard-test
 "
** Matching TEL and N induce merge
*** Storing a new person
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyL;FirstL
TEL;TYPE=work:111100001
TEL;TYPE=home:111100002
TEL:111100003
ORG:CompanyL
END:VCARD
"
 ["FirstL" "FamilyL"
  nil
  "CompanyL"
  (["Office" "111100003"]
   ["Home" "111100002"]
   ["Office" "111100001"])
  nil
  nil
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstL FamilyL")


(bbdb-vcard-test
 "
*** Not quite the same person: no matching TEL.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyL;FirstL
TEL;TYPE=work:222200001
TEL;TYPE=home:222200002
TEL:222200003
ORG:CompanyL2
END:VCARD
"
 ["FirstL" "FamilyL"
  nil
  "CompanyL2"
  (["Office" "222200003"]
   ["Home" "222200002"]
   ["Office" "222200001"])
  nil
  nil
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstL FamilyL"
 "CompanyL2")


(bbdb-vcard-test
 "
*** Known person: matching TEL (but different ORG).
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
N:FamilyL;FirstL
TEL;TYPE=work:333300001
TEL;TYPE=work:111100002
TEL:333300003
ORG:CompanyL3
END:VCARD
"
 ["FirstL" "FamilyL"
  nil
  "CompanyL3"
  (["Office" "111100001"]
   ["Home" "111100002"]
   ["Office" "111100003"]
   ["Office" "333300003"]
   ["Office" "111100002"]
   ["Office" "333300001"])
  nil
  nil
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "FirstL FamilyL"
 "CompanyL3")



(bbdb-vcard-test
 "
** From RFC 2426: author's address.  Note the omission or type N
   which is declared mandatory by this very RFC.
------------------------------------------------------------
BEGIN:vCard
VERSION:3.0
FN:Frank Dawson
ORG:Lotus Development Corporation
ADR;TYPE=WORK,POSTAL,PARCEL:;;6544 Battleford Drive
 ;Raleigh;NC;27613-3502;U.S.A.
TEL;TYPE=VOICE,MSG,WORK:+1-919-676-9515
TEL;TYPE=FAX,WORK:+1-919-676-9564
EMAIL;TYPE=INTERNET,PREF:Frank_Dawson@Lotus.com
EMAIL;TYPE=INTERNET:fdawson@earthlink.net
URL:http://home.earthlink.net/~fdawson
END:vCard
"
 ["" ""
  ("Frank Dawson")
  "Lotus Development Corporation"
  (["Office" "+1-919-676-9564"]
   ["Office" "+1-919-676-9515"])
  (["Office" ("6544 Battleford Drive") "Raleigh" "NC" "27613-3502" "U.S.A."])
  ("fdawson@earthlink.net"
   "Frank_Dawson@Lotus.com")
  ((www . "http://home.earthlink.net/~fdawson")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
"Frank Dawson")

(bbdb-vcard-test
 "
** The other author of RFC 2426
------------------------------------------------------------
BEGIN:vCard
VERSION:3.0
FN:Tim Howes
ORG:Netscape Communications Corp.
ADR;TYPE=WORK:;;501 E. Middlefield Rd.;Mountain View;
 CA; 94043;U.S.A.
TEL;TYPE=VOICE,MSG,WORK:+1-415-937-3419
TEL;TYPE=FAX,WORK:+1-415-528-4164
EMAIL;TYPE=INTERNET:howes@netscape.com
END:vCard
"
 ["" ""
  ("Tim Howes")
  "Netscape Communications Corp."
  (["Office" "+1-415-528-4164"]
   ["Office" "+1-415-937-3419"])
  (["Office" ("501 E. Middlefield Rd.") "Mountain View" "CA" " 94043" "U.S.A."])
  ("howes@netscape.com")
  ((creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "Tim Howes")