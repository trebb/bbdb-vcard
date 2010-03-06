;;  For the sake of minimality, not all test cases are rfc compliant.
;;
;;  You should probably save your production bbdb file.

(defun bbdb-vcard-test (vcard bbdb-entry
                              search-name &optional search-company search-net)
  "Import VCARD and search for it in bbdb by SEARCH-NAME, SEARCH-COMPANY,
SEARCH-NET.  If search result disagrees with BBDB-ENTRY, talk about it in
buffer bbdb-vcard-test.  In BBDB-ENTRY, both timestamp and creation-date
should be `2010-03-04'"
  (bbdb-vcard-iterate-vcards vcard 'bbdb-vcard-process-vcard)
  (let ((bbdb-search-result (car (bbdb-search (bbdb-records) search-name))))
    (setf (cdr(assoc 'creation-date (elt bbdb-search-result 7))) "2010-03-04"
          (cdr(assoc 'timestamp (elt bbdb-search-result 7))) "2010-03-04"
          (cdr(assoc 'creation-date (elt bbdb-entry 7))) "2010-03-04"
          (cdr(assoc 'timestamp (elt bbdb-entry 7))) "2010-03-04")
    (unless
        (equal (subseq bbdb-search-result 0 8)
               (subseq bbdb-entry 0 8))
      (princ "\nTest failed:\n" (get-buffer-create "bbdb-vcard-test"))
      (prin1 vcard (get-buffer-create "bbdb-vcard-test"))
      (princ "\nwas stored as\n" (get-buffer-create "bbdb-vcard-test"))
      (prin1 (subseq bbdb-search-result 0 8) (get-buffer-create "bbdb-vcard-test"))
      (princ "\nbut was expected as\n" (get-buffer-create "bbdb-vcard-test"))
      (prin1 bbdb-entry (get-buffer-create "bbdb-vcard-test")))))


;;; Try not to mess up our real BBDB:
(when (get-buffer "bbdb")
  (save-buffer "bbdb") ; hopefully, this is the bbdb buffer
  (kill-buffer "bbdb"))
(when (get-buffer "test-bbdb") (kill-buffer "test-bbdb"))
(setq bbdb-file "/tmp/test-bbdb")
(when (file-exists-p bbdb-file) (delete-file bbdb-file))
(when (get-buffer "bbdb-vcard-test") (kill-buffer "bbdb-vcard-test"))


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
TEL=home:+11111111
EMAIL:first1@provider1
MAILER:Wanderlust1
TZ:+01:00
GEO:37.386013;-122.082932
TITLE:Director\, Research and Development
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
URL:first1@host1.org
CLASS:CONFIDENTIAL
KEY:The Key No 1
X-foo:extended type 1
END:VCARD
"
 ["First1" "Last1"
  ("First1 Last1" "Firsty1")
  "Company1
Unit1
Subunit1"
  nil
  (["Office"
    ("Box111" "Room 111" "First Street,First Corner")
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
   (rev . "1995-10-31T22:27:10Z")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (categories . "category1")
   (agent . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo . "encoded logo #1")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "+01:00")
   (mailer . "Wanderlust1")
   (tel=home . "+11111111")
   (label . "Label 1")
   (photo . "The Alphabet:abcdefghijklmnopqrstuvwsyz")
   (anniversary . "1999-12-05 birthday")
   (notes . "This vcard uses every type defined in rfc2426.")
   (www . "first1@host1.org")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "First1 Last1")


(bbdb-vcard-test
 "
** The following is made of examples from rfc2426.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Mr. John Q. Public\, Esq.
N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
NICKNAME:Robbie
PHOTO;VALUE=uri:http://www.abc.com/pub/photos
 /jqpublic.gif
BDAY:1996-04-15
ADR;TYPE=dom,home,postal,parcel:;;123 Main
  Street;Any Town;CA;91921-1234
LABEL;TYPE=dom,home,postal,parcel:Mr.John Q. Public\, Esq.\n
 Mail Drop: TNE QB\n123 Main Street\nAny Town\, CA  91921-1234
 \nU.S.A.
TEL;TYPE=work,voice,pref,msg:+1-213-555-1234
EMAIL;TYPE=internet:jqpublic@xyz.dom1.com
EMAIL;TYPE=internet:jdoe@isp.net
MAILER:PigeonMail 2.1
TZ:-05:00
GEO:37.386013;-122.082932
TITLE:Director\, Research and Development
ROLE:Programmer
LOGO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
AGENT;VALUE=uri:
 CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com
ORG:ABC\, Inc.;North American Division;Marketing
CATEGORIES:TRAVEL AGENT
NOTE:This fax number is operational 0800 to 1715
  EST\, Mon-Fri.
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
  (["Dom,Home,Postal,Parcel"
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
   (rev . "1995-10-31T22:27:10Z")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (categories . "TRAVEL AGENT")
   (agent\;value=uri
    . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo\;encoding=b\;type=jpeg
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "-05:00")
   (mailer . "PigeonMail 2.1")
   (label\;type=dom\,home\,postal\,parcel . "Mr.John Q. Public, Esq.")
   (photo\;value=uri . "http://www.abc.com/pub/photos/jqpublic.gif")
   (anniversary . "1996-04-15 birthday")
   (notes . "This fax number is operational 0800 to 1715 EST, Mon-Fri.")
   (www . "http://www.swbyps.restaurant.french/~chezchic.html")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04")) ]
 "John")


(bbdb-vcard-test
 "
** Exactly the same as before.
   Re-reading it shouldn't duplicate anything but the NOTE.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Mr. John Q. Public\, Esq.
N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
NICKNAME:Robbie
PHOTO;VALUE=uri:http://www.abc.com/pub/photos
 /jqpublic.gif
BDAY:1996-04-15
ADR;TYPE=dom,home,postal,parcel:;;123 Main
  Street;Any Town;CA;91921-1234
LABEL;TYPE=dom,home,postal,parcel:Mr.John Q. Public\, Esq.\n
 Mail Drop: TNE QB\n123 Main Street\nAny Town\, CA  91921-1234
 \nU.S.A.
TEL;TYPE=work,voice,pref,msg:+1-213-555-1234
EMAIL;TYPE=internet:jqpublic@xyz.dom1.com
EMAIL;TYPE=internet:jdoe@isp.net
MAILER:PigeonMail 2.1
TZ:-05:00
GEO:37.386013;-122.082932
TITLE:Director\, Research and Development
ROLE:Programmer
LOGO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
AGENT;VALUE=uri:
 CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com
ORG:ABC\, Inc.;North American Division;Marketing
CATEGORIES:TRAVEL AGENT
NOTE:This fax number is operational 0800 to 1715
  EST\, Mon-Fri.
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
  (["Dom,Home,Postal,Parcel"
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
   (rev . "1995-10-31T22:27:10Z")
   (prodid . "-//ONLINE DIRECTORY//NONSGML Version 1//EN")
   (categories . "TRAVEL AGENT")
   (agent\;value=uri . "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
   (logo\;encoding=b\;type=jpeg
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (role . "Programmer")
   (title . "Director, Research and Development")
   (geo . "37.386013;-122.082932")
   (tz . "-05:00")
   (mailer . "PigeonMail 2.1")
   (label\;type=dom\,home\,postal\,parcel . "Mr.John Q. Public, Esq.")
   (photo\;value=uri . "http://www.abc.com/pub/photos/jqpublic.gif")
   (anniversary . "1996-04-15 birthday")
   (vcard-notes . "This fax number is operational 0800 to 1715 EST, Mon-Fri.")
   (www . "http://www.swbyps.restaurant.french/~chezchic.html")
   (notes . "This fax number is operational 0800 to 1715 EST, Mon-Fri.")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "John")


(bbdb-vcard-test
 "
** More example entries from rfc2426.
------------------------------------------------------------
BEGIN:VCARD
VERSION:3.0
FN:Mr. John Q. Public\, Esq.
N:Public;John;Quinlan;Mr.;Esq.
NICKNAME:Jim,Jimmie
PHOTO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
 AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
 ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0
BDAY:1987-09-27T08:30:00-06:00
EMAIL;TYPE=internet,pref:jane_doe@abc.com
TZ;VALUE=text:-05:00; EST; Raleigh/North America
LOGO;VALUE=uri:http://www.abc.com/pub/logos/abccorp.jpg
AGENT:BEGIN:VCARD\nFN:Susan Thomas\nTEL:+1-919-555-
 1234\nEMAIL\;INTERNET:sthomas@host.com\nEND:VCARD\n
CATEGORIES:INTERNET,IETF,INDUSTRY,INFORMATION TECHNOLOGY
REV:1997-11-15
SOUND;TYPE=BASIC;VALUE=uri:CID:JOHNQPUBLIC.part8.
 19960229T080000.xyzMail@host1.com
CLASS:PRIVATE
NOTE:A note
END:VCARD
"
 ["Mr. John Quinlan" "Public Esq."
  ("Mr. John Q. Public, Esq." "Susan Thomas" "Jim" "Jimmie")
  nil
  (["Office" "+1-919-555-1234"])
  nil
  ("sthomas@host.com" "jane_doe@abc.com")
  ((agent . "BEGIN:VCARD")
   (logo\;value=uri . "http://www.abc.com/pub/logos/abccorp.jpg")
   (tz\;value=text . "-05:00; EST; Raleigh/North America")
   (photo\;encoding=b\;type=jpeg
    . "MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcNAQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bmljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0")
   (anniversary . "1987-09-27T08:30:00-06:00 birthday")
   (creation-date . "2010-03-04") (timestamp . "2010-03-04"))]
 "Public")



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
   "Effy Bee"
   "FirstB FamilyB")
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
