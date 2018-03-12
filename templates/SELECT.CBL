       $set ANIM
       $set ANS85
       $set AUTOLOCK
       $set BELL
       $set FILESHARE
       $set NOWARNING
       $set WRITETHROUGH
       IDENTIFICATION DIVISION.
       PROGRAM-ID SELECT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
            SELECT FILE-NAME ASSIGN "{{ DATAFILE }}"
                 ORGANIZATION IS  INDEXED
                 ACCESS IS  DYNAMIC
                 {{ KEYDEFS }}
                 FILE STATUS IS FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD FILE-NAME.
{{ FIELDS }}

       WORKING-STORAGE SECTION.
       01   FILE-STATUS                 PIC X(02) VALUE SPACES.

       LINKAGE SECTION.
       01   ARGV.
            03 FILLER                   PIC X(02).
            03 FILE-KEY                 {{ KEYPIC }}.

       PROCEDURE DIVISION USING ARGV.

       BEGIN.
            MOVE FILE-KEY TO {{ KEYNAME }}.
            PERFORM FILE-OPEN.
            PERFORM FILE-START.
            STOP RUN.

       FILE-START SECTION.
            START FILE-NAME KEY NOT LESS {{ SUPERKEYNAME }}
            IF FILE-STATUS = "00"
                 PERFORM FILE-READ.

       FILE-READ SECTION.
            READ FILE-NAME NEXT IGNORE LOCK
            IF {{ KEYNAME }} = FILE-KEY{{ SUPERKEYPART }}
                 PERFORM FILE-READ-NEXT
                      UNTIL {{ KEYNAME }} NOT EQUAL FILE-KEY{{ SUPERKEYPART }}
                      OR FILE-STATUS = "10".

       FILE-READ-NEXT SECTION.
            IF {{ KEYNAME }} EQUAL FILE-KEY{{ SUPERKEYPART }}
{{ DISPLAY }}
            ELSE
                 PERFORM FILE-CLOSE.
            READ FILE-NAME NEXT.

       FILE-OPEN SECTION.
            OPEN INPUT FILE-NAME.

       FILE-CLOSE SECTION.
            CLOSE FILE-NAME.
            STOP RUN.
