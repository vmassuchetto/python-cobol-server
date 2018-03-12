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
            03 FILLER-KEY               PIC X(02).
            03 FILE-KEY                 {{ KEYPIC }}.
            03 FILLER-VALUES            PIC X(01).
{{ ARGS }}

       PROCEDURE DIVISION USING ARGV.

       BEGIN.
            MOVE FILE-KEY TO {{ KEYNAME }}.
            PERFORM FILE-OPEN.
            PERFORM FILE-WRITE.
            STOP RUN.

       FILE-WRITE SECTION.
            READ FILE-NAME IGNORE LOCK
            IF FILE-STATUS = "00"
                AND {{ KEYNAME }} = FILE-KEY
                PERFORM FILE-REWRITE
            ELSE IF FILE-STATUS = "9D"
                DISPLAY "ERRO: REGISTRO BLOQUEADO."
            ELSE
                DISPLAY "ERRO: REGISTRO NAO ENCONTRADO.".

       FILE-REWRITE SECTION.
{{ MOVE }}
            REWRITE {{ MAINRECORD }}.
            PERFORM FILE-CLOSE.

       FILE-OPEN SECTION.
            OPEN I-O FILE-NAME.

       FILE-CLOSE SECTION.
            CLOSE FILE-NAME.
            STOP RUN.
