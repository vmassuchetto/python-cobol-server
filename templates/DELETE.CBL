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
            PERFORM FILE-WRITE.
            STOP RUN.

       FILE-WRITE SECTION.
            DELETE FILE-NAME.
            IF FILE-STATUS NOT = "00"
                DISPLAY "ERRO: DELECAO FALHOU. " WITH NO ADVANCING
                DISPLAY "FILE-STATUS " WITH NO ADVANCING
                DISPLAY FILE-STATUS WITH NO ADVANCING
                DISPLAY ".".
            PERFORM FILE-CLOSE.

       FILE-OPEN SECTION.
            OPEN I-O FILE-NAME.

       FILE-CLOSE SECTION.
            CLOSE FILE-NAME.
            STOP RUN.
