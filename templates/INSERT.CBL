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
            03 ARGFILLER                PIC X(02).
{{ ARGS }}

       PROCEDURE DIVISION USING ARGV.

       BEGIN.
            PERFORM FILE-OPEN.
            PERFORM FILE-WRITE.
            STOP RUN.

       FILE-WRITE SECTION.
{{ MOVE }}
            WRITE {{ MAINRECORD }}.
            IF FILE-STATUS = "22"
                DISPLAY "ERRO: REGISTRO EXISTENTE."
			ELSE IF FILE-STATUS = "02"
				PERFORM FILE-CLOSE
            ELSE IF FILE-STATUS NOT = "00"
                DISPLAY "ERRO: INSERCAO FALHOU. " WITH NO ADVANCING
                DISPLAY "FILE-STATUS " WITH NO ADVANCING
                DISPLAY FILE-STATUS WITH NO ADVANCING
                DISPLAY ".".
            PERFORM FILE-CLOSE.

       FILE-OPEN SECTION.
            OPEN I-O FILE-NAME.

       FILE-CLOSE SECTION.
            CLOSE FILE-NAME.
            STOP RUN.
