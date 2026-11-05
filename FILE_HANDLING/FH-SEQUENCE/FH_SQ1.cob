       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ1.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
      * 
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE1
              ASSIGN TO "exercise.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SEQUENCE1
           LABEL RECORD IS STANDARD
           DATA RECORD IS REC.

       01  REC            PIC X(40).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
           OPEN EXTEND SEQUENCE1.
           MOVE "DION" TO REC.
           WRITE REC.

           MOVE "DION" TO REC.
           WRITE REC.

           MOVE "DION" TO REC.
           WRITE REC.

           MOVE "DION" TO REC.
           WRITE REC.

           MOVE "DION" TO REC.
           WRITE REC.

           CLOSE SEQUENCE1

           STOP RUN.
