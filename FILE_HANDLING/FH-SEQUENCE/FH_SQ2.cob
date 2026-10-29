       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ2.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE2
              ASSIGN TO "Sequence2.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SEQUENCE2.
       01  REC            PIC X(40).

       WORKING-STORAGE SECTION.
       01  A     PIC ZZ9.
       01  B     PIC ZZ9.
       01  TEMP  PIC 9(2).

       PROCEDURE DIVISION.
           OPEN OUTPUT SEQUENCE2.
           MOVE "BEFORE" TO REC
           WRITE REC.

           MOVE 10 TO A.
           MOVE A TO REC.
           MOVE FUNCTION TRIM(A) TO REC
           WRITE REC.

           MOVE 5 TO B.
           MOVE B TO REC.
           MOVE FUNCTION TRIM(B) TO REC
           WRITE REC.

           MOVE A TO TEMP.
           MOVE B TO A.
           MOVE TEMP TO B.
           
           MOVE "AFTER" TO REC.
           WRITE REC.

           MOVE A TO REC.
           MOVE FUNCTION TRIM(A) TO REC
           WRITE REC.

           MOVE B TO REC.
           MOVE FUNCTION TRIM(B) TO REC
           WRITE REC.

           CLOSE SEQUENCE2

           STOP RUN.
