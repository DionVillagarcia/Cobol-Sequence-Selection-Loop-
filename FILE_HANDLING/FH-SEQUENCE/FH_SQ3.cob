       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ3.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE3
              ASSIGN TO "Sequence3.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQUENCE3.
       01  REC            PIC X(40).

       WORKING-STORAGE SECTION.
       01 N1         PIC 9(2).
       01 N2         PIC 9(2).

       01 SUMOF      PIC 9(3).
       01 DIFFOF     PIC 9(3).
       01 PRODOF     PIC 9(3).
       01 QUOOF      PIC 9(3)V99.

       01 DISP-N1    PIC ZZ9.
       01 DISP-N2    PIC ZZ9.
       01 DISP-SUM   PIC ZZZ9.
       01 DISP-DIFF  PIC ZZ9.
       01 DISP-PROD  PIC ZZ9.
       01 DISP-QUO   PIC ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT SEQUENCE3.

           MOVE 6 TO N1.
           MOVE 7 TO N2.
           ADD N1 TO N2 GIVING SUMOF.
           SUBTRACT N1 FROM N2 GIVING DIFFOF.
           MULTIPLY N1 BY N2 GIVING PRODOF.
           DIVIDE N1 BY N2 GIVING QUOOF.

           MOVE N1 TO DISP-N1.
           MOVE N2 TO DISP-N2.
           MOVE SUMOF TO DISP-SUM.
           MOVE DIFFOF TO DISP-DIFF.
           MOVE PRODOF TO DISP-PROD.
           MOVE QUOOF TO DISP-QUO.

           DISPLAY "The Sum of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-SUM).

           DISPLAY "The Difference of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-DIFF).

           DISPLAY "The Product of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-PROD).

           DISPLAY "The Quotient of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-QUO).

           MOVE SPACES TO REC.
           STRING "The Sum of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-SUM)
                   DELIMITED BY SIZE INTO REC.
           WRITE REC.

           MOVE SPACES TO REC.
           STRING "The Difference of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-DIFF)
                   DELIMITED BY SIZE INTO REC.
           WRITE REC.

           MOVE SPACES TO REC.
           STRING "The Product of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-PROD)
                   DELIMITED BY SIZE INTO REC.
           WRITE REC.

           MOVE SPACES TO REC.
           STRING "The Quotient of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-QUO)
                   DELIMITED BY SIZE INTO REC.
           WRITE REC.

           CLOSE SEQUENCE3.

           STOP RUN.