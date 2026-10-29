       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1-N7-VILLAGARCIA.
       AUTHOR. DION-VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POWER-FILE
               ASSIGN TO "Power.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  POWER-FILE.
       01  POWER-REC        PIC X(60).

       WORKING-STORAGE SECTION.
       01 NUM           PIC 9(5).
       01 SQUARED       PIC 9(10).
       01 CUBED         PIC 9(15).

       01 DISP-NUM      PIC ZZZZ9.
       01 DISP-SQUARED  PIC Z(9)9.
       01 DISP-CUBED    PIC Z(14)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT NUM.

           COMPUTE SQUARED = NUM * NUM.
           COMPUTE CUBED = NUM * NUM * NUM.

           MOVE NUM TO DISP-NUM.
           MOVE SQUARED TO DISP-SQUARED.
           MOVE CUBED TO DISP-CUBED.

           DISPLAY "The Squared of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-SQUARED).
           DISPLAY "The Cubed   of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-CUBED).

           OPEN OUTPUT POWER-FILE.

           MOVE SPACES TO POWER-REC.
           STRING "The Squared of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-SQUARED)
                   DELIMITED BY SIZE INTO POWER-REC.
           WRITE POWER-REC.

           MOVE SPACES TO POWER-REC.
           STRING "The Cubed   of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-CUBED)
                   DELIMITED BY SIZE INTO POWER-REC.
           WRITE POWER-REC.

           CLOSE POWER-FILE.

           STOP RUN.
       END PROGRAM F1-N7-VILLAGARCIA.