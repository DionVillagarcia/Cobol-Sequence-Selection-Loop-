       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ4.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE4
              ASSIGN TO "Sequence4.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  SEQUENCE4.
       01  REC            PIC X(40).

       WORKING-STORAGE SECTION.
       01  F              PIC 9(5)V99.
       01  CELSIUS        PIC 9(5)V99.
       01  CELSIUS-DISP   PIC ZZZZ9.99.
       01  F-DISP         PIC ZZZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN OUTPUT SEQUENCE4.

           DISPLAY "Input Celsius: " WITH NO ADVANCING.
           ACCEPT CELSIUS.

           COMPUTE F = CELSIUS * 9 / 5 + 32.

           MOVE CELSIUS TO CELSIUS-DISP.
           MOVE F TO F-DISP.

           DISPLAY "The converted Fahrenheit of "
                   FUNCTION TRIM(CELSIUS-DISP)
                   " is "
                   FUNCTION TRIM(F-DISP).

           MOVE SPACES TO REC.

           STRING "The converted Fahrenheit of "
                  FUNCTION TRIM(CELSIUS-DISP)
                  " is "
                  FUNCTION TRIM(F-DISP)
                  DELIMITED BY SIZE
                  INTO REC.

           WRITE REC.

           CLOSE SEQUENCE4.

           STOP RUN.