       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1-N9-VILLAGARCIA.
       AUTHOR. DION-VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIRCLE-FILE
               ASSIGN TO "Circle.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CIRCLE-FILE.
       01  CIRCLE-REC           PIC X(60).

       WORKING-STORAGE SECTION.
       01 DIAMETER            PIC 9(3)V99.
       01 CIRCUMFERENCE       PIC 9(4)V99.
       01 AREA1               PIC 9(6)V99.

       01 DISP-DIAMETER       PIC ZZ9.99.
       01 DISP-AREA1          PIC Z(5)9.99.
       01 DISP-CIRCUMFERENCE  PIC Z(3)9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input Diameter: " WITH NO ADVANCING
           ACCEPT DIAMETER.

           COMPUTE AREA1 = 3.1416 * (DIAMETER / 2) ** 2.
           COMPUTE CIRCUMFERENCE = 3.1416 * DIAMETER.

           MOVE DIAMETER TO DISP-DIAMETER.
           MOVE AREA1 TO DISP-AREA1.
           MOVE CIRCUMFERENCE TO DISP-CIRCUMFERENCE.

           DISPLAY "The Circle area is         : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The Circle Circumference is: "
                   FUNCTION TRIM(DISP-CIRCUMFERENCE).

           OPEN OUTPUT CIRCLE-FILE.

           MOVE SPACES TO CIRCLE-REC.
           STRING "Diameter                   : "
                   FUNCTION TRIM(DISP-DIAMETER)
                   DELIMITED BY SIZE INTO CIRCLE-REC.
           WRITE CIRCLE-REC.

           MOVE SPACES TO CIRCLE-REC.
           STRING "The Circle area is         : "
                   FUNCTION TRIM(DISP-AREA1)
                   DELIMITED BY SIZE INTO CIRCLE-REC.
           WRITE CIRCLE-REC.

           MOVE SPACES TO CIRCLE-REC.
           STRING "The Circle Circumference is: "
                   FUNCTION TRIM(DISP-CIRCUMFERENCE)
                   DELIMITED BY SIZE INTO CIRCLE-REC.
           WRITE CIRCLE-REC.

           CLOSE CIRCLE-FILE.

           STOP RUN.
       END PROGRAM F1-N9-VILLAGARCIA.