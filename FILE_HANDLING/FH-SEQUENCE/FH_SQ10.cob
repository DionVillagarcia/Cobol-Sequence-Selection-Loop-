       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1-N10-VILLAGARCIA.
       AUTHOR. DION-VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRIANGLE-FILE
               ASSIGN TO "Triangle.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRIANGLE-FILE.
       01  TRIANGLE-REC     PIC X(60).

       WORKING-STORAGE SECTION.
       01 BASE            PIC 9(3)V99.
       01 HEIGHT          PIC 9(3)V99.
       01 SIDE1           PIC 9(5)V99.
       01 SIDE2           PIC 9(5)V99.
       01 SIDE3           PIC 9(5)V99.
       01 AREA1           PIC 9(6)V99.
       01 PERIMETER       PIC 9(6)V99.

       01 DISP-BASE       PIC ZZ9.99.
       01 DISP-HEIGHT     PIC ZZ9.99.
       01 DISP-SIDE1      PIC Z(4)9.99.
       01 DISP-SIDE2      PIC Z(4)9.99.
       01 DISP-SIDE3      PIC Z(4)9.99.
       01 DISP-AREA1      PIC Z(5)9.99.
       01 DISP-PERIMETER  PIC Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input Base: " WITH NO ADVANCING
           ACCEPT BASE.
           DISPLAY "Input Height: " WITH NO ADVANCING
           ACCEPT HEIGHT.
           DISPLAY "Input Side 1: " WITH NO ADVANCING
           ACCEPT SIDE1.
           DISPLAY "Input Side 2: " WITH NO ADVANCING
           ACCEPT SIDE2.
           DISPLAY "Input Side 3: " WITH NO ADVANCING
           ACCEPT SIDE3.

           COMPUTE AREA1 = 1 / 2 * BASE * HEIGHT.
           COMPUTE PERIMETER = SIDE1 + SIDE2 + SIDE3.

           MOVE BASE TO DISP-BASE.
           MOVE HEIGHT TO DISP-HEIGHT.
           MOVE SIDE1 TO DISP-SIDE1.
           MOVE SIDE2 TO DISP-SIDE2.
           MOVE SIDE3 TO DISP-SIDE3.
           MOVE AREA1 TO DISP-AREA1.
           MOVE PERIMETER TO DISP-PERIMETER.

           DISPLAY "The Triangle area is     : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The Triangle perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER).

           OPEN OUTPUT TRIANGLE-FILE.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "Base                     : "
                   FUNCTION TRIM(DISP-BASE)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "Height                   : "
                   FUNCTION TRIM(DISP-HEIGHT)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "Side 1                   : "
                   FUNCTION TRIM(DISP-SIDE1)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "Side 2                   : "
                   FUNCTION TRIM(DISP-SIDE2)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "Side 3                   : "
                   FUNCTION TRIM(DISP-SIDE3)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "The Triangle area is     : "
                   FUNCTION TRIM(DISP-AREA1)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           MOVE SPACES TO TRIANGLE-REC.
           STRING "The Triangle perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER)
                   DELIMITED BY SIZE INTO TRIANGLE-REC.
           WRITE TRIANGLE-REC.

           CLOSE TRIANGLE-FILE.

           STOP RUN.
       END PROGRAM F1-N10-VILLAGARCIA.