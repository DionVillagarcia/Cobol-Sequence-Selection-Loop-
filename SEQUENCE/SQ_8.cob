       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N8_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 LENGTH1         PIC 9(3).
       01 WIDTH           PIC 9(3).
       01 AREA1           PIC 9(5).
       01 PERIMETER       PIC 9(5).

       01 DISP-AREA1      PIC ZZZZ9.
       01 DISP-PERIMETER  PIC ZZZZ9.

       PROCEDURE DIVISION.
           DISPLAY "Input Length: "
           ACCEPT LENGTH1.
           DISPLAY "Input Width: "
           ACCEPT WIDTH.

           COMPUTE AREA1 = LENGTH1 * WIDTH.
           COMPUTE PERIMETER = 2 *(LENGTH1 + WIDTH).

           MOVE AREA1 TO DISP-AREA1.
           MOVE PERIMETER TO DISP-PERIMETER.

           DISPLAY "The object area is     : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The object Perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER).

           STOP RUN.