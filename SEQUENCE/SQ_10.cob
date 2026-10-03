       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N10_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BASE            PIC 9(3)V99.
       01 HEIGHT          PIC 9(3)V99.
       01 SIDE1           PIC 9(5)V99.
       01 SIDE2           PIC 9(5)V99.
       01 SIDE3           PIC 9(5)V99.
       01 AREA1           PIC 9(5)V99.
       01 PERIMETER       PIC 9(5)V99.

       01 DISP-AREA1      PIC ZZZZ9.99.
       01 DISP-PERIMETER  PIC ZZZZ9.99.

       PROCEDURE DIVISION.
           DISPLAY "Input Base: "
           ACCEPT BASE.
           DISPLAY "Input Height: "
           ACCEPT HEIGHT.
           DISPLAY "Input Side 1: "
           ACCEPT SIDE1.
           DISPLAY "Input Side 2: "
           ACCEPT SIDE2.
           DISPLAY "Input Side 3: "
           ACCEPT SIDE3.

           COMPUTE AREA1 = 1 / 2 * BASE * HEIGHT.
           COMPUTE PERIMETER = SIDE1 + SIDE2 + SIDE3.

           MOVE AREA1 TO DISP-AREA1.
           MOVE PERIMETER TO DISP-PERIMETER.

           DISPLAY "The Triangle area is     : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The Triangle perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER).
           STOP RUN.