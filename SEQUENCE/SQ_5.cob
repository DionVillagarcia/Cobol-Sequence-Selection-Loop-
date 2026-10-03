       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N1_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SALESMAN_NUMBER  PIC 9(11).
       01 SALESMAN_NAME    PIC A(20).
       01 UNIT_SOLD        PIC 9(3).
       01 UNIT_PRICE       PIC 9(6)V99.
       01 TOTAL_SALES      PIC 9(6)V99.

       01 DISP-TS          PIC ZZZZZZ9.99.

       PROCEDURE DIVISION.
           DISPLAY "What is the Salesman number? "
           ACCEPT SALESMAN_NUMBER.
           DISPLAY "What is the Salesman name? "
           ACCEPT SALESMAN_NAME.
           DISPLAY "Unit Sold?"
           ACCEPT UNIT_SOLD.
           DISPLAY "Unit Price? "
           ACCEPT UNIT_PRICE.

           COMPUTE TOTAL_SALES = UNIT_SOLD * UNIT_PRICE.

           MOVE TOTAL_SALES TO DISP-TS.

           DISPLAY FUNCTION TRIM(SALESMAN_NAME)
                   " Total Sales is: "
                   FUNCTION TRIM(DISP-TS).

           STOP RUN.