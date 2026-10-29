       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQUENCE5.
       AUTHOR. DION-VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE
               ASSIGN TO "Sequence5.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE.
       01  SALES-REC        PIC X(60).

       WORKING-STORAGE SECTION.
       01 SALESMAN-NUMBER  PIC 9(11).
       01 SALESMAN-NAME    PIC A(20).
       01 UNIT-SOLD        PIC 9(3).
       01 UNIT-PRICE       PIC 9(6)V99.
       01 TOTAL-SALES      PIC 9(6)V99.

       01 DISP-NUM         PIC Z(10)9.
       01 DISP-SOLD        PIC ZZ9.
       01 DISP-PRICE       PIC ZZZZZ9.99.
       01 DISP-TS          PIC ZZZZZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "What is the Salesman number? " WITH NO ADVANCING
           ACCEPT SALESMAN-NUMBER.
           DISPLAY "What is the Salesman name? " WITH NO ADVANCING
           ACCEPT SALESMAN-NAME.
           DISPLAY "Unit Sold? " WITH NO ADVANCING
           ACCEPT UNIT-SOLD.
           DISPLAY "Unit Price? " WITH NO ADVANCING
           ACCEPT UNIT-PRICE.

           COMPUTE TOTAL-SALES = UNIT-SOLD * UNIT-PRICE.

           MOVE SALESMAN-NUMBER TO DISP-NUM.
           MOVE UNIT-SOLD TO DISP-SOLD.
           MOVE UNIT-PRICE TO DISP-PRICE.
           MOVE TOTAL-SALES TO DISP-TS.

           DISPLAY FUNCTION TRIM(SALESMAN-NAME)
                   " Total Sales is: "
                   FUNCTION TRIM(DISP-TS).

           OPEN OUTPUT SALES-FILE.

           MOVE SPACES TO SALES-REC.
           STRING "Salesman Number: " FUNCTION TRIM(DISP-NUM)
               DELIMITED BY SIZE INTO SALES-REC.
           WRITE SALES-REC.

           MOVE SPACES TO SALES-REC.
           STRING "Salesman Name:   " FUNCTION TRIM(SALESMAN-NAME)
               DELIMITED BY SIZE INTO SALES-REC.
           WRITE SALES-REC.

           MOVE SPACES TO SALES-REC.
           STRING "Units Sold:      " FUNCTION TRIM(DISP-SOLD)
               DELIMITED BY SIZE INTO SALES-REC.
           WRITE SALES-REC.

           MOVE SPACES TO SALES-REC.
           STRING "Unit Price:      " FUNCTION TRIM(DISP-PRICE)
               DELIMITED BY SIZE INTO SALES-REC.
           WRITE SALES-REC.

           MOVE SPACES TO SALES-REC.
           STRING "Total Sales:     " FUNCTION TRIM(DISP-TS)
               DELIMITED BY SIZE INTO SALES-REC.
           WRITE SALES-REC.

           CLOSE SALES-FILE.

           STOP RUN.
