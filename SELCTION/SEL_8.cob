       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-COMMISSION.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TOTAL-SALES  PIC 9(10)V99.
       01 COMMISSION   PIC 9(10)V99.
       01 DISP-TS      PIC ZZZZZZZZZ9.99.
       01 DISP-CN      PIC ZZZZZZZZZ9.99.


       PROCEDURE DIVISION.

           DISPLAY "Input Total Sales  : " WITH NO ADVANCING
           ACCEPT TOTAL-SALES.

           IF TOTAL-SALES <= 15000
              COMPUTE COMMISSION = TOTAL-SALES * 0.15
              DISPLAY "Commission base is .15"

           ELSE
              IF TOTAL-SALES <= 20000
                 COMPUTE COMMISSION = TOTAL-SALES * 0.20
                 DISPLAY "Commission base is .20"

              ELSE
                 IF TOTAL-SALES <= 25000
                    COMPUTE COMMISSION = TOTAL-SALES * 0.25
                    DISPLAY "Commission base is .25"

                 ELSE
                    IF TOTAL-SALES <= 30000
                       COMPUTE COMMISSION = TOTAL-SALES * 0.30
                       DISPLAY "Commission base is .30"

                    ELSE
                       COMPUTE COMMISSION = TOTAL-SALES * 0.40
                       DISPLAY "Commission base is .40"

                    END-IF.
           
           MOVE TOTAL-SALES TO DISP-TS.
           MOVE COMMISSION TO DISP-CN.
           DISPLAY "Total Sales is     : " FUNCTION TRIM(DISP-TS).
           DISPLAY "Sales Commission is: " FUNCTION TRIM(DISP-CN).

           STOP RUN.