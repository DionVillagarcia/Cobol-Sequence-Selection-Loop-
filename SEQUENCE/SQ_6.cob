       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N6_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PRELIM    PIC 9(3).
       01 MIDTERMS  PIC 9(3).
       01 FINALS    PIC 9(3).
       01 SUMOF     PIC 9(3).
       01 AVE       PIC 9(2)V99.

       PROCEDURE DIVISION.
           DISPLAY "Prelim Grade? "
           ACCEPT PRELIM.
           DISPLAY "Midterms Grade? "
           ACCEPT MIDTERMS.
           DISPLAY "Finals Grade? "
           ACCEPT FINALS.

           COMPUTE SUMOF = PRELIM + MIDTERMS + FINALS.

           COMPUTE AVE = SUMOF / 3.

           DISPLAY "The overall Average is: " AVE.

           STOP RUN.