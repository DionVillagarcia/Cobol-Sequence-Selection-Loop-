       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADES.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 PRELIM    PIC 9(5)V99.
       01 MIDTERM   PIC 9(5)V99.
       01 FINALS    PIC 9(5)V99.
       01 AVERAGE   PIC 9(5)V99.

       01 DISP-AVE  PIC ZZZZ9.99.

       PROCEDURE DIVISION.
           DISPLAY "Enter PRELIM Grade: " WITH NO ADVANCING
           ACCEPT PRELIM.

           DISPLAY "Enter MIDTERM Grade: " WITH NO ADVANCING
           ACCEPT MIDTERM.

           DISPLAY "Enter FINALS Grade: " WITH NO ADVANCING
           ACCEPT FINALS.

           COMPUTE AVERAGE =(PRELIM + MIDTERM + FINALS) / 3.

           MOVE AVERAGE TO DISP-AVE

           DISPLAY "Average Grade: " FUNCTION TRIM(DISP-AVE).

           IF
              AVERAGE >= 75
              DISPLAY "PASSED!"

           ELSE
              DISPLAY "FAILED"
           END-IF.

           STOP RUN.