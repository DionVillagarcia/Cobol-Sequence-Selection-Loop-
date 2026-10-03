       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSITIVE-NEGATIVE.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.
       

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1       PIC S9(10).
       01 DISP-NUM1  PIC -ZZZZZZZZZ9.

       PROCEDURE DIVISION.

           DISPLAY "Enter a Number: " WITH NO ADVANCING
           ACCEPT NUM1.

           MOVE NUM1 TO DISP-NUM1.
    
           IF NUM1 > 0
              DISPLAY FUNCTION TRIM(DISP-NUM1) " is positive."

           ELSE
              IF NUM1 < 0
                 DISPLAY FUNCTION TRIM(DISP-NUM1) " is negative."

              ELSE
                 IF NUM1 = 0
                    DISPLAY FUNCTION TRIM(DISP-NUM1) " is neutral."

                 END-IF.
    
           STOP RUN.