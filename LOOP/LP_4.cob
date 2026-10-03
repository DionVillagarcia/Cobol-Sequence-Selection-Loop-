       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP_EVEN_RANGE.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LOW       PIC 99.
       01 HIGH      PIC 99.

       01 DISP-LOW  PIC Z9.

       PROCEDURE DIVISION.
        
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT LOW.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT HIGH.
           

           PERFORM UNTIL LOW > HIGH
                   IF FUNCTION MOD(LOW, 2) = 0
                      MOVE LOW TO DISP-LOW 
                      DISPLAY FUNCTION TRIM(DISP-LOW)
                   END-IF
                   ADD 1 TO LOW

           END-PERFORM.

           STOP RUN.