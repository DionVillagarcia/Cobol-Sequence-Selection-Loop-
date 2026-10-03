       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP_NUMBERS1-5.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMBER1   PIC 99.
       01 COUNTER   PIC 99.

       01 DISP-NUM  PIC Z9.

       PROCEDURE DIVISION.

           MOVE 1 TO COUNTER.

           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           MOVE NUMBER1 TO DISP-NUM
           
           PERFORM UNTIL COUNTER > NUMBER1
                   IF FUNCTION MOD(COUNTER 2) = 0
                      MOVE COUNTER TO DISP-NUM
                      DISPLAY FUNCTION TRIM(DISP-NUM)
                   END-IF
                   ADD 1 TO COUNTER
                 
           END-PERFORM.

           STOP RUN.