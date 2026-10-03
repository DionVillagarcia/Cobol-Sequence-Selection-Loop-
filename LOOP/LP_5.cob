       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP_SUM_ODD.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LOW           PIC 99.
       01 HIGH          PIC 99.
       01 SUM1          PIC 999 VALUE 0.
       01 COUNTER       PIC 99.

       01 DISP-SUM      PIC ZZ9.
       01 DISP-COUNTER  PIC ZZ9.

       PROCEDURE DIVISION.
        
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT LOW.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT HIGH.
           
           DISPLAY "The sum of odd numbers: " WITH NO ADVANCING
           PERFORM UNTIL COUNTER > HIGH
                   IF FUNCTION MOD(COUNTER, 2) = 1
                      COMPUTE SUM1 = SUM1 + COUNTER
                      MOVE COUNTER TO DISP-COUNTER
                      DISPLAY FUNCTION TRIM(DISP-COUNTER)
                              " " WITH NO ADVANCING
                   END-IF
                   ADD 1 TO COUNTER 
           END-PERFORM.
           
           MOVE SUM1 TO DISP-SUM.
           DISPLAY "is: "
                   FUNCTION TRIM(DISP-SUM).

           STOP RUN.