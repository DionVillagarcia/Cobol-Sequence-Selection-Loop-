       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLICATION.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE1          PIC 99.
       01 COUNTER         PIC 99 VALUE 1.
       01 MULTIPLAYER     PIC 99 VALUE 0.
       01 PRODUCT         PIC 9(4).

       01 DISP-P         PIC ZZZ9.
       01 DISP-C         PIC Z9.
       01 DISP-M         PIC ZZ9.

       PROCEDURE DIVISION.

           DISPLAY "Input how many table: " WITH NO ADVANCING
           ACCEPT TABLE1

           PERFORM UNTIL COUNTER > TABLE1
              MOVE 1 TO MULTIPLAYER
              DISPLAY " "
                 PERFORM UNTIL MULTIPLAYER > 10   
                    COMPUTE PRODUCT = COUNTER * MULTIPLAYER  
                    MOVE PRODUCT TO DISP-P
                    MOVE COUNTER TO DISP-C
                    MOVE MULTIPLAYER TO DISP-M
                    DISPLAY FUNCTION TRIM(DISP-C) " * " 
                    DISP-M "  =  " 
                    FUNCTION TRIM(DISP-P)
                    ADD 1 TO MULTIPLAYER
                 END-PERFORM
              ADD 1 TO COUNTER
           END-PERFORM

           STOP RUN.