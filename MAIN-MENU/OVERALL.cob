       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERALL-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 CHOICE   PIC 99.
       01 KEY1     PIC X.
       01 COMMAND  PIC X(5) VALUE "clear".

       PROCEDURE DIVISION.

       MAIN-MENU-OVERALL.
           
           PERFORM UNTIL CHOICE = 4
                   PERFORM CLEAR-SCREEN
                   DISPLAY "OVERALL MAIN MENU"
                   DISPLAY SPACE
                   DISPLAY "1 - SEQUENCE MAIN MENU"
                   DISPLAY "2 - SELECTION MAIN MENU"
                   DISPLAY "3 - LOOP MAIN MENU"
                   DISPLAY "4 - EXIT"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT CHOICE

                   EVALUATE CHOICE
                   WHEN 1 
                        PERFORM SEQUENCE-MENU
                   WHEN 2
                        PERFORM SELECTION-MENU
                   WHEN 3
                        PERFORM LOOP-MENU
                   WHEN 4
                        CONTINUE
                   WHEN OTHER
                        DISPLAY "INVALID CHOICE!"
                        PERFORM PAUSE-KEY

                   END-EVALUATE
           END-PERFORM.

       SEQUENCE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Sequence check"
           PERFORM PAUSE-KEY.

       SELECTION-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Selection check"
           PERFORM PAUSE-KEY.

       LOOP-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Loop check"
           PERFORM PAUSE-KEY.
       
       PAUSE-KEY.
           DISPLAY "Input any any key to continue..."
           ACCEPT KEY1. 

       CLEAR-SCREEN.
           CALL "SYSTEM" USING COMMAND.