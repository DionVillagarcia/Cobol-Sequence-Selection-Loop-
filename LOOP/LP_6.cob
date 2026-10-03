       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTORIAL.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMBER1    PIC 99.
       01 FACTORIAL  PIC 9(4) VALUE 1.
       01 COUNTER    PIC 99   VALUE 1.

       01 DISP-FAC   PIC ZZZ9.
       01 DISP-NUM   PIC Z9.

       PROCEDURE DIVISION.

           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           PERFORM UNTIL COUNTER > NUMBER1 
                   COMPUTE FACTORIAL = FACTORIAL * COUNTER
                   ADD 1 TO COUNTER
           END-PERFORM
           
           MOVE FACTORIAL TO DISP-FAC.
           MOVE NUMBER1 TO DISP-NUM
           DISPLAY "Factorial of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-FAC).
        
           STOP RUN.