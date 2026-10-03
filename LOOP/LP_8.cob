       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUMBER1  PIC 99.
       01 A        PIC 9999.
       01 B        PIC 9999.
       01 C        PIC 9999.

       01 DISP-C   PIC ZZZ9.
       01 DISP-B   PIC ZZZ9.
       01 DISP-A   PIC ZZZ9.

       01 COUNTER  PIC 99.

       PROCEDURE DIVISION.

           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           MOVE 0 TO A.
           MOVE 1 TO B.
           MOVE 1 TO COUNTER.

           MOVE A TO DISP-A.
           MOVE B TO DISP-B.
           
           DISPLAY FUNCTION TRIM(DISP-A) WITH NO ADVANCING
           DISPLAY " " FUNCTION TRIM(DISP-B) WITH NO ADVANCING

           ADD 2 TO COUNTER.

           PERFORM UNTIL COUNTER > NUMBER1

                   COMPUTE C = A + B
                    
                   MOVE C TO DISP-C
                   DISPLAY " " FUNCTION TRIM(DISP-C)
                      WITH NO ADVANCING

                   MOVE B TO A
                   MOVE C TO B

                   ADD 1 TO COUNTER

           END-PERFORM.

           DISPLAY " ".

           STOP RUN.