       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMECHECK.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N         PIC 9(5).
       01 CTR       PIC 9(5).
       01 IS-PRIME  PIC X    VALUE 'Y'.

       PROCEDURE DIVISION.
           MAIN-LOGIC.
           DISPLAY "Enter Number: " WITH NO ADVANCING
           ACCEPT N.

           IF N < 2
               DISPLAY "Not prime number"
           ELSE
               MOVE 2 TO CTR
               MOVE 'Y' TO IS-PRIME

               PERFORM UNTIL CTR > N - 1
                   IF FUNCTION MOD(N, CTR) = 0
                       MOVE 'N' TO IS-PRIME
                       EXIT PERFORM
                   END-IF

                   ADD 1 TO CTR
               END-PERFORM

               IF IS-PRIME = 'Y'
                   DISPLAY "Prime number"
               ELSE
                   DISPLAY "Not prime number"
               END-IF
           END-IF.

           STOP RUN.
           