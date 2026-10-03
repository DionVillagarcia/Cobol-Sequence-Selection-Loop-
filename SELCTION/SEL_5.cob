       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMALLEST-THREE.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUMBER1        PIC 9(5).
       01 NUMBER2        PIC 9(5).
       01 NUMBER3        PIC 9(5).
       01 SMALLEST       PIC 9(5).

       01 DISP-SMALLEST  PIC ZZZZ9.


       PROCEDURE DIVISION.

           DISPLAY "Input first number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           DISPLAY "Input Second number: " WITH NO ADVANCING
           ACCEPT NUMBER2.

           DISPLAY "Input Third number: " WITH NO ADVANCING
           ACCEPT NUMBER3.

           IF NUMBER1 < NUMBER2
              IF NUMBER1 < NUMBER3
                 MOVE NUMBER1 TO SMALLEST
              ELSE
                 MOVE NUMBER3 TO SMALLEST
              END-IF
           ELSE
              IF NUMBER2 < NUMBER3
                 MOVE NUMBER2 TO SMALLEST
              ELSE
                 MOVE NUMBER3 TO SMALLEST
              END-IF
           END-IF.
           
           MOVE SMALLEST TO DISP-SMALLEST.
           DISPLAY "Smallest Number is: " FUNCTION TRIM(DISP-SMALLEST).

           STOP RUN.