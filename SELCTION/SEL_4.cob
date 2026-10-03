       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEST-TWO.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUM1 PIC 9(5).
       01 NUM2 PIC 9(5).

       01 DISP-NUM1  PIC ZZZZ9.
       01 DISP-NUM2  PIC ZZZZ9.


       PROCEDURE DIVISION.

           DISPLAY "Input First Number: " WITH NO ADVANCING
           ACCEPT NUM1.
           MOVE NUM1 TO DISP-NUM1.

           DISPLAY "Input Second Number: " WITH NO ADVANCING
           ACCEPT NUM2.
           MOVE NUM2 TO DISP-NUM2.

            IF NUM1 = NUM2
               DISPLAY FUNCTION TRIM(DISP-NUM1)
                   " is equal to "
                   FUNCTION TRIM(DISP-NUM2)

           ELSE IF NUM1 > NUM2
               DISPLAY FUNCTION TRIM(DISP-NUM1)
                   " is the larger number than "
                   FUNCTION TRIM(DISP-NUM2)

           ELSE
               DISPLAY FUNCTION TRIM(DISP-NUM2)
                   " is the larger number than "
                   FUNCTION TRIM(DISP-NUM1)

           END-IF.

           STOP RUN.
