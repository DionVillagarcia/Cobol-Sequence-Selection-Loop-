       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY-NAME.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 DAY-NUMBER  PIC 99.

       PROCEDURE DIVISION.

           DISPLAY "Input the day number (1-7): " WITH NO ADVANCING
           ACCEPT DAY-NUMBER.

           IF DAY-NUMBER = 1
              DISPLAY "Today is Monday"
           ELSE
              IF DAY-NUMBER = 2
                 DISPLAY "Today is Tuesday"
              ELSE
                 IF DAY-NUMBER = 3
                    DISPLAY "Today is Wednesday"
                 ELSE
                    IF DAY-NUMBER = 4
                       DISPLAY "Today is Thursday"
                    ELSE
                       IF DAY-NUMBER = 5
                          DISPLAY "Today is Friday"
                       ELSE
                          IF DAY-NUMBER = 6
                             DISPLAY "Today is Saturday"
                          ELSE
                             IF DAY-NUMBER = 7
                                DISPLAY "Today is Sunday"
                             ELSE
                                DISPLAY "INVALID DAY NUMBER"
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

           STOP RUN.