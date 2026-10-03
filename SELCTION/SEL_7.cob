       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMP.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TEMPERATURE-VALUE  PIC S9(3)V99.

       PROCEDURE DIVISION.

           DISPLAY "Input Temperature: " WITH NO ADVANCING
           ACCEPT TEMPERATURE-VALUE.

           IF TEMPERATURE-VALUE < 0
              DISPLAY "FREEZING WEATHER"

           ELSE
              IF TEMPERATURE-VALUE >= 0
                 IF TEMPERATURE-VALUE < 10
                    DISPLAY "VERY COLD WEATHER"

                 ELSE
                    IF TEMPERATURE-VALUE >= 10
                       IF TEMPERATURE-VALUE < 20
                          DISPLAY "COLD WEATHER"

                       ELSE
                          IF TEMPERATURE-VALUE >= 20
                             IF TEMPERATURE-VALUE < 30
                                DISPLAY "NORMAL WEATHER"

                             ELSE
                                IF TEMPERATURE-VALUE >= 30
                                   IF TEMPERATURE-VALUE < 40
                                      DISPLAY "HOT WEATHER"

                                   ELSE
                                      DISPLAY "VERY HOT WEATHER"
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF.

           STOP RUN.