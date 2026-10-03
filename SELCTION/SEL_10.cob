       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOWEL-CONSONANT.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 LETTER  PIC A.

       PROCEDURE DIVISION.

           DISPLAY "Input a letter(A-Z or a-z): " WITH NO ADVANCING
           ACCEPT LETTER.

           IF (LETTER >= "0" AND LETTER <= "9")
              DISPLAY "INVALID INPUT"

           ELSE
              IF LETTER = "A" OR LETTER = "a"
                 DISPLAY "VOWEL"

              ELSE
                 IF LETTER = "E" OR LETTER = "e"
                    DISPLAY "VOWEL"

                 ELSE
                    IF LETTER = "I" OR LETTER = "i"
                       DISPLAY "VOWEL"

                    ELSE
                       IF LETTER = "O" OR LETTER = "o"
                          DISPLAY "VOWEL"

                       ELSE
                          IF LETTER = "U" OR LETTER = "u"
                             DISPLAY "VOWEL"

                          ELSE
                             IF (LETTER >= "A" AND LETTER <= "Z")
                                OR (LETTER >= "a" AND LETTER <= "z")
                                DISPLAY "CONSONANT"

                             ELSE
                                DISPLAY "INVALID INPUT"

                             END-IF.

           STOP RUN.