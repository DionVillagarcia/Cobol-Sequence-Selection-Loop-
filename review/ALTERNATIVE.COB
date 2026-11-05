       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORD-MIDTERMS.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WORD-INPUT       PIC X(50).
       01 ORIG-WORD        PIC X(50).
       01 WS-WORD          PIC X(50).
       01 REVERSE-WORD     PIC X(50).

       01 LETTER           PIC X.
       01 LETTER-COUNT     PIC 99 VALUE 0.
       01 VOWEL-COUNT      PIC 99 VALUE 0.
       01 CONSONANT-COUNT  PIC 99 VALUE 0.

       01 INDEX1           PIC 99 VALUE 0.
       01 REVERSE-INDEX    PIC 99 VALUE 0.

       01 TOTAL-WORDS      PIC 99 VALUE 0.
       01 TOTAL-PAL        PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           PERFORM UNTIL WORD-INPUT = "0"

               MOVE SPACES TO WORD-INPUT

               DISPLAY "Input a word (0 to stop): "
                   WITH NO ADVANCING
               ACCEPT WORD-INPUT

               IF WORD-INPUT NOT = "0"

                   ADD 1 TO TOTAL-WORDS

                   MOVE FUNCTION UPPER-CASE(WORD-INPUT)
                       TO WORD-INPUT

                   MOVE WORD-INPUT TO ORIG-WORD
                   MOVE WORD-INPUT TO WS-WORD

                   MOVE SPACES TO REVERSE-WORD

                   MOVE 0 TO LETTER-COUNT
                   MOVE 0 TO VOWEL-COUNT
                   MOVE 0 TO CONSONANT-COUNT
                   MOVE 0 TO INDEX1
                   MOVE 0 TO REVERSE-INDEX

                   *> COUNT LETTERS
                   PERFORM VARYING INDEX1 FROM 1 BY 1
                       UNTIL INDEX1 > 50
                          OR WS-WORD(INDEX1:1) = SPACE

                       ADD 1 TO LETTER-COUNT

                       MOVE WS-WORD(INDEX1:1) TO LETTER

                       IF LETTER = "A"
                          OR LETTER = "E"
                          OR LETTER = "I"
                          OR LETTER = "O"
                          OR LETTER = "U"

                           ADD 1 TO VOWEL-COUNT

                       ELSE

                           ADD 1 TO CONSONANT-COUNT

                       END-IF

                   END-PERFORM

                   *> REVERSE THE WORD
                   MOVE LETTER-COUNT TO REVERSE-INDEX

                   PERFORM VARYING INDEX1 FROM 1 BY 1
                       UNTIL INDEX1 > LETTER-COUNT

                       MOVE WS-WORD(REVERSE-INDEX:1)
                           TO REVERSE-WORD(INDEX1:1)

                       SUBTRACT 1 FROM REVERSE-INDEX

                   END-PERFORM

                   DISPLAY "Number of letters: "
                       LETTER-COUNT

                   DISPLAY "Vowels: "
                       VOWEL-COUNT

                   DISPLAY "Consonants: "
                       CONSONANT-COUNT

                   DISPLAY "Reverse: "
                       FUNCTION TRIM(REVERSE-WORD)

                   *> PALINDROME CHECK
                   IF ORIG-WORD(1:LETTER-COUNT)
                       = REVERSE-WORD(1:LETTER-COUNT)

                       DISPLAY "PALINDROME"
                       ADD 1 TO TOTAL-PAL

                   ELSE

                       DISPLAY "NOT A PALINDROME"

                   END-IF

                   DISPLAY " "

               END-IF

           END-PERFORM

           DISPLAY "Total words processed: "
               TOTAL-WORDS

           DISPLAY "Total palindromes found: "
               TOTAL-PAL

           STOP RUN.