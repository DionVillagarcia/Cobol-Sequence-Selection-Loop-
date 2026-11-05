       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERMS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUM                PIC 9(9) VALUE 1.
       01 ORIG-NUM           PIC 9(9).
       01 WS-NUM             PIC 9(9).

       01 DIGIT              PIC 9.
       01 QUOTIENT           PIC 9(5).

       01 DIGIT-COUNT        PIC 9 VALUE 0.
       01 SUM1               PIC 9(9).
       01 REVERSE1           PIC 9(9).

       01 TOTAL-NUM          PIC 9.
       01 TOTAL-PAL          PIC 9.

       01 DISP-S             PIC ZZZZZZZZ9.
       01 DISP-R             PIC ZZZZZZZZ9.

       01 MD                 PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM UNTIL NUM = 0
              DISPLAY "Input a number: " WITH NO ADVANCING
              ACCEPT NUM

              IF NUM NOT = 0
              ADD 1 TO TOTAL-NUM
              MOVE NUM TO ORIG-NUM
              MOVE NUM TO WS-NUM

              MOVE 0 TO DIGIT-COUNT
              MOVE 0 TO SUM1
              MOVE 0 TO REVERSE1

                 PERFORM VARYING DIGIT-COUNT FROM 1 BY 1
                  UNTIL WS-NUM = 0
                 DIVIDE WS-NUM BY 10 GIVING QUOTIENT REMAINDER DIGIT
                 END-DIVIDE
                 COMPUTE SUM1 = SUM1 + DIGIT
                 COMPUTE REVERSE1 = (REVERSE1 * 10) + DIGIT
                 MOVE QUOTIENT TO WS-NUM
                 END-PERFORM

                 MOVE SUM1 TO DISP-S
                 MOVE REVERSE1 TO DISP-R
                 COMPUTE MD = DIGIT-COUNT - 1
                 DISPLAY "Digit count: " MD
                 DISPLAY "Digit sum: " FUNCTION TRIM(DISP-S)
                 DISPLAY "Reverse number: " FUNCTION TRIM(DISP-R)

                 IF ORIG-NUM = REVERSE1 
                    DISPLAY "PALINDROME"
                    ADD 1 TO TOTAL-PAL
                 ELSE 
                    DISPLAY "NOT A PALINDROME"
                 END-IF

                 DISPLAY " "
              END-IF

           END-PERFORM.

           DISPLAY "Total number processed: " TOTAL-NUM
           DISPLAY "Total palindrome found: " TOTAL-PAL

           STOP RUN.