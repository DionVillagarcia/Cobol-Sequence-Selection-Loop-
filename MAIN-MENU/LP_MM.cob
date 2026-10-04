       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHOICE              PIC 99         VALUE 0.
       01 KEY1                PIC X.
       01 COMMAND             PIC X(5)       VALUE "clear".

       *>Numbers 1-5
       01 COUNTER  PIC 9.

       *>Even Numbers 1-N
       01 NUMBER1    PIC 99.
       01 COUNTER-1  PIC 99.

       01 DISP-NUM   PIC Z9.

       *>Even Numbers Range
       01 LOW       PIC 99.
       01 HIGH      PIC 99.

       01 DISP-LOW  PIC Z9.

       *> sum of odd num N-M
       01 LOW-1         PIC 99.
       01 HIGH-1        PIC 99.
       01 SUM1          PIC 999 VALUE 0.
       01 COUNTER-2     PIC 99.

       01 DISP-SUM      PIC ZZ9.
       01 DISP-COUNTER  PIC ZZ9.

       *> Factorial
       01 NUMBER1-1  PIC 99.
       01 FACTORIAL  PIC 9(4) VALUE 1.
       01 COUNTER-3  PIC 99   VALUE 1.

       01 DISP-FAC   PIC ZZZ9.
       01 DISP-NUM-1   PIC Z9.

       *> Digit Sum
       01 NUMBER1-2   PIC 9(5).
       01 SUM1-1      PIC 99999 VALUE 0.
       01 DIGIT       PIC 9.

       01 DISP-SUM-1  PIC ZZZZ9.

       *>Fibonacci
       01 NUMBER1-3  PIC 99.
       01 A        PIC 9999.
       01 B        PIC 9999.
       01 C        PIC 9999.

       01 DISP-C   PIC ZZZ9.
       01 DISP-B   PIC ZZZ9.
       01 DISP-A   PIC ZZZ9.

       01 COUNTER-4  PIC 99.

       *>Prime Numbers
       01 N         PIC 9(5).
       01 CTR       PIC 9(5).
       01 IS-PRIME  PIC X    VALUE 'Y'.

       *>Binary
       01 NUMBER-VALUE     PIC 9(10).
       01 COUNTER-5        PIC 9(10) VALUE 1.
       01 BINARY-RESULT    PIC 9(10) VALUE 0.
       01 REMAINDER-VALUE  PIC 9.

       01 DISP-RESULT      PIC ZZZZZZZZZ9.

       




       PROCEDURE DIVISION.

       PERFORM UNTIL CHOICE = 11
                    PERFORM CLEAR-SCREEN
                    DISPLAY "MAIN MENU"
                    DISPLAY SPACE
                    DISPLAY "1 - Print Name"
                    DISPLAY "2 - Print numbers 1-5"
                    DISPLAY "3 - Print even numbers 1-N"
                    DISPLAY "4 - Print even numbers N-M"
                    DISPLAY "5 - Print sum of odd numbers N-M"
                    DISPLAY "6 - Factorial"
                    DISPLAY "7 - Sum of digits"
                    DISPLAY "8 - Fibonacci"
                    DISPLAY "9 - Prime Numbers"
                    DISPLAY "10 - Decimals to Binary"
                    DISPLAY "11 - Exit"
                    DISPLAY "Enter your choice: " WITH NO ADVANCING
                    ACCEPT CHOICE

                    EVALUATE CHOICE
                    WHEN 1
                         PERFORM PRINT-NAME
                    WHEN 2
                         PERFORM PRINT-NUM
                    WHEN 3
                         PERFORM PRINT-EVEN-NUM
                    WHEN 4
                         PERFORM PRINT-EVEN-RANGE
                    WHEN 5
                         PERFORM PRINT-ODD-RANGE
                    WHEN 6
                         PERFORM FACTORIAL-1
                    WHEN 7
                         PERFORM DIGITS
                    WHEN 8
                         PERFORM FIBONACCI
                    WHEN 9
                         PERFORM PRIME-NUM
                    WHEN 10
                         PERFORM DEC-BINARY
                    WHEN 11
                         CONTINUE
                    WHEN OTHER 
                          DISPLAY "Invalid Choice!"
                          PERFORM PAUSE-KEY

                    END-EVALUATE
           END-PERFORM
           STOP RUN.

       PRINT-NAME.
           PERFORM CLEAR-SCREEN
           PERFORM 5 TIMES
                   DISPLAY "DION"
           END-PERFORM.

           STOP RUN.
           PERFORM PAUSE-KEY.


       PRINT-NUM.
           PERFORM CLEAR-SCREEN
           MOVE 1 TO COUNTER
           
           PERFORM UNTIL COUNTER > 5
                   DISPLAY COUNTER
                   ADD 1 TO COUNTER
           END-PERFORM. 
           PERFORM PAUSE-KEY.


       PRINT-EVEN-NUM.
           PERFORM CLEAR-SCREEN
           MOVE 1 TO COUNTER-1.

           DISPLAY "Input a last number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           MOVE NUMBER1 TO DISP-NUM
           
           PERFORM UNTIL COUNTER-1 > NUMBER1
                   IF FUNCTION MOD(COUNTER-1, 2) = 0
                      MOVE COUNTER-1 TO DISP-NUM
                      DISPLAY FUNCTION TRIM(DISP-NUM)
                   END-IF
                   ADD 1 TO COUNTER-1
                 
           END-PERFORM.
           PERFORM PAUSE-KEY.


       PRINT-EVEN-RANGE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT LOW.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT HIGH.
           

           PERFORM UNTIL LOW > HIGH
                   IF FUNCTION MOD(LOW, 2) = 0
                      MOVE LOW TO DISP-LOW 
                      DISPLAY FUNCTION TRIM(DISP-LOW)
                   END-IF
                   ADD 1 TO LOW

           END-PERFORM.
           PERFORM PAUSE-KEY.


       PRINT-ODD-RANGE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT LOW-1.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT HIGH-1.
           
           DISPLAY "The sum of odd numbers: " WITH NO ADVANCING
           PERFORM UNTIL COUNTER-2 > HIGH-1
                   IF FUNCTION MOD(COUNTER-2, 2) = 1
                      COMPUTE SUM1 = SUM1 + COUNTER-2
                      MOVE COUNTER-2 TO DISP-COUNTER
                      DISPLAY FUNCTION TRIM(DISP-COUNTER)
                              " " WITH NO ADVANCING
                   END-IF
                   ADD 1 TO COUNTER-2
           END-PERFORM.
           
           MOVE SUM1 TO DISP-SUM.
           DISPLAY "is: "
                   FUNCTION TRIM(DISP-SUM).
           PERFORM PAUSE-KEY.


       FACTORIAL-1.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT NUMBER1-1.

           PERFORM UNTIL COUNTER-3 > NUMBER1-1
                   COMPUTE FACTORIAL = FACTORIAL * COUNTER-3
                   ADD 1 TO COUNTER-3
           END-PERFORM
           
           MOVE FACTORIAL TO DISP-FAC.
           MOVE NUMBER1-1 TO DISP-NUM-1
           DISPLAY "Factorial of "
                   FUNCTION TRIM(DISP-NUM-1)
                   " is "
                   FUNCTION TRIM(DISP-FAC).
           PERFORM PAUSE-KEY.


       DIGITS.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUMBER1-2.

           PERFORM UNTIL NUMBER1-2 <= 0
                   COMPUTE DIGIT = FUNCTION MOD(NUMBER1-2, 10)
                   COMPUTE SUM1-1 = SUM1-1 + DIGIT
                   COMPUTE NUMBER1-2 = NUMBER1-2 / 10
           END-PERFORM.
           
           MOVE SUM1-1 TO DISP-SUM-1
           DISPLAY "Digit sum is: " FUNCTION TRIM(DISP-SUM-1).
           PERFORM PAUSE-KEY.


       FIBONACCI.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUMBER1-3.

           MOVE 0 TO A.
           MOVE 1 TO B.
           MOVE 1 TO COUNTER-4.

           MOVE A TO DISP-A.
           MOVE B TO DISP-B.
           
           DISPLAY FUNCTION TRIM(DISP-A) WITH NO ADVANCING
           DISPLAY " " FUNCTION TRIM(DISP-B) WITH NO ADVANCING

           ADD 2 TO COUNTER-4.

           PERFORM UNTIL COUNTER-4 > NUMBER1-3

                   COMPUTE C = A + B
                    
                   MOVE C TO DISP-C
                   DISPLAY " " FUNCTION TRIM(DISP-C)
                      WITH NO ADVANCING

                   MOVE B TO A
                   MOVE C TO B

                   ADD 1 TO COUNTER-4

           END-PERFORM.

           DISPLAY " ".
           PERFORM PAUSE-KEY.


       PRIME-NUM.
           PERFORM CLEAR-SCREEN
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
           PERFORM PAUSE-KEY.


       DEC-BINARY.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Decimal Number: " WITH NO ADVANCING
           ACCEPT NUMBER-VALUE.

           MOVE 1 TO COUNTER-5.
           MOVE 0 TO BINARY-RESULT.

           PERFORM UNTIL NUMBER-VALUE = 0
                   COMPUTE REMAINDER-VALUE = FUNCTION MOD(NUMBER-VALUE,
                      2)
                   COMPUTE BINARY-RESULT = BINARY-RESULT +
                      (REMAINDER-VALUE * COUNTER-5)
                   COMPUTE COUNTER-5 = COUNTER-5 * 10
                   COMPUTE NUMBER-VALUE = NUMBER-VALUE / 2
           END-PERFORM.

           MOVE BINARY-RESULT TO DISP-RESULT.
           DISPLAY FUNCTION TRIM(DISP-RESULT).
           PERFORM PAUSE-KEY.

 
       CLEAR-SCREEN.
           CALL "SYSTEM" USING COMMAND.
    
    
       PAUSE-KEY.
           DISPLAY "Press any key to continue..." WITH NO ADVANCING
           ACCEPT KEY1.     





