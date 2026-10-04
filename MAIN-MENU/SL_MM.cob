       IDENTIFICATION DIVISION
       PROGRAM-ID. SELECTION-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHOICE              PIC 99         VALUE 0.
       01 KEY1                PIC X.
       01 COMMAND             PIC X(5)       VALUE "clear".

       *>Grade (passed/failed) variables
       01 PRELIM    PIC 9(5)V99.
       01 MIDTERM   PIC 9(5)V99.
       01 FINALS    PIC 9(5)V99.
       01 AVERAGE   PIC 9(5)V99.

       01 DISP-AVE  PIC ZZZZ9.99.

       *>Positive/Negative Variables
       01 NUM1       PIC S9(2).
       01 DISP-NUM1  PIC -Z9.

       *>Odd/Even
       01 NUM1-1       PIC 9(5).
       01 DISP-NUMPUT  PIC ZZZZ9.

       *>Largest number Variables
       01 NUM1-2 PIC 9(5).
       01 NUM2 PIC 9(5).

       01 DISP-NUM1-1  PIC ZZZZ9.
       01 DISP-NUM2  PIC ZZZZ9.

       *>Smallest number Variables
       01 NUMBER1        PIC 9(5).
       01 NUMBER2        PIC 9(5).
       01 NUMBER3        PIC 9(5).
       01 SMALLEST-1     PIC 9(5).

       01 DISP-SMALLEST  PIC ZZZZ9.

       *>Temperature Variables
       01 TEMPERATURE-VALUE  PIC S9(3)V99.

       *>Sales Variables
       01 TOTAL-SALES  PIC 9(10)V99.
       01 COMMISSION   PIC 9(10)V99.
       01 DISP-TS      PIC ZZZZZZZZZ9.99.
       01 DISP-CN      PIC ZZZZZZZZZ9.99.

       *>Day Name Variables
       01 DAY-NUMBER  PIC 99.

       *>Vowel/Consonant Variables
       01 LETTER  PIC A.



       PROCEDURE DIVISION.

       MAIN-MENU.
           PERFORM UNTIL CHOICE = 11
                    PERFORM CLEAR-SCREEN
                    DISPLAY "MAIN MENU"
                    DISPLAY SPACE
                    DISPLAY "1 - Grade (PASSED or FAILED)"
                    DISPLAY "2 - Positive or Negative"
                    DISPLAY "3 - Odd or Even"
                    DISPLAY "4 - Largest two"
                    DISPLAY "5 - Smallest Three"
                    DISPLAY "6 - Equivalent Grade"
                    DISPLAY "7 - Temperature"
                    DISPLAY "8 - Sales Commission"
                    DISPLAY "9 - Day Name"
                    DISPLAY "10 - Vowel or Consonant"
                    DISPLAY "11 - Exit"
                    DISPLAY "Enter your choice: " WITH NO ADVANCING
                    ACCEPT CHOICE

                    EVALUATE CHOICE
                    WHEN 1 
                         PERFORM GRADE-P-F
                    WHEN 2 
                         PERFORM POSITIVE-NEGATIVE
                    WHEN 3 
                         PERFORM ODD-EVEN
                    WHEN 4 
                         PERFORM LARGEST
                    WHEN 5 
                         PERFORM SMALLEST
                    WHEN 6 
                         PERFORM EQUI-GRADE
                    WHEN 7 
                         PERFORM TEMPERATURE
                    WHEN 8 
                         PERFORM SALES
                    WHEN 9 
                         PERFORM DAY-NAME
                    WHEN 10 
                         PERFORM ALPHA
                    WHEN 11
                         CONTINUE
                    WHEN OTHER
                         DISPLAY "Invalid Choice!"
                         PERFORM PAUSE-KEY
                    
                     END-EVALUATE
           END-PERFORM
           STOP RUN.

       GRADE-P-F.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter PRELIM Grade: " WITH NO ADVANCING
           ACCEPT PRELIM.

           DISPLAY "Enter MIDTERM Grade: " WITH NO ADVANCING
           ACCEPT MIDTERM.

           DISPLAY "Enter FINALS Grade: " WITH NO ADVANCING
           ACCEPT FINALS.

           COMPUTE AVERAGE =(PRELIM + MIDTERM + FINALS) / 3.

           MOVE AVERAGE TO DISP-AVE

           DISPLAY "Average Grade: " FUNCTION TRIM(DISP-AVE).

           IF
              AVERAGE >= 75
              DISPLAY "PASSED!"

           ELSE
              DISPLAY "FAILED"
           END-IF.
           PERFORM PAUSE-KEY.


       POSITIVE-NEGATIVE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter a Number: " WITH NO ADVANCING
           ACCEPT NUM1.

           MOVE NUM1 TO DISP-NUM1.
    
           IF NUM1 > 0
              DISPLAY FUNCTION TRIM(DISP-NUM1) " is positive."

           ELSE
              IF NUM1 < 0
                 DISPLAY FUNCTION TRIM(DISP-NUM1) " is negative."

              ELSE
                 IF NUM1 = 0
                    DISPLAY FUNCTION TRIM(DISP-NUM1) " is neutral."

                 END-IF.
           PERFORM PAUSE-KEY.


       ODD-EVEN.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUM1-1.

           MOVE NUM1-1 TO DISP-NUMPUT.

           IF NUM1-1 = 0 
              DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is neutral"
           ELSE
              IF FUNCTION MOD(NUM1-1, 2) = 0
                 DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is even"
              ELSE
                 DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is odd"
              END-IF.

           PERFORM PAUSE-KEY.


       LARGEST.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input First Number: " WITH NO ADVANCING
           ACCEPT NUM1-2.
           MOVE NUM1-2 TO DISP-NUM1-1.

           DISPLAY "Input Second Number: " WITH NO ADVANCING
           ACCEPT NUM2.
           MOVE NUM2 TO DISP-NUM2.

            IF NUM1-2= NUM2
               DISPLAY FUNCTION TRIM(DISP-NUM1-1)
                   " is equal to "
                   FUNCTION TRIM(DISP-NUM2)

           ELSE IF NUM1 > NUM2
               DISPLAY FUNCTION TRIM(DISP-NUM1-1)
                   " is the larger number than "
                   FUNCTION TRIM(DISP-NUM2)

           ELSE
               DISPLAY FUNCTION TRIM(DISP-NUM2)
                   " is the larger number than "
                   FUNCTION TRIM(DISP-NUM1-1)

           END-IF.
           PERFORM PAUSE-KEY.


       SMALLEST.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           DISPLAY "Input Second number: " WITH NO ADVANCING
           ACCEPT NUMBER2.

           DISPLAY "Input Third number: " WITH NO ADVANCING
           ACCEPT NUMBER3.

           IF NUMBER1 < NUMBER2
              IF NUMBER1 < NUMBER3
                 MOVE NUMBER1 TO SMALLEST-1
              ELSE
                 MOVE NUMBER3 TO SMALLEST-1
              END-IF
           ELSE
              IF NUMBER2 < NUMBER3
                 MOVE NUMBER2 TO SMALLEST-1
              ELSE
                 MOVE NUMBER3 TO SMALLEST-1
              END-IF
           END-IF.
           
           MOVE SMALLEST-1 TO DISP-SMALLEST.
           DISPLAY "Smallest Number is: " FUNCTION TRIM(DISP-SMALLEST).
           PERFORM PAUSE-KEY.


       EQUI-GRADE.
           PERFORM CLEAR-SCREEN
           DISPLAY "TESTING"
           PERFORM PAUSE-KEY.


       TEMPERATURE.
           PERFORM CLEAR-SCREEN
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
           PERFORM PAUSE-KEY.


       SALES.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Total Sales  : " WITH NO ADVANCING
           ACCEPT TOTAL-SALES.

           IF TOTAL-SALES <= 15000
              COMPUTE COMMISSION = TOTAL-SALES * 0.15
              DISPLAY "Commission base is  .15"

           ELSE
              IF TOTAL-SALES <= 20000
                 COMPUTE COMMISSION = TOTAL-SALES * 0.20
                 DISPLAY "Commission base is  .20"

              ELSE
                 IF TOTAL-SALES <= 25000
                    COMPUTE COMMISSION = TOTAL-SALES * 0.25
                    DISPLAY "Commission base is  .25"

                 ELSE
                    IF TOTAL-SALES <= 30000
                       COMPUTE COMMISSION = TOTAL-SALES * 0.30
                       DISPLAY "Commission base is  .30"

                    ELSE
                       COMPUTE COMMISSION = TOTAL-SALES * 0.40
                       DISPLAY "Commission base is  .40"

                    END-IF.
           
           MOVE TOTAL-SALES TO DISP-TS.
           MOVE COMMISSION TO DISP-CN.
           DISPLAY "Total Sales is     : " FUNCTION TRIM(DISP-TS).
           DISPLAY "Sales Commission is: " FUNCTION TRIM(DISP-CN).
           PERFORM PAUSE-KEY.


       DAY-NAME.
           PERFORM CLEAR-SCREEN
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
           PERFORM PAUSE-KEY.


       ALPHA.
           PERFORM CLEAR-SCREEN
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
           PERFORM PAUSE-KEY.


       CLEAR-SCREEN.
           CALL "SYSTEM" USING COMMAND.
    
    
       PAUSE-KEY.
           DISPLAY "Press any key to continue..." WITH NO ADVANCING
           ACCEPT KEY1. 