       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECTION-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROGRAM-1-FILE
               ASSIGN TO "Program1.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-2-FILE
               ASSIGN TO "Program2.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-3-FILE
               ASSIGN TO "Program3.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-4-FILE
               ASSIGN TO "Program4.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-5-FILE
               ASSIGN TO "Program5.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-6-FILE
               ASSIGN TO "Program6.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-7-FILE
               ASSIGN TO "Program7.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-8-FILE
               ASSIGN TO "Program8.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-9-FILE
               ASSIGN TO "Program9.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-10-FILE
               ASSIGN TO "Program10.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PROGRAM-1-FILE.
       01  PROGRAM-1-REC      PIC X(100).
       FD  PROGRAM-2-FILE.
       01  PROGRAM-2-REC      PIC X(100).
       FD  PROGRAM-3-FILE.
       01  PROGRAM-3-REC      PIC X(100).
       FD  PROGRAM-4-FILE.
       01  PROGRAM-4-REC      PIC X(100).
       FD  PROGRAM-5-FILE.
       01  PROGRAM-5-REC      PIC X(100).
       FD  PROGRAM-6-FILE.
       01  PROGRAM-6-REC      PIC X(100).
       FD  PROGRAM-7-FILE.
       01  PROGRAM-7-REC      PIC X(100).
       FD  PROGRAM-8-FILE.
       01  PROGRAM-8-REC      PIC X(100).
       FD  PROGRAM-9-FILE.
       01  PROGRAM-9-REC      PIC X(100).
       FD  PROGRAM-10-FILE.
       01  PROGRAM-10-REC     PIC X(100).

       WORKING-STORAGE SECTION.
       01 CHOICE             PIC 99            VALUE 0.
       01 KEY1               PIC X.
       01 COMMAND            PIC X(5)          VALUE "clear".

       *>Grade (passed/failed) variables
       01 PRELIM             PIC 9(5)V99.
       01 MIDTERM            PIC 9(5)V99.
       01 FINALS             PIC 9(5)V99.
       01 AVERAGE            PIC 9(5)V99.

       01 DISP-AVE           PIC ZZZZ9.99.

       *>Positive/Negative Variables
       01 NUM1               PIC S9(2).
       01 DISP-NUM1          PIC -Z9.

       *>Odd/Even
       01 NUM1-1             PIC 9(5).
       01 DISP-NUMPUT        PIC ZZZZ9.

       *>Largest number Variables
       01 NUM1-2             PIC 9(5).
       01 NUM2               PIC 9(5).

       01 DISP-NUM1-1        PIC ZZZZ9.
       01 DISP-NUM2          PIC ZZZZ9.

       *>Smallest number Variables
       01 NUMBER1            PIC 9(5).
       01 NUMBER2            PIC 9(5).
       01 NUMBER3            PIC 9(5).
       01 SMALLEST-1         PIC 9(5).

       01 DISP-SMALLEST      PIC ZZZZ9.

       *>Temperature Variables
       01 TEMPERATURE-VALUE  PIC S9(3)V99.
       01 DISP-TEMPERATURE   PIC -ZZ9.99.

       *>Sales Variables
       01 TOTAL-SALES        PIC 9(10)V99.
       01 COMMISSION         PIC 9(10)V99.
       01 DISP-TS            PIC ZZZZZZZZZ9.99.
       01 DISP-CN            PIC ZZZZZZZZZ9.99.

       *>Day Name Variables
       01 DAY-NUMBER         PIC 99.

       *>Vowel/Consonant Variables
       01 LETTER             PIC A.

       *> Equivalent grade variables
       01 EQ-PRELIM          PIC 9(3)V99.
       01 EQ-MIDTERMS        PIC 9(3)V99.
       01 EQ-FINALS          PIC 9(3)V99.
       01 EQ-AVERAGE         PIC 9(3)V99.
       01 EQ-GRADE           PIC 9(1)V99.
       01 DISP-EQ-PRELIM     PIC ZZ9.99.
       01 DISP-EQ-MIDTERMS   PIC ZZ9.99.
       01 DISP-EQ-FINALS     PIC ZZ9.99.
       01 DISP-EQ-AVERAGE    PIC ZZ9.99.
       01 DISP-EQ-GRADE      PIC 9.99.



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
           OPEN OUTPUT PROGRAM-1-FILE
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

           MOVE AVERAGE TO DISP-AVE
           MOVE SPACES TO PROGRAM-1-REC
           STRING "Average Grade: " FUNCTION TRIM(DISP-AVE)
               DELIMITED BY SIZE INTO PROGRAM-1-REC
           WRITE PROGRAM-1-REC
           IF AVERAGE >= 75
              MOVE "PASSED!" TO PROGRAM-1-REC
           ELSE
              MOVE "FAILED" TO PROGRAM-1-REC
           END-IF
           WRITE PROGRAM-1-REC
           CLOSE PROGRAM-1-FILE.
           PERFORM PAUSE-KEY.


       POSITIVE-NEGATIVE.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-2-FILE
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
           MOVE SPACES TO PROGRAM-2-REC
           STRING "Number: " FUNCTION TRIM(DISP-NUM1)
               DELIMITED BY SIZE INTO PROGRAM-2-REC
           WRITE PROGRAM-2-REC
           MOVE SPACES TO PROGRAM-2-REC
           IF NUM1 > 0
              STRING FUNCTION TRIM(DISP-NUM1) " is positive."
                  DELIMITED BY SIZE INTO PROGRAM-2-REC
           ELSE
              IF NUM1 < 0
                 STRING FUNCTION TRIM(DISP-NUM1) " is negative."
                     DELIMITED BY SIZE INTO PROGRAM-2-REC
              ELSE
                 STRING FUNCTION TRIM(DISP-NUM1) " is neutral."
                     DELIMITED BY SIZE INTO PROGRAM-2-REC
              END-IF
           END-IF
           WRITE PROGRAM-2-REC
           CLOSE PROGRAM-2-FILE.
           PERFORM PAUSE-KEY.


       ODD-EVEN.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-3-FILE
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

           MOVE SPACES TO PROGRAM-3-REC
           STRING "Number: " FUNCTION TRIM(DISP-NUMPUT)
               DELIMITED BY SIZE INTO PROGRAM-3-REC
           WRITE PROGRAM-3-REC
           MOVE SPACES TO PROGRAM-3-REC
           IF NUM1-1 = 0
              STRING FUNCTION TRIM(DISP-NUMPUT) " is neutral."
                  DELIMITED BY SIZE INTO PROGRAM-3-REC
           ELSE
              IF FUNCTION MOD(NUM1-1, 2) = 0
                 STRING FUNCTION TRIM(DISP-NUMPUT) " is even."
                     DELIMITED BY SIZE INTO PROGRAM-3-REC
              ELSE
                 STRING FUNCTION TRIM(DISP-NUMPUT) " is odd."
                     DELIMITED BY SIZE INTO PROGRAM-3-REC
              END-IF
           END-IF
           WRITE PROGRAM-3-REC
           CLOSE PROGRAM-3-FILE.
           PERFORM PAUSE-KEY.


       LARGEST.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-4-FILE
           DISPLAY "Input First Number: " WITH NO ADVANCING
           ACCEPT NUM1-2.
           MOVE NUM1-2 TO DISP-NUM1-1.

           DISPLAY "Input Second Number: " WITH NO ADVANCING
           ACCEPT NUM2.
           MOVE NUM2 TO DISP-NUM2.

           IF NUM1-2 = NUM2
              DISPLAY FUNCTION TRIM(DISP-NUM1-1)
                      " is equal to "
                      FUNCTION TRIM(DISP-NUM2)

           ELSE
              IF NUM1 > NUM2
                 DISPLAY FUNCTION TRIM(DISP-NUM1-1)
                         " is the larger number than "
                         FUNCTION TRIM(DISP-NUM2)

              ELSE
                 DISPLAY FUNCTION TRIM(DISP-NUM2)
                         " is the larger number than "
                         FUNCTION TRIM(DISP-NUM1-1)

              END-IF.
           MOVE SPACES TO PROGRAM-4-REC
           STRING "First number: " FUNCTION TRIM(DISP-NUM1-1)
               DELIMITED BY SIZE INTO PROGRAM-4-REC
           WRITE PROGRAM-4-REC
           MOVE SPACES TO PROGRAM-4-REC
           STRING "Second number: " FUNCTION TRIM(DISP-NUM2)
               DELIMITED BY SIZE INTO PROGRAM-4-REC
           WRITE PROGRAM-4-REC
           MOVE SPACES TO PROGRAM-4-REC
           IF NUM1-2 = NUM2
              STRING "The values are equal: "
                  FUNCTION TRIM(DISP-NUM1-1)
                  DELIMITED BY SIZE INTO PROGRAM-4-REC
           ELSE
              IF NUM1 > NUM2
                 STRING FUNCTION TRIM(DISP-NUM1-1)
                     " is the larger number."
                     DELIMITED BY SIZE INTO PROGRAM-4-REC
              ELSE
                 STRING FUNCTION TRIM(DISP-NUM2)
                     " is the larger number."
                     DELIMITED BY SIZE INTO PROGRAM-4-REC
              END-IF
           END-IF
           WRITE PROGRAM-4-REC
           CLOSE PROGRAM-4-FILE.
           PERFORM PAUSE-KEY.


       SMALLEST.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-5-FILE
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
           MOVE SPACES TO PROGRAM-5-REC
           STRING "Smallest Number is: "
               FUNCTION TRIM(DISP-SMALLEST)
               DELIMITED BY SIZE INTO PROGRAM-5-REC
           WRITE PROGRAM-5-REC
           CLOSE PROGRAM-5-FILE.
           PERFORM PAUSE-KEY.


       EQUI-GRADE.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-6-FILE
           DISPLAY "Enter Prelim Grade: " WITH NO ADVANCING
           ACCEPT EQ-PRELIM.

           DISPLAY "Enter Midterms Grade: " WITH NO ADVANCING
           ACCEPT EQ-MIDTERMS.

           DISPLAY "Enter Finals Grade: " WITH NO ADVANCING
           ACCEPT EQ-FINALS.

           COMPUTE EQ-AVERAGE =
               (EQ-PRELIM + EQ-MIDTERMS + EQ-FINALS) / 3.

           EVALUATE TRUE
              WHEN EQ-AVERAGE >= 97
                 MOVE 1.00 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 94
                 MOVE 1.25 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 91
                 MOVE 1.50 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 88
                 MOVE 1.75 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 85
                 MOVE 2.00 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 82
                 MOVE 2.25 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 79
                 MOVE 2.50 TO EQ-GRADE
              WHEN EQ-AVERAGE >= 76
                 MOVE 2.75 TO EQ-GRADE
              WHEN EQ-AVERAGE = 75
                 MOVE 3.00 TO EQ-GRADE
              WHEN OTHER
                 MOVE 5.00 TO EQ-GRADE
           END-EVALUATE.

           MOVE EQ-AVERAGE TO DISP-EQ-AVERAGE.
           MOVE EQ-GRADE TO DISP-EQ-GRADE.
           MOVE EQ-PRELIM TO DISP-EQ-PRELIM.
           MOVE EQ-MIDTERMS TO DISP-EQ-MIDTERMS.
           MOVE EQ-FINALS TO DISP-EQ-FINALS.
           DISPLAY "AVERAGE: "
               FUNCTION TRIM(DISP-EQ-AVERAGE).
           DISPLAY "EQUIVALENT GRADE: "
               FUNCTION TRIM(DISP-EQ-GRADE).

           MOVE SPACES TO PROGRAM-6-REC
           STRING "Prelim Grade: "
               FUNCTION TRIM(DISP-EQ-PRELIM)
               DELIMITED BY SIZE INTO PROGRAM-6-REC
           WRITE PROGRAM-6-REC
           MOVE SPACES TO PROGRAM-6-REC
           STRING "Midterms Grade: "
               FUNCTION TRIM(DISP-EQ-MIDTERMS)
               DELIMITED BY SIZE INTO PROGRAM-6-REC
           WRITE PROGRAM-6-REC
           MOVE SPACES TO PROGRAM-6-REC
           STRING "Finals Grade: "
               FUNCTION TRIM(DISP-EQ-FINALS)
               DELIMITED BY SIZE INTO PROGRAM-6-REC
           WRITE PROGRAM-6-REC
           MOVE SPACES TO PROGRAM-6-REC
           STRING "AVERAGE: "
               FUNCTION TRIM(DISP-EQ-AVERAGE)
               DELIMITED BY SIZE INTO PROGRAM-6-REC
           WRITE PROGRAM-6-REC
           MOVE SPACES TO PROGRAM-6-REC
           STRING "EQUIVALENT GRADE: "
               FUNCTION TRIM(DISP-EQ-GRADE)
               DELIMITED BY SIZE INTO PROGRAM-6-REC
           WRITE PROGRAM-6-REC
           CLOSE PROGRAM-6-FILE
           PERFORM PAUSE-KEY.


       TEMPERATURE.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-7-FILE
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
           MOVE TEMPERATURE-VALUE TO DISP-TEMPERATURE
           MOVE SPACES TO PROGRAM-7-REC
           STRING "Temperature: "
               FUNCTION TRIM(DISP-TEMPERATURE)
               DELIMITED BY SIZE INTO PROGRAM-7-REC
           WRITE PROGRAM-7-REC
           EVALUATE TRUE
              WHEN TEMPERATURE-VALUE < 0
                 MOVE "FREEZING WEATHER" TO PROGRAM-7-REC
              WHEN TEMPERATURE-VALUE < 10
                 MOVE "VERY COLD WEATHER" TO PROGRAM-7-REC
              WHEN TEMPERATURE-VALUE < 20
                 MOVE "COLD WEATHER" TO PROGRAM-7-REC
              WHEN TEMPERATURE-VALUE < 30
                 MOVE "NORMAL WEATHER" TO PROGRAM-7-REC
              WHEN TEMPERATURE-VALUE < 40
                 MOVE "HOT WEATHER" TO PROGRAM-7-REC
              WHEN OTHER
                 MOVE "VERY HOT WEATHER" TO PROGRAM-7-REC
           END-EVALUATE
           WRITE PROGRAM-7-REC
           CLOSE PROGRAM-7-FILE.
           PERFORM PAUSE-KEY.


       SALES.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-8-FILE
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
           MOVE SPACES TO PROGRAM-8-REC
           STRING "Total Sales: " FUNCTION TRIM(DISP-TS)
               DELIMITED BY SIZE INTO PROGRAM-8-REC
           WRITE PROGRAM-8-REC
           MOVE SPACES TO PROGRAM-8-REC
           STRING "Sales Commission: " FUNCTION TRIM(DISP-CN)
               DELIMITED BY SIZE INTO PROGRAM-8-REC
           WRITE PROGRAM-8-REC
           CLOSE PROGRAM-8-FILE.
           PERFORM PAUSE-KEY.


       DAY-NAME.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-9-FILE
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
              MOVE SPACES TO PROGRAM-9-REC
              STRING "Day number: " DAY-NUMBER
                  DELIMITED BY SIZE INTO PROGRAM-9-REC
              WRITE PROGRAM-9-REC
              EVALUATE DAY-NUMBER
                 WHEN 1
                    MOVE "Today is Monday" TO PROGRAM-9-REC
                 WHEN 2
                    MOVE "Today is Tuesday" TO PROGRAM-9-REC
                 WHEN 3
                    MOVE "Today is Wednesday" TO PROGRAM-9-REC
                 WHEN 4
                    MOVE "Today is Thursday" TO PROGRAM-9-REC
                 WHEN 5
                    MOVE "Today is Friday" TO PROGRAM-9-REC
                 WHEN 6
                    MOVE "Today is Saturday" TO PROGRAM-9-REC
                 WHEN 7
                    MOVE "Today is Sunday" TO PROGRAM-9-REC
                 WHEN OTHER
                    MOVE "INVALID DAY NUMBER" TO PROGRAM-9-REC
              END-EVALUATE
              WRITE PROGRAM-9-REC
              CLOSE PROGRAM-9-FILE.
              PERFORM PAUSE-KEY.


       ALPHA.
           PERFORM CLEAR-SCREEN
           OPEN OUTPUT PROGRAM-10-FILE
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
           MOVE SPACES TO PROGRAM-10-REC
           STRING "Letter: " LETTER
               DELIMITED BY SIZE INTO PROGRAM-10-REC
           WRITE PROGRAM-10-REC
           IF LETTER = "A" OR LETTER = "a"
               OR LETTER = "E" OR LETTER = "e"
               OR LETTER = "I" OR LETTER = "i"
               OR LETTER = "O" OR LETTER = "o"
               OR LETTER = "U" OR LETTER = "u"
              MOVE "VOWEL" TO PROGRAM-10-REC
           ELSE
              IF (LETTER >= "A" AND LETTER <= "Z")
                  OR (LETTER >= "a" AND LETTER <= "z")
                 MOVE "CONSONANT" TO PROGRAM-10-REC
              ELSE
                 MOVE "INVALID INPUT" TO PROGRAM-10-REC
              END-IF
           END-IF
           WRITE PROGRAM-10-REC
           CLOSE PROGRAM-10-FILE.
           PERFORM PAUSE-KEY.


       CLEAR-SCREEN.
           CALL "SYSTEM" USING COMMAND.
    
    
       PAUSE-KEY.
           DISPLAY "Press any key to continue..." WITH NO ADVANCING.
           ACCEPT KEY1.
