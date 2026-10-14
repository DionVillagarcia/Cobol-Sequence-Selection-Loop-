       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERALL-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MENU-CHOICE               PIC 99.
       01 PAUSE-KEY-INPUT           PIC X.
       01 CLEAR-SCREEN-COMMAND      PIC X(5) VALUE "clear".
       
       *> SEQUENCE MAIN MENU VARIABLES ===============================
       *>swap variables
       01 SWAP-FIRST-VALUE          PIC 99.
       01 SWAP-SECOND-VALUE         PIC 99.
       01 SWAP-TEMP-VALUE           PIC 99.

       01 DISPLAY-SWAP-FIRST-VALUE  PIC Z9.
       01 DISPLAY-SWAP-SECOND-VALUE PIC Z9.

       *>Calculation variables
       01 CALC-FIRST-NUMBER         PIC 9(2).
       01 CALC-SECOND-NUMBER        PIC 9(2).

       01 CALC-SUM                  PIC 9(3).
       01 CALC-DIFFERENCE           PIC 9(3).
       01 CALC-PRODUCT              PIC 9(3).
       01 CALC-QUOTIENT             PIC 9(3)V99.

       01 DISPLAY-CALC-FIRST-NUMBER  PIC ZZ9.
       01 DISPLAY-CALC-SECOND-NUMBER PIC ZZ9.
       01 DISPLAY-CALC-SUM           PIC ZZZ9.
       01 DISPLAY-CALC-DIFFERENCE    PIC ZZ9.
       01 DISPLAY-CALC-PRODUCT       PIC ZZ9.
       01 DISPLAY-CALC-QUOTIENT      PIC ZZ9.99.

       *>Fahrenheit variables
       01 FAHRENHEIT-TEMPERATURE     PIC 9(2)V99.
       01 CELSIUS-TEMPERATURE        PIC 9(2)V99.

       *> Total Sales variables
       01 SALESPERSON-NUMBER         PIC 9(11).
       01 SALESPERSON-NAME           PIC A(20).
       01 UNITS-SOLD                 PIC 9(3).
       01 UNIT-PRICE                 PIC 9(6)V99.
       01 SEQUENCE-SALES-TOTAL       PIC 9(6)V99.

       01 DISPLAY-SEQUENCE-SALES-TOTAL PIC ZZZZZZ9.99.

       *>Average variable
       01 AVERAGE-PRELIM-GRADE        PIC 9(3).
       01 AVERAGE-MIDTERM-GRADE       PIC 9(3).
       01 AVERAGE-FINAL-GRADE         PIC 9(3).
       01 AVERAGE-GRADE-TOTAL         PIC 9(3).
       01 OVERALL-AVERAGE             PIC 9(2)V99.

       *>Square and cube variable
       01 SQUARE-VALUE               PIC 9(5).
       01 CUBE-VALUE                 PIC 9(5).
       01 CUBE-SQUARE-INPUT          PIC 9(5).

       01 DISPLAY-CUBE-SQUARE-INPUT  PIC ZZZZ9.
       01 DISPLAY-SQUARE-VALUE       PIC ZZZZ9.
       01 DISPLAY-CUBE-VALUE         PIC ZZZZ9.

       *>Rectangle variable
       01 RECTANGLE-LENGTH           PIC 9(3).
       01 RECTANGLE-WIDTH            PIC 9(3).
       01 RECTANGLE-AREA             PIC 9(5).
       01 RECTANGLE-PERIMETER        PIC 9(5).

       01 DISPLAY-RECTANGLE-AREA     PIC ZZZZ9.
       01 DISPLAY-RECTANGLE-PERIMETER PIC ZZZZ9.

       *>Circle Variable
       01 CIRCLE-DIAMETER            PIC 9(3)V99.
       01 CIRCLE-CIRCUMFERENCE       PIC 9(3)V99.
       01 CIRCLE-AREA                PIC 9(5)V99.

       01 DISPLAY-CIRCLE-AREA        PIC ZZZZ9.99.
       01 DISPLAY-CIRCLE-CIRCUMFERENCE PIC ZZZZ9.99.

       *>Triangle variable

       01 TRIANGLE-BASE              PIC 9(3)V99.
       01 TRIANGLE-HEIGHT            PIC 9(3)V99.
       01 TRIANGLE-SIDE-ONE          PIC 9(5)V99.
       01 TRIANGLE-SIDE-TWO          PIC 9(5)V99.
       01 TRIANGLE-SIDE-THREE        PIC 9(5)V99.
       01 TRIANGLE-AREA              PIC 9(5)V99.
       01 TRIANGLE-PERIMETER         PIC 9(5)V99.

       01 DISPLAY-TRIANGLE-AREA      PIC ZZZZ9.99.
       01 DISPLAY-TRIANGLE-PERIMETER PIC ZZZZ9.99.

       *> SELECTION MAIN MENU VARIABLES ================================
       *>Grade (passed/failed) variables
       01 GRADE-PRELIM               PIC 9(5)V99.
       01 GRADE-MIDTERM              PIC 9(5)V99.
       01 GRADE-FINAL                PIC 9(5)V99.
       01 GRADE-AVERAGE              PIC 9(5)V99.

       01 DISPLAY-GRADE-AVERAGE      PIC ZZZZ9.99.

       *>Positive/Negative Variables
       01 SIGN-CHECK-NUMBER          PIC S9(2).
       01 DISPLAY-SIGN-CHECK-NUMBER  PIC -Z9.

       *>Odd/Even
       01 PARITY-CHECK-NUMBER         PIC 9(5).
       01 DISPLAY-PARITY-CHECK-NUMBER PIC ZZZZ9.

       *>Largest number Variables
       01 LARGEST-FIRST-NUMBER        PIC 9(5).
       01 LARGEST-SECOND-NUMBER       PIC 9(5).

       01 DISPLAY-LARGEST-FIRST-NUMBER PIC ZZZZ9.
       01 DISPLAY-LARGEST-SECOND-NUMBER PIC ZZZZ9.

       *>Smallest number Variables
       01 SMALLEST-FIRST-NUMBER       PIC 9(5).
       01 SMALLEST-SECOND-NUMBER      PIC 9(5).
       01 SMALLEST-THIRD-NUMBER       PIC 9(5).
       01 SMALLEST-RESULT             PIC 9(5).

       01 DISPLAY-SMALLEST-RESULT    PIC ZZZZ9.

       *>Temperature Variables
       01 TEMPERATURE-CELSIUS        PIC S9(3)V99.

       *>Sales Variables
       01 COMMISSION-SALES-TOTAL     PIC 9(10)V99.
       01 SALES-COMMISSION           PIC 9(10)V99.
       01 DISPLAY-COMMISSION-SALES-TOTAL PIC ZZZZZZZZZ9.99.
       01 DISPLAY-SALES-COMMISSION   PIC ZZZZZZZZZ9.99.

       *>Day Name Variables
       01 WEEKDAY-NUMBER             PIC 99.

       *>Vowel/Consonant Variables
       01 ALPHABET-LETTER            PIC A.

       *>LOOP MAIN MENU VARIABLES =================================
       *>Numbers 1-5
       01 NUMBER-LOOP-COUNTER         PIC 9.

       *>Even Numbers 1-N
       01 EVEN-NUMBER-LIMIT           PIC 99.
       01 EVEN-NUMBER-COUNTER         PIC 99.

       01 DISPLAY-EVEN-NUMBER         PIC Z9.

       *>Even Numbers Range
       01 RANGE-LOWER-BOUND           PIC 99.
       01 RANGE-UPPER-BOUND           PIC 99.

       01 DISPLAY-RANGE-LOWER-BOUND   PIC Z9.

       *> sum of odd num N-M
       01 ODD-RANGE-LOWER-BOUND       PIC 99.
       01 ODD-RANGE-UPPER-BOUND       PIC 99.
       01 ODD-RANGE-SUM               PIC 999        VALUE 0.
       01 ODD-RANGE-COUNTER           PIC 99.

       01 DISPLAY-ODD-RANGE-SUM       PIC ZZ9.
       01 DISPLAY-ODD-RANGE-COUNTER   PIC ZZ9.

       *> Factorial
       01 FACTORIAL-INPUT             PIC 99.
       01 FACTORIAL-RESULT            PIC 9(4)       VALUE 1.
       01 FACTORIAL-COUNTER           PIC 99         VALUE 1.

       01 DISPLAY-FACTORIAL-RESULT    PIC ZZZ9.
       01 DISPLAY-FACTORIAL-INPUT     PIC Z9.

       *> Digit Sum
       01 DIGIT-SUM-INPUT             PIC 9(5).
       01 DIGIT-SUM-TOTAL             PIC 99999      VALUE 0.
       01 CURRENT-DIGIT               PIC 9.

       01 DISPLAY-DIGIT-SUM           PIC ZZZZ9.

       *>Fibonacci
       01 FIBONACCI-TERM-COUNT        PIC 99.
       01 FIBONACCI-FIRST-TERM        PIC 9999.
       01 FIBONACCI-SECOND-TERM       PIC 9999.
       01 FIBONACCI-NEXT-TERM         PIC 9999.

       01 DISPLAY-FIBONACCI-NEXT-TERM PIC ZZZ9.
       01 DISPLAY-FIBONACCI-SECOND-TERM PIC ZZZ9.
       01 DISPLAY-FIBONACCI-FIRST-TERM PIC ZZZ9.

       01 FIBONACCI-COUNTER           PIC 99.

       *>Prime Numbers
       01 PRIME-CANDIDATE             PIC 9(5).
       01 PRIME-DIVISOR               PIC 9(5).
       01 PRIME-STATUS                PIC X          VALUE 'Y'.

       *>Binary
       01 BINARY-INPUT-NUMBER         PIC 9(10).
       01 BINARY-POSITION             PIC 9(10)      VALUE 1.
       01 BINARY-CONVERTED-VALUE      PIC 9(10)      VALUE 0.
       01 BINARY-REMAINDER            PIC 9.

       01 DISPLAY-BINARY-RESULT       PIC ZZZZZZZZZ9.

       PROCEDURE DIVISION.

       MAIN-MENU-OVERALL.
           
           PERFORM UNTIL MENU-CHOICE = 4
                   PERFORM CLEAR-SCREEN
                   DISPLAY "OVERALL MAIN MENU"
                   DISPLAY SPACE
                   DISPLAY "1 - SEQUENCE MAIN MENU"
                   DISPLAY "2 - SELECTION MAIN MENU"
                   DISPLAY "3 - LOOP MAIN MENU"
                   DISPLAY "4 - EXIT"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT MENU-CHOICE

                   EVALUATE MENU-CHOICE
                   WHEN 1 
                        PERFORM SEQUENCE-MENU
                   WHEN 2
                        PERFORM SELECTION-MENU
                   WHEN 3
                        PERFORM LOOP-MENU
                   WHEN 4
                        CONTINUE
                   WHEN OTHER
                        DISPLAY "INVALID CHOICE!"
                        PERFORM PAUSE-KEY

                   END-EVALUATE
           END-PERFORM

           STOP RUN.

       SEQUENCE-MENU.
           MOVE 0 TO MENU-CHOICE
           PERFORM CLEAR-SCREEN.
           PERFORM UNTIL MENU-CHOICE = 11
                   PERFORM CLEAR-SCREEN
                   DISPLAY "MAIN MENU"
                   DISPLAY " " 
                   DISPLAY "1 - Print Name"
                   DISPLAY "2 - Swap"
                   DISPLAY "3 - Sum, Difference, Product, and Quotient"
                   DISPLAY "4 - Equivalent Fahrenheit"
                   DISPLAY "5 - Total Sales"
                   DISPLAY "6 - Overall Average"
                   DISPLAY "7 - Cube and Square"
                   DISPLAY "8 - Rectangle Length and Width"
                   DISPLAY "9 - Circle Area and Circumference"
                   DISPLAY "10 - Triangle Base, Height, and Sides"
                   DISPLAY "11 - RETURN"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT MENU-CHOICE

                   EVALUATE MENU-CHOICE
                   WHEN 1
                        PERFORM PRINT-NAME
                   WHEN 2
                        PERFORM SWAP
                   WHEN 3 
                        PERFORM CALCULATION
                   WHEN 4
                        PERFORM FAHRENHEIT
                   WHEN 5
                        PERFORM TOTAL-SALES1
                   WHEN 6
                        PERFORM OVERALL-AVE
                   WHEN 7
                        PERFORM CUBE-SQUARE
                   WHEN 8
                        PERFORM RECTANGLE
                   WHEN 9
                        PERFORM CIRCLE
                   WHEN 10
                        PERFORM TRIANGLE
                   WHEN 11
                        CONTINUE
                    WHEN OTHER
                        DISPLAY "Invalid Choice!"
                        PERFORM PAUSE-KEY

                   END-EVALUATE

           END-PERFORM.

       PRINT-NAME.
           PERFORM CLEAR-SCREEN
           DISPLAY "Dion".
           DISPLAY "Dion".
           DISPLAY "Dion".
           DISPLAY "Dion".
           DISPLAY "Dion".
           PERFORM PAUSE-KEY.


       SWAP.
           PERFORM CLEAR-SCREEN
           MOVE 10 TO SWAP-FIRST-VALUE.
           MOVE 5 TO SWAP-SECOND-VALUE.
           DISPLAY "BEFORE: " SWAP-FIRST-VALUE
                   " & " SWAP-SECOND-VALUE.
           MOVE SWAP-FIRST-VALUE TO SWAP-TEMP-VALUE.
           MOVE SWAP-SECOND-VALUE TO SWAP-FIRST-VALUE.
           MOVE SWAP-TEMP-VALUE TO SWAP-SECOND-VALUE.
           MOVE SWAP-FIRST-VALUE TO DISPLAY-SWAP-FIRST-VALUE.
           MOVE SWAP-SECOND-VALUE TO DISPLAY-SWAP-SECOND-VALUE.
           DISPLAY "NOW:     "
                   FUNCTION TRIM(DISPLAY-SWAP-FIRST-VALUE)
                   " & "
                   FUNCTION TRIM(DISPLAY-SWAP-SECOND-VALUE).
           PERFORM PAUSE-KEY.
    
    
       CALCULATION.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input number: " WITH NO ADVANCING
           ACCEPT CALC-FIRST-NUMBER.
           DISPLAY "Input number: " WITH NO ADVANCING
           ACCEPT CALC-SECOND-NUMBER.
           ADD CALC-FIRST-NUMBER TO CALC-SECOND-NUMBER
               GIVING CALC-SUM.
           SUBTRACT CALC-FIRST-NUMBER FROM CALC-SECOND-NUMBER
               GIVING CALC-DIFFERENCE.
           MULTIPLY CALC-FIRST-NUMBER BY CALC-SECOND-NUMBER
               GIVING CALC-PRODUCT.
           DIVIDE CALC-FIRST-NUMBER BY CALC-SECOND-NUMBER
               GIVING CALC-QUOTIENT.
   
           MOVE CALC-FIRST-NUMBER TO DISPLAY-CALC-FIRST-NUMBER.
           MOVE CALC-SECOND-NUMBER TO DISPLAY-CALC-SECOND-NUMBER.
           MOVE CALC-SUM TO DISPLAY-CALC-SUM.
           MOVE CALC-DIFFERENCE TO DISPLAY-CALC-DIFFERENCE.
           MOVE CALC-PRODUCT TO DISPLAY-CALC-PRODUCT.
           MOVE CALC-QUOTIENT TO DISPLAY-CALC-QUOTIENT.

           DISPLAY "The Sum of the "
                   FUNCTION TRIM(DISPLAY-CALC-FIRST-NUMBER)
                   " and "
                   FUNCTION TRIM(DISPLAY-CALC-SECOND-NUMBER)
                   " is "
                   FUNCTION TRIM(DISPLAY-CALC-SUM).

           DISPLAY "The Difference of the "
                   FUNCTION TRIM(DISPLAY-CALC-FIRST-NUMBER)
                   " and "
                   FUNCTION TRIM(DISPLAY-CALC-SECOND-NUMBER)
                   " is "
                   FUNCTION TRIM(DISPLAY-CALC-DIFFERENCE).

           DISPLAY "The Product of the "
                   FUNCTION TRIM(DISPLAY-CALC-FIRST-NUMBER)
                   " and "
                   FUNCTION TRIM(DISPLAY-CALC-SECOND-NUMBER)
                   " is "
                   FUNCTION TRIM(DISPLAY-CALC-PRODUCT).

           DISPLAY "The Quotient of the "
                   FUNCTION TRIM(DISPLAY-CALC-FIRST-NUMBER)
                   " and "
                   FUNCTION TRIM(DISPLAY-CALC-SECOND-NUMBER)
                   " is "
                   FUNCTION TRIM(DISPLAY-CALC-QUOTIENT).
           PERFORM PAUSE-KEY.
    
    
       FAHRENHEIT.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Celsius: " WITH NO ADVANCING
           ACCEPT CELSIUS-TEMPERATURE.
           COMPUTE FAHRENHEIT-TEMPERATURE =
               CELSIUS-TEMPERATURE * 9 / 5 + 32.
           DISPLAY "The converted Celsius of "
                   CELSIUS-TEMPERATURE " is "
                   FAHRENHEIT-TEMPERATURE.
           PERFORM PAUSE-KEY.
    
    
       TOTAL-SALES1.
           PERFORM CLEAR-SCREEN
           DISPLAY "What is the Salesman number? " WITH NO ADVANCING
           ACCEPT SALESPERSON-NUMBER.
           DISPLAY "What is the Salesman name? " WITH NO ADVANCING
           ACCEPT SALESPERSON-NAME.
           DISPLAY "Unit Sold?" WITH NO ADVANCING
           ACCEPT UNITS-SOLD.
           DISPLAY "Unit Price? " WITH NO ADVANCING
           ACCEPT UNIT-PRICE.
   
           COMPUTE SEQUENCE-SALES-TOTAL = UNITS-SOLD * UNIT-PRICE.
   
           MOVE SEQUENCE-SALES-TOTAL TO DISPLAY-SEQUENCE-SALES-TOTAL.
   
           DISPLAY FUNCTION TRIM(SALESPERSON-NAME)
                   " Total Sales is: "
                   FUNCTION TRIM(DISPLAY-SEQUENCE-SALES-TOTAL).
           PERFORM PAUSE-KEY.
    
    
       OVERALL-AVE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Prelim Grade? " WITH NO ADVANCING
           ACCEPT AVERAGE-PRELIM-GRADE.
           DISPLAY "Midterms Grade? " WITH NO ADVANCING
           ACCEPT AVERAGE-MIDTERM-GRADE.
           DISPLAY "Finals Grade? " WITH NO ADVANCING
           ACCEPT AVERAGE-FINAL-GRADE.
           COMPUTE AVERAGE-GRADE-TOTAL =
               AVERAGE-PRELIM-GRADE + AVERAGE-MIDTERM-GRADE
               + AVERAGE-FINAL-GRADE.
           COMPUTE OVERALL-AVERAGE = AVERAGE-GRADE-TOTAL / 3.
           DISPLAY "The overall Average is: " OVERALL-AVERAGE.
           PERFORM PAUSE-KEY.
    
    
       CUBE-SQUARE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT CUBE-SQUARE-INPUT.
           COMPUTE SQUARE-VALUE = CUBE-SQUARE-INPUT
               * CUBE-SQUARE-INPUT.
           COMPUTE CUBE-VALUE = CUBE-SQUARE-INPUT
               * CUBE-SQUARE-INPUT * CUBE-SQUARE-INPUT.
           MOVE CUBE-SQUARE-INPUT TO DISPLAY-CUBE-SQUARE-INPUT.
           MOVE SQUARE-VALUE TO DISPLAY-SQUARE-VALUE.
           MOVE CUBE-VALUE TO DISPLAY-CUBE-VALUE.
           DISPLAY "The Squared of "
                   FUNCTION TRIM(DISPLAY-CUBE-SQUARE-INPUT)
                   " is "
                   FUNCTION TRIM(DISPLAY-SQUARE-VALUE).
           DISPLAY "The Cubed   of "
                   FUNCTION TRIM(DISPLAY-CUBE-SQUARE-INPUT)
                   " is "
                   FUNCTION TRIM(DISPLAY-CUBE-VALUE).
           PERFORM PAUSE-KEY.


       RECTANGLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Length: " WITH NO ADVANCING
           ACCEPT RECTANGLE-LENGTH.
           DISPLAY "Input Width: " WITH NO ADVANCING
           ACCEPT RECTANGLE-WIDTH.
           COMPUTE RECTANGLE-AREA = RECTANGLE-LENGTH * RECTANGLE-WIDTH.
           COMPUTE RECTANGLE-PERIMETER =
               2 * (RECTANGLE-LENGTH + RECTANGLE-WIDTH).
           MOVE RECTANGLE-AREA TO DISPLAY-RECTANGLE-AREA.
           MOVE RECTANGLE-PERIMETER TO DISPLAY-RECTANGLE-PERIMETER.
           DISPLAY "The object area is     : "
                   FUNCTION TRIM(DISPLAY-RECTANGLE-AREA).
           DISPLAY "The object Perimeter is: "
                   FUNCTION TRIM(DISPLAY-RECTANGLE-PERIMETER).
           PERFORM PAUSE-KEY.


       CIRCLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Diameter: "
           ACCEPT CIRCLE-DIAMETER.

           COMPUTE CIRCLE-AREA =
               3.1416 * (CIRCLE-DIAMETER / 2) ** 2.
           COMPUTE CIRCLE-CIRCUMFERENCE = 3.1416 * CIRCLE-DIAMETER.

           MOVE CIRCLE-AREA TO DISPLAY-CIRCLE-AREA.
           MOVE CIRCLE-CIRCUMFERENCE TO DISPLAY-CIRCLE-CIRCUMFERENCE.

           DISPLAY "The Circle area is         : "
                   FUNCTION TRIM(DISPLAY-CIRCLE-AREA).
           DISPLAY "The Circle Circumference is: "
                   FUNCTION TRIM(DISPLAY-CIRCLE-CIRCUMFERENCE).
           PERFORM PAUSE-KEY.
    
    
       TRIANGLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Base: " WITH NO ADVANCING
           ACCEPT TRIANGLE-BASE.
           DISPLAY "Input Height: " WITH NO ADVANCING
           ACCEPT TRIANGLE-HEIGHT.
           DISPLAY "Input Side 1: " WITH NO ADVANCING
           ACCEPT TRIANGLE-SIDE-ONE.
           DISPLAY "Input Side 2: " WITH NO ADVANCING
           ACCEPT TRIANGLE-SIDE-TWO.
           DISPLAY "Input Side 3: " WITH NO ADVANCING
           ACCEPT TRIANGLE-SIDE-THREE.

           COMPUTE TRIANGLE-AREA = 1 / 2 * TRIANGLE-BASE
               * TRIANGLE-HEIGHT.
           COMPUTE TRIANGLE-PERIMETER = TRIANGLE-SIDE-ONE
               + TRIANGLE-SIDE-TWO + TRIANGLE-SIDE-THREE.

           MOVE TRIANGLE-AREA TO DISPLAY-TRIANGLE-AREA.
           MOVE TRIANGLE-PERIMETER TO DISPLAY-TRIANGLE-PERIMETER.

           DISPLAY "The Triangle area is     : "
                   FUNCTION TRIM(DISPLAY-TRIANGLE-AREA).
           DISPLAY "The Triangle perimeter is: "
                   FUNCTION TRIM(DISPLAY-TRIANGLE-PERIMETER).
           PERFORM PAUSE-KEY.           PERFORM PAUSE-KEY.

       SELECTION-MENU.
           MOVE 0 TO MENU-CHOICE
           PERFORM CLEAR-SCREEN.
           PERFORM UNTIL MENU-CHOICE = 11
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
                   DISPLAY "11 - RETURN"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT MENU-CHOICE

                   EVALUATE MENU-CHOICE
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
           END-PERFORM.

       GRADE-P-F.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter PRELIM Grade: " WITH NO ADVANCING
           ACCEPT GRADE-PRELIM.

           DISPLAY "Enter MIDTERM Grade: " WITH NO ADVANCING
           ACCEPT GRADE-MIDTERM.

           DISPLAY "Enter FINALS Grade: " WITH NO ADVANCING
           ACCEPT GRADE-FINAL.

           COMPUTE GRADE-AVERAGE =
               (GRADE-PRELIM + GRADE-MIDTERM + GRADE-FINAL) / 3.

           MOVE GRADE-AVERAGE TO DISPLAY-GRADE-AVERAGE

           DISPLAY "Average Grade: "
                   FUNCTION TRIM(DISPLAY-GRADE-AVERAGE).

           IF
              GRADE-AVERAGE >= 75
              DISPLAY "PASSED!"

           ELSE
              DISPLAY "FAILED"
           END-IF.
           PERFORM PAUSE-KEY.


       POSITIVE-NEGATIVE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter a Number: " WITH NO ADVANCING
           ACCEPT SIGN-CHECK-NUMBER.

           MOVE SIGN-CHECK-NUMBER TO DISPLAY-SIGN-CHECK-NUMBER.
    
           IF SIGN-CHECK-NUMBER > 0
              DISPLAY FUNCTION TRIM(DISPLAY-SIGN-CHECK-NUMBER)
                      " is positive."

           ELSE
              IF SIGN-CHECK-NUMBER < 0
                 DISPLAY FUNCTION TRIM(DISPLAY-SIGN-CHECK-NUMBER)
                         " is negative."

              ELSE
                 IF SIGN-CHECK-NUMBER = 0
                    DISPLAY FUNCTION TRIM(DISPLAY-SIGN-CHECK-NUMBER)
                            " is neutral."

                 END-IF.
           PERFORM PAUSE-KEY.


       ODD-EVEN.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT PARITY-CHECK-NUMBER.

           MOVE PARITY-CHECK-NUMBER TO DISPLAY-PARITY-CHECK-NUMBER.

           IF PARITY-CHECK-NUMBER = 0
              DISPLAY FUNCTION TRIM(DISPLAY-PARITY-CHECK-NUMBER)
                      " is neutral"
           ELSE
              IF FUNCTION MOD(PARITY-CHECK-NUMBER, 2) = 0
                 DISPLAY FUNCTION TRIM(DISPLAY-PARITY-CHECK-NUMBER)
                         " is even"
              ELSE
                 DISPLAY FUNCTION TRIM(DISPLAY-PARITY-CHECK-NUMBER)
                         " is odd"
              END-IF.

           PERFORM PAUSE-KEY.


       LARGEST.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input First Number: " WITH NO ADVANCING
           ACCEPT LARGEST-FIRST-NUMBER.
           MOVE LARGEST-FIRST-NUMBER TO DISPLAY-LARGEST-FIRST-NUMBER.

           DISPLAY "Input Second Number: " WITH NO ADVANCING
           ACCEPT LARGEST-SECOND-NUMBER.
           MOVE LARGEST-SECOND-NUMBER TO DISPLAY-LARGEST-SECOND-NUMBER.

           IF LARGEST-FIRST-NUMBER = LARGEST-SECOND-NUMBER
              DISPLAY FUNCTION TRIM(DISPLAY-LARGEST-FIRST-NUMBER)
                      " is equal to "
                      FUNCTION TRIM(DISPLAY-LARGEST-SECOND-NUMBER)

           ELSE
              IF LARGEST-FIRST-NUMBER > LARGEST-SECOND-NUMBER
                 DISPLAY FUNCTION TRIM(DISPLAY-LARGEST-FIRST-NUMBER)
                         " is the larger number than "
                         FUNCTION TRIM(DISPLAY-LARGEST-SECOND-NUMBER)

              ELSE
                 DISPLAY FUNCTION TRIM(DISPLAY-LARGEST-SECOND-NUMBER)
                         " is the larger number than "
                         FUNCTION TRIM(DISPLAY-LARGEST-FIRST-NUMBER)

              END-IF.
           PERFORM PAUSE-KEY.


       SMALLEST.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first number: " WITH NO ADVANCING
           ACCEPT SMALLEST-FIRST-NUMBER.

           DISPLAY "Input Second number: " WITH NO ADVANCING
           ACCEPT SMALLEST-SECOND-NUMBER.

           DISPLAY "Input Third number: " WITH NO ADVANCING
           ACCEPT SMALLEST-THIRD-NUMBER.

           IF SMALLEST-FIRST-NUMBER < SMALLEST-SECOND-NUMBER
              IF SMALLEST-FIRST-NUMBER < SMALLEST-THIRD-NUMBER
                 MOVE SMALLEST-FIRST-NUMBER TO SMALLEST-RESULT
              ELSE
                 MOVE SMALLEST-THIRD-NUMBER TO SMALLEST-RESULT
              END-IF
           ELSE
              IF SMALLEST-SECOND-NUMBER < SMALLEST-THIRD-NUMBER
                 MOVE SMALLEST-SECOND-NUMBER TO SMALLEST-RESULT
              ELSE
                 MOVE SMALLEST-THIRD-NUMBER TO SMALLEST-RESULT
              END-IF
           END-IF.
           
           MOVE SMALLEST-RESULT TO DISPLAY-SMALLEST-RESULT.
           DISPLAY "Smallest Number is: "
                   FUNCTION TRIM(DISPLAY-SMALLEST-RESULT).
           PERFORM PAUSE-KEY.


       EQUI-GRADE.
           PERFORM CLEAR-SCREEN
           DISPLAY "TESTING"
           PERFORM PAUSE-KEY.


       TEMPERATURE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Temperature: " WITH NO ADVANCING
           ACCEPT TEMPERATURE-CELSIUS.

           IF TEMPERATURE-CELSIUS < 0
              DISPLAY "FREEZING WEATHER"

           ELSE
              IF TEMPERATURE-CELSIUS >= 0
                 IF TEMPERATURE-CELSIUS < 10
                    DISPLAY "VERY COLD WEATHER"

                 ELSE
                    IF TEMPERATURE-CELSIUS >= 10
                       IF TEMPERATURE-CELSIUS < 20
                          DISPLAY "COLD WEATHER"

                       ELSE
                          IF TEMPERATURE-CELSIUS >= 20
                             IF TEMPERATURE-CELSIUS < 30
                                DISPLAY "NORMAL WEATHER"

                             ELSE
                                IF TEMPERATURE-CELSIUS >= 30
                                   IF TEMPERATURE-CELSIUS < 40
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
           ACCEPT COMMISSION-SALES-TOTAL.

           IF COMMISSION-SALES-TOTAL <= 15000
              COMPUTE SALES-COMMISSION =
                  COMMISSION-SALES-TOTAL * 0.15
              DISPLAY "Commission base is  .15"

           ELSE
              IF COMMISSION-SALES-TOTAL <= 20000
                 COMPUTE SALES-COMMISSION =
                     COMMISSION-SALES-TOTAL * 0.20
                 DISPLAY "Commission base is  .20"

              ELSE
                 IF COMMISSION-SALES-TOTAL <= 25000
                    COMPUTE SALES-COMMISSION =
                        COMMISSION-SALES-TOTAL * 0.25
                    DISPLAY "Commission base is  .25"

                 ELSE
                    IF COMMISSION-SALES-TOTAL <= 30000
                       COMPUTE SALES-COMMISSION =
                           COMMISSION-SALES-TOTAL * 0.30
                       DISPLAY "Commission base is  .30"

                    ELSE
                       COMPUTE SALES-COMMISSION =
                           COMMISSION-SALES-TOTAL * 0.40
                       DISPLAY "Commission base is  .40"

                    END-IF.
           
           MOVE COMMISSION-SALES-TOTAL
               TO DISPLAY-COMMISSION-SALES-TOTAL.
           MOVE SALES-COMMISSION TO DISPLAY-SALES-COMMISSION.
           DISPLAY "Total Sales is     : "
                   FUNCTION TRIM(DISPLAY-COMMISSION-SALES-TOTAL).
           DISPLAY "Sales Commission is: "
                   FUNCTION TRIM(DISPLAY-SALES-COMMISSION).
           PERFORM PAUSE-KEY.

       DAY-NAME.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input the day number (1-7): " WITH NO ADVANCING
           ACCEPT WEEKDAY-NUMBER.

           IF WEEKDAY-NUMBER = 1
              DISPLAY "Today is Monday"
           ELSE
              IF WEEKDAY-NUMBER = 2
                 DISPLAY "Today is Tuesday"
              ELSE
                 IF WEEKDAY-NUMBER = 3
                    DISPLAY "Today is Wednesday"
                 ELSE
                    IF WEEKDAY-NUMBER = 4
                       DISPLAY "Today is Thursday"
                    ELSE
                       IF WEEKDAY-NUMBER = 5
                          DISPLAY "Today is Friday"
                       ELSE
                          IF WEEKDAY-NUMBER = 6
                             DISPLAY "Today is Saturday"
                          ELSE
                             IF WEEKDAY-NUMBER = 7
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
           ACCEPT ALPHABET-LETTER.

           IF (ALPHABET-LETTER >= "0" AND ALPHABET-LETTER <= "9")
              DISPLAY "INVALID INPUT"

           ELSE
              IF ALPHABET-LETTER = "A" OR ALPHABET-LETTER = "a"
                 DISPLAY "VOWEL"

              ELSE
                 IF ALPHABET-LETTER = "E" OR ALPHABET-LETTER = "e"
                    DISPLAY "VOWEL"

                 ELSE
                    IF ALPHABET-LETTER = "I" OR ALPHABET-LETTER = "i"
                       DISPLAY "VOWEL"

                    ELSE
                       IF ALPHABET-LETTER = "O" OR ALPHABET-LETTER = "o"
                          DISPLAY "VOWEL"

                       ELSE
                          IF ALPHABET-LETTER = "U"
                             OR ALPHABET-LETTER = "u"
                             DISPLAY "VOWEL"

                          ELSE
                             IF (ALPHABET-LETTER >= "A"
                                 AND ALPHABET-LETTER <= "Z")
                                OR (ALPHABET-LETTER >= "a"
                                    AND ALPHABET-LETTER <= "z")
                                DISPLAY "CONSONANT"

                             ELSE
                                DISPLAY "INVALID INPUT"

                             END-IF.
           PERFORM PAUSE-KEY.

       LOOP-MENU.
           PERFORM CLEAR-SCREEN.
           MOVE 0 TO MENU-CHOICE
           PERFORM UNTIL MENU-CHOICE = 11
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
                   DISPLAY "11 - RETURN"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT MENU-CHOICE

                   EVALUATE MENU-CHOICE
                   WHEN 1
                        PERFORM LOOP-PRINT-NAME
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
           END-PERFORM.

       LOOP-PRINT-NAME.
           PERFORM CLEAR-SCREEN
           PERFORM 5 TIMES
                   DISPLAY "DION"
           END-PERFORM.

           PERFORM PAUSE-KEY.


       PRINT-NUM.
           PERFORM CLEAR-SCREEN
           MOVE 1 TO NUMBER-LOOP-COUNTER
           
           PERFORM UNTIL NUMBER-LOOP-COUNTER > 5
                   DISPLAY NUMBER-LOOP-COUNTER
                   ADD 1 TO NUMBER-LOOP-COUNTER
           END-PERFORM. 
           PERFORM PAUSE-KEY.


       PRINT-EVEN-NUM.
           PERFORM CLEAR-SCREEN
           MOVE 1 TO EVEN-NUMBER-COUNTER.

           DISPLAY "Input a last number: " WITH NO ADVANCING
           ACCEPT EVEN-NUMBER-LIMIT.

           PERFORM UNTIL EVEN-NUMBER-COUNTER > EVEN-NUMBER-LIMIT
                   IF FUNCTION MOD(EVEN-NUMBER-COUNTER, 2) = 0
                      MOVE EVEN-NUMBER-COUNTER TO DISPLAY-EVEN-NUMBER
                      DISPLAY FUNCTION TRIM(DISPLAY-EVEN-NUMBER)
                   END-IF
                   ADD 1 TO EVEN-NUMBER-COUNTER
                 
           END-PERFORM.
           PERFORM PAUSE-KEY.


       PRINT-EVEN-RANGE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT RANGE-LOWER-BOUND.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT RANGE-UPPER-BOUND.
           

           PERFORM UNTIL RANGE-LOWER-BOUND > RANGE-UPPER-BOUND
                   IF FUNCTION MOD(RANGE-LOWER-BOUND, 2) = 0
                      MOVE RANGE-LOWER-BOUND
                          TO DISPLAY-RANGE-LOWER-BOUND
                      DISPLAY FUNCTION TRIM(DISPLAY-RANGE-LOWER-BOUND)
                   END-IF
                   ADD 1 TO RANGE-LOWER-BOUND

           END-PERFORM.
           PERFORM PAUSE-KEY.


       PRINT-ODD-RANGE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input first range: " WITH NO ADVANCING
           ACCEPT ODD-RANGE-LOWER-BOUND.

           DISPLAY "Input last range: " WITH NO ADVANCING
           ACCEPT ODD-RANGE-UPPER-BOUND.

           MOVE ODD-RANGE-LOWER-BOUND TO ODD-RANGE-COUNTER.
           MOVE 0 TO ODD-RANGE-SUM.
           DISPLAY "The sum of odd numbers: " WITH NO ADVANCING
           PERFORM UNTIL ODD-RANGE-COUNTER > ODD-RANGE-UPPER-BOUND
                   IF FUNCTION MOD(ODD-RANGE-COUNTER, 2) = 1
                      COMPUTE ODD-RANGE-SUM =
                          ODD-RANGE-SUM + ODD-RANGE-COUNTER
                      MOVE ODD-RANGE-COUNTER
                          TO DISPLAY-ODD-RANGE-COUNTER
                      DISPLAY FUNCTION TRIM(DISPLAY-ODD-RANGE-COUNTER)
                              " " WITH NO ADVANCING
                   END-IF
                   ADD 1 TO ODD-RANGE-COUNTER
           END-PERFORM.
           
           MOVE ODD-RANGE-SUM TO DISPLAY-ODD-RANGE-SUM.
           DISPLAY "is: "
                   FUNCTION TRIM(DISPLAY-ODD-RANGE-SUM).
           PERFORM PAUSE-KEY.


       FACTORIAL-1.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT FACTORIAL-INPUT.

           MOVE 1 TO FACTORIAL-RESULT FACTORIAL-COUNTER.
           PERFORM UNTIL FACTORIAL-COUNTER > FACTORIAL-INPUT
                   COMPUTE FACTORIAL-RESULT =
                       FACTORIAL-RESULT * FACTORIAL-COUNTER
                   ADD 1 TO FACTORIAL-COUNTER
           END-PERFORM.
           
           MOVE FACTORIAL-RESULT TO DISPLAY-FACTORIAL-RESULT.
           MOVE FACTORIAL-INPUT TO DISPLAY-FACTORIAL-INPUT.
           DISPLAY "Factorial of "
                   FUNCTION TRIM(DISPLAY-FACTORIAL-INPUT)
                   " is "
                   FUNCTION TRIM(DISPLAY-FACTORIAL-RESULT).
           PERFORM PAUSE-KEY.


       DIGITS.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT DIGIT-SUM-INPUT.

           MOVE 0 TO DIGIT-SUM-TOTAL.
           PERFORM UNTIL DIGIT-SUM-INPUT <= 0
                   COMPUTE CURRENT-DIGIT =
                       FUNCTION MOD(DIGIT-SUM-INPUT, 10)
                   COMPUTE DIGIT-SUM-TOTAL =
                       DIGIT-SUM-TOTAL + CURRENT-DIGIT
                   COMPUTE DIGIT-SUM-INPUT = DIGIT-SUM-INPUT / 10
           END-PERFORM.
           
           MOVE DIGIT-SUM-TOTAL TO DISPLAY-DIGIT-SUM.
           DISPLAY "Digit sum is: "
                   FUNCTION TRIM(DISPLAY-DIGIT-SUM).
           PERFORM PAUSE-KEY.


       FIBONACCI.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT FIBONACCI-TERM-COUNT.

           MOVE 0 TO FIBONACCI-FIRST-TERM.
           MOVE 1 TO FIBONACCI-SECOND-TERM.
           MOVE 1 TO FIBONACCI-COUNTER.

           MOVE FIBONACCI-FIRST-TERM
               TO DISPLAY-FIBONACCI-FIRST-TERM.
           MOVE FIBONACCI-SECOND-TERM
               TO DISPLAY-FIBONACCI-SECOND-TERM.
           
           DISPLAY FUNCTION TRIM(DISPLAY-FIBONACCI-FIRST-TERM)
               WITH NO ADVANCING
           DISPLAY " " FUNCTION TRIM(DISPLAY-FIBONACCI-SECOND-TERM)
               WITH NO ADVANCING

           ADD 2 TO FIBONACCI-COUNTER.

           PERFORM UNTIL FIBONACCI-COUNTER > FIBONACCI-TERM-COUNT

                   COMPUTE FIBONACCI-NEXT-TERM =
                       FIBONACCI-FIRST-TERM + FIBONACCI-SECOND-TERM
                    
                   MOVE FIBONACCI-NEXT-TERM
                       TO DISPLAY-FIBONACCI-NEXT-TERM
                   DISPLAY " "
                      FUNCTION TRIM(DISPLAY-FIBONACCI-NEXT-TERM)
                      WITH NO ADVANCING

                   MOVE FIBONACCI-SECOND-TERM
                       TO FIBONACCI-FIRST-TERM
                   MOVE FIBONACCI-NEXT-TERM
                       TO FIBONACCI-SECOND-TERM

                   ADD 1 TO FIBONACCI-COUNTER

           END-PERFORM.

           DISPLAY " ".
           PERFORM PAUSE-KEY.


       PRIME-NUM.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Number: " WITH NO ADVANCING
           ACCEPT PRIME-CANDIDATE.

           IF PRIME-CANDIDATE < 2
              DISPLAY "Not prime number"
           ELSE
              MOVE 2 TO PRIME-DIVISOR
              MOVE 'Y' TO PRIME-STATUS

              PERFORM UNTIL PRIME-DIVISOR > PRIME-CANDIDATE - 1
                      IF FUNCTION MOD(PRIME-CANDIDATE,
                                      PRIME-DIVISOR) = 0
                         MOVE 'N' TO PRIME-STATUS
                         EXIT PERFORM
                      END-IF

                      ADD 1 TO PRIME-DIVISOR
              END-PERFORM

              IF PRIME-STATUS = 'Y'
                 DISPLAY "Prime number"
              ELSE
                 DISPLAY "Not prime number"
              END-IF
           END-IF.
           PERFORM PAUSE-KEY.


       DEC-BINARY.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Decimal Number: " WITH NO ADVANCING
           ACCEPT BINARY-INPUT-NUMBER.

           MOVE 1 TO BINARY-POSITION.
           MOVE 0 TO BINARY-CONVERTED-VALUE.

           PERFORM UNTIL BINARY-INPUT-NUMBER = 0
                   COMPUTE BINARY-REMAINDER =
                       FUNCTION MOD(BINARY-INPUT-NUMBER,
                      2)
                   COMPUTE BINARY-CONVERTED-VALUE =
                       BINARY-CONVERTED-VALUE +
                       (BINARY-REMAINDER * BINARY-POSITION)
                   COMPUTE BINARY-POSITION = BINARY-POSITION * 10
                   COMPUTE BINARY-INPUT-NUMBER =
                       BINARY-INPUT-NUMBER / 2
           END-PERFORM.

           MOVE BINARY-CONVERTED-VALUE TO DISPLAY-BINARY-RESULT.
           DISPLAY FUNCTION TRIM(DISPLAY-BINARY-RESULT).
           PERFORM PAUSE-KEY.
       
       PAUSE-KEY.
           DISPLAY "Input any any key to continue..." WITH NO ADVANCING
           ACCEPT PAUSE-KEY-INPUT.
           
       CLEAR-SCREEN.
           CALL "SYSTEM" USING CLEAR-SCREEN-COMMAND.
