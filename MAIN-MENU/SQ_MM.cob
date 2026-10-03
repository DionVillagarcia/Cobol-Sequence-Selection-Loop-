       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQUENCE-MAIN-MENU.
       AUTHOR. DION VILLAGARCIA.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHOICE              PIC 99         VALUE 0.
       01 KEY1                PIC X.
       01 COMMAND             PIC X(5)       VALUE "clear".

       *>swap variables
       01 A                   PIC 99.
       01 B                   PIC 99.
       01 TEMP                PIC 99.

       01 DISP-A              PIC Z9.
       01 DISP-B              PIC Z9.

       *>Calculation variables
       01 N1                  PIC 9(2).
       01 N2                  PIC 9(2).

       01 SUMOF               PIC 9(3).
       01 DIFFOF              PIC 9(3).
       01 PRODOF              PIC 9(3).
       01 QUOOF               PIC 9(3)V99.

       01 DISP-N1             PIC ZZ9.
       01 DISP-N2             PIC ZZ9.
       01 DISP-SUM            PIC ZZZ9.
       01 DISP-DIFF           PIC ZZ9.
       01 DISP-PROD           PIC ZZ9.
       01 DISP-QUO            PIC ZZ9.99.

       *>Fahrenheit variables
       01 F                   PIC 9(2)V99.
       01 CELSIUS             PIC 9(2)V99.

       *> Total Sales variables
       01 SALESMAN_NUMBER     PIC 9(11).
       01 SALESMAN_NAME       PIC A(20).
       01 UNIT_SOLD           PIC 9(3).
       01 UNIT_PRICE          PIC 9(6)V99.
       01 TOTAL_SALES         PIC 9(6)V99.

       01 DISP-TS             PIC ZZZZZZ9.99.     

       *>Average variable
       01 PRELIM              PIC 9(3).
       01 MIDTERMS            PIC 9(3).
       01 FINALS              PIC 9(3).
       01 SUMOF-1             PIC 9(3).
       01 AVE                 PIC 9(2)V99.

       *>Square and cube variable
       01 SQUARED             PIC 9(5).
       01 CUBED               PIC 9(5).
       01 NUM                 PIC 9(5).

       01 DISP-NUM            PIC ZZZZ9.
       01 DISP-SQUARED        PIC ZZZZ9.
       01 DISP-CUBED          PIC ZZZZ9.

       *>Rectangle variable
       01 LENGTH1             PIC 9(3).
       01 WIDTH               PIC 9(3).
       01 AREA1               PIC 9(5).
       01 PERIMETER           PIC 9(5).

       01 DISP-AREA1          PIC ZZZZ9.
       01 DISP-PERIMETER      PIC ZZZZ9.

       *>Circle Variable
       01 DIAMETER            PIC 9(3)V99.
       01 CIRCUMFERENCE       PIC 9(3)V99.
       01 AREA1-1             PIC 9(5)V99.

       01 DISP-AREA1-1        PIC ZZZZ9.99.
       01 DISP-CIRCUMFERENCE  PIC ZZZZ9.99.

       *>Triangle variable

       01 BASE                PIC 9(3)V99.
       01 HEIGHT              PIC 9(3)V99.
       01 SIDE1               PIC 9(5)V99.
       01 SIDE2               PIC 9(5)V99.
       01 SIDE3               PIC 9(5)V99.
       01 AREA1-2             PIC 9(5)V99.
       01 PERIMETER-1         PIC 9(5)V99.

       01 DISP-AREA1-2        PIC ZZZZ9.99.
       01 DISP-PERIMETER-1    PIC ZZZZ9.99.

       PROCEDURE DIVISION.

       MAIN-MENU.
           PERFORM UNTIL CHOICE = 11
                   PERFORM CLEAR-SCREEN
                   DISPLAY "MAIN MENU"
                   DISPLAY "" 
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
                   DISPLAY "11 - Exit"
                   DISPLAY "Enter your choice: " WITH NO ADVANCING
                   ACCEPT CHOICE

                   EVALUATE CHOICE
                   WHEN 1
                        PERFORM PRINT-NAME
                   WHEN 2
                        PERFORM SWAP
                   WHEN 3 
                        PERFORM CALCULATION
                   WHEN 4
                        PERFORM FAHRENHEIT
                   WHEN 5
                        PERFORM TOTAL-SALES
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

           END-PERFORM

           STOP RUN.

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
           MOVE 10 TO A.
           MOVE 5 TO B.
           DISPLAY "BEFORE: " A " & " B.
           MOVE A TO TEMP.
           MOVE B TO A.
           MOVE TEMP TO B.
           MOVE A TO DISP-A.
           MOVE B TO DISP-B.
           DISPLAY "NOW:     "
                   FUNCTION TRIM(DISP-A)
                   " & "
                   FUNCTION TRIM(DISP-B).
           PERFORM PAUSE-KEY.
    
    
       CALCULATION.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input number: " WITH NO ADVANCING
           ACCEPT N1.
           DISPLAY "Input number: " WITH NO ADVANCING
           ACCEPT N2.
           ADD N1 TO N2 GIVING SUMOF.
           SUBTRACT N1 FROM N2 GIVING DIFFOF.
           MULTIPLY N1 BY N2 GIVING PRODOF.
           DIVIDE N1 BY N2 GIVING QUOOF.
   
           MOVE N1 TO DISP-N1.
           MOVE N2 TO DISP-N2.
           MOVE SUMOF TO DISP-SUM.
           MOVE DIFFOF TO DISP-DIFF.
           MOVE PRODOF TO DISP-PROD.
           MOVE QUOOF TO DISP-QUO.

           DISPLAY "The Sum of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-SUM).

           DISPLAY "The Difference of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-DIFF).

           DISPLAY "The Product of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-PROD).

           DISPLAY "The Quotient of the "
                   FUNCTION TRIM(DISP-N1)
                   " and "
                   FUNCTION TRIM(DISP-N2)
                   " is "
                   FUNCTION TRIM(DISP-QUO).
           PERFORM PAUSE-KEY.
    
    
       FAHRENHEIT.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Celsius: " WITH NO ADVANCING
           ACCEPT CELSIUS.
           COMPUTE F = CELSIUS * 9 / 5 + 32.
           DISPLAY "The converted Celsius of " CELSIUS " is " F.
           PERFORM PAUSE-KEY.
    
    
       TOTAL-SALES.
           PERFORM CLEAR-SCREEN
           DISPLAY "What is the Salesman number? " WITH NO ADVANCING
           ACCEPT SALESMAN_NUMBER.
           DISPLAY "What is the Salesman name? " WITH NO ADVANCING
           ACCEPT SALESMAN_NAME.
           DISPLAY "Unit Sold?" WITH NO ADVANCING
           ACCEPT UNIT_SOLD.
           DISPLAY "Unit Price? " WITH NO ADVANCING
           ACCEPT UNIT_PRICE.
   
           COMPUTE TOTAL_SALES = UNIT_SOLD * UNIT_PRICE.
   
           MOVE TOTAL_SALES TO DISP-TS.
   
           DISPLAY FUNCTION TRIM(SALESMAN_NAME)
                   " Total Sales is: "
                   FUNCTION TRIM(DISP-TS).
           PERFORM PAUSE-KEY.
    
    
       OVERALL-AVE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Prelim Grade? " WITH NO ADVANCING
           ACCEPT PRELIM.
           DISPLAY "Midterms Grade? " WITH NO ADVANCING
           ACCEPT MIDTERMS.
           DISPLAY "Finals Grade? " WITH NO ADVANCING
           ACCEPT FINALS.
           COMPUTE SUMOF-1 = PRELIM + MIDTERMS + FINALS.
           COMPUTE AVE = SUMOF-1 / 3.
           DISPLAY "The overall Average is: " AVE.
           PERFORM PAUSE-KEY.
    
    
       CUBE-SQUARE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input a number: " WITH NO ADVANCING
           ACCEPT NUM.
           COMPUTE SQUARED = NUM * NUM.
           COMPUTE CUBED = NUM * NUM * NUM.
           MOVE NUM TO DISP-NUM.
           MOVE SQUARED TO DISP-SQUARED.
           MOVE CUBED TO DISP-CUBED.
           DISPLAY "The Squared of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-SQUARED).
           DISPLAY "The Cubed   of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-CUBED).
           PERFORM PAUSE-KEY.


       RECTANGLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Length: " WITH NO ADVANCING
           ACCEPT LENGTH1.
           DISPLAY "Input Width: " WITH NO ADVANCING
           ACCEPT WIDTH.
           COMPUTE AREA1 = LENGTH1 * WIDTH.
           COMPUTE PERIMETER = 2 *(LENGTH1 + WIDTH).
           MOVE AREA1 TO DISP-AREA1.
           MOVE PERIMETER TO DISP-PERIMETER.
           DISPLAY "The object area is     : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The object Perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER).
           PERFORM PAUSE-KEY.


       CIRCLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Diameter: "
           ACCEPT DIAMETER.

           COMPUTE AREA1-1 = 3.1416 *(DIAMETER / 2) ** 2.
           COMPUTE CIRCUMFERENCE = 3.1416 * DIAMETER.

           MOVE AREA1-1 TO DISP-AREA1-1.
           MOVE CIRCUMFERENCE TO DISP-CIRCUMFERENCE.

           DISPLAY "The Circle area is         : "
                   FUNCTION TRIM(DISP-AREA1-1).
           DISPLAY "The Circle Circumference is: "
                   FUNCTION TRIM(DISP-CIRCUMFERENCE).
           PERFORM PAUSE-KEY.
    
    
       TRIANGLE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Input Base: " WITH NO ADVANCING
           ACCEPT BASE.
           DISPLAY "Input Height: " WITH NO ADVANCING
           ACCEPT HEIGHT.
           DISPLAY "Input Side 1: " WITH NO ADVANCING
           ACCEPT SIDE1.
           DISPLAY "Input Side 2: " WITH NO ADVANCING
           ACCEPT SIDE2.
           DISPLAY "Input Side 3: " WITH NO ADVANCING
           ACCEPT SIDE3.

           COMPUTE AREA1-2 = 1 / 2 * BASE * HEIGHT.
           COMPUTE PERIMETER-1 = SIDE1 + SIDE2 + SIDE3.

           MOVE AREA1-2 TO DISP-AREA1-2.
           MOVE PERIMETER-1 TO DISP-PERIMETER-1.

           DISPLAY "The Triangle area is     : "
                   FUNCTION TRIM(DISP-AREA1-2).
           DISPLAY "The Triangle perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER-1).
           PERFORM PAUSE-KEY.
    
    
       CLEAR-SCREEN.
           CALL "SYSTEM" USING COMMAND.
    
    
       PAUSE-KEY.
           DISPLAY "Press any key to continue..." WITH NO ADVANCING
           ACCEPT KEY1.