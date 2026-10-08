       IDENTIFICATION DIVISION.
       PROGRAM-ID. GCD-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  A              PIC 9(5) VALUE 0.
       01  B              PIC 9(5) VALUE 0.
       01  ORIGINAL-A     PIC ZZZ9 VALUE 0.
       01  ORIGINAL-B     PIC ZZZ9 VALUE 0.
       01  REMAINDER-V    PIC 9(5) VALUE 0.
       01  LOOP-COUNT     PIC 9(5) VALUE 0.


       01 DISP-A          PIC ZZZZ9.
       01 DISP-B          PIC ZZZZ9.
       01 DISP-R          PIC ZZZZ9.
       01 DISP-LC         PIC ZZZZ9.
       01 GCD             PIC ZZZZ9.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           PERFORM UNTIL A > 0 AND B > 0
               DISPLAY "Input 1st Positive whole number: " 
               WITH NO ADVANCING
               ACCEPT A
               DISPLAY "Input 2nd Positive whole number: "
               WITH NO ADVANCING
               ACCEPT B

               IF A <= 0 OR B <= 0
                   DISPLAY "INVALID INPUT."
                   DISPLAY "BOTH NUMBERS MUST BE GREATER THAN 0."
               END-IF
           END-PERFORM

           MOVE A TO ORIGINAL-A
           MOVE B TO ORIGINAL-B

           DISPLAY " ".
           DISPLAY "ORIGINAL A: " FUNCTION TRIM(ORIGINAL-A).
           DISPLAY "ORIGINAL B: " FUNCTION TRIM(ORIGINAL-B).
           DISPLAY " ".

           PERFORM UNTIL B = 0
               COMPUTE REMAINDER-V = FUNCTION MOD(A B)

               ADD 1 TO LOOP-COUNT

               MOVE A TO DISP-A
               MOVE B TO DISP-B
               MOVE REMAINDER-V TO DISP-R
               MOVE LOOP-COUNT TO DISP-LC
              
               DISPLAY "ITERATION: " FUNCTION TRIM(DISP-LC)
               DISPLAY "A = " FUNCTION TRIM(DISP-A)
               DISPLAY "B = " FUNCTION TRIM(DISP-B)
               DISPLAY "REMAINDER = " FUNCTION TRIM(DISP-R)
               DISPLAY " "

               MOVE B TO A
               MOVE REMAINDER-V TO B
           END-PERFORM

           MOVE A TO GCD.

           DISPLAY "ORIGINAL A: " FUNCTION TRIM(ORIGINAL-A).
           DISPLAY "ORIGINAL B: " FUNCTION TRIM(ORIGINAL-B).
           DISPLAY "GCD: " FUNCTION TRIM(GCD).
           DISPLAY "TOTAL LOOP ITERATIONS: " FUNCTION TRIM(DISP-LC).


           STOP RUN. 

           