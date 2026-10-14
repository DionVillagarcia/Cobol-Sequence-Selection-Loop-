       IDENTIFICATION DIVISION.
       PROGRAM-ID. GCD.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A            PIC 9(5) VALUE 0.
       01 B            PIC 9(5) VALUE 0.
       01 ORIG-A.      PIC ZZZZ9 VALUE 0.
       01 ORIG-B       PIC ZZZZ9 VALUE 0.
       01 REMINDER     PIC 9(5) VALUE 0.
       01 LOOP-COUNT   PIC 9(5) VALUE 0.

       01 DISP-A       PIC ZZZZ9.
       01 DISP-B       PIC ZZZZ9.
       01 DISP-R       PIC ZZZZ9.
       01 DISP-LC      PIC ZZZZ9.
       01 GCD          PIC ZZZZ9.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM UNTIL B > 0 AND A > 0
              DISPLAY "Input 1st positive number: " WITH NO ADVANCING
              ACCEPT A.
              DISPLAY "Input 2nd positive number: " WITH NO ADVANCING
              ACCEPT B.

              IF A <= 0 OR B <= 0 
                 DISPLAY "INVALID!"
                 DISPLAY "The numbers must be greater than 0"
              END-IF
           END-PERFORM

           MOVE A TO ORIG-A.
           MOVE B TO ORIG-B.

           DISPLAY " ". 
           DISPLAY FUNCTION MOD(ORIG-A) WITH NO ADVANCING
           DISPLAY FUNCTION MOD(ORIG-B) WITH NO ADVANCING
           DISPLAY " ".

           PERFORM UNTIL B = 0
              COMPUTE REMINDER = FUNCTION MOD(A B)
              




           
