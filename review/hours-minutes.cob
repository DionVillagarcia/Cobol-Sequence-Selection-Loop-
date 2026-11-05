       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  NUMBER-TABLE.
           05  NUM PIC 9(3) OCCURS 5 TIMES.

       01  COUNTER PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           DISPLAY "===== ARRAY INPUT =====".

           PERFORM VARYING COUNTER FROM 1 BY 1
               UNTIL COUNTER > 5

               DISPLAY "ENTER NUMBER " COUNTER ": " WITH NO ADVANCING
               ACCEPT NUM(COUNTER)

           END-PERFORM.

           DISPLAY " ".
           DISPLAY "===== RESULTS =====".

           PERFORM VARYING COUNTER FROM 1 BY 1
               UNTIL COUNTER > 5

               DISPLAY "NUM(" COUNTER ") = " NUM(COUNTER)

           END-PERFORM.

           DISPLAY " ".
           DISPLAY "PROGRAM FINISHED."

           STOP RUN.
