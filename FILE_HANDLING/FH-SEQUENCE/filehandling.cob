      *>*****************************************************************
      *> Purpose: Compute average of 3 grades and store it in a file
      *> Compile: cobc -x -free filehandlingAverage.cob
      *>*****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AVERAGE.
       AUTHOR. Programmer.
       SECURITY. PRIVATE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRADE-FILE
               ASSIGN TO "Average.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  GRADE-FILE.
       01  GRADE-REC       PIC X(40).
 
       WORKING-STORAGE SECTION.
       01  PRELIM          PIC 9(3).
       01  MIDTERM         PIC 9(3).
       01  FINALS          PIC 9(3).
       01  AVE             PIC 9(3)V9(2).
       01  SCORE-OUT       PIC ZZ9.
       01  AVERAGE         PIC ZZ9.99.
       01  OUT-LINE        PIC X(40).

         PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY " AVERAGE ".
           DISPLAY "Prelim Score: " WITH NO ADVANCING
           ACCEPT PRELIM.
           DISPLAY "Midterm Score: " WITH NO ADVANCING
           ACCEPT MIDTERM.
           DISPLAY "Finals Score: " WITH NO ADVANCING
           ACCEPT FINALS.
 
           COMPUTE AVE = (PRELIM + MIDTERM + FINALS) / 3.
           MOVE AVE TO AVERAGE.
           DISPLAY "AVERAGE = " AVERAGE.
 
           OPEN OUTPUT GRADE-FILE.
           
            MOVE PRELIM TO SCORE-OUT.
           MOVE SPACES TO OUT-LINE.
           STRING "Prelim:  " FUNCTION TRIM(SCORE-OUT)
               DELIMITED BY SIZE INTO OUT-LINE.
           WRITE GRADE-REC FROM OUT-LINE.
 
           MOVE MIDTERM TO SCORE-OUT.
           MOVE SPACES TO OUT-LINE.
           STRING "Midterm: " FUNCTION TRIM(SCORE-OUT)
               DELIMITED BY SIZE INTO OUT-LINE.
           WRITE GRADE-REC FROM OUT-LINE.
 
           MOVE FINALS TO SCORE-OUT.
           MOVE SPACES TO OUT-LINE.
           STRING "Finals:  " FUNCTION TRIM(SCORE-OUT)
               DELIMITED BY SIZE INTO OUT-LINE.
           WRITE GRADE-REC FROM OUT-LINE.
 
           MOVE SPACES TO OUT-LINE.
           WRITE GRADE-REC FROM OUT-LINE.
 
           MOVE SPACES TO OUT-LINE.
           STRING "Average: " FUNCTION TRIM(AVERAGE)
               DELIMITED BY SIZE INTO OUT-LINE.
           WRITE GRADE-REC FROM OUT-LINE.
 
           CLOSE GRADE-FILE.
           STOP RUN.
       END PROGRAM AVERAGE.
 