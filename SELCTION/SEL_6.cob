       IDENTIFICATION DIVISION.
       PROGRAM-ID. EQUIVALENT-GRADE.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 PRELIM           PIC 9(3)V99.
       01 MIDTERMS         PIC 9(3)V99.
       01 FINALS           PIC 9(3)V99.
       01 AVERAGE-GRADE    PIC 9(3)V99.
       01 EQUIVALENT       PIC 9(4)V99.

       01 DISP-AVE         PIC ZZ9.99.
       01 DISP-EQUIVALENT  PIC ZZZ9.99.

       PROCEDURE DIVISION.

           DISPLAY "Enter Prelim Grade: " WITH NO ADVANCING
           ACCEPT PRELIM.

           DISPLAY "Enter Midterms Grade: " WITH NO ADVANCING
           ACCEPT MIDTERMS.

           DISPLAY "ENTER Finals GRADE: " WITH NO ADVANCING
           ACCEPT FINALS.

           COMPUTE AVERAGE-GRADE =
              (PRELIM + MIDTERMS + FINALS) / 3.

           IF AVERAGE-GRADE >= 97
              MOVE "1.0" TO EQUIVALENT
           ELSE
              IF AVERAGE-GRADE >= 94
                 MOVE "1.25" TO EQUIVALENT
              ELSE
                 IF AVERAGE-GRADE >= 91
                    MOVE "1.5" TO EQUIVALENT
                 ELSE
                    IF AVERAGE-GRADE >= 88
                       MOVE "1.75" TO EQUIVALENT
                    ELSE
                       IF AVERAGE-GRADE >= 85
                          MOVE "2.0" TO EQUIVALENT
                       ELSE
                          IF AVERAGE-GRADE >= 82
                             MOVE "2.25" TO EQUIVALENT
                          ELSE
                             IF AVERAGE-GRADE >= 79
                                MOVE "2.5" TO EQUIVALENT
                             ELSE
                                IF AVERAGE-GRADE >= 76
                                   MOVE "2.75" TO EQUIVALENT
                                ELSE
                                   IF AVERAGE-GRADE = 75
                                      MOVE "3.0" TO EQUIVALENT
                                   ELSE
                                      MOVE "5.0" TO EQUIVALENT
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

           MOVE AVERAGE-GRADE TO DISP-AVE.
           MOVE EQUIVALENT TO DISP-EQUIVALENT.
           DISPLAY "AVERAGE: " FUNCTION TRIM(DISP-AVE).
           DISPLAY "EQUIVALENT GRADE: " FUNCTION TRIM(DISP-EQUIVALENT).

           STOP RUN.