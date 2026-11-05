       IDENTIFICATION DIVISION.
       PROGRAM-ID. EQUIVALENT-GRADE.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 PRELIM1           PIC 9(3)V99.
       01 MIDTERMS1         PIC 9(3)V99.
       01 FINALS1           PIC 9(3)V99.
       01 AVERAGE-GRADE1    PIC 9(3)V99.
       01 EQUIVALENT1       PIC 9(4)V99.

       01 DISP-AVE1         PIC ZZ9.99.
       01 DISP-EQUIVALENT1  PIC ZZZ9.99.

       PROCEDURE DIVISION.

           DISPLAY "Enter Prelim Grade: " WITH NO ADVANCING
           ACCEPT PRELIM1.

           DISPLAY "Enter Midterms Grade: " WITH NO ADVANCING
           ACCEPT MIDTERMS1.

           DISPLAY "ENTER Finals GRADE: " WITH NO ADVANCING
           ACCEPT FINALS1.

           COMPUTE AVERAGE-GRADE1 =
              (PRELIM1 + MIDTERMS1 + FINALS1) / 3.

           IF AVERAGE-GRADE1 >= 97
              MOVE "1.0" TO EQUIVALENT1
           ELSE
              IF AVERAGE-GRADE1 >= 94
                 MOVE "1.25" TO EQUIVALENT1
              ELSE
                 IF AVERAGE-GRADE1 >= 91
                    MOVE "1.5" TO EQUIVALENT1
                 ELSE
                    IF AVERAGE-GRADE1 >= 88
                       MOVE "1.75" TO EQUIVALENT1
                    ELSE
                       IF AVERAGE-GRADE1 >= 85
                          MOVE "2.0" TO EQUIVALENT1
                       ELSE
                          IF AVERAGE-GRADE1 >= 82
                             MOVE "2.25" TO EQUIVALENT1
                          ELSE
                             IF AVERAGE-GRADE1 >= 79
                                MOVE "2.5" TO EQUIVALENT1
                             ELSE
                                IF AVERAGE-GRADE1 >= 76
                                   MOVE "2.75" TO EQUIVALENT1
                                ELSE
                                   IF AVERAGE-GRADE1 = 75
                                      MOVE "3.0" TO EQUIVALENT1
                                   ELSE
                                      MOVE "5.0" TO EQUIVALENT1
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

           MOVE AVERAGE-GRADE1 TO DISP-AVE1.
           MOVE EQUIVALENT1 TO DISP-EQUIVALENT1.
           DISPLAY "AVERAGE: " FUNCTION TRIM(DISP-AVE1).
           DISPLAY "EQUIVALENT GRADE: " FUNCTION TRIM(DISP-EQUIVALENT1).

           STOP RUN.