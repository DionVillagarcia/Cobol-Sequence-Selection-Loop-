       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1-N8-VILLAGARCIA.
       AUTHOR. DION-VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OBJECT-FILE
               ASSIGN TO "Object.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OBJECT-FILE.
       01  OBJECT-REC       PIC X(60).

       WORKING-STORAGE SECTION.
       01 LENGTH1         PIC 9(3).
       01 WIDTH           PIC 9(3).
       01 AREA1           PIC 9(6).
       01 PERIMETER       PIC 9(5).

       01 DISP-LENGTH1    PIC ZZ9.
       01 DISP-WIDTH      PIC ZZ9.
       01 DISP-AREA1      PIC Z(5)9.
       01 DISP-PERIMETER  PIC Z(4)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input Length: " WITH NO ADVANCING
           ACCEPT LENGTH1.
           DISPLAY "Input Width: " WITH NO ADVANCING
           ACCEPT WIDTH.

           COMPUTE AREA1 = LENGTH1 * WIDTH.
           COMPUTE PERIMETER = 2 * (LENGTH1 + WIDTH).

           MOVE LENGTH1 TO DISP-LENGTH1.
           MOVE WIDTH TO DISP-WIDTH.
           MOVE AREA1 TO DISP-AREA1.
           MOVE PERIMETER TO DISP-PERIMETER.

           DISPLAY "The object area is     : "
                   FUNCTION TRIM(DISP-AREA1).
           DISPLAY "The object Perimeter is: "
                   FUNCTION TRIM(DISP-PERIMETER).

           OPEN OUTPUT OBJECT-FILE.

           MOVE SPACES TO OBJECT-REC.
           STRING "Length:                   "
                   FUNCTION TRIM(DISP-LENGTH1)
                   DELIMITED BY SIZE INTO OBJECT-REC.
           WRITE OBJECT-REC.

           MOVE SPACES TO OBJECT-REC.
           STRING "Width:                    "
                   FUNCTION TRIM(DISP-WIDTH)
                   DELIMITED BY SIZE INTO OBJECT-REC.
           WRITE OBJECT-REC.

           MOVE SPACES TO OBJECT-REC.
           STRING "The object area is      : "
                   FUNCTION TRIM(DISP-AREA1)
                   DELIMITED BY SIZE INTO OBJECT-REC.
           WRITE OBJECT-REC.

           MOVE SPACES TO OBJECT-REC.
           STRING "The object Perimeter is : "
                   FUNCTION TRIM(DISP-PERIMETER)
                   DELIMITED BY SIZE INTO OBJECT-REC.
           WRITE OBJECT-REC.

           CLOSE OBJECT-FILE.

           STOP RUN.
       END PROGRAM F1-N8-VILLAGARCIA.