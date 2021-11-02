      ******************************************************************
      * Author: Alex G. B.
      * Date: Nov 02, 2021
      * Purpose: Register an user into the records file.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETEK-REGISTER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS ASSIGN TO "USERS-DB"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS REC-USER-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  USERS.
       01  USER-FILE.
       05      REC-USER-ID     PIC 9(3).
       05      REC-USER-EMAIL  PIC X(15).
       05      REC-SUB-EXPIR   PIC 99/99/9999. *> DD/MM/YYYY

       WORKING-STORAGE SECTION.
       01  WS-USER.
       05      WS-USER-ID     PIC 9(3).
       05      WS-USER-EMAIL  PIC X(15).
       05      WS-SUB-EXPIR   PIC 99/99/9999. *> DD/MM/YYYY

       77  EOF PIC 9.
       77  IND PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM GET-END-IND. *> GET THE BIGGEST AVALIBLE INDEX
           *>PERFORM APPEND-USER.
           STOP RUN.

       APPEND-USER.
           OPEN I-O USERS
               MOVE REC-USER-ID TO WS-USER-ID.
               MOVE "USER@LOCAL.HOST" TO WS-USER-EMAIL.
               MOVE 03062022 TO WS-SUB-EXPIR.

               WRITE USER-FILE FROM WS-USER
                   INVALID KEY DISPLAY "INVALID KEY"
                   NOT INVALID KEY DISPLAY "RECORD PUSHED"
               END-WRITE.
           CLOSE USERS.

       GET-END-IND.
           OPEN INPUT USERS
               MOVE 1 TO REC-USER-ID.
               MOVE 0 TO EOF.
               PERFORM UNTIL EOF EQUALS 1
                   READ USERS RECORD INTO WS-USER
                       KEY IS REC-USER-ID
                       INVALID KEY MOVE 1 TO EOF
                       NOT INVALID KEY
                           DISPLAY REC-USER-ID " RECORD: " WS-USER
                           ADD 1 TO REC-USER-ID
                   END-READ
               END-PERFORM.
           CLOSE USERS.
       END PROGRAM RETEK-REGISTER.
