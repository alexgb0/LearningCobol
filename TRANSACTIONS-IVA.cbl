      ******************************************************************
      * Author: Alex G. B.
      * Date: Oct. 30 2021
      * Purpose: Applying iva in a tansaction (learning).
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTIONS-IVA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  TRANSACTION.
       02  TRANSACTION-PRODUCT-ID  PIC AAA999999.
       02  TRANSACTION-DATE        PIC 99/99/9999. *> DDMMYYYY
       02  TRANSACTION-AMMOUNT     PIC 9999V99 COMP-4.
       02  TRANSACTION-IVA         PIC 9.
       88      TRANSACTION-IVA-APPLYED VALUE 1.
       88      TRANSACTION-IVA-NOT-APPLYED VALUE 0.

       77  IVA-CONST PIC 99 VALUE 21.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 23.95 TO TRANSACTION-AMMOUNT.
           MOVE 0 TO TRANSACTION-IVA.
           EVALUATE TRUE
               WHEN TRANSACTION-IVA-APPLYED PERFORM TRANSACTION-WITH-IVA
               WHEN TRANSACTION-IVA-NOT-APPLYED PERFORM TRANSACTION-PROC
           END-EVALUATE.
           
           DISPLAY TRANSACTION-AMMOUNT.

           STOP RUN.

       TRANSACTION-WITH-IVA.

       TRANSACTION-PROC.
           COMPUTE TRANSACTION-AMMOUNT = TRANSACTION-AMMOUNT + 
               ((TRANSACTION-AMMOUNT * IVA-CONST) / 100).
           

       END PROGRAM TRANSACTIONS-IVA.
