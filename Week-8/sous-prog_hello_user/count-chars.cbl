       IDENTIFICATION DIVISION.
       PROGRAM-ID. count-chars.

       DATA DIVISION.

       LINKAGE SECTION.
       
       01  LK-NOM         PIC X(20).

       01  LK-COUNT       PIC 9(02).

       01  LK-REPONSE     PIC X(30).

       PROCEDURE DIVISION USING LK-NOM LK-COUNT LK-REPONSE.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-NOM)) TO LK-COUNT.
       
       END PROGRAM count-chars.
           
