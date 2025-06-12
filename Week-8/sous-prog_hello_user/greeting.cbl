       IDENTIFICATION DIVISION.
       PROGRAM-ID. greeting.

       DATA DIVISION.

       LINKAGE SECTION.
       
       01  LK-COUNT       PIC 9(02).

       01  LK-REPONSE     PIC X(30).

       PROCEDURE DIVISION USING LK-COUNT LK-REPONSE.

           STRING "Hello, "  LK-COUNT INTO LK-REPONSE.

       END PROGRAM greeting.
           
