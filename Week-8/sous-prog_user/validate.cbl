       IDENTIFICATION DIVISION.
       PROGRAM-ID. validate.

       DATA DIVISION.

       WORKING-STORAGE SECTION.


       LINKAGE SECTION.
       01  LK-USER-DATA.
           05 LK-ID-USER          PIC X(10).
           05 LK-NOM-USER         PIC X(46).
           05 LK-EMAIL-USER       PIC X(30).

       01  LK-COUNT               PIC 9(02).

       PROCEDURE DIVISION USING LK-USER-DATA LK-COUNT.

           INSPECT LK-EMAIL-USER TALLYING LK-COUNT FOR ALL "@".

       END PROGRAM validate.
