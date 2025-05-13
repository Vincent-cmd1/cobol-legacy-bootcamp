       IDENTIFICATION DIVISION.
       PROGRAM-ID. INFOSS.
       AUTHOR. VINCENT-CMD1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-AGENTS-OSS.
           05 WS-AGENT-01 PIC X(55) VALUE 
             "117;Hubert;Bonisseur de La Bath;Agent très spécial".
           05 WS-AGENT-02 PIC X(55) VALUE 
             "118;Jack;Jefferson;Faux diplomate".
           05 WS-AGENT-03 PIC X(55) VALUE 
             "119;Armand;Lesignac;Directeur du renseignement".
           05 WS-AGENT-04 PIC X(55) VALUE 
             "120;Dolorès;Koulechov;Archéologue soupçonnée".
           05 WS-AGENT-05 PIC X(55) VALUE 
             "121;Heinrich;von Zimmel;Nazi recyclé".
           05 WS-AGENT-06 PIC X(55) VALUE 
             "122;Larmina;El Akmar Betouche;Assistante pas si naïve".

       01 WS-TB-AGENTS.
           05 WS-AGENT OCCURS 6 TIMES.
               10 WS-NUMERO-OSS  PIC 9(3).
               10 WS-PRENOM      PIC X(10).
               10 WS-NOM         PIC X(20).
               10 WS-ROLE        PIC X(30).

       01 WS-TB-IDX              PIC 9(3) VALUE 1.

       PROCEDURE DIVISION.

           UNSTRING WS-AGENT-01
               DELIMITED BY ";"
               INTO WS-NUMERO-OSS (WS-TB-IDX)
                    WS-PRENOM     (WS-TB-IDX)
                    WS-NOM        (WS-TB-IDX)
                    WS-ROLE       (WS-TB-IDX)

           DISPLAY "NUMERO : " WS-NUMERO-OSS(WS-TB-IDX)
           DISPLAY "PRENOM : " WS-PRENOM(WS-TB-IDX)
           DISPLAY "NOM    : " WS-NOM(WS-TB-IDX)
           DISPLAY "ROLE   : " WS-ROLE(WS-TB-IDX)

           STOP RUN.
