       IDENTIFICATION DIVISION. 
       PROGRAM-ID. assurance-test.
       AUTHOR. Vincent Faivre.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

       SELECT F-DATA-ASSURANCE
           ASSIGN TO "datassur.csv"
           ORGANIZATION IS SEQUENTIAL.

       SELECT F-DATA-ASSURANCE-OUT 
           ASSIGN TO "datassur-output.txt"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD F-DATA-ASSURANCE  
           RECORD CONTAINS 122 CHARACTERS
           DATA RECORDS IS FS-ENR-ASSURANCE.

       01 FS-ENR-ASSURANCE.
           05 FS-ASSURANCE   PIC X(122).

       FD F-DATA-ASSURANCE-OUT  
           RECORD CONTAINS 150 CHARACTERS
           DATA RECORDS IS FS-ENR-ASSURANCE-OUT.

       01 FS-ENR-ASSURANCE-OUT.
           05 FS-ASSURANCE-OUT   PIC X(150).  

       WORKING-STORAGE SECTION.
       
       01 WS-FS-ASSURANCE    PIC X(02).

       01 WS-ENR-ASSURANCE.
           05 WS-TAB-ASSURANCE OCCURS 999 TIMES.
              10 WS-NUM         PIC X(08).
              10 WS-CODE        PIC X(14).
              10 WS-CONTRAT     PIC X(14).
              10 WS-NOM         PIC X(41).
              10 WS-STATUT      PIC X(08).
              10 WS-DATE-ENTREE PIC X(08).
              10 WS-DATE-SORTIE PIC X(08).
              10 WS-VALEUR      PIC X(09).
              10 WS-SYMBOLE     PIC X(04).
     
       
       01 WS-STOP            PIC X(01).
       01 WS-IDX             PIC 9(03)      VALUE 1.
       01 WS-NB-LIGNES       PIC 9(03)      VALUE ZEROS.

       01 WS-LIGNE-ED        PIC X(150).
       01 WS-TITRE           PIC X(20)      VALUE "RAPPORT DE SYNTHESE".
       01 WS-ID              PIC X(20)      VALUE "PAR VINCENT FAIVRE".
       01 WS-DATE            PIC X(10)      VALUE "06/06/2025".
       01 WS-TIRET           PIC X(20)      VALUE ALL "-".
       01 WS-COMPT           PIC 9(02).  


       PROCEDURE DIVISION.

       0000-INIT-DEB.

           MOVE "N" TO WS-STOP.

           PERFORM 6010-OPEN-F-ASSU-DEB
              THRU 6010-OPEN-F-ASSU-FIN.

           PERFORM 6010-OPEN-F-ASSU-OUT-DEB
              THRU 6010-OPEN-F-ASSU-OUT-FIN.              
           
       0000-INIT-FIN.
           EXIT.


       6010-READ-F-ASSU-DEB.
       
           PERFORM UNTIL WS-STOP = "O"
           READ F-DATA-ASSURANCE
              AT END 
                 MOVE "O" TO WS-STOP
              NOT AT END
            
               ADD 1 TO WS-NB-LIGNES  

               PERFORM VARYING WS-IDX FROM 1 BY 1 
                                     UNTIL WS-IDX >= WS-NB-LIGNES
                 MOVE FS-ASSURANCE(1:8)   TO WS-NUM(WS-IDX)
                 MOVE FS-ASSURANCE(10:14) TO WS-CODE(WS-IDX)
                 MOVE FS-ASSURANCE(25:14) TO WS-CONTRAT(WS-IDX)
                 MOVE FS-ASSURANCE(40:41) TO WS-NOM(WS-IDX)
                 MOVE FS-ASSURANCE(82:8) TO WS-STATUT(WS-IDX)
                 MOVE FS-ASSURANCE(91:8) TO WS-DATE-ENTREE(WS-IDX)
                 MOVE FS-ASSURANCE(100:8) TO WS-DATE-SORTIE(WS-IDX)
                 MOVE FS-ASSURANCE(109:9) TO WS-VALEUR(WS-IDX)
                 MOVE FS-ASSURANCE(119:4) TO WS-SYMBOLE(WS-IDX)
                 
                 DISPLAY "------------------------------------"
                 DISPLAY "Ligne numero : " WS-NB-LIGNES
                 DISPLAY "Numero : " WS-NUM(WS-IDX) 
                 DISPLAY "Code : " WS-CODE(WS-IDX) 
                 DISPLAY "Contrat : " WS-CONTRAT(WS-IDX) 
                 DISPLAY "Nom : " WS-NOM(WS-IDX)
                 DISPLAY "Statut : " WS-STATUT(WS-IDX)
                 DISPLAY "Date entree : " WS-DATE-ENTREE(WS-IDX)
                 DISPLAY "Date sortie : " WS-DATE-SORTIE(WS-IDX)
                 DISPLAY "Valeur : " WS-VALEUR(WS-IDX) " " 
                                     WS-SYMBOLE(WS-IDX)
                 DISPLAY "------------------------------------"  

              PERFORM 6010-READ-F-ASSU-DEB
                 THRU 6010-READ-F-ASSU-FIN

              END-PERFORM  

           END-PERFORM.           
       6010-READ-F-ASSU-FIN.
           EXIT.

           PERFORM 6010-CLOSE-F-ASSU-DEB
              THRU 6010-CLOSE-F-ASSU-FIN.


           
      * Ecriture de l'entete

           WRITE FS-ENR-ASSURANCE-OUT FROM WS-TIRET.
           WRITE FS-ENR-ASSURANCE-OUT FROM WS-TITRE AFTER 1.
           WRITE FS-ENR-ASSURANCE-OUT FROM WS-ID AFTER 1.
           WRITE FS-ENR-ASSURANCE-OUT FROM WS-DATE AFTER 1.
           WRITE FS-ENR-ASSURANCE-OUT FROM WS-TIRET AFTER 1.


      * Ecriture DE L'ENTETE corps du rapport
           MOVE "Numero" TO WS-LIGNE-ED(1:8).
           MOVE "Code" TO WS-LIGNE-ED(10:14).
           MOVE "Contrat" TO WS-LIGNE-ED(25:14).
           MOVE "Nom" TO WS-LIGNE-ED(40:41).
           MOVE "Statut" TO WS-LIGNE-ED(82:8).
           MOVE "Date E" TO WS-LIGNE-ED(82:8).
           MOVE "Date S" TO WS-LIGNE-ED(100:8).
           MOVE "Valeur" TO WS-LIGNE-ED(109:9).

           WRITE FS-ENR-ASSURANCE-OUT FROM WS-LIGNE-ED AFTER 1.

      * Ecriture du corps du rapport 

           INITIALIZE WS-IDX.

           PERFORM VARYING WS-IDX FROM 1 BY 1
                                  UNTIL WS-IDX > WS-NB-LIGNES
              DISPLAY " IDX : " WS-IDX  

              MOVE WS-NUM(WS-IDX) TO WS-LIGNE-ED(1:8)
              MOVE WS-CODE(WS-IDX) TO WS-LIGNE-ED(10:14)
              MOVE WS-CONTRAT(WS-IDX) TO WS-LIGNE-ED(25:14)
              MOVE WS-NOM(WS-IDX) TO WS-LIGNE-ED(40:41)
              MOVE WS-STATUT(WS-IDX) TO WS-LIGNE-ED(82:8)
              MOVE WS-DATE-ENTREE(WS-IDX) TO WS-LIGNE-ED(82:8)
              MOVE WS-DATE-SORTIE(WS-IDX) TO WS-LIGNE-ED(100:8)
              MOVE WS-VALEUR(WS-IDX) TO WS-LIGNE-ED(109:9)
              MOVE WS-SYMBOLE(WS-IDX) TO WS-LIGNE-ED(118:4)            

              WRITE FS-ENR-ASSURANCE-OUT
                           FROM WS-LIGNE-ED AFTER 1

           END-PERFORM.

           DISPLAY " IDX : " WS-IDX  
           DISPLAY " NB-LIGNES : " WS-NB-LIGNES    

           PERFORM 6010-CLOSE-F-ASSU-OUT-DEB
              THRU 6010-CLOSE-F-ASSU-OUT-FIN.

           STOP RUN.
      *----------------------------------------------------------------* 


       6010-OPEN-F-ASSU-DEB.
           OPEN INPUT F-DATA-ASSURANCE.
           DISPLAY "Ouverture OK".
       6010-OPEN-F-ASSU-FIN.
           EXIT.



       6010-OPEN-F-ASSU-OUT-DEB.
           OPEN OUTPUT F-DATA-ASSURANCE-OUT.
           DISPLAY "Ouverture OK".
       6010-OPEN-F-ASSU-OUT-FIN.
           EXIT.




       6010-CLOSE-F-ASSU-DEB.
           CLOSE F-DATA-ASSURANCE.
           DISPLAY "Fermeture OK".
       6010-CLOSE-F-ASSU-FIN.
           EXIT.           


       6010-CLOSE-F-ASSU-OUT-DEB.
           CLOSE F-DATA-ASSURANCE-OUT.
           DISPLAY "Fermeture OK".
       6010-CLOSE-F-ASSU-OUT-FIN.
           EXIT. 


      


