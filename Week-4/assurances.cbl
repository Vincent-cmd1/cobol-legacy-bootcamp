      ******************************************************************
      * PROGRAMME : ASSURANCES.CBL
      * AUTEURS   : Anais & Vincent
      * OBJET     : Ce programme lit un fichier de contrats d'assurance,
      *             stocke les enregistrements en mémoire,
      *             formate les dates et affiche certains contrats.
      *
      * FORMAT    : Le fichier source est en format texte séquentiel,
      *             avec les dates au format AAAAMMJJ.
      *             Ce programme les affiche en format JJ/MM/AAAA.
      *
      * REMARQUE  : Seuls les contrats aux index 3 et 7 sont affichés.
      ******************************************************************    
         
       IDENTIFICATION DIVISION.
       PROGRAM-ID. assurances.
       AUTHOR. Anais & Vincent.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.   

      * Déclaration du fichier source
           SELECT F-ASSURANCES ASSIGN TO "assurances.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * Description du fichier d'entrée
       FD F-ASSURANCES.
       01 FS-ENR-ASSURANCES.
           05 FS-CODE-CONTRAT             PIC X(08).
           05 FILLER                      PIC X(01).
           05 FS-NOM-CONTRAT              PIC X(14). 
           05 FILLER                      PIC X(01).
           05 FS-NOM-PRODUIT              PIC X(14).
           05 FILLER                      PIC X(01).
           05 FS-NOM-CLIENT               PIC X(41).
           05 FILLER                      PIC X(01). 
           05 FS-STATUT                   PIC X(08).
           05 FILLER                      PIC X(01). 
           05 FS-DATE-DEBUT               PIC 9(08).
           05 FILLER                      PIC X(01). 
           05 FS-DATE-FIN                 PIC 9(08).
           05 FILLER                      PIC X(01). 
           05 FS-MONTANT                  PIC 9(07)V9(02).
           05 FILLER                      PIC X(01). 
           05 FS-DEVISE                   PIC X(04).

       WORKING-STORAGE SECTION.

      * Tableau mémoire pour stocker jusqu'à 36 contrats d'assurances
      * Chaque ligne est un enregistrement complet du fichier
       01 WS-ENR-ASSURANCES.
           05 WS-ASSURANCES OCCURS 36 TIMES.
             10 WS-CODE-CONTRAT           PIC X(08).
             10 FILLER                    PIC X(01). 
             10 WS-NOM-CONTRAT            PIC X(14).
             10 FILLER                    PIC X(01). 
             10 WS-NOM-PRODUIT            PIC X(14).
             10 FILLER                    PIC X(01). 
             10 WS-NOM-CLIENT             PIC X(41).
             10 FILLER                    PIC X(01). 
             10 WS-STATUT                 PIC X(08).
             10 FILLER                    PIC X(01). 
             10 WS-DATE-DEBUT             PIC 9(08).
             10 FILLER                    PIC X(01). 
             10 WS-DATE-FIN               PIC 9(08).
             10 FILLER                    PIC X(01). 
             10 WS-MONTANT                PIC 9(07)V9(02).
             10 FILLER                    PIC X(01). 
             10 WS-DEVISE                 PIC X(04).
             10 FILLER                    PIC X VALUE X"0A". 

      * Index pour boucle de traitement
       01 WS-IDX                          PIC 9(03)    VALUE 1.
       01 WS-IDX-FIN                      PIC 9(03)    VALUE 36.

      * Drapeau de fin de lecture du fichier
      * La valeur 'Y' de la condition QUITTER permet de sortir duPERFORM

       01 FLAG-STOP                       PIC X.
           88 QUITTER                                  VALUE 'Y'.

      * Varaiable d'affichage
       01 WS-TIRET                        PIC X(123)   VALUE ALL '-'.  

      * Variables pour reformater les dates AAAAMMJJ en JJ/MM/AAAA
       01 WS-ANNEE                 PIC X(4).
       01 WS-MOIS                  PIC X(2).
       01 WS-JOUR                  PIC X(2).
       01 WS-DATE-DEB-FORMATTEE    PIC X(10).
       01 WS-DATE-FIN-FORMATTEE    PIC X(10).
       01 WS-DATE-TEMP             PIC X(8).


       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : enchaînement des traitements
      ******************************************************************

       0000-TRT-PRINCIPAL-DEB.
       
      * Ouverture du fichier d'entrée
           PERFORM 6010-OPEN-F-ASSURANCES-DEB
              THRU 6010-OPEN-F-ASSURANCES-FIN.
       
      * Lecture fichier + stockage en mémoire
           PERFORM 6110-READ-F-ASSURANCES-DEB
              THRU 6110-READ-F-ASSURANCES-FIN.

      * Fermeture du fichier d'entrée
           PERFORM 6210-CLOSE-F-ASSURANCES-DEB
              THRU 6210-CLOSE-F-ASSURANCES-FIN.

      * Affichage des assurances
           PERFORM 8000-AFFICHAGE-ASSU-DEB
              THRU 8000-AFFICHAGE-ASSU-FIN.       

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * ORDRES D'OUVERTURE ET FERMETURE DE FICHIER
      ******************************************************************
      
      * Ouvre le fichier d'entrée contenant les contrats
       6010-OPEN-F-ASSURANCES-DEB.
           OPEN INPUT F-ASSURANCES.
       6010-OPEN-F-ASSURANCES-FIN.
           EXIT.
       
      * Ferme le fichier d'entrée après lecture complète
       6210-CLOSE-F-ASSURANCES-DEB.
           CLOSE F-ASSURANCES.
       6210-CLOSE-F-ASSURANCES-FIN.
           EXIT.

      ******************************************************************
      * LECTURE DU FICHIER ASSURANCES ET STOCKAGE EN MÉMOIRE
      ******************************************************************
      
      * Lecture du fichier ligne par ligne
      * Chaque enregistrement est stocké dans le tableau WS-ASSURANCES
      * L'indice WS-IDX contrôle le nombre d'enregistrements à stocker
       6110-READ-F-ASSURANCES-DEB.
           MOVE 'N' TO FLAG-STOP.
           PERFORM UNTIL QUITTER
               READ F-ASSURANCES
                   AT END
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX <= WS-IDX-FIN
                           MOVE FS-CODE-CONTRAT 
                               TO WS-CODE-CONTRAT(WS-IDX)
                           MOVE FS-NOM-CONTRAT  
                               TO WS-NOM-CONTRAT(WS-IDX)
                           MOVE FS-NOM-PRODUIT 
                               TO WS-NOM-PRODUIT (WS-IDX)
                           MOVE FS-NOM-CLIENT  
                               TO WS-NOM-CLIENT(WS-IDX)
                           MOVE FS-STATUT 
                               TO WS-STATUT(WS-IDX)
                           MOVE FS-DATE-DEBUT  
                               TO WS-DATE-DEBUT(WS-IDX)
                           MOVE FS-DATE-FIN  
                               TO WS-DATE-FIN(WS-IDX)
                           MOVE FS-MONTANT  
                               TO WS-MONTANT(WS-IDX)
                           MOVE FS-DEVISE  
                               TO WS-DEVISE(WS-IDX)
                           ADD 1 TO WS-IDX     
                       ELSE
                           SET QUITTER TO TRUE
                       END-IF
                END-READ         
           END-PERFORM.
       6110-READ-F-ASSURANCES-FIN.
           EXIT.

      ******************************************************************
      * COMPOSANT INDEPENDANT
      ******************************************************************
      
      * Ce composant reformate les dates de début et de fin d'un contrat
      * Il utilise WS-DATE-TEMP pour extraire AAAA MM JJ
      * Puis construit la date JJ/MM/AAAA dans WS-DATE-*-FORMATTEE
       7000-FORMATER-DATES-DEB.
           MOVE WS-DATE-DEBUT(WS-IDX) TO WS-DATE-TEMP.
           MOVE WS-DATE-TEMP(1:4) TO WS-ANNEE
           MOVE WS-DATE-TEMP(5:2) TO WS-MOIS
           MOVE WS-DATE-TEMP(7:2) TO WS-JOUR
           STRING WS-JOUR "/" WS-MOIS "/" WS-ANNEE
               INTO WS-DATE-DEB-FORMATTEE
           
           MOVE WS-DATE-FIN(WS-IDX) TO WS-DATE-TEMP.
           MOVE WS-DATE-TEMP(1:4) TO WS-ANNEE
           MOVE WS-DATE-TEMP(5:2) TO WS-MOIS
           MOVE WS-DATE-TEMP(7:2) TO WS-JOUR
           STRING WS-JOUR "/" WS-MOIS "/" WS-ANNEE
               INTO WS-DATE-FIN-FORMATTEE.
       7000-FORMATER-DATES-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE À L'ÉCRAN
      ******************************************************************
      
      * Affiche l'entête de tableau pour l'utilisateur
      * Parcourt les enregistrements stockés
      * Affiche uniquement ceux d'index 3 et 7 avec les dates formatées
       8000-AFFICHAGE-ASSU-DEB. 
           DISPLAY " Code    |" 
                   " Nom contrat    |" 
                   " Nom produit    |" 
                   " Nom client                                |"
                   " Statut   |"
                   "Deb. contrat|"
                   " Fin contrat|"
                   " Montant".
           DISPLAY WS-TIRET.
               
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-IDX-FIN
               IF WS-IDX = 3 OR WS-IDX = 7 
                   PERFORM 7000-FORMATER-DATES-DEB
                      THRU 7000-FORMATER-DATES-FIN
        
                   DISPLAY WS-CODE-CONTRAT(WS-IDX) " | "
                        WS-NOM-CONTRAT(WS-IDX) " | "
                        WS-NOM-PRODUIT(WS-IDX) " | "
                        WS-NOM-CLIENT(WS-IDX) " | "
                        WS-STATUT(WS-IDX) " | "
                        WS-DATE-DEB-FORMATTEE " | "
                        WS-DATE-FIN-FORMATTEE " | "
                        WS-MONTANT(WS-IDX) " "
                        WS-DEVISE(WS-IDX)
               END-IF
           END-PERFORM.
       8000-AFFICHAGE-ASSU-FIN.
           EXIT.
      

