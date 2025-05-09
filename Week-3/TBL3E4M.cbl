      *> ---------------------------------------------------------------
      *> PROGRAMME : TBL3E4M
      *> OBJET     : Ce programme permet la saisie des notes de 3 élèves
      *>             dans 4 matières (Histoire, Math, Science, Anglais)
      *>             à l’aide d’un tableau à deux dimensions :
      *>             - 1ère dimension : les élèves (3)
      *>             - 2ème dimension : les matières (4)
      *>             Après saisie, le programme affiche pour chaque élève :
      *>             - Son nom
      *>             - Le nom des 4 matières
      *>             - Sa note correspondante dans chaque matière
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilisation de `OCCURS` imbriqués dans WS-TABLEAU
      *>             - WS-LISTE-MATIERE initialise dynamiquement les noms
      *>               des matières, partagés pour tous les élèves
      *>             - Double boucle PERFORM pour gérer la saisie
      *>               et l'affichage des données par élève et matière
      *>             - Notes formatées sur 2 chiffres + 2 décimales
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBL3E4M.
       AUTHOR. Vincent-cmd1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Liste des matières (commune à tous les élèves)
       01 WS-LISTE-MATIERE.
           05 WS-MATIERE-NOM OCCURS 4 TIMES PIC X(10).

      * Tableau principal : 3 élèves avec 4 notes chacun
       01 WS-TABLEAU-NOTE-ELEVE.
           05 WS-ELEVE OCCURS 3 TIMES.
              10 WS-NOM     PIC X(12).
              10 WS-NOTE    PIC 9(2)V99 OCCURS 4 TIMES.

      * Index de boucle
       01 WS-INDEX-A     PIC 9(1).
       01 WS-INDEX-B     PIC 9(1).

       PROCEDURE DIVISION.

      * Initialisation des noms des matières
           MOVE "HISTOIRE  " TO WS-MATIERE-NOM(1)
           MOVE "MATH      " TO WS-MATIERE-NOM(2)
           MOVE "SCIENCE   " TO WS-MATIERE-NOM(3)
           MOVE "ANGLAIS   " TO WS-MATIERE-NOM(4)

      * Saisie des noms et notes
           PERFORM VARYING WS-INDEX-A FROM 1 BY 1 UNTIL WS-INDEX-A > 3
               DISPLAY "Veuillez entrer le nom de l'élève :"
               ACCEPT WS-NOM(WS-INDEX-A)

               PERFORM VARYING WS-INDEX-B FROM 1 BY 1 UNTIL 
                                                         WS-INDEX-B > 4
                   DISPLAY "Note pour la matière : " 
                                          WS-MATIERE-NOM(WS-INDEX-B)
                   ACCEPT WS-NOTE(WS-INDEX-A, WS-INDEX-B)
               END-PERFORM
           END-PERFORM

      * Affichage des résultats
           PERFORM VARYING WS-INDEX-A FROM 1 BY 1 UNTIL WS-INDEX-A > 3
               DISPLAY "----------------------------"
               DISPLAY "NOM : " WS-NOM(WS-INDEX-A)

               PERFORM VARYING WS-INDEX-B FROM 1 BY 1 UNTIL 
                                                         WS-INDEX-B > 4
                   DISPLAY "MATIERE : " WS-MATIERE-NOM(WS-INDEX-B)
                   DISPLAY "NOTE    : " WS-NOTE(WS-INDEX-A, WS-INDEX-B)
               END-PERFORM
           END-PERFORM.

           STOP RUN.
           
