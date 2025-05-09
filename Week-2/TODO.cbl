      *> ---------------------------------------------------------------
      *> PROGRAMME : TODO
      *> OBJET     : Application interactive de gestion d'une liste de
      *>             tâches (TO-DO list). L'utilisateur peut :
      *>             - Ajouter des tâches (jusqu’à 5)
      *>             - Afficher les tâches enregistrées
      *>             - Supprimer une tâche selon son numéro
      *>             - Quitter proprement le programme
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : 
      *>             - Le menu principal repose sur un EVALUATE.
      *>             - Les interactions sont contrôlées via des flags
      *>               de retour (`RETOUR1`, `RETOUR2`, etc.).
      *>             - Les tâches sont stockées dans des variables
      *>               `WS-TASK1` à `WS-TASK5`.
      *>             - Des messages d'affichage formatés encadrent
      *>               chaque action avec une "bande graphique" (`WS-ASTER`).
      *>             - La validation utilisateur inclut la vérification
      *>               de saisies numériques.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TODO.      
 
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Menu : choix de l'action à effectuer 
       01 WS-CHOIX        PIC 9(1).

      * Variables de saisie de tâche 
       01 WS-ADD-ID1      PIC 9 VALUE 0.
       01 WS-ADD-ID2      PIC 9 VALUE 0.
       01 WS-ADD-ID3      PIC 9 VALUE 0.

      * Variables de saisie/reponse de tâche 
       01 WS-REPONSE-ADD        PIC X.
           88 RETOUR1             VALUE 'R' OR 'r'.
       01 WS-REPONSE-SUPPR      PIC X.
           88 RETOUR2             VALUE 'R' OR 'r'.
       01 WS-REPONSE-AFFICHAGE  PIC X.
           88 RETOUR3             VALUE 'R' OR 'r'.

      * Stockage des 5 tâches 
       01 WS-TASK1        PIC X(40).
       01 WS-TASK2        PIC X(40).
       01 WS-TASK3        PIC X(40). 
       01 WS-TASK4        PIC X(40). 
       01 WS-TASK5        PIC X(40).

      * Drapeau de sortie  
       01 WS-QUIT         PIC X.
           88 QUITTER     VALUE 'Y' OR 'y'.

      * Bande d'affichage graphique
       01 WS-ASTER        PIC X(29) VALUE ALL "*".

       PROCEDURE DIVISION.

      *> Lancement du menu principal      
           PERFORM 0000-AFFICHAGE-START
              THRU 0000-AFFICHAGE-END.          

      *----------------------------------------------------------------*
      *> Titre du menu
       0000-AFFICHAGE-START.
           MOVE 0 TO WS-CHOIX.
           DISPLAY WS-ASTER.
           DISPLAY "*      MENU TO-DO LIST      *".
           DISPLAY WS-ASTER.
           DISPLAY "* Veuillez saisir un numéro *".
           DISPLAY "*---------------------------*".
           DISPLAY "* 1 - Ajouter une tâche     *".
           DISPLAY "* 2 - Afficher les tâches   *".
           DISPLAY "* 3 - Supprimer les tâches  *".
           DISPLAY "* 4 - Quitter le programme  *".
           DISPLAY WS-ASTER.
          *> Saisie utilisateur
           ACCEPT WS-CHOIX.
           PERFORM 0000-INITIALIZE-START
              THRU 0000-INITIALIZE-END.
       0000-AFFICHAGE-END.
           EXIT.

       0000-INITIALIZE-START.
           DISPLAY WS-ASTER.
           DISPLAY "*        TO-DO LIST         *".
           DISPLAY WS-ASTER.
      *> Dispatch vers les fonctionnalités     
           EVALUATE TRUE
           WHEN WS-CHOIX EQUAL 1
                   PERFORM 0010-ADDTODO-START
                      THRU 0010-ADDTODO-END
           WHEN WS-CHOIX EQUAL 2
                   PERFORM 0020-DISPLAYTODO-START
                      THRU 0020-DISPLAYTODO-END
           WHEN WS-CHOIX EQUAL 3
                   PERFORM 0030-SUPPRTODO-START
                      THRU 0030-SUPPRTODO-END
           WHEN WS-CHOIX EQUAL 4
                   PERFORM 0040-STOPTODO-START
                      THRU 0040-STOPTODO-END
           WHEN OTHER
                   DISPLAY "Erreur! Veuillez ressaisir le numéro."
                   PERFORM 0000-AFFICHAGE-START
                      THRU 0000-AFFICHAGE-END
           END-EVALUATE.
       0000-INITIALIZE-END.
           EXIT.


       0010-ADDTODO-START. 
      *> Boucle d’ajout
           MOVE SPACE TO WS-REPONSE-ADD.     
           PERFORM UNTIL RETOUR1
               DISPLAY "Ajoutez vos tâches de 1 à 5"
               DISPLAY "Retour menu ? Tapez R"
               ACCEPT WS-REPONSE-ADD
               IF NOT RETOUR1 THEN
                   IF WS-REPONSE-ADD IS NUMERIC THEN
                   MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) TO WS-ADD-ID1
                       EVALUATE WS-ADD-ID1
                       WHEN 1
                           DISPLAY "*************[1]*************"
                           ACCEPT WS-TASK1
                           DISPLAY WS-ASTER
                       WHEN 2
                           DISPLAY "*************[2]*************"
                           ACCEPT WS-TASK2
                           DISPLAY WS-ASTER
                       WHEN 3
                           DISPLAY "*************[3]*************"
                           ACCEPT WS-TASK3
                           DISPLAY WS-ASTER
                       WHEN 4
                           DISPLAY "*************[4]*************"
                           ACCEPT WS-TASK4
                           DISPLAY WS-ASTER
                       WHEN 5
                           DISPLAY "*************[5]*************"
                           ACCEPT WS-TASK5
                           DISPLAY WS-ASTER
                       WHEN OTHER
                           DISPLAY "Numéro incorrect. (1 à 5)"
                       END-EVALUATE
                    ELSE
                       DISPLAY "Erreur : saisie invalide."
                    END-IF
                END-IF
           END-PERFORM.
       0010-ADDTODO-END.
           EXIT.

       0020-DISPLAYTODO-START.
      *> Affichage brut 
           MOVE SPACE TO WS-REPONSE-AFFICHAGE.
           PERFORM UNTIL RETOUR3
               DISPLAY WS-ASTER
               DISPLAY "*        VOS TACHES       *"
               DISPLAY WS-ASTER                 
               DISPLAY "* 1 - " WS-TASK1 " *"
               DISPLAY "* 2 - " WS-TASK2 " *"
               DISPLAY "* 3 - " WS-TASK3 " *"
               DISPLAY "* 4 - " WS-TASK4 " *"
               DISPLAY "* 5 - " WS-TASK5 " *"
               DISPLAY WS-ASTER
               DISPLAY "Retour menu ? Tapez R"
               ACCEPT WS-REPONSE-AFFICHAGE
           END-PERFORM.                
       0020-DISPLAYTODO-END.
           EXIT.   

       0030-SUPPRTODO-START.
      *> Boucle suppression 
           MOVE SPACE TO WS-REPONSE-SUPPR.
           PERFORM UNTIL RETOUR2
               DISPLAY "Supprimez vos tâches de 1 à 5"
               DISPLAY "Retour menu ? Tapez R"
               ACCEPT WS-REPONSE-SUPPR
               IF NOT RETOUR2 THEN
                   IF WS-REPONSE-SUPPR IS NUMERIC THEN
                   MOVE FUNCTION NUMVAL(WS-REPONSE-SUPPR) TO WS-ADD-ID2
                       EVALUATE WS-ADD-ID2
                       WHEN 1
                           MOVE SPACE TO WS-TASK1
                           DISPLAY WS-ASTER
                           DISPLAY "*     TACHE 1 SUPPRIMEE     *"
                           DISPLAY WS-ASTER
                       WHEN 2
                           MOVE SPACE TO WS-TASK2
                           DISPLAY WS-ASTER
                           DISPLAY "*     TACHE 2 SUPPRIMEE     *"
                           DISPLAY WS-ASTER
                       WHEN 3
                           MOVE SPACE TO WS-TASK3
                           DISPLAY WS-ASTER
                           DISPLAY "*     TACHE 3 SUPPRIMEE     *"
                           DISPLAY WS-ASTER
                       WHEN 4
                           MOVE SPACE TO WS-TASK4
                           DISPLAY WS-ASTER
                           DISPLAY "*     TACHE 4 SUPPRIMEE     *"
                           DISPLAY WS-ASTER
                       WHEN 5
                           MOVE SPACE TO WS-TASK5
                           DISPLAY WS-ASTER
                           DISPLAY "*     TACHE 5 SUPPRIMEE     *"
                           DISPLAY WS-ASTER
                       WHEN OTHER
                           DISPLAY "Numéro incorrect. (1 à 5)"
                       END-EVALUATE
                   ELSE
                       DISPLAY "Erreur : saisie invalide."
                   END-IF
                END-IF
           END-PERFORM.
       0030-SUPPRTODO-END.
           EXIT.

       0040-STOPTODO-START.
           DISPLAY WS-ASTER.
           DISPLAY "*   Voullez-vous quitter ?  *".
           DISPLAY "*        Tapez (Y/N)        *".
           DISPLAY WS-ASTER.
           ACCEPT WS-QUIT.
      *> Fin normale du programme     
           IF QUITTER
               DISPLAY WS-ASTER
               DISPLAY "*          BYE BYE          *"
               DISPLAY WS-ASTER
               STOP RUN
           ELSE 
               PERFORM 0000-AFFICHAGE-START
                  THRU 0000-AFFICHAGE-END
           END-IF.
       0040-STOPTODO-END.
           EXIT.
