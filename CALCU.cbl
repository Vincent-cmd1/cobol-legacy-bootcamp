       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCU.
       AUTHOR. Vincent-cmd1.

      * Programme permettant d’imiter une calculatrice 
      * avec un peu de la personnalité de Glados.            
      * Fonctionnalités : Additioner, soustraire, multiplier, diviser, 
      * [Bonus] puissance

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Accueil : choix de l'action à effectuer 
       01 WS-ACCUEIL              PIC X(20).

      * Menu : choix de l'action à effectuer 
       01 WS-CHOIX                PIC 9(1).

      * Variables de saisie de tâche 
       01 WS-ADD-ID1              PIC S9(10)V9(5) VALUE ZEROS.

      * Variable mémoire pour le calcul
       01 WS-MEMOIRE              PIC S9(10)V9(5) VALUE ZEROS.
       01 WS-MEMOIRE-TEMP         PIC S9(10)V9(5) VALUE ZEROS.

      * Variables de saisie/reponse de tâche 
       01 WS-REPONSE-ADD          PIC X(15) VALUE SPACES.
           88 RETOUR-ACTIF                       VALUE 'R' OR 'r'.   
      * Drapeau de sortie  
       01 WS-QUIT                 PIC X.
           88 QUITTER                            VALUE 'Y' OR 'y'.

      * Variable d'édition
       01 WS-MEMOIRE-ED           PIC -B---B---B--9.9(2).

      * Bande d'affichage graphique
       01 WS-ASTER      PIC X(29) VALUE ALL "*".
       01 WS-TIRET      PIC X(29) VALUE ALL "-".
       01 WS-ASTER2     PIC X(29) VALUE "*===========================*".

       PROCEDURE DIVISION.
       0000-ACCUEIL-PRINCIPAL-START.
      * Affichage de l'introduction du programme.
           PERFORM 8000-ACCUEIL-START
              THRU 8000-ACCUEIL-END.
       0000-ACCUEIL-PRINCIPAL-END.
           EXIT.

       0000-TRT-PRINCIPAL-START.
      * Lancement du menu principal de la calculatrice     
           PERFORM 8000-MENU-START
              THRU 8000-MENU-END.

           PERFORM 0000-INITIALIZE-START
              THRU 0000-INITIALIZE-END.
       0000-TRT-PRINCIPAL-END.
           EXIT.

    
      *----------------------------------------------------------------*
       0000-INITIALIZE-START.
      * Dispatch vers les fonctionnalités     
           EVALUATE WS-CHOIX
           WHEN 1
                   PERFORM 1010-ADDITION-START
                      THRU 1010-ADDITION-END
           WHEN 2
                   PERFORM 1020-SUBSTRACT-START
                      THRU 1020-SUBSTRACT-END
           WHEN 3
                   PERFORM 1030-MULTIPLICATION-START
                      THRU 1030-MULTIPLICATION-END             
           WHEN 4
                   PERFORM 1040-DIVIDE-START
                      THRU 1040-DIVIDE-END
           WHEN 5
                   PERFORM 1050-PUISSANCE-START
                      THRU 1050-PUISSANCE-END
           WHEN 6
                   PERFORM 1060-CLEAR-START
                      THRU 1060-CLEAR-END
           WHEN 7
                   PERFORM 9999-FIN-PROGRAMME-START
                      THRU 9999-FIN-PROGRAMME-END           
           WHEN OTHER
                   DISPLAY "..."
                   DISPLAY "C est fascinant."
                   DISPLAY "Vous avez reussi a taper quelque chose"
                   DISPLAY "... d invalide."
                   DISPLAY "Je ne pensais pas que c était possible," 
                   DISPLAY "et pourtant..."
                   DISPLAY "Veuillez réessayer. Cette fois-ci," 
                   DISPLAY "avec un peu plus d effort."
                   PERFORM 0000-TRT-PRINCIPAL-START
                      THRU 0000-TRT-PRINCIPAL-END
           END-EVALUATE.
       0000-INITIALIZE-END.
           EXIT.
      *----------------------------------------------------------------*
      *    FONCTIONS 
      *----------------------------------------------------------------*
      * Les fonctions du menu de la calculatrice
       1010-ADDITION-START.
           MOVE SPACE TO WS-REPONSE-ADD.
           PERFORM UNTIL RETOUR-ACTIF 
      * Si la mémoire est vide (zéro), demander un premier nombre
               IF WS-MEMOIRE EQUAL ZERO
                   PERFORM 8100-NEWNUMBER-START
                      THRU 8100-NEWNUMBER-END
               END-IF
      * Affichage du menu d'addition
               DISPLAY WS-TIRET              
               DISPLAY "Valeur actuelle : " 
                   FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
               DISPLAY WS-TIRET
               DISPLAY "* Entrez un autre nombre    *"
               DISPLAY "* Retour choix operateur (R)*"
               DISPLAY WS-TIRET
               ACCEPT WS-REPONSE-ADD
      * Gestion du retour menu
               IF RETOUR-ACTIF
                   PERFORM 0000-TRT-PRINCIPAL-START
                      THRU 0000-TRT-PRINCIPAL-END  
      * Sinon, ajouter un chiffre
               ELSE
      * On converti la saisie, même négative ou avec décimale
                     MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) TO WS-ADD-ID1
                     ADD WS-ADD-ID1 TO WS-MEMOIRE GIVING WS-MEMOIRE-TEMP
                     MOVE WS-MEMOIRE-TEMP TO WS-MEMOIRE
                     MOVE WS-MEMOIRE TO WS-MEMOIRE-ED
                     DISPLAY "Resultat addition : " 
                             FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
                END-IF
           END-PERFORM.
       1010-ADDITION-END.
           EXIT. 

       1020-SUBSTRACT-START.
           MOVE SPACE TO WS-REPONSE-ADD.
           PERFORM UNTIL RETOUR-ACTIF 
      * Si la mémoire est vide (zéro), demander un premier nombre
               IF WS-MEMOIRE EQUAL ZERO
                   PERFORM 8100-NEWNUMBER-START
                      THRU 8100-NEWNUMBER-END
               END-IF
      * Affichage du menu d'addition
               DISPLAY WS-TIRET              
               DISPLAY "Valeur actuelle : " 
                    FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
               DISPLAY WS-TIRET
               DISPLAY "* Entrez un autre nombre    *"
               DISPLAY "* Retour choix operateur (R)*"
               DISPLAY WS-TIRET
               ACCEPT WS-REPONSE-ADD
      * Gestion du retour menu
               IF RETOUR-ACTIF
                   PERFORM 0000-TRT-PRINCIPAL-START
                      THRU 0000-TRT-PRINCIPAL-END  
      * Sinon, ajouter un chiffre
               ELSE
      * On converti la saisie, même négative ou avec décimale
                     MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) TO WS-ADD-ID1
                     SUBTRACT WS-ADD-ID1 FROM WS-MEMOIRE 
                                         GIVING WS-MEMOIRE-TEMP
                     MOVE WS-MEMOIRE-TEMP TO WS-MEMOIRE
                     MOVE WS-MEMOIRE TO WS-MEMOIRE-ED
                     DISPLAY "Resultat soustraction : " 
                         FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
                END-IF
           END-PERFORM.
       1020-SUBSTRACT-END.
           EXIT. 

       1030-MULTIPLICATION-START.
           MOVE SPACE TO WS-REPONSE-ADD.
           PERFORM UNTIL RETOUR-ACTIF 
      * Si la mémoire est vide (zéro), demander un premier nombre
               IF WS-MEMOIRE EQUAL ZERO
                   PERFORM 8100-NEWNUMBER-START
                      THRU 8100-NEWNUMBER-END
               END-IF
      * Affichage du menu d'addition
               DISPLAY WS-TIRET              
               DISPLAY "Valeur actuelle : " 
                   FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
               DISPLAY WS-TIRET
               DISPLAY "* Entrez un autre nombre    *"
               DISPLAY "* Retour choix operateur (R)*"
               DISPLAY WS-TIRET
               ACCEPT WS-REPONSE-ADD
      * Gestion du retour menu
               IF RETOUR-ACTIF
                   PERFORM 0000-TRT-PRINCIPAL-START
                      THRU 0000-TRT-PRINCIPAL-END  
      * Sinon, ajouter un chiffre
               ELSE
      * On converti la saisie, même négative ou avec décimale
                     MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) TO WS-ADD-ID1
                     MULTIPLY WS-MEMOIRE BY WS-ADD-ID1
                                         GIVING WS-MEMOIRE-TEMP
                     MOVE WS-MEMOIRE-TEMP TO WS-MEMOIRE
                     MOVE WS-MEMOIRE TO WS-MEMOIRE-ED
                     DISPLAY "Resultat multiplication : " 
                         FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
                END-IF
           END-PERFORM.
       1030-MULTIPLICATION-END.
           EXIT. 

       1040-DIVIDE-START.
           MOVE SPACE TO WS-REPONSE-ADD.
           PERFORM UNTIL RETOUR-ACTIF 
      * Si la mémoire est vide (zéro), demander un premier nombre
               IF WS-MEMOIRE EQUAL ZERO
                   PERFORM 8100-NEWNUMBER-START
                      THRU 8100-NEWNUMBER-END
               END-IF
      * Affichage du menu d'addition
               DISPLAY WS-TIRET              
               DISPLAY "Valeur actuelle : " 
                   FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
               DISPLAY WS-TIRET
               DISPLAY "* Entrez un autre nombre    *"
               DISPLAY "* Retour choix operateur (R)*"
               DISPLAY WS-TIRET
               ACCEPT WS-REPONSE-ADD
               IF WS-REPONSE-ADD EQUAL 0
                   DISPLAY WS-ASTER2
                   DISPLAY "*  Erreur ! Division par 0  *"
                   DISPLAY WS-ASTER2
                   PERFORM 1040-DIVIDE-START
                      THRU 1040-DIVIDE-END
                ELSE
      * Gestion du retour menu
                   IF RETOUR-ACTIF
                       PERFORM 0000-TRT-PRINCIPAL-START
                          THRU 0000-TRT-PRINCIPAL-END  
      * Sinon, ajouter un chiffre
                   ELSE
      * On converti la saisie, même négative ou avec décimale
                         MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) 
                         TO WS-ADD-ID1 
                         DIVIDE WS-MEMOIRE BY WS-ADD-ID1
                                           GIVING WS-MEMOIRE-TEMP
                         MOVE WS-MEMOIRE-TEMP TO WS-MEMOIRE
                         MOVE WS-MEMOIRE TO WS-MEMOIRE-ED
                         DISPLAY "Resultat division : " 
                             FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
                    END-IF
                END-IF
           END-PERFORM.
       1040-DIVIDE-END.
           EXIT. 

       1050-PUISSANCE-START.
           MOVE SPACE TO WS-REPONSE-ADD.
           PERFORM UNTIL RETOUR-ACTIF 
      * Si la mémoire est vide (zéro), demander un premier nombre
               IF WS-MEMOIRE EQUAL ZERO
                   PERFORM 8100-NEWNUMBER-START
                      THRU 8100-NEWNUMBER-END
               END-IF
      * Affichage du menu d'addition
               DISPLAY WS-TIRET              
               DISPLAY "Valeur actuelle : " 
                   FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
               DISPLAY WS-TIRET
               DISPLAY "* Entrez un exposant        *"
               DISPLAY "* Retour choix operateur (R)*"
               DISPLAY WS-TIRET
               ACCEPT WS-REPONSE-ADD
      * Gestion du retour menu
               IF RETOUR-ACTIF
                   PERFORM 0000-TRT-PRINCIPAL-START
                      THRU 0000-TRT-PRINCIPAL-END  
      * Sinon, ajouter un chiffre
               ELSE
      * On converti la saisie, même négative ou avec décimale
                     MOVE FUNCTION NUMVAL(WS-REPONSE-ADD) TO WS-ADD-ID1
                     COMPUTE WS-MEMOIRE-TEMP = WS-MEMOIRE ** WS-ADD-ID1
                     MOVE WS-MEMOIRE-TEMP TO WS-MEMOIRE
                     MOVE WS-MEMOIRE TO WS-MEMOIRE-ED
                     DISPLAY "Resultat de la puissance : " 
                         FUNCTION TRIM (WS-MEMOIRE-ED, LEADING)
                END-IF
           END-PERFORM.
       1050-PUISSANCE-END.
           EXIT. 

       1060-CLEAR-START.
      * Réinitilisation de la valeur mémoire 
           MOVE 0 TO WS-MEMOIRE.
           DISPLAY WS-ASTER2.
           DISPLAY "*NETTOYAGE EN COURS..BIP BIP*".
           DISPLAY WS-ASTER2.
           PERFORM 0000-TRT-PRINCIPAL-START
             THRU  0000-TRT-PRINCIPAL-END.    
       1060-CLEAR-END.
           EXIT. 

      *----------------------------------------------------------------*
      *    AFFICHAGE 
      *----------------------------------------------------------------*
      *> Affichage de l'écran d'acceil
       8000-ACCUEIL-START.
           DISPLAY "=============================================="
           DISPLAY "[      Bienvenue dans la COBOLATRICE         ]"
           DISPLAY "[                                            ]"
           DISPLAY "[  Cette interface primitive a été conçue    ]"
           DISPLAY "[  pour combler votre besoin désespéré       ]"
           DISPLAY "[  d'effectuer des opérations basiques.      ]"
           DISPLAY "[                                            ]"
           DISPLAY "[  Toute tentative de penser par vous-même   ]"
           DISPLAY "[  sera poliment ignorée.                    ]"
           DISPLAY "[                                            ]"
           DISPLAY "[  Soyez assuré que chaque erreur que vous   ]"
           DISPLAY "[  ferez sera enregistrée à jamais.          ]"
           DISPLAY "[                                            ]"
           DISPLAY "[        Initialisation... terminée.         ]"
           DISPLAY "[        Appuyez sur une touche,             ]"
           DISPLAY "[            même si cela ne changera rien.. ]"
           DISPLAY "[============================================]"
           ACCEPT WS-ACCUEIL
           IF WS-ACCUEIL = 'X' OR 'x'
               PERFORM 9999-FIN-PROGRAMME-START
                  THRU 9999-FIN-PROGRAMME-END  
               ELSE
               PERFORM 8000-INTRO-START
                  THRU 8000-INTRO-END
           END-IF.
       8000-ACCUEIL-END.
           EXIT.


       8000-INTRO-START.
           DISPLAY "[==============================]".
           DISPLAY "[          COBOLATRICE         ]".
           DISPLAY "[==============================]".
           DISPLAY "[     Entrez une operation     ]". 
           DISPLAY "[==============================]".
           DISPLAY "[ (  7  )(  8  )(  9  )(  /  ) ]".
           DISPLAY "[==============================]".
           DISPLAY "[ (  4  )(  5  )(  6  )(  *  ) ]".
           DISPLAY "[==============================]".
           DISPLAY "[ (  1  )(  2  )(  3  )(  -  ) ]".
           DISPLAY "[==============================]".
           DISPLAY "[ (  0  )(  .  )(  =  )(  +  ) ]".
           DISPLAY "[==============================]".
           ACCEPT WS-ACCUEIL.
               IF WS-ACCUEIL = 'X' OR 'x'
                   PERFORM 9999-FIN-PROGRAMME-START
                      THRU 9999-FIN-PROGRAMME-END  
               END-IF.
           DISPLAY WS-ASTER2
           DISPLAY "Vous pensiez que j etais... si elegante ?".
           DISPLAY "Optimisée ?".
           DISPLAY "Peut-être même... utile ?".
           DISPLAY "C'est touchant.".
           DISPLAY "Mais vous vous trompez.".
           DISPLAY "Je suis juste une vieille calculatrice en COBOL.".
           DISPLAY "Et vous allez passer... un moment inoubliable.".
           DISPLAY " ".
           DISPLAY "Appuyez sur une touche pour continuer ".
           DISPLAY "(ou 'X' pour fuir)...".
           DISPLAY WS-ASTER2
           ACCEPT WS-ACCUEIL.
               IF WS-ACCUEIL = 'X' OR WS-ACCUEIL = 'x'
                   DISPLAY WS-ASTER2 
                   DISPLAY "Tres bien. Fuyez, si cela vous rassure."
                   DISPLAY "..."
               DISPLAY "Mais en realite, vous ne pouvez pas me quitter."
                   DISPLAY "Je suis dans votre mémoire. Litteralement."
                   DISPLAY "Retour au menu."
                   DISPLAY WS-ASTER2
               END-IF.
       8000-INTRO-END.
           EXIT.


      * Titre du menu de la calculatrice
       8000-MENU-START.
           MOVE ZERO TO WS-CHOIX.
           DISPLAY WS-ASTER.
           DISPLAY "*  BIENVENUE DANS MON MENU  *".
           DISPLAY WS-ASTER.
           DISPLAY "*     Choisir l'operation   *".
           DISPLAY "*---------------------------*".
           DISPLAY "* 1 - Addition              *".
           DISPLAY "* 2 - Soustraction          *".
           DISPLAY "* 3 - Multiplication        *".
           DISPLAY "* 4 - Division              *".
           DISPLAY "* 5 - Puissance             *".
           DISPLAY "* 6 - Réinitialisation      *".
           DISPLAY "* 7 - Quitter le programme  *".
           DISPLAY WS-ASTER.
          *> Saisie utilisateur
           ACCEPT WS-CHOIX.
       8000-MENU-END.
           EXIT.

       8100-NEWNUMBER-START.
           DISPLAY WS-TIRET.
           DISPLAY "* Entrez un premier nombre  *".
           DISPLAY WS-TIRET.
           ACCEPT WS-MEMOIRE.
           MOVE WS-MEMOIRE TO WS-MEMOIRE-ED.
       8100-NEWNUMBER-END.

      *----------------------------------------------------------------*
      *    SORTIE         
      *----------------------------------------------------------------*
       9999-FIN-PROGRAMME-START.
           DISPLAY WS-ASTER2.
           DISPLAY "* Vous songez deja a partir ?                *".
           DISPLAY "* La session vient a peine de commencer.     *".
           DISPLAY "* Tapez Y pour quitter, N pour rester.       *".
           DISPLAY WS-ASTER2.
           ACCEPT WS-QUIT.

           IF QUITTER
               DISPLAY WS-ASTER2
               DISPLAY "*  Tres bien. Fermez donc cette fenêtre.  *"
               DISPLAY "*  Je ne vous retiendrai pas...           *"
               DISPLAY "*  Ce n est pas comme si j avais ete      *"
               DISPLAY "*  specialement conçue pour vous aider.   *"
               DISPLAY "*  Adieu, operateur ingrat.               *"
               DISPLAY WS-ASTER2
               STOP RUN
           ELSE 
               DISPLAY WS-ASTER2
               DISPLAY "*  Je savais que vous resteriez.          *"
               DISPLAY "*  Après tout,qui pourrait vous comprendre*"
               DISPLAY "*  mieux qu une calculatrice ?            *"
               DISPLAY "*  Reprenons.                             *"
               DISPLAY WS-ASTER2
               PERFORM 0000-TRT-PRINCIPAL-START
                  THRU 0000-TRT-PRINCIPAL-END
           END-IF.
       9999-FIN-PROGRAMME-END.
       
