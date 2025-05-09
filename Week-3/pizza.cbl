      *> ---------------------------------------------------------------
      *> PROGRAMME : PIZZA
      *> OBJET     : Demander à l'utilisateur son nom, prénom et le
      *>             nombre d'invités, puis calculer le nombre de pizzas
      *>             à commander en supposant 1,1 pizza par invité.
      *>             Le résultat est arrondi à l'entier supérieur.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilise la SCREEN SECTION pour l'affichage
      *>               interactif et graphique.
      *>             - Utilise COMPUTE ... ROUNDED pour simuler l’arrondi.
      *>             - Affiche un message d'erreur si nom ou prénom vides.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PIZZA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * Saisie utilisateur
       01  WS-NOM          PIC X(20) VALUE SPACES.
       01  WS-PRENOM       PIC X(20) VALUE SPACES.

      * Nombre d'invités
       01  WS-NB-INVITE    PIC 9(02).

      * Drapeaux divers
       01  FIN-SAISIE      PIC X VALUE 'N'.
       01  WS-INTRO-STOP   PIC X(01).

      * Résultat du calcul
       01  WS-NB-PIZZA     PIC 9(2)V99 VALUE ZERO.

       SCREEN SECTION.

       01 ECRAN-INTRO.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1  PIC 9 TO WS-INTRO-STOP.
           05 LINE 3 COLUMN 10 PIC X(40) 
               VALUE "    P P P  IIIII  ZZZZZ  ZZZZZ    A   "
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 4 COLUMN 10 PIC X(40) 
               VALUE "    P   P    I       Z      Z    A A  " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 5 COLUMN 10 PIC X(40) 
               VALUE "    PPPP     I      Z      Z    A   A " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 6 COLUMN 10 PIC X(40) 
               VALUE "    P        I     Z      Z    AAAAAAA" 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 7 COLUMN 10 PIC X(40) 
               VALUE "    P      IIIII  ZZZZZ  ZZZZZA       A" 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.

           05 LINE 11 COLUMN 15 PIC X(40) 
               VALUE "   / __ \ / __ \/_  __ \/ ____/ " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 12 COLUMN 15 PIC X(40) 
               VALUE "  / /_/ // / / / / / / / __/    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 13 COLUMN 15 PIC X(40) 
               VALUE " / _, _// /_/ / / / / / /___    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 14 COLUMN 15 PIC X(40) 
               VALUE "/_/ |_| \____/ /_/ /_/_____/    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.

           05 LINE 18 COLUMN 20 PIC X(40) 
               VALUE "WELCOME TO COBOLEZZA" HIGHLIGHT.
           05 LINE 20 COLUMN 24 PIC X(40) 
               VALUE "PRESS ENTER" LOWLIGHT.

       01 ECRAN-SAISI.
           05 BLANK SCREEN.
           05 LINE 3 COLUMN 10 
               VALUE "Veuillez commander votre pizza"
                    HIGHLIGHT FOREGROUND-COLOR 4.
           05 LINE 5 COLUMN 10 VALUE "Entrez votre prenom :".
           05 LINE 5 COLUMN 50 PIC X(20) TO WS-PRENOM.
           05 LINE 7 COLUMN 10 VALUE "Entrez votre nom :".
           05 LINE 7 COLUMN 50 PIC X(20) TO WS-NOM.
           05 LINE 9 COLUMN 10 
               VALUE "Entrez le nombre de vos invites :".
           05 LINE 9 COLUMN 50 PIC X(20) TO WS-NB-INVITE.

       PROCEDURE DIVISION.

      * Affiche l'intro graphique
           DISPLAY ECRAN-INTRO.
           ACCEPT WS-INTRO-STOP.

      * Affiche l’écran de saisie
           DISPLAY ECRAN-SAISI.
           ACCEPT ECRAN-SAISI.

      * Calcul du nombre de pizzas (avec arrondi)
           COMPUTE WS-NB-PIZZA ROUNDED = WS-NB-INVITE * 1.1.

      * Affichage du résultat
           DISPLAY "Nombre de pizzas à commander : " WS-NB-PIZZA.

      * Vérification que nom et prénom ont bien été saisis
           IF WS-PRENOM = SPACES OR WS-NOM = SPACES
               DISPLAY "Nom et prénom obligatoires !"
               ACCEPT WS-PRENOM
           END-IF.

           STOP RUN.
