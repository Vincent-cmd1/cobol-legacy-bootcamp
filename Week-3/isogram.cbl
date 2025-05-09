      *> ---------------------------------------------------------------
      *> PROGRAMME : ISOGRAM
      *> OBJET     : Vérifier si un mot saisi par l'utilisateur est un
      *>             isogramme, c’est-à-dire qu’il ne contient aucune
      *>             lettre répétée.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Le mot saisi doit contenir uniquement des lettres.
      *>             - Vérification caractère par caractère.
      *>             - Sensible à la casse (A ≠ a).
      *>             - Utilise un tableau de lettres déjà vues.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. isogram.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Saisie utilisateur (jusqu’à 20 caractères)
       01 WS-MOT             PIC X(20).
       01 WS-MOT-LEN         PIC 99.

      * Tableau pour marquer les lettres déjà rencontrées
       01 WS-LETTRES-VUES.
          05 WS-LETTRE-VUE  OCCURS 26 TIMES PIC X VALUE SPACES.

      * Index de boucle
       01 WS-I              PIC 99 VALUE 1.
       01 WS-J              PIC 99.

      * Caractère en cours d’analyse
       01 WS-CHAR           PIC X.

      * Flag de détection de doublon
       01 WS-DOUBLON        PIC X VALUE 'N'. *> Y = trouvé, N = ok

       PROCEDURE DIVISION.

      * Demande du mot à analyser
           DISPLAY "Entrez un mot (sans accent, max 20 caractères) :"
           ACCEPT WS-MOT.

      * Déterminer la longueur réelle du mot
           COMPUTE WS-MOT-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-MOT)).

      * Boucle de traitement des lettres du mot
           PERFORM VARYING WS-I FROM 1 BY 1 
                         UNTIL WS-I > WS-MOT-LEN OR WS-DOUBLON = 'Y'
               MOVE WS-MOT(WS-I:1) TO WS-CHAR

      * Vérifie si la lettre a déjà été vue
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 26
                   IF WS-LETTRE-VUE(WS-J) = WS-CHAR
                       MOVE 'Y' TO WS-DOUBLON
                   END-IF
               END-PERFORM

      * Si pas vue, on l’ajoute dans le tableau
               IF WS-DOUBLON NOT = 'Y'
                   MOVE WS-CHAR TO WS-LETTRE-VUE(WS-I)
               END-IF
           END-PERFORM.

      * Affichage du résultat
           IF WS-DOUBLON = 'Y'
               DISPLAY "Le mot n'est PAS un isogramme."
           ELSE
               DISPLAY "Le mot est un isogramme."
           END-IF.

           STOP RUN.
