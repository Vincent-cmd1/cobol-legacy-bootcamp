      *> ---------------------------------------------------------------
      *> PROGRAMME : PGCD
      *> OBJET     : Demander à l'utilisateur deux entiers positifs
      *>             et calculer leur Plus Grand Commun Diviseur (PGCD)
      *>             en utilisant l'algorithme d’Euclide par soustraction.
      *> AUTEUR    : Terry & Leocrabe225
      *> REMARQUE  :
      *>             - Le PGCD est trouvé par soustractions successives
      *>               jusqu'à ce que les deux nombres soient égaux.
      *>             - L'utilisateur doit entrer deux nombres > 0,
      *>               sinon le programme se termine immédiatement.
      *>             - La version n'utilise pas de fonction récursive.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGCD.
       AUTHOR. Terry&Leocrabe225.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Nombres à comparer, saisis par l'utilisateur
       01  NUM-1        PIC 9(10).
       01  NUM-2        PIC 9(10).

       PROCEDURE DIVISION.

      * Saisie des deux entiers
           DISPLAY "Saisissez deux nombres entiers positifs".
           ACCEPT NUM-1
           ACCEPT NUM-2

      * Contrôle basique : les deux nombres doivent être > 0
           IF NUM-1 < 1 OR NUM-2 < 1 THEN
               DISPLAY "Les deux nombres doivent être > 0."
               STOP RUN
           END-IF.

      * Algorithme d'Euclide par soustraction
           PERFORM UNTIL NUM-1 EQUAL NUM-2

      * Soustraction du plus petit au plus grand
               IF NUM-1 > NUM-2
                   SUBTRACT NUM-2 FROM NUM-1
               ELSE 
                   SUBTRACT NUM-1 FROM NUM-2
               END-IF

      * Affichage de l’état après chaque opération
               DISPLAY "NUM-1 : " NUM-1 " ||| NUM-2 : " NUM-2

           END-PERFORM.

      * Affichage du PGCD (quand les deux nombres sont égaux)
           DISPLAY "LE PGCD est : " NUM-1.

           STOP RUN.
