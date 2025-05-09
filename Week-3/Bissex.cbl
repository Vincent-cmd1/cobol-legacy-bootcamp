      *> ---------------------------------------------------------------
      *> PROGRAMME : BISSEX
      *> OBJET     : Demande à l'utilisateur une année, puis détermine
      *>             si cette année est bissextile ou non.
      *>             Une année est bissextile si :
      *>             - Elle est divisible par 4 ET non divisible par 100
      *>             - OU elle est divisible par 400
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilise trois DIVIDE ... REMAINDER pour tester les cas
      *>             - WS-POUBELLE est utilisé comme quotient jetable
      *>             - Affichage conditionnel basé sur le test combiné
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BISSEX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable contenant l'année saisie
       01 WS-ANNEE        PIC 9(4).

      * Reste de la division par 4
       01 WS-ANNEE-R4     PIC 9(3).

      * Reste de la division par 400
       01 WS-ANNEE-R400   PIC 9(3).

      * Reste de la division par 100
       01 WS-ANNEE-R100   PIC 9(3).

      * Quotient inutile, utilisé comme poubelle
       01 WS-POUBELLE     PIC 9(1).

       PROCEDURE DIVISION.

      * Demande de saisie de l'année
           DISPLAY "Entrez une année".
           ACCEPT WS-ANNEE.

      * Calcul des restes pour tests de divisibilité
           DIVIDE WS-ANNEE BY 4 GIVING WS-POUBELLE 
                                        REMAINDER WS-ANNEE-R4.
           DIVIDE WS-ANNEE BY 400 GIVING WS-POUBELLE 
                                          REMAINDER WS-ANNEE-R400.
           DIVIDE WS-ANNEE BY 100 GIVING WS-POUBELLE 
                                          REMAINDER WS-ANNEE-R100.

      * Vérification des conditions de bissextilité
           IF WS-ANNEE-R4 = 0 AND WS-ANNEE-R100 NOT = 0
                OR WS-ANNEE-R400 = 0
               DISPLAY "Année bissextile"
           ELSE 
               DISPLAY "Année normale"
           END-IF.

           STOP RUN.
