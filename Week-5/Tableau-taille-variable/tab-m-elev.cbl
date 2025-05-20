      ******************************************************************
      * PROGRAMME : TAB-M-ELEV                                        *
      * AUTEUR    : Vincent-Cmd1                                      *
      * OBJET     : Saisie et affichage de noms/prénoms d'élèves      *
      *             répartis dans deux classes (CM1 et CM2).          *
      *                                                               *
      * STRUCTURE  :                                                  *
      *   - Saisie des noms de classe                                 *
      *   - Saisie des élèves pour chaque classe                      *
      *   - Affichage des résultats saisis                            *
      *                                                               *
      * NB : Aucun contrôle de validité n'est effectué.               *
      *                                                               *
      * DATE : Un jour où le COBOL n'est pas encore mort.             *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. tab-m-elev.
       AUTHOR. Vincent-Cmd1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Déclaration du tableau des élèves pour 2 classes de 6 élèves 
       01 TAB-ELEVES.
           05 WS-CLASSE OCCURS 2 TIMES.        
               10 WS-NOM-CLASSE             PIC X(04).  *> Classe
               10 WS-ELEVE OCCURS 6 TIMES.
                   15 WS-NOM-ELEVES         PIC X(15).  *> Nom 
                   15 WS-PRENOM-ELEVES      PIC X(15).  *> Prénom 

      * Index de boucle
       01 WS-I-CLASSE                       PIC 9(01) VALUE 1.
       01 WS-I-ELEVE                        PIC 9(01) VALUE 1.
       01 WS-F-CLASSE                       PIC 9(01) VALUE 2.
       01 WS-F-ELEVE                        PIC 9(01) VALUE 6.

       PROCEDURE DIVISION.

      ******************************************************************
      * 1. SAISIE DES CLASSES ET DES ÉLÈVES
      ******************************************************************         
       PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 
               UNTIL WS-I-CLASSE > WS-F-CLASSE

           DISPLAY "Veuillez entrer la classe des élèves (CM1/CM2)."
           ACCEPT WS-NOM-CLASSE(WS-I-CLASSE) 

           PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
                   UNTIL WS-I-ELEVE > WS-F-ELEVE

               DISPLAY "Veuillez entrer son nom, puis son prénom."
               DISPLAY "Nom : " SPACE WITH NO ADVANCING 
               ACCEPT WS-NOM-ELEVES(WS-I-CLASSE, WS-I-ELEVE) 

               DISPLAY "Prénom : " SPACE WITH NO ADVANCING
               ACCEPT WS-PRENOM-ELEVES(WS-I-CLASSE, WS-I-ELEVE) 

           END-PERFORM

       END-PERFORM.

      ******************************************************************
      * 2. AFFICHAGE DES DONNÉES SAISIES
      ******************************************************************         
       PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 
               UNTIL WS-I-CLASSE > WS-F-CLASSE

           DISPLAY "Classe : " WS-NOM-CLASSE(WS-I-CLASSE)
       
           PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
                   UNTIL WS-I-ELEVE > WS-F-ELEVE

               DISPLAY "Nom : " WS-NOM-ELEVES(WS-I-CLASSE, WS-I-ELEVE)
               SPACE WITH NO ADVANCING 
               DISPLAY "Prénom : " 
               WS-PRENOM-ELEVES(WS-I-CLASSE, WS-I-ELEVE)

           END-PERFORM

       END-PERFORM.

      ******************************************************************
      * FIN DU PROGRAMME
      ******************************************************************
           STOP RUN.
