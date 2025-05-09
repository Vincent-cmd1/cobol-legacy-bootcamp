      *> ---------------------------------------------------------------
      *> PROGRAMME : CPT10
      *> OBJET     : Ce programme compte de 1 à 10 et affiche chaque
      *>             valeur à l'écran avec un message d'accompagnement.
      *>             Utilise une boucle PERFORM VARYING.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Exemple de PERFORM VARYING avec incrément simple.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPT10.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Compteur de boucle (1 à 10)
       01 WS-CPT          PIC 99 VALUE 0.                         

       PROCEDURE DIVISION.
      *> Message d’introduction
           DISPLAY "COMPTE A REBOURS LANCE :"                   
      *> Boucle de 1 à 10
           PERFORM VARYING WS-CPT FROM 1 BY 1 UNTIL WS-CPT > 10 
      *> Affiche le numéro actuel
               DISPLAY "EXPLOSION IMMINENTE : " WS-CPT        
           END-PERFORM.
      *> Message final dramatique
           DISPLAY "PAF PASTEQUE !".                             

           STOP RUN.                                        

