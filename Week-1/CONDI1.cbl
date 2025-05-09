      *> ---------------------------------------------------------------
      *> PROGRAMME : CONDI1
      *> OBJET     : Saisie d’un âge entre -99 et 99.
      *>             Affiche des messages selon l’âge saisi :
      *>             - Vérification signe (positif/négatif)
      *>             - Catégorisation par tranche d’âge
      *>             - Détection pair / impair
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise des conditions IF et EVALUATE TRUE,
      *>             ainsi qu’un DIVIDE avec REMAINDER.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDI1.
       AUTHOR. Vincent-cmd1

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Âge saisi (signé)
       01 WS-AGE1            PIC S9(3). 
      *> Résultat de la division par 2                          
       01 WS-AGE2            PIC 9(3).
      *> Reste de la division (pair/impair)                            
       01 WS-AGE3            PIC 9(3).                           

       PROCEDURE DIVISION.
      *> Invite à entrer l'âge
           DISPLAY 'Entrez un âge entre -99 et 99.'.             
           ACCEPT WS-AGE1.

           IF WS-AGE1 >= 0 THEN
      *> Affiche message pour âge positif
               DISPLAY "Bravo ! Vous êtes vivant !"             
           ELSE    
      *> Message pour âge négatif
               DISPLAY "Votre age est négatif. Etrange..."       
           END-IF.

      *> Début de la catégorisation
           DISPLAY "Nous allons catégoriser votre âge : ".      

           EVALUATE TRUE
               WHEN WS-AGE1 < 0
       *> Catégorie : âge négatif
                   DISPLAY "Vous n'existez pas !"  
       *> Catégorie : 0            
               WHEN WS-AGE1 = 0
                   DISPLAY "Vous êtes un bébé !" 
      *> Catégorie : 1–9             
               WHEN WS-AGE1 > 0 AND WS-AGE1 < 10
                   DISPLAY "Vous êtes un enfant !"
      *> Catégorie : 10–18              
               WHEN WS-AGE1 > 9 AND WS-AGE1 < 19
                   DISPLAY "Vous êtes un adolescent !" 
      *> Catégorie : 19–60         
               WHEN WS-AGE1 > 18 AND WS-AGE1 < 61
                   DISPLAY "Vous êtes un adulte !" 
      *> Catégorie : 61–99            
               WHEN WS-AGE1 > 60 AND WS-AGE1 < 100
                   DISPLAY "Vous êtes vieux !"   
      *> Au-delà de 99 (ou farfelu)              
               WHEN OTHER
                   DISPLAY "Vous êtes une légende vivante."    
           END-EVALUATE. 

      *> Division entière
           DIVIDE WS-AGE1 BY 2 GIVING WS-AGE2  
       *> Reste de la division (pair/impair)
                        REMAINDER WS-AGE3.                      
           IF WS-AGE3 > 0
      *> Affiche si impair
               DISPLAY "Votre âge est impair"                  
           ELSE 
       *> Affiche si pair
               DISPLAY "Votre âge est pair"                     
           END-IF.

           STOP RUN.                                           
