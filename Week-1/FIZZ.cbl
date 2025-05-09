      *> ---------------------------------------------------------------
      *> PROGRAMME : FIZZ
      *> OBJET     : Programme COBOL implémentant la logique FIZZBUZZ.
      *>             L'utilisateur saisit un nombre entre 1 et 100.
      *>             Le programme affiche :
      *>               - "FIZZBUZZ" si divisible par 3 et 5
      *>               - "FIZZ" si divisible par 3
      *>               - "BUZZ" si divisible par 5
      *>               - Le nombre sinon
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise DIVIDE avec REMAINDER, et EVALUATE TRUE
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZ.
       AUTHOR. Vincent-cmd1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Nombre saisi
       01 WS-NUMA          PIC 9(3).    
      *> Reste de la division par 3                        
       01 WS-FIZZ          PIC 9(3).      
      *> Reste de la division par 5                      
       01 WS-BUZZ          PIC 9(3).                           

       PROCEDURE DIVISION.
      *> Invite utilisateur
           DISPLAY "Veuillez entrer un chiffre entre 1 et 100." 
      *> Saisie du nombre
           ACCEPT WS-NUMA                                      

      *> Calcule le reste /3
           DIVIDE WS-NUMA BY 3 GIVING WS-FIZZ REMAINDER WS-FIZZ 
      *> Calcule le reste /5
           DIVIDE WS-NUMA BY 5 GIVING WS-BUZZ REMAINDER WS-BUZZ 

           EVALUATE TRUE
      *> Divisible par 3 et 5
               WHEN WS-FIZZ = 0 AND WS-BUZZ = 0
                   DISPLAY "FIZZBUZZ"    
      *> Divisible par 3 uniquement                    
               WHEN WS-FIZZ = 0
                   DISPLAY "FIZZ"  
      *> Divisible par 5 uniquement                            
               WHEN WS-BUZZ = 0
                   DISPLAY "BUZZ"      
      *> Sinon, affiche le nombre                         
               WHEN OTHER
                   DISPLAY "Valeur saisie : " WS-NUMA         
           END-EVALUATE.

           STOP RUN.                                        

       

