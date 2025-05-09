      *> ---------------------------------------------------------------
      *> PROGRAMME : VIP
      *> OBJET     : Interroge l'utilisateur sur son statut (VIP ou Standard)
      *>             et son solde, puis affiche un message selon :
      *>             - VIP avec solde > 10000    -> "VIP Premium"
      *>             - VIP avec solde ≤ 10000    -> "VIP Privilégié"
      *>             - Standard avec solde > 5000 -> "Client fidèle"
      *>             - Standard avec solde ≤ 5000 -> "Client standard"
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise EVALUATE pour dispatcher le traitement selon le statut.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP.
       AUTHOR. Vincent-cmd1

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Contient le type de client (VIP ou Standard)
       01 WS-STATUT         PIC X(8).                           
      *> Solde saisi pour un client VIP 
       01 WS-SOLDE1         PIC 9(10). 
      *> Solde saisi pour un client Standard                         
       01 WS-SOLDE2         PIC 9(10).                          

       PROCEDURE DIVISION.
      *> Invite à entrer le statut
           DISPLAY 'Quel est votre statut ? (VIP/standard)'.    
           ACCEPT WS-STATUT.

           EVALUATE WS-STATUT
      *> Cas des clients VIP (sensibilité à la casse)
               WHEN EQUAL TO 'VIP' OR 'Vip' OR 'vip'            
                   DISPLAY "Bienvenue VIP ! Quel est votre solde ?"
      *> Saisie du solde
                   ACCEPT WS-SOLDE1                             
                   IF WS-SOLDE1 > 10000
      *> Solde élevé pour VIP
                       DISPLAY "Vous êtes un VIP Premium"       
                   ELSE
      *> Solde modeste pour VIP
                       DISPLAY "Vous êtes un VIP Privilégié"    
                   END-IF

               WHEN EQUAL TO 'STANDARD' OR 'Standard' OR 'standard'
                   DISPLAY "Bienvenue Standard ! Quel est votre solde ?"
       *> Saisie du solde pour client Standard
                   ACCEPT WS-SOLDE2                          
                   IF WS-SOLDE2 > 5000
      *> Solde élevé
                       DISPLAY "Vous êtes un client standard fidèle"  
                   ELSE
      *> Solde modeste
                       DISPLAY "Vous êtes un client standard"        
                   END-IF

               WHEN OTHER
      *> Statut invalide
                   DISPLAY "Erreur dans votre saisie du statut." 
                   DISPLAY "Veuillez de nouveau entrer votre statut."
      *> Nouvelle tentative de saisie
                   ACCEPT WS-STATUT                            
           END-EVALUATE.
      *> Fin du programme
           STOP RUN.                                      
