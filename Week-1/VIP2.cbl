      *> ---------------------------------------------------------------
      *> PROGRAMME : VIP2
      *> OBJET     : Identifier le type de client (VIP ou Standard)
      *>             puis afficher une catégorie selon son solde :
      *>             - VIP avec solde > 10000    -> VIP Premium
      *>             - VIP avec solde <= 10000   -> VIP Privilégié
      *>             - Standard avec solde > 5000 -> Client fidèle
      *>             - Standard avec solde <= 5000 -> Client standard
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Programme structuré avec paragraphes HN.
      *>             Gestion partielle d'erreur pour le statut.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Contient le statut client
       01 WS-STATUT         PIC X(8).  
      *> Solde saisi pour VIP                           
       01 WS-SOLDE1         PIC 9(10). 
      *> Solde saisi pour Standard                           
       01 WS-SOLDE2         PIC 9(10).                           

       PROCEDURE DIVISION.

       0000-INITIALISATION-DEB.
      *> Invite à saisir le statut
           DISPLAY 'Quel est votre statut ? (VIP/standard)'.     
           ACCEPT WS-STATUT.
       0000-INITIALISATION-FIN.

           IF WS-STATUT = 'VIP' OR 'Vip' OR 'vip'
      *> Invite à saisir le solde VIP
               DISPLAY "Bienvenue VIP ! Quel est votre solde ?"  
               ACCEPT WS-SOLDE1
               IF WS-SOLDE1 > 10000
      *> VIP avec solde élevé
                   DISPLAY "Vous êtes un VIP Premium"           
               ELSE
      *> VIP avec solde ≤ 10000
                   DISPLAY "Vous êtes un VIP Privilégié"        
               END-IF

           ELSE
      *> Statut non reconnu
               DISPLAY "Erreur dans la saisie de votre statut" 
      *> Redemande statut 
               PERFORM 0000-INITIALISATION-DEB
                   THRU 0000-INITIALISATION-FIN                
           END-IF.

           IF WS-STATUT = 'STANDARD' OR 'Standard' OR 'standard'
      *> Saisie solde Standard
               DISPLAY "Bienvenue Standard ! Quel est votre solde ?" 
               ACCEPT WS-SOLDE2
               IF WS-SOLDE2 > 5000
      *> Client fidèle
                   DISPLAY "Vous êtes un client standard fidèle"   
               ELSE
      *> Client normal
                   DISPLAY "Vous êtes un client standard"         
               END-IF
           END-IF.

           STOP RUN.                                            
