# Guide d'Exercices JCL - Synthèse et Corrections

## Objectifs pédagogiques
- Maîtriser les **principes de structure** d'un JCL avec la mise en pratique
- Savoir **écrire, lire et modifier** des scripts JCL

---

## Exercice 1 - Créer un job minimal

### Objectif
Écrire le JCL le plus simple possible pour comprendre la structure de base.

### Correction
```jcl
//TESTJOB1    JOB 'JOB1',MSGCLASS=X,CLASS=A,MSGLEVEL=(1,1),
//            NOTIFY=&SYSUID                               
//STEP1       EXEC PGM=IEFBR14                             
```

### 💡 Explications et astuces
- **IEFBR14** : Programme système vide, parfait pour les tests de syntaxe
- **MSGCLASS=X** : Classe de messages pour la sortie
- **CLASS=A** : Classe d'exécution du job
- **MSGLEVEL=(1,1)** : Affiche toutes les cartes JCL et tous les messages
- **NOTIFY=&SYSUID** : Notification à l'utilisateur à la fin du job
- **TESTJOB1** et **STEP1** sont des noms libres, pas des normes

---

## Exercice 2 - Définir un fichier temporaire

### Objectif
Apprendre à utiliser les datasets temporaires avec la carte DD.

### Correction
```jcl
//TESTJOB2    JOB 'JOB2',MSGCLASS=X,CLASS=A,MSGLEVEL=(1,1), 
//            NOTIFY=&SYSUID                                
//STEP1       EXEC PGM=IEFBR14                              
//FICTEMP     DD DSN=&&MONFIC,                              
//            DISP=(NEW,PASS),                              
//            UNIT=SYSDA,SPACE=(TRK,(1,1))                  
```

### 💡 Explications et astuces
- **DSN=&&MONFIC** : Les `&&` indiquent un fichier temporaire
- **DISP=(NEW,PASS)** : 
  - `NEW` : Crée un nouveau fichier
  - `PASS` : Le transmet aux étapes suivantes du même job
- **UNIT=SYSDA** : Unité de stockage système
- **SPACE=(TRK,(1,1))** : Allocation d'1 piste primaire + 1 piste secondaire
- ⚠️ **Attention** : Les fichiers temporaires sont supprimés à la fin du job

---

## Exercice 3 - Créer un fichier permanent

### Objectif
Créer un dataset physique permanent dans le catalogue.

### Correction
```jcl
//TESTJOB3    JOB 'JOB3',MSGCLASS=X,CLASS=A,MSGLEVEL=(1,1),    
//            NOTIFY=&SYSUID                                   
//STEP1       EXEC PGM=IEFBR14                                 
//FICOUT      DD DSN=Z65134.COPIE,                             
//            DISP=(NEW,CATLG,DELETE),                         
//            UNIT=SYSDA,SPACE=(TRK,(1,1)),                    
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)              
```

### 💡 Explications et astuces
- **DSN=Z65134.COPIE** : Nom qualifié permanent (commence par le userid)
- **DISP=(NEW,CATLG,DELETE)** :
  - `NEW` : Nouveau fichier
  - `CATLG` : Catalogue le fichier si succès
  - `DELETE` : Supprime si échec
- **DCB** (Data Control Block) :
  - `RECFM=FB` : Format d'enregistrement fixe bloqué
  - `LRECL=80` : Longueur d'enregistrement logique
  - `BLKSIZE=800` : Taille du bloc (multiple de LRECL)
- 🔍 **Vérification** : Consultable via ISPF 3.4

---

## Exercice 4 - Impression avec SYSOUT

### Objectif
Utiliser SYSOUT pour diriger la sortie vers l'imprimante/affichage.

### Correction
```jcl
//TESTJOB4    JOB 'JOB4',MSGCLASS=X,CLASS=A,MSGLEVEL=(1,1),             
//            NOTIFY=&SYSUID                                            
//STEP1       EXEC PGM=IEBGENER                                         
//FICOUT      DD DSN=Z65134.COPIE,                                      
//            DISP=(NEW,CATLG,DELETE),                                  
//            UNIT=SYSDA,SPACE=(TRK,(1,1)),                             
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                       
//IMPRIM      DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)        
//SYSUT1      DD *                                                      
CECI EST UN TEST D'IMPRESSION JCL                                       
/*                                                                      
//SYSUT2      DD SYSOUT=*                                               
//SYSPRINT    DD SYSOUT=*                                               
//SYSIN       DD DUMMY                                                  
```

### 💡 Explications et astuces
- **IEBGENER** : Utilitaire système de copie de données
- **SYSOUT=*** : Sortie vers la file d'attente système par défaut
- **DD *** : Données en ligne (inline data)
- **/*** : Délimiteur de fin de données inline
- **RECFM=FBA** : Format fixe bloqué avec caractère de contrôle ANSI
- **LRECL=133** : Standard pour impression (132 + 1 pour contrôle)
- **DD DUMMY** : Fichier factice (vide)
- 📋 **Résultat** : Visible dans SDSF (System Display and Search Facility)

---

## Exercice 5 - Réutiliser un fichier avec référence arrière

### Objectif
Utiliser la référence arrière pour réutiliser un dataset défini précédemment.

### Correction
```jcl
//TESTJOB5    JOB 'JOB5',MSGCLASS=X,CLASS=A,MSGLEVEL=(1,1),             
//            NOTIFY=&SYSUID                                            
//STEP1       EXEC PGM=IEBGENER                                         
//FICOUT1     DD DSN=&&MONFI,                                           
//            DISP=(NEW,PASS),                                          
//            UNIT=SYSDA,SPACE=(TRK,(1,1)),                             
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                       
//SYSUT1      DD *                                                      
CECI EST UN TEST TEMPORAIRE                                             
/*                                                                      
//SYSUT2      DD DSN=&&MONFI                                            
//SYSPRINT    DD SYSOUT=*                                               
//SYSIN       DD DUMMY                                                  
//STEP2       EXEC PGM=IEBGENER                                         
//SYSUT1      DD DSN=*.STEP1.FICOUT1                                    
//SYSUT2      DD SYSOUT=*                                               
//SYSPRINT    DD SYSOUT=*                                               
//SYSIN       DD DUMMY                                                  
```

### 💡 Explications et astuces
- **DSN=*.STEP1.FICOUT1** : Référence arrière (backward reference)
  - `*` : Indique une référence dans le même job
  - `STEP1` : Nom de l'étape précédente
  - `FICOUT1` : Nom de la DD dans cette étape
- **Avantages** : 
  - Évite la redéfinition du DSN
  - Maintient la cohérence entre étapes
  - Simplifie la maintenance du JCL
- ⚡ **Astuce** : La référence arrière ne fonctionne que vers des étapes précédentes

---

## Exercice 6 - Utiliser la carte EXEC avec paramètres

### Objectif
Paramétrer une exécution avec TIME, REGION et PARM dans un contexte de compilation/exécution COBOL.

### Correction (code de Terry)
```jcl
//TESTJOB6     JOB TESTJOB6,MSGCLASS=X,CLASS=A,                      
//                   MSGLEVEL=(1,1),NOTIFY=&SYSUID                       
//STEP1        EXEC IGYWCL                                            
//COBOL.SYSIN  DD DSN=Z72426.SOURCE.COBOL(HELLOCOB),DISP=SHR,     
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)               
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HELLOCOB),DISP=SHR           
//****************************************************************
//             IF RC = 0 THEN                                               
//****************************************************************
//RUN          EXEC PGM=HELLOCOB,                                    
//             TIME=(1,0),REGION=512K,PARM='TEST01,PROD'             
//STEPLIB      DD DSN=&SYSUID..LOAD,DISP=SHR                         
//SYSOUT       DD SYSOUT=*                                           
//             ENDIF
```

### 💡 Explications et astuces

#### Structure du JCL
- **IGYWCL** : Procédure cataloguée IBM pour compiler et link-éditer un programme COBOL
  - Combine compilation COBOL + link-édition en une seule étape
  - Plus efficace que de faire IGYCRCTL puis IEWL séparément

#### Compilation COBOL (STEP1)
- **COBOL.SYSIN** : Source COBOL à compiler
  - `DSN=Z72426.SOURCE.COBOL(HELLOCOB)` : Membre HELLOCOB dans la PDS SOURCE.COBOL
  - `DISP=SHR` : Partage en lecture seule
- **LKED.SYSLMOD** : Où stocker le module exécutable
  - `&SYSUID..LOAD(HELLOCOB)` : Dans la bibliothèque de load modules personnelle

#### Exécution conditionnelle (RUN)
- **Structure IF/ENDIF** : Exécute seulement si compilation réussie (RC=0)
- **PGM=HELLOCOB** : Nom du programme compilé à exécuter
- **Paramètres d'exécution** :
  - `TIME=(1,0)` : Limite CPU à 1 minute
  - `REGION=512K` : Allocation mémoire de 512 Ko
  - `PARM='TEST01,PROD'` : Paramètres métier passés au programme COBOL

#### Datasets d'exécution
- **STEPLIB** : Indique où trouver le programme exécutable
  - `DSN=&SYSUID..LOAD` : Bibliothèque personnelle de load modules
- **SYSOUT** : Sortie standard du programme

#### 🔧 Points techniques importants
- ⚠️ **Condition d'exécution** : Le `IF RC = 0` évite d'exécuter un programme mal compilé
- 🎯 **STEPLIB obligatoire** : Sans STEPLIB, le système ne trouvera pas HELLOCOB
- 📊 **Gestion mémoire** : REGION=512K adapté pour un programme simple
- 🔄 **Workflow complet** : Compilation → Test du RC → Exécution si OK

#### 💡 Astuces pratiques
- Utiliser `//` suivi d'une continuation pour les longues lignes
- Les commentaires avec `//*` améliorent la lisibilité
- IGYWCL est plus pratique que les procédures séparées pour du développement
- Toujours tester avec des paramètres TIME conservateurs

---

## 🎯 Points clés à retenir

### Structure générale d'un JCL
1. **Carte JOB** : Identification et paramètres globaux
2. **Carte EXEC** : Programme à exécuter et ses paramètres
3. **Cartes DD** : Définition des datasets (fichiers)

### Bonnes pratiques
- ✅ Utiliser des noms explicites pour les steps et DD
- ✅ Toujours spécifier MSGLEVEL=(1,1) pour le debug
- ✅ Prévoir la gestion d'erreur avec DISP
- ✅ Optimiser les allocations d'espace
- ✅ Documenter les paramètres complexes

### Syntaxe importante
- `//*` : Commentaire
- `//` : Début de carte JCL
- `&&` : Fichier temporaire
- `*` : Référence arrière ou données inline
- `/*` : Fin de données inline
