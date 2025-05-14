# 🧾 COBOL Cheat Sheet — Gestion de fichiers séquentiels

## 📁 ENVIRONMENT DIVISION

### CONFIGURATION SECTION
Pas toujours utilisée pour les fichiers simples. Elle décrit la machine cible. Ignore-la pour les programmes de base.

### INPUT-OUTPUT SECTION

#### FILE-CONTROL
C'est ici que tu **déclares les fichiers** utilisés par le programme :

```cobol
SELECT F-EMPLOYE 
       ASSIGN TO "EMPLOYES.DAT"
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS WS-FS-EMPLOYE.
```

* `SELECT` : nom logique du fichier dans le programme.
* `ASSIGN TO` : nom réel (fichier physique ou DDNAME en JCL).
* `ORGANIZATION IS SEQUENTIAL` : type d'organisation (séquentiel ici).
* `FILE STATUS IS` : permet de gérer les erreurs d'accès.

## 🗃 DATA DIVISION

### FILE SECTION
Décrit la structure de l'enregistrement du fichier :

```cobol
FD F-EMPLOYE
   RECORD CONTAINS 100 CHARACTERS
   BLOCK CONTAINS 0 RECORDS
   DATA RECORD IS FS-ENR-EMPLOYE.
01 FS-ENR-EMPLOYE.
   05 NUM-EMPLOYE    PIC X(5).
   05 CODE-EMPLOYE   PIC X(1).
   05 FILLER         PIC X(94).
```

* `FD` : File Description.
* `01` : niveau de l'enregistrement.
* `FILLER` : zone ignorée.

### WORKING-STORAGE SECTION

```cobol
01 WS-FS-EMPLOYE     PIC X(2).  *> File status
01 WS-ENR-EMPLOYE.
   05 WS-NUM-EMPLOYE  PIC X(5).
   05 WS-CODE-EMPLOYE PIC X(1).
```

## 🛠 PROCEDURE DIVISION

### Commandes de gestion de fichier

#### 📂 OUVERTURE

```cobol
OPEN INPUT F-EMPLOYE
```

Autres options :
* `OUTPUT` : pour écrire dans un fichier.
* `EXTEND` : pour ajouter à la fin (append).

#### 📖 LECTURE

```cobol
READ F-EMPLOYE INTO WS-ENR-EMPLOYE
   AT END
      MOVE "OUI" TO FIN-FICHIER.
```

* `AT END` : exécute une action à la fin du fichier.
* Utiliser une variable booléenne pour sortir de boucle.

#### 📝 ÉCRITURE

```cobol
WRITE FS-ENR-EMPLOYE
```
(Ne pas oublier d'avoir OPEN en mode `OUTPUT` ou `EXTEND`)

#### ❌ FERMETURE

```cobol
CLOSE F-EMPLOYE
```

## 🔗 STRUCTURE COMPLÈTE : EXEMPLE MINIMAL

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. LECTURE-EMPLOYE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT F-EMPLOYE 
           ASSIGN TO "EMPLOYES.DAT"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-FS-EMPLOYE.

DATA DIVISION.
FILE SECTION.
FD F-EMPLOYE.
01 FS-ENR-EMPLOYE.
   05 NUM-EMPLOYE    PIC X(5).
   05 CODE-EMPLOYE   PIC X(1).
   05 FILLER         PIC X(94).

WORKING-STORAGE SECTION.
01 WS-FS-EMPLOYE     PIC X(2).
01 FIN-FICHIER       PIC X VALUE "NON".
01 WS-ENR-EMPLOYE.
   05 WS-NUM-EMPLOYE  PIC X(5).
   05 WS-CODE-EMPLOYE PIC X(1).

PROCEDURE DIVISION.
    OPEN INPUT F-EMPLOYE
    
    PERFORM UNTIL FIN-FICHIER = "OUI"
        READ F-EMPLOYE INTO WS-ENR-EMPLOYE
            AT END
                MOVE "OUI" TO FIN-FICHIER
            NOT AT END
                DISPLAY "Employé: " WS-NUM-EMPLOYE
        END-READ
    END-PERFORM
    
    CLOSE F-EMPLOYE
    STOP RUN.
```

## 🧠 Astuces de vieux briscard

* Toujours tester `FILE STATUS` si tu ne veux pas finir avec un ABEND classe 0C7.
* Ne fais jamais confiance aux données du fichier — même un FILLER peut cacher un drame.
* Documente bien chaque `FD` et `SELECT`, ou ton successeur (toi dans 3 mois) te maudira.