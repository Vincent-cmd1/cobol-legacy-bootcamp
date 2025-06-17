# 🧾 COBOL Cheat Sheet – Structure & Nommage

> Un programme COBOL, c'est comme une partition pour IBM 360 : chaque division a sa place, sinon c'est la cacophonie. Voici l'essentiel pour survivre à ton premier vrai projet.

## 🧩 Structure générale d'un programme COBOL

Un programme COBOL est découpé en **4 DIVISIONS** obligatoires :

### 1️⃣ IDENTIFICATION DIVISION
> Déclare l'identité du programme (nom obligatoire).

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MON-PROGRAMME.
```

* `PROGRAM-ID` = nom du programme (max. 30 caractères)
* Autres possibles : `AUTHOR`, `DATE-WRITTEN` (facultatif)

### 2️⃣ ENVIRONMENT DIVISION
> Décrit les fichiers utilisés et leur association physique/logique.

```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT F-NOM-FICHIER ASSIGN TO 'nomphysique'
    FILE STATUS IS WS-STATUS-FICHIER.
```

* `SELECT` : fait le lien entre le nom logique et le fichier réel
* `FILE STATUS` : code retour à tester après chaque I/O

### 3️⃣ DATA DIVISION
> Décrit **toutes les données** utilisées (fichiers, variables, etc.).

#### 📁 FILE SECTION

```cobol
FILE SECTION.
FD F-NOM-FICHIER.
    01 ENR-FICHIER PIC X(50).
```

* `FD` : décrit un fichier logique
* `01` : enregistrement (record)

#### 🧠 WORKING-STORAGE SECTION

```cobol
WORKING-STORAGE SECTION.
01 WS-NOM       PIC X(20).
01 WS-NOTE      PIC 9(3).
01 WS-MOYENNE   PIC 9(3)V9(2).
```

* Variables en mémoire (état du programme)

### 4️⃣ PROCEDURE DIVISION
> Partie **exécutable** du programme.

```cobol
PROCEDURE DIVISION.
    DISPLAY "Bonjour".
    MOVE 10 TO WS-NOTE.
    ADD WS-NOTE TO WS-MOYENNE.
```

* Instructions COBOL : `MOVE`, `ADD`, `IF`, `DISPLAY`, `PERFORM`…
* Paragraphes (optionnels) : identifiants logiques pour structurer

## 📏 Types de Données – Clause PIC

| Type | Syntaxe PIC | Exemple | Description |
|------|-------------|---------|-------------|
| Alphanumérique | `PIC X(n)` | `PIC X(10)` | Chaîne de 10 caractères |
| Numérique | `PIC 9(n)` | `PIC 9(4)` | 4 chiffres |
| Num. décimal | `PIC 9(n)V99` | `PIC 9(3)V99` | 3 chiffres + 2 décimales invisibles |
| Signé | `S9(n)` | `PIC S9(4)` | Nombre signé (+/-) |

* Le `V` est une **virgule virtuelle** : invisible à l'écran
* Le `S` indique une valeur **signée** (positive ou négative)

## 🏷 Règles de nommage des identifiants

COBOL n'est pas là pour plaisanter avec les noms.

### ✅ Autorisé
* Commencer par une **lettre**
* Contenir : lettres, chiffres, `-` (tiret)
* Jusqu'à **30 caractères**
* Ne **pas** utiliser de **mots réservés**

### 🚫 Interdit
* `1TOTAL` → commence par un chiffre ❌
* `TOTAL SALAIRE` → contient un espace ❌
* `MOVE-VAR` → mot réservé dans le nom ❌

## 🔖 Conventions de préfixes (recommandées)

| Préfixe | Signification |
|---------|---------------|
| `WS-` | Variable de travail (`WORKING-STORAGE`) |
| `FS-` | Données de fichier (`FILE SECTION`) |
| `F-` | Nom logique de fichier (`FILE-CONTROL`) |
| `LS-` | Variable en `LINKAGE SECTION` |

## 🧪 Exemple minimal complet

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BONJOUR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT F-CLIENT ASSIGN TO 'clients.txt'
    FILE STATUS IS WS-FS-CLIENT.

DATA DIVISION.
FILE SECTION.
FD F-CLIENT.
    01 FS-CLIENT-ENR PIC X(80).

WORKING-STORAGE SECTION.
01 WS-FS-CLIENT PIC XX.
01 WS-NOM      PIC X(20).

PROCEDURE DIVISION.
    DISPLAY "Bonjour".
    STOP RUN.
```
