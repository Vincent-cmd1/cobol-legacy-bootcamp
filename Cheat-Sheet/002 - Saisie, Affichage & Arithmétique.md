# 🧾 COBOL Cheat Sheet – Saisie, Affichage & Arithmétique

## 🎤 Saisie et affichage

### 🔹 ACCEPT
> Permet de saisir une donnée via le terminal (entrée standard).

```cobol
ACCEPT WS-NOM.
```

* La valeur saisie par l'utilisateur est stockée dans la variable `WS-NOM`.

### 🔹 DISPLAY
> Affiche un message ou la valeur d'une variable.

```cobol
DISPLAY "Bonjour, " WS-NOM.
```

* Peut afficher des littéraux et des variables, à la suite.

### 🧪 Exemple : Hello World personnalisé

```cobol
WORKING-STORAGE SECTION.
01 WS-NOM PIC X(20).

PROCEDURE DIVISION.
    DISPLAY "Quel est votre nom ?".
    ACCEPT WS-NOM.
    DISPLAY "Bonjour, " WS-NOM.
```

## ➕➖✖️➗ Arithmétique de base

### 🔹 ADD

```cobol
ADD WS-A TO WS-SOMME.
```
* Ajoute la valeur de `WS-A` à `WS-SOMME`.

```cobol
ADD WS-A WS-B GIVING WS-SOMME.
```
* Additionne A + B et stocke dans `WS-SOMME`.

### 🔹 SUBTRACT

```cobol
SUBTRACT WS-B FROM WS-A.
```
* Fait `WS-A = WS-A - WS-B`.

```cobol
SUBTRACT WS-A WS-B FROM WS-TOTAL GIVING WS-RESULTAT.
```
* Calcule `TOTAL - A - B` et stocke dans `RESULTAT`.

### 🔹 MULTIPLY

```cobol
MULTIPLY WS-A BY WS-B.
```
* Fait `WS-B = WS-A * WS-B`.

```cobol
MULTIPLY WS-A BY WS-B GIVING WS-RESULTAT.
```
* Calcule `A * B` et stocke dans `RESULTAT`.

### 🔹 DIVIDE

```cobol
DIVIDE WS-B INTO WS-A.
```
* Fait `WS-A = WS-A / WS-B`.

```cobol
DIVIDE WS-B INTO WS-A GIVING WS-RESULTAT.
```
* Calcule `A / B` et stocke dans `RESULTAT`.

```cobol
DIVIDE WS-B INTO WS-A GIVING WS-RESULTAT REMAINDER WS-RESTE.
```
* Calcule quotient et reste de la division.

### 🧮 COMPUTE – Calculs complexes

```cobol
COMPUTE WS-MOYENNE = (WS-NOTE1 + WS-NOTE2 + WS-NOTE3) / 3.
```
* Permet des calculs multi-opérations avec parenthèses.
* Plus flexible que ADD/SUBTRACT/etc.

## 📏 Format des données numériques

| Format | Exemple | Utilisation |
|--------|---------|-------------|
| `PIC 9(3)` | `123` | Entier à 3 chiffres |
| `PIC 9(3)V9(2)` | `123.45` (virtuel) | Décimal avec 2 chiffres après V |
| `PIC S9(4)` | `-1234` ou `+1234` | Nombre signé |

* Le `V` est une virgule **virtuelle** (pas visible à l'affichage)
* Le `S` signifie **signé**

## 🧪 Exemples d'exercices pratiques

### 💡 Somme de 2 nombres

```cobol
WORKING-STORAGE SECTION.
01 WS-NB1    PIC 9(3).
01 WS-NB2    PIC 9(3).
01 WS-SOMME  PIC 9(4).

PROCEDURE DIVISION.
    DISPLAY "Entrez le premier nombre :".
    ACCEPT WS-NB1.
    DISPLAY "Entrez le deuxième nombre :".
    ACCEPT WS-NB2.
    ADD WS-NB1 TO WS-NB2 GIVING WS-SOMME.
    DISPLAY "La somme est : " WS-SOMME.
```

### 💡 Produit de 2 nombres

```cobol
MULTIPLY WS-NB1 BY WS-NB2 GIVING WS-PRODUIT.
```

### 💡 Moyenne de 3 nombres

```cobol
WORKING-STORAGE SECTION.
01 WS-N1      PIC 9(3).
01 WS-N2      PIC 9(3).
01 WS-N3      PIC 9(3).
01 WS-MOYENNE PIC 9(3)V9(2).

PROCEDURE DIVISION.
    ACCEPT WS-N1.
    ACCEPT WS-N2.
    ACCEPT WS-N3.
    COMPUTE WS-MOYENNE = (WS-N1 + WS-N2 + WS-N3) / 3.
    DISPLAY "Moyenne : " WS-MOYENNE.
```
