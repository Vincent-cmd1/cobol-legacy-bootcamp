# 🧾 Cheat Sheet – Tableaux (OCCURS) en COBOL

Tu veux stocker plusieurs valeurs dans une même variable ? Tu veux répéter une structure ? Tu es prêt pour OCCURS, l'outil COBOL pour créer des tableaux.

---

## 📌 OCCURS – Déclaration de tableau

### 🔹 Syntaxe de base

```cobol
01 WS-TABLE.
   05 WS-ELEMENT OCCURS 10 TIMES.
      10 WS-VALUE PIC 9(3).
```

* `OCCURS n TIMES` indique un tableau de `n` éléments
* Le **niveau hiérarchique** est crucial :
   * `05` contient l'instruction `OCCURS`
   * `10` définit le contenu répété (les "champs" du tableau)

### 🧠 Exemple : tableau de 5 notes

```cobol
01 WS-NOTES.
   05 WS-NOTE OCCURS 5 TIMES.
      10 WS-VALEUR PIC 9(2).
```

* Définit 5 `WS-VALEUR` accessibles via : `WS-NOTE (1)`, `WS-NOTE (2)`, etc.

---

## 🧪 Utilisation du tableau

### 🔹 Affecter une valeur

```cobol
MOVE 18 TO WS-NOTE (1).
```

### 🔹 Lire toutes les valeurs

```cobol
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
   DISPLAY "Note : " WS-NOTE (IDX)
END-PERFORM.
```

N'oublie **jamais** de déclarer `IDX` dans `WORKING-STORAGE SECTION` !

---

## 🔁 Tableaux multidimensionnels

Tu veux du tableau dans le tableau ? Voilà comment on fait.

### 🔹 Exemple : tableau 3x4 (3 lignes, 4 colonnes)

```cobol
01 WS-MATRIX.
   05 WS-LIGNE OCCURS 3 TIMES.
      10 WS-COL OCCURS 4 TIMES.
         15 WS-VALUE PIC 9(2).
```

### 🔹 Accès à une cellule

```cobol
MOVE 99 TO WS-VALUE (2, 3).
```

* Cela modifie la **2e ligne**, **3e colonne**

---

## 🔢 Gestion des indices

### 🔹 Déclaration des indices

```cobol
01 WS-INDICES.
   05 IDX             PIC 9(3).
   05 I               PIC 9(3).
   05 J               PIC 9(3).
```

### 🔹 Utilisation avec INDEXED BY

```cobol
01 WS-TABLE.
   05 WS-ELEMENT OCCURS 10 TIMES INDEXED BY IDX-EL.
      10 WS-VALUE PIC 9(3).
```

Avantage : `INDEXED BY` déclare automatiquement l'indice et optimise les accès.

### 🔹 Verbes spécifiques aux indices

```cobol
* Initialiser un indice
SET IDX-EL TO 1.

* Incrémenter un indice
SET IDX-EL UP BY 1.

* Décrémenter un indice
SET IDX-EL DOWN BY 1.
```

### 🔹 Recherche dans un tableau

```cobol
SEARCH WS-ELEMENT
   WHEN WS-VALUE(IDX-EL) = 42
      DISPLAY "Trouvé à l'indice " IDX-EL
END-SEARCH.
```

## 🛠 Bonnes pratiques

* Les indices commencent à **1** (pas 0 !)
* Déclare un compteur (`IDX`, `I`, `J`, etc.) pour naviguer
* Toujours respecter les niveaux hiérarchiques (`01`, `05`, `10`, etc.)
* Ne jamais dépasser la taille maximale (`OCCURS n TIMES`) : sinon, ABEND.

---

## 💡 Astuces et techniques avancées

### 🔹 Initialisation d'un tableau

```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
   MOVE 0 TO WS-NOTE (I)
END-PERFORM.
```

### 🔹 Initialiser avec une valeur unique

```cobol
INITIALIZE WS-NOTES REPLACING NUMERIC DATA BY 0.
```

## 📝 Conventions de nommage

Pour un code clair et maintenable, adoptez ces conventions :

### 🔹 Préfixes pour tableaux et indices

| Préfixe | Utilisation | Exemple |
|---------|-------------|---------|
| `TB-`   | Nom du tableau | `TB-CLIENTS` |
| `IDX-`  | Indice principal | `IDX-CLIENT` |
| `I-`    | Premier niveau d'indice | `I-LIGNE` |
| `J-`    | Deuxième niveau d'indice | `J-COLONNE` |

### 🔹 Structure recommandée

```cobol
01 TB-PRODUITS.
   05 TB-PRODUIT OCCURS 100 TIMES INDEXED BY IDX-PROD.
      10 PROD-CODE          PIC X(8).
      10 PROD-LIBELLE       PIC X(30).
      10 PROD-PRIX          PIC 9(5)V99.
      10 PROD-STOCK         PIC 9(4).
```

### 🔹 Documentation des tailles

```cobol
* Constantes pour les dimensions
01 WS-CONSTANTES.
   05 WS-MAX-PRODUITS       PIC 9(3) VALUE 100.
   05 WS-MAX-LIGNES         PIC 9(2) VALUE 50.
   05 WS-MAX-COLONNES       PIC 9(2) VALUE 10.

* Utilisation dans le code
PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MAX-PRODUITS
   ...
END-PERFORM.
```

Cette approche facilite la maintenance quand les dimensions changent.
