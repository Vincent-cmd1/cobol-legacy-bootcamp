# 🧾 COBOL Cheat Sheet – Branches conditionnelles

> La programmation sans conditions, c'est comme un mainframe sans ruban magnétique : inutile. Voici l'essentiel pour utiliser IF, EVALUATE, et autres joyeusetés logiques en COBOL.

## 🔁 Structure IF / ELSE / END-IF

### 🔹 Syntaxe de base
```cobol
IF condition
   instruction
END-IF.
```

### 🔹 Avec ELSE
```cobol
IF WS-NB > 10
   DISPLAY "Plus grand que 10"
ELSE
   DISPLAY "Inférieur ou égal à 10"
END-IF.
```

> ✅ Toujours **INDENTER** le contenu
> ❌ Ne jamais oublier le `END-IF` !

## ⚖️ Comparateurs COBOL

| Comparaison | Syntaxe COBOL |
|-------------|---------------|
| égal | `IF A = B` ou `EQUAL` |
| différent | `IF A NOT = B` |
| supérieur | `IF A > B` ou `GREATER THAN` |
| inférieur | `IF A < B` ou `LESS THAN` |
| supérieur ou égal | `IF A >= B` |
| inférieur ou égal | `IF A <= B` |

Exemple :
```cobol
IF WS-AGE GREATER THAN 17
   DISPLAY "Majeur"
END-IF.
```

## 🔗 Opérateurs logiques

| Opérateur | Utilisation |
|-----------|-------------|
| `AND` | Toutes les conditions doivent être vraies |
| `OR` | Au moins une doit être vraie |
| `NOT` | Inverse la condition |

Exemple :
```cobol
IF WS-AGE > 17 AND WS-CITOYEN = "OUI"
   DISPLAY "Peut voter"
END-IF.
```

```cobol
IF NOT WS-ACTIF = "OUI"
   DISPLAY "Inactif"
END-IF.
```

## 🧠 EVALUATE – Alternative à IF

L'équivalent du `switch-case` des autres langages. Plus propre quand tu as plusieurs cas à tester.

### 🔹 Syntaxe de base
```cobol
EVALUATE WS-VALEUR
   WHEN 1
      DISPLAY "Un"
   WHEN 2
      DISPLAY "Deux"
   WHEN OTHER
      DISPLAY "Autre"
END-EVALUATE.
```

### 🔹 Avec `TRUE` (condition booléenne)
```cobol
EVALUATE TRUE
   WHEN WS-AGE < 12
      DISPLAY "Enfant"
   WHEN WS-AGE < 18
      DISPLAY "Adolescent"
   WHEN WS-AGE < 65
      DISPLAY "Adulte"
   WHEN OTHER
      DISPLAY "Senior"
END-EVALUATE.
```

> ✅ Plus lisible qu'une cascade de `IF / ELSE IF`

## 💡 Rappel important

* Chaque `IF` **doit** avoir son `END-IF`
* Chaque `EVALUATE` **doit** finir par `END-EVALUATE`
* COBOL n'aime pas les conditions ambigües : sois clair ou subis l'ABEND

## 🧪 Exemple complet

```cobol
IF WS-NOMBRE < 0
   DISPLAY "Nombre négatif"
ELSE 
   IF WS-NOMBRE = 0
      DISPLAY "Zéro"
   ELSE
      DISPLAY "Nombre positif"
   END-IF
END-IF.
```

### Version avec EVALUATE
```cobol
EVALUATE TRUE
   WHEN WS-NOMBRE < 0
      DISPLAY "Nombre négatif"
   WHEN WS-NOMBRE = 0
      DISPLAY "Zéro"
   WHEN OTHER
      DISPLAY "Nombre positif"
END-EVALUATE.
```
