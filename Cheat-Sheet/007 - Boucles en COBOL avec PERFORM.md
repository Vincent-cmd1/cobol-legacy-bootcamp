# 🧾 Cheat Sheet – Boucles en COBOL avec PERFORM

Le COBOL n'a pas de for, ni de while, ni de do-while. Il a un seul maître : **PERFORM**. Et avec lui, tu peux tout faire… ou planter ton programme dans une boucle infinie si t'es distrait.

---

## 🔁 PERFORM – la boucle universelle

Le mot-clé PERFORM permet de :
- Exécuter un paragraphe
- Répéter un bloc
- Créer des boucles avec ou sans condition

---

## 🔸 PERFORM n TIMES

> Répète une action un **nombre fixe** de fois.

```cobol
PERFORM AFFICHER-MESSAGE 5 TIMES.
```

```cobol
AFFICHER-MESSAGE.
    DISPLAY "Hello COBOL!".
```

## 🔸 PERFORM UNTIL

Répète **jusqu'à** ce qu'une condition soit vraie.

```cobol
PERFORM TRAITER-LIGNE UNTIL WS-FIN = 'O'.
```

```cobol
TRAITER-LIGNE.
    DISPLAY "Traitement en cours".
    ACCEPT WS-FIN.
```

Attention : la condition est testée **avant chaque exécution**. Boucle **non exécutée** si la condition est vraie dès le début.

## 🔸 PERFORM VARYING

Boucle avec **compteur** (comme un `for` moderne).

```cobol
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
    DISPLAY "Compteur : " WS-I
END-PERFORM.
```

| Élément | Rôle |
|---------|------|
| `FROM`  | Valeur initiale |
| `BY`    | Pas de l'incrément |
| `UNTIL` | Condition d'arrêt **inclusif** |

## ⚖️ Compteur vs Condition

| Type de boucle | Syntaxe | Cas d'usage |
|----------------|---------|-------------|
| Boucle fixe | `n TIMES` | Répéter un nombre exact |
| Conditionnelle | `UNTIL` | Répéter jusqu'à une condition |
| Contrôlée par compteur | `VARYING` | Boucle avec compteur croissant |

`VARYING` = **plus lisible** et **plus moderne** que `n TIMES` si tu veux accéder à l'indice courant.

## 🧪 Exemples combinés

### Boucle avec condition

```cobol
PERFORM AFFICHAGE UNTIL WS-FIN = "O".

AFFICHAGE.
    DISPLAY "Encore ? (O/N)".
    ACCEPT WS-FIN.
```

### Boucle avec compteur

```cobol
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
    DISPLAY "Itération n°" WS-I
END-PERFORM.
```
