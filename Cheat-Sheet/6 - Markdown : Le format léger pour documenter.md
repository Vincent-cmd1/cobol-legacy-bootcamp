# 🧾 Cheat Sheet – Markdown : Le format léger pour documenter

Markdown, c'est un langage de **balisage léger**. Il te permet d'écrire des documents lisibles, formatés, **sans éditeur graphique**.

Parfait pour :
- Rédiger un README.md
- Documenter du code
- Faire des cheatsheets (comme celle-ci)

---

## ✅ Intérêt du Markdown

| Avantage | Pourquoi c'est utile |
|------------------------|-----------------------------------------------|
| Simple à écrire | Pas de syntaxe lourde comme HTML |
| Facilement lisible | Même brut, ça reste compréhensible |
| Compatible GitHub | Tous les fichiers .md sont bien rendus |
| Exportable | Peut être converti en PDF, HTML, etc. |

---

## ✍️ Syntaxes essentielles

### 🔹 Titres
```markdown
# Titre niveau 1
## Titre niveau 2
### Titre niveau 3
```

### 🔹 Texte en gras / italique
```markdown
**Gras** *Italique*
```

### 🔹 Listes
```markdown
- Élément 1
- Élément 2
  - Sous-élément
```

```markdown
1. Premier
2. Deuxième
```

### 🔹 Code & blocs
```markdown
`code inline`
```

````markdown
```bash
commande terminal
```

```cobol
DISPLAY "Hello COBOL".
```
````

### 🔹 Liens & images
```markdown
[Texte du lien](https://exemple.com)
![Alt image](https://url/image.png)
```

### 🔹 Tableaux
```markdown
| Colonne 1 | Colonne 2 |
|-----------|-----------| 
| Valeur A  | Valeur B  |
```

## 🚀 Astuce GitHub – README du dépôt

* Un fichier `README.md` à la racine est **automatiquement affiché** sur la page principale du dépôt GitHub.

## 🧠 Parcours conseillé

### 🎓 How-to-Markdown - NodeSchool.io
Atelier interactif pour apprendre Markdown directement dans ton terminal.
Installe via `npm install -g how-to-markdown`.

## 🔁 Fork, clone & upstream

1. Fork un dépôt sur GitHub
2. Clone le dépôt forké :
   ```bash
   git clone git@github.com:ton-user/nom-du-repo.git
   cd nom-du-repo
   ```
3. Ajoute l'upstream (le repo d'origine) :
   ```bash
   git remote add upstream git@github.com:source-user/nom-du-repo.git
   ```
4. Vérifie les remotes :
   ```bash
   git remote -v
   ```