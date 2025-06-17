# 🧾 Cheat Sheet – Git & GitHub pour les débutants

> Git, c'est un gestionnaire de versions. GitHub, c'est une plateforme de partage de dépôts. Ensemble, c'est le combo ultime pour travailler proprement (ou planter tout un projet en un commit mal placé).

## ⚙️ Installation & Configuration

### 🔹 Installation
- Télécharger [Git](https://git-scm.com/)
- Sous Windows : Git Bash recommandé pour le terminal

### 🔹 Configuration de base (à faire UNE FOIS)
```bash
git config --global user.name "TonNom"
git config --global user.email "ton@email.com"
```

## 🔁 Concepts fondamentaux

| Terme | Définition rapide |
|-------|------------------|
| Versionning | Historique des modifications d'un projet |
| Local | Ton dépôt **en local** (sur ton PC) |
| Remote | Une version **hébergée** (ex: GitHub) |
| Origin | Le dépôt distant principal |
| Upstream | Le dépôt **source** d'un fork |
| Staging | Zone intermédiaire entre modifications et commit |
| Commit | Enregistrement d'un "état" du projet |
| Branch | Une ligne de développement parallèle |
| Fork | Copie d'un dépôt pour proposer des modifs |
| Pull Request | Proposition de modification (depuis un fork ou branche) |
| Issues | Système de suivi de bugs et demandes |

## 🚀 Commandes Git de base

### 🔹 Initialiser un dépôt
```bash
git init
```
> Crée un dépôt Git dans le dossier courant.

### 🔹 Suivre les fichiers
```bash
git add mon-fichier.ext
git add .  # tous les fichiers modifiés
```
> Ajoute les fichiers au **staging**.

### 🔹 Vérifier l'état
```bash
git status
```
> Affiche les fichiers modifiés, suivis, etc.

### 🔹 Enregistrer un commit
```bash
git commit -m "Message clair et utile"
```
> Enregistre l'état actuel (staging → historique)

### 🔹 Ajouter un dépôt distant (GitHub)
```bash
git remote add origin git@github.com:ton-utilisateur/ton-repo.git
```
> Lie ton dépôt local à GitHub.

### 🔹 Envoyer ses commits sur GitHub
```bash
git push origin main
```
> Envoie tes commits vers la branche `main` du dépôt distant.

### 🔹 Récupérer les modifications distantes
```bash
git pull origin main
```
> Récupère et fusionne les modifications de GitHub.

### 🔹 Cloner un dépôt existant
```bash
git clone git@github.com:utilisateur/repo.git
```
> Crée une copie locale d'un dépôt GitHub.

## 🌿 Branches

```bash
git branch ma-branche
git checkout ma-branche
```
> Crée et passe sur une nouvelle branche.

```bash
git checkout -b nouvelle-branche
```
> Crée **et** se place dessus (en une commande).

## 🌐 GitHub – Actions recommandées

* Faire le parcours **Git-it** (NodeSchool)
* Passer le badge **IBM Git & GitHub Essentials** sur SkillsBuild
* Pimper ton profil GitHub (README, photo, bio)
* Ajouter ta clé SSH pour sécuriser les push

## 📅 Bonnes pratiques pour débutant

* ✅ Faire **plusieurs commits par jour**
* ✅ Commits clairs et fréquents (`git commit -m "Ajout de la fonction calcul"`)
* ✅ Pousser (`git push`) ses *daily objectives* chaque jour
* ✅ Nommer les branches de façon lisible (`feat/conditions`, `fix/bug-hello`)

## 🧠 Workflow typique

```bash
# 1. Cloner le repo ou initialiser
git clone https://github.com/utilisateur/repo.git
# ou
git init

# 2. Créer une branche pour ta feature
git checkout -b feature/ma-nouvelle-fonction

# 3. Faire des modifications
# ... édition de fichiers ...

# 4. Ajouter les modifications au staging
git add .

# 5. Créer un commit
git commit -m "Ajout de la fonctionnalité X"

# 6. Pousser vers GitHub
git push origin feature/ma-nouvelle-fonction

# 7. Créer une Pull Request sur GitHub
# (via l'interface web)
```

## 🆘 Commandes de secours

```bash
# Oups, j'ai fait des modifications que je ne veux pas garder
git checkout -- .  # Annule toutes les modifications non commit

# Oups, j'ai ajouté un fichier au staging que je ne voulais pas
git reset HEAD fichier.txt  # Retire du staging

# Oups, j'ai fait un commit que je veux modifier
git commit --amend -m "Message corrigé"  # Modifie le dernier commit

# Oups, j'ai créé une branche sur la mauvaise base
git rebase main  # Rebaser sur main (attention, puissant !)

# Oups, je suis perdu dans mes branches
git log --graph --oneline --all  # Visualiser l'historique
```
