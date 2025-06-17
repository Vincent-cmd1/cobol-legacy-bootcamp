# 🧾 Cheat Sheet – Terminal, Shell & Commandes de base

> Le terminal, c'est ton couteau suisse. Plus rapide, plus puissant, et plus effrayant qu'une interface graphique. Voici les notions essentielles pour comprendre et survivre en ligne de commande.

## 🖥 Interface graphique vs ligne de commande

| Interface graphique (GUI) | Terminal (CLI) |
|---------------------------|----------------|
| Utilise la souris | Utilise le clavier |
| Boutons, menus | Commandes textuelles |
| Plus intuitive | Plus rapide & scriptable |
| Consomme plus de ressources | Léger, efficace |

> **CLI (Command Line Interface)** : C'est le terminal.
> 
> **GUI (Graphical User Interface)** : C'est ce que tu cliques.

## 🐚 Qu'est-ce qu'un shell ?

- Un **shell** est un interpréteur de commandes textuelles.
- Il lit ce que tu tapes, l'exécute, et affiche le résultat.

### 🔸 BASH vs ZSH

| Shell | Avantages |
|-------|-----------|
| BASH | Le plus répandu, scriptable |
| ZSH | Plus moderne, autocomplétion avancée |

> BASH : par défaut sur beaucoup de systèmes Unix/Linux
> 
> ZSH : utilisé avec Oh My Zsh pour une meilleure expérience

## ⚙️ Fonctionnement du shell

Un shell :
1. Lit ta commande
2. Cherche le programme correspondant
3. Exécute la commande
4. Affiche le résultat (ou une erreur)

## 🧙 Alias – Créer des raccourcis

```bash
alias ll='ls -alF'
```

* Crée un alias pour une commande longue ou fréquente
* Les alias peuvent être ajoutés dans `.bashrc` ou `.zshrc`

### 🔹 Exemple

```bash
alias gs='git status'
alias cls='clear'
```

## 🧰 Commandes de base du terminal

| Commande | Fonction |
|----------|----------|
| `cd` | Change de répertoire |
| `ls` | Liste le contenu du dossier |
| `rm` | Supprime un fichier |
| `rmdir` | Supprime un dossier vide |
| `mv` | Déplace ou renomme un fichier/dossier |
| `cp` | Copie un fichier/dossier |
| `mkdir` | Crée un nouveau dossier |

### 🔹 Exemples pratiques

```bash
cd dossier          # Aller dans "dossier"
cd ..               # Remonter d'un niveau
ls                  # Lister les fichiers
ls -l               # Liste détaillée
mkdir projet        # Créer un dossier "projet"
cp fichier.txt copie.txt    # Copier
mv copie.txt dossier/       # Déplacer
rm fichier.txt      # Supprimer un fichier
rmdir dossiervide   # Supprimer un dossier vide
```

## 📂 Navigation avancée dans les répertoires

```bash
cd ~                # Aller dans le répertoire utilisateur
cd -                # Revenir au répertoire précédent
pwd                 # Afficher le chemin du répertoire actuel
pushd dossier       # Aller dans un dossier en gardant l'ancien en mémoire
popd                # Revenir au dossier précédent (sauvegardé par pushd)
```

## 📝 Manipulation de fichiers avancée

```bash
cat fichier.txt     # Afficher le contenu d'un fichier
less fichier.txt    # Afficher le contenu avec défilement
head fichier.txt    # Afficher les 10 premières lignes
tail fichier.txt    # Afficher les 10 dernières lignes
tail -f fichier.log # Suivre les modifications en temps réel
touch fichier.txt   # Créer un fichier vide ou mettre à jour son timestamp
grep "texte" fichier.txt  # Rechercher "texte" dans le fichier
```

## 👨‍💻 Astuces pour débutants

* Utilise la touche `Tab` pour l'autocomplétion des commandes et chemins
* Flèches haut/bas pour naviguer dans l'historique des commandes
* `Ctrl+R` pour rechercher dans l'historique des commandes
* `Ctrl+C` pour arrêter une commande en cours
* `Ctrl+L` pour effacer l'écran (équivalent à `clear`)
* `Ctrl+A` pour aller au début de la ligne
* `Ctrl+E` pour aller à la fin de la ligne

## 🔒 Permissions et droits

```bash
ls -la              # Afficher les fichiers avec leurs permissions
chmod +x script.sh  # Rendre un fichier exécutable
chmod 755 dossier   # Définir des permissions (rwx r-x r-x)
chown user:group fichier  # Changer le propriétaire d'un fichier
```

## 🔄 Redirection et pipes

```bash
command > fichier.txt  # Rediriger la sortie vers un fichier (écrase)
command >> fichier.txt # Rediriger et ajouter à la fin du fichier
command1 | command2    # Pipe - passer la sortie à une autre commande
cat fichier.txt | grep "mot"  # Rechercher "mot" dans le fichier
```
