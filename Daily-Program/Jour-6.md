# Objectifs journaliers MS COBOL P4

### Mardi 29 Avril

- [ ] Réfléchir aux cas où les conditions peuvent être utiles dans des programmes réels

#### Introduction aux boucles en COBOL

- [ ] Comprendre la boucle PERFORM
  - [ ] PERFORM n TIMES : exécution un nombre fixe de fois
  - [ ] PERFORM UNTIL : boucle avec condition d’arrêt
  - [ ] PERFORM VARYING : boucle avec compteur
- [ ] Différence entre boucles contrôlées par un compteur et par une condition

#### Exercices pratiques

- [ ] Créer un programme qui affiche "Bonjour" 5 fois
  - [ ] Utiliser PERFORM 5 TIMES
- [ ] Créer un programme qui compte de 1 à 10 et affiche chaque nombre
  - [ ] Utiliser PERFORM VARYING
- [ ] Créer un programme qui demande un nombre et s’arrête quand l’utilisateur entre 0
  - [ ] Utiliser PERFORM UNTIL
- [ ] Crée un programme qui affiche tous les multiples de 3 entre 1 et 100, en vérifiant à chaque itération si le nombre est divisible par 3.
- [ ] Crée un programme qui demande à l'utilisateur un nombre et affiche sa table de multiplication (de 1 à 10).
- [ ] Crée un programme qui demande à l'utilisateur d'entrer une série de nombres. La boucle continue jusqu'à ce que l'utilisateur entre 0. Le programme doit afficher le plus grand nombre parmi les entrées.



- [ ] **(Bonus)** Créer un programme qui permet à l'utilisateur de gérer une todo liste avec les fonctionnalités suivantes :
    - Ajouter une tâche.
    - Afficher les tâches.
    - Supprimer une tâche.
    - Quitter le programme.
    
     Chaque tâche sera simplement un texte que l'utilisateur pourra entrer. Le programme permettra de gérer jusqu'à 5 tâches. Le menu sera interactif et permettra de sélectionner les actions via des touches. 


- [ ] **(Bonus)** Créer un programme qui permet à l'utilisateur de se déplacer dans un menu pour afficher la moyenne des notes, la note la plus basse et la note la plus haute parmi un ensemble de notes inscrites en dur.
  - Afficher la Moyenne des Notes : Calculer et afficher la moyenne des notes.
  - Afficher la Note la Plus Basse : Afficher la note la plus basse.
  - Afficher la Note la Plus Haute : Afficher la note la plus haute.
  - Quitter : Quitter le programme.
#### Consolidation

- [ ] Ajouter une section "Boucles" à sa cheat sheet Markdown
- [ ] Noter les différences entre les types de PERFORM avec des exemples
