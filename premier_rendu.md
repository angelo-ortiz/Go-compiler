# Premier rendu 
---
## Généralités
Le projet a été réalisé dans le langage **OCaml**. Les outils utilisés sont **ocamllex** pour l'analyse lexicale et
**menhir** pour l'analyse sémantique.
Les arbres issues des analyses sémantique et sémantique sont construit selon différentes grammaires, 
ce qui a alourdi le travail du _typer_ (en effet, il fait un peu plus de 800 lignes de code), mais qui anticipe certains
besoins de la production de code que j'espère ainsi plus facile à faire.

Pour la compilation automatique du projet, j'ai utilisé un fichier Makefile. Les commandes suivantes s'avèrent utiles:
```Makefile
make # compilation + lancement de toute la suite de tests
make test_[syntax/typing/compil] # compilation + tests de l'anayse lexicale/sémantique et la production de code
```
## Analyse lexicale
Pendant cette étape, j'ai séparé en lexemes le code source du fichier *.go donné.
J'ai suivi la grammaire donnée dans l'énoncé. De cette manière, le seul _import_ valide est **"fmt"** ; 
de plus, ce _package_ ne contient que la fonction **Print**.

J'ai rencontré pendant cette phase ma première difficulté, à savoir, l'ajout des points-virgules, 
que j'ai pu résoudre en assez facilement une fois la précision sur les instructions vides était donnée.
En ce qui concerne les règles du lexer, la seule mention importante est celle sur les chaînes de caractères,
que je traite caractère par caractère (stockée dans un buffer) et dont j'ai dû sauvegarder la position initiale
pour la remettre dans le lexbuf juste avant la renvoyer.

## Analyse syntaxique
Etant donné la séquence de lexemes dans le fichier source, je m'apprête à vérifier que ceux-la respectent bien
la grammaire de Petit Go et, surtout, je construit l'arbre de syntaxe abstraite (AST) correspondant.
C'est bien dans cette étape que j'ai dû faire face à des erreurs surprenantes, notamment sur les conflits
de point-virgule et de virgule, vu qu'il est possible de pas en mettre pour faciliter la tâche au programmeur.
J'ai crée mes propres règles paramétrées à cet effet.
Je n'ai pas vraiment utilisé de types complexes pour cette étape car je voulais la garder simple; ainsi, j'utilise seulement
des types somme/produit et listes pour les séquences dans la syntaxe abstraite.

Les instructions vides (_nop_) ont été virées des blocs les contenant. Par ailleurs, afin de garder la cohérence
avec Petit Go, le _parsing_ des constantes entières a été repoussé à l'analyse syntaxique (au contraire de Go).
Pour les nombres suivis d'un enchaînement valide de "-", j'ai fait la réduction en une seule expression arithmétique.
Je commence par stocker le nombre de "couches" de symboles "-", que j'augmente à chaque fois que j'en rencontre, et que
je décremente juste avant de renvoyer l'élement AST correspondant. Une fois que j'arrive à la constante entière, je 
change sa valeur selon si elle est négative ou positive. Puis, je remonte dans l'arbre de "-" sans vérifier la valeur
jusqu'au niveau 0 (pour les constantes positives pures, _i.e._ sans signes - devant, le niveau 0 est par défaut), 
où je vérifie que la constante entière (signée) peut bien être représente sur 64 bits.

## Analyse sémantique
Finalement, dans la dernière phase avant la production de code, je reprends mon AST et vérifie qu'il est bien typé tout en
renvoyant l'arbre de typage correspondant.

Il est à noter que Go ne fait de l'inférence de type que dans les déclarations de variables. C'est pourquoi j'ai pu effectuer
le typage du fichier en utilisant les types bien définis dans l'énoncé et en rajoutant des types simples.
J'ai notamment utilisé des types **unit** et **tuple** pour les valeurs de retour de fonctions à 0 ou plusieurs élements,
ainsi que **nil** et **untyped** qui sont _unifiés_ selon les types des variables correspondantes.

La variable "_" est typée pendant l'analyse sémantique mais n'est jamais rajoutée dans l'environnement de variables.
J'ai ainsi distingué 3 environnements : celui des variables qui sont dans la porté de l'expression/instruction en question,
celui des structures et celui des fonctions.
Pour améliorer les performances de recherche dans ces environnements, j'ai opté par une structure de données basées sur
Map.Make au détriment des listes dans l'AST.

Le code du typer suit l'énoncé du projet et les suggestions faites.
Il est à noter que la fonction **new** n'est pas créée, mais remplacée par une expression spéciale qui sera traitée 
différemment dans la phase suivante, si elle n'est pas définie ailleurs.

En ce qui concerne les **return**, mon typer diffère légèrement dans le fait que, pour les boucles for, si la condition est
**true**, il ne regarde pas la post-instruction puiqu'un **return** dans ce cas-là est sûr.
Par contre, partout ailleurs, une instruction **return** suivi d'une instruction **non-return** engendre une erreur de typage
du fait qu'elle est redondante. Ainsi, j'ai juste à vérifier que les instructions **return** sont les dernières dans les
blocs les contenant.

