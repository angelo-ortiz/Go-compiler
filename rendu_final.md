# Rendu final (complémentaire du premier rendu) 
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

**NB:** Ici, j'ai dû échapper les *specifiers* printf-*like*, étant donné que la fonction d'affichage de Petit Go n'est
pas formatée.

## Analyse syntaxique
Etant donné la séquence de lexemes dans le fichier source, je m'apprête à vérifier que ceux-la respectent bien
la grammaire de Petit Go et, surtout, je construit l'arbre de syntaxe abstraite (AST) correspondant.
C'est bien dans cette étape que j'ai dû faire face à des erreurs surprenantes, notamment sur les conflits
de point-virgule et de virgule, vu qu'il est possible de pas en mettre pour faciliter la tâche au programmeur.
J'ai créé mes propres règles paramétrées à cet effet.
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
Pour améliorer les performances de recherche dans ces environnements, j'ai opté par des structures de données basées sur
Map.Make au détriment des listes dans l'AST.

Il est à noter que la fonction **new** n'est pas créée, mais remplacée par une expression spéciale qui sera traitée 
différemment dans la phase suivante, si elle n'est pas définie ailleurs.

En ce qui concerne les **return**, mon typer diffère légèrement dans le fait que, pour les boucles for, si la condition est
**true**, il ne regarde pas la post-instruction puiqu'un **return** dans ce cas-là est sûr.
Par contre, partout ailleurs, une instruction **return** suivi d'une instruction **non-return** engendre une erreur de typage
du fait qu'elle est redondante. Ainsi, j'ai juste à vérifier que les instructions **return** sont les dernières dans les
blocs les contenant.

**NB:** J'ai modifié mon *typer* de sorte qu'il renvoye le format 'à la printf' pour les affichages. On rencontre ainsi la
première fonctionnalité perdu: mon compilateur ne peut pas afficher les résultats d'une fonction qui renvoye plusieurs valeurs.
Aussi, je fais notamment la distinction entre les accès à structures (équivalent du *struct.field* du C) et à pointeurs
de strutuctures (équivalent du "struct->field" du C).

## Production de code
Enfin, j'ai suivi la méthode de production de code efficace donnée dans les cours 11 et 12.
Ceci m'a coûté très cher en termes de temps, et c'est surtout à cause de ça que j'ai tardé beaucoup plus à avoir un
compilateur fonctionnel. En effet, je n'ai pas eu trop de temps pour bien le tester et déboguer.

### Sélection d'instructions
J'ai simplifié certains calculs dits *faciles*. Par example, les opérations arithmétiques sont ici précalculées autant que
possible, sans faire pour autant l'optimisation de la multiplication pour les fonctions pures.
De plus, les opérateurs _*_ et _&_ s'annulent s'ils sont contigüs.
Avant de dégager les types dans les nouveaux arbres, j'en ai profité pour sauvegarder la *taille* des expressions (en nombre
de blocks de 8 octets).

**NB:** Pour ce qui est de la division par zéro, j'ai suivi la consigne du sujet de sorte que le compilateur laisse faire
et repousse cette erreur à l'exécution.

### RTL
Dans cette étape, j'ai rajoute des pseudo-registres nécessaires pour effectuer les calculs dans les expressions du programme.
Pour l'opérateur *&* sur les accès à champs d'une structure, j'ai dû distinguer le cas pointeur du non-pointeur.
En effet, dans ce dernier, je dois forcer la structure à être stocké en mémoire. J'ai donc gardé tous les registres de la
structure pour que dans la phase de traduction à ERTL je puisse les forcer à être en mémoire.

Pour les opérateurs de comparaison sur des structures, j'ai utilisé des comparaisons champ à champ en utilisant de
saut conditionnels.
En ce qui concerne les branchements conditionnels, j'ai estimé que certaines comparaisons étaient moins souvent vraies.
Ainsi, j'en ai inversé certaines pour essayer de réduire les sauts conditionnels dans le code assembleur.

**NB:** Je ne l'ai pas testé au complet, mais il me semble que ma façon de gérer les affectations dans des champs de
structures (le "." du C) n'est pas correcte.

### ERTL
Dans cette étape, j'ai explicité les registres selon les conventions d'appel de fonction x86-64:
1. Les six premiers _arguments_ sont passés dans %rdi, %rsi, %rdx, %rcx, %r8 et %r9 et les suivants sur la pile
1. Le résultat est renvoyé de %rax s'il tient sur 8 octets, sinon l'appelant réserve suffisamment de space entre les arguments
passés dans la pile et l'adresse de retour, et l'appelé est tenu d'y transférer les résultats en fin de fonction.
1. Les fonctions *malloc* (new) et *printf* (Print) sont des fonctions de bibliothèques: printf est *variadique*
il faut donc mettre le registre AL (%rax) à zéro avant de l'appeler.
1. La division *idivq* ompose dividende et quotient dans %rax.
1. Les registres *callee-saved* sont %rbx, %r12, %r13, %r14 et %r15.
1. Les registres *caller-saved* sont %rax, %r10 et tous les registres de passage d'arguments.
1. %rsp est le pointeur de pile et %rbp est le pointeur de *frame*.

**NB:** J'entends par *argument* des blocks de 8 octets. En effet, les structures sont copiées dans les arguments de fonction
et peuvent potentiellement demander plus de places de paramètres. 

A la fin de la traduction RTL->ERTL, les fonctions connaissent les registres qui doivent être stockés en mémoire et, dans
le cas des structures, dans quel ordre ils doivent y être stockés.

### LTL
J'ai commencé par faire l'analyse de vie des registres en suivant l'algorithme de Kildall sur une map label -> instruction.
Puis, j'ai construit le graphe d'interférence sans oublier les arêtes de préférence.
Enfin, j'ai colorié les registres en utilisant les registres callee-saved et caller-saved comme couleurs.
J'ai dérécursifié l'algorithme donné en cours afin de remplacer la pile d'appels récursifs (non-terminaux) par une
pile physique. 

**NB:** Les registres physiques sont considérés comme précoloriés, et donc on ne peut pas effectuer de *simplify/freeze/spill* dessus.

Je fais deux passes dans le coloriage: dans un premier temps, les sommets *spillés* sont juste mis à côté, puis je réduis
le graphe au sommets spillés et j'effectue un nouveau coloriage sans restrictions en nombre de couleurs.
A l'issue de l'allocation de registres, on a donc le registres physique ou l'emplacement mémoire pour chaque pseudo-registre,
mais aussi le nombre de variables locales utilisées par la fonction.

Ensuite, je traduit proprement les instructions d'ERTL à LTL en explicitant les manipulations de pile avec %rsp et %rbp.
Il est à noter que les registres %r11 et %r15 ne sont pas allouables car ils sont réservés pour des usages temporaires pour les
éviter les doubles accès mémoire des instructions, qui ne le permettent pas en général.

### LIN
Finalement, j'ai traduit le flot d'instructions en du code assembleur séquentiel.
Tout d'abord, j'ai utilisé une structure Union-Find pour unifier les *goto* qui se succèdent et sont donc éliminables.
Je me sers de deux tables (l'une pour les labels déjà visités et l'autre pou stocker les labels qui doivent être dans le code.
De plus, ce n'est qu'à ce stade-là que je remplace les chaînes de caractères par les labels correspondant à leur emplacement
dans le segment de données.

