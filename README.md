Ce qui fonctionne :

- Nous nous sommes concentrés sur les actions de base du logiciel, c'est-à-dire la création de nouvelles fenêtres, le déplacement dans celles-ci (actualisation de la fenêtre active symbolisé par une bordure blanche autour de celle-ci), le redimensionnement des différentes fenêtres et la suppression d'une fenêtre voulue. Cela étant, nous avons implémenté la possibilité de changer la couleur de la fenêtre active de 2 manières différentes.

Liste des actions et comment nous les avons implémenté :

Tout d'abord, pour utiliser le logiciel il faut (après avoir exécuté le programme) simplement appuyer sur les touches du clavier correspodant aux actions prédéfinies. Voici la liste des touches et comment nous avons implémenté leur actions :

- Touche "q" :
Cette touche permet de quitter le programme et renvoie le nombre de fenêtres créées avant la fermeture.
Cette fonction était déjà implémentée et nous ne l'avons pas modifié.

- Touche "h" :
Cette touche permet la création d'une nouvelle fenêtre s'il n'y en a pas déjà et la séparation d'une fenêtre existante en deux. La séparation se fait de manière horizontale.
La première étape de cette fonction est de tester si il existe déjà une fenêtre. S'il n'en existe pas on en crée une avec la taille maximale qui est de 640x480 pixels. S'il existe déjà une fenêtre alors on sépare la fenêtre active en deux fenêtres dont la taille dépend du ratio donné par défaut qui est de 0.5. Pour finir la dernière fenêtre créée lors de cette séparation sera toujours la fenêtre active.

- Touche "v" :
Cette touche fonctionne exactement comme la touche "h", mais la séparation se fait de manière verticale.
La fonction marche quasiment pareil que la fonction précédent, la seule différence est que nous modifions pas les mêmes coordonnées dans la fenêtre.

- Touche "n" :
Cette touche permet un déplacement vers la fenêtre suivante, dans la mesure du possible en actualisant la fenêtre active si le déplacement se fait.
La fonction teste d'abord s'il existe bien une fenêtre, si celle-ci est unique, si celle-ci est la dernière créée, si aucune de ces conditions est vraie alors on effectue le déplacement à droite si on est dans une séparation horizontale ou vers le haut si c'est une séparation verticale.

- Touche "p" :
Cette touche permet un déplacement vers la fenêtre précédente, dans la mesure du possible en actualisant la fenêtre active si le déplacement se fait.
La structure reste la même que la fonction de la touche "n", le seul changement étant le déplacement. Si c'est une séparation horizontale, le déplacement se fait à gauche et si c'est une séparation verticale, le déplacement se fait vers le bas.

- Touche "+" :
Cette touche permet une augmentation de la taille de la fenêtre active, dans la mesure du possible. Cette action a pour effet l'actualisation de la taille des autres fenêtres.
Pour la structure de la fonction, on teste s'il y a une fenêtre de créée, si celle-ci est unique, si aucune des ces conditons sont remplies on effectue le redimensionnement de la fenêtre active. A noter que le redimensionnement dépend du type de séparation. Les fenêtres suivantes ou précédentes (selon la position de la fenêtre active) sont ainsi réduites. Pour finir le redimensionnement de la fenêtre active ne peut pas excéder un certain ratio (0.95) afin de pouvoir afficher les fenêtres suivantes.

- Touche "-" : 
Cette touche permet un diminution de la taille de la fenêtre active, dans la mesure du possible. Cette action a pour effet l'actualisation de la taille des autres fenêtres.
La structure de la fonction est similaire à celle de la touche "+" sauf qu'on réduit la taille de la fenêtre active et donc que l'on augmente la taille des fenêtre suivantes ou précédentes (toujours la position de la fenêtre active). Pour finir le redimensionnement de la fenêtre active ne peut pas excéder un certain ratio (0.05) afin de pouvoir afficher celle-ci.

- Touche "r" : 
Cette touche permet la suppresion de la fenêtre active, si cela est possible. Cette action engendre le redimensionnement des autres fenêtres. De plus on change la fenêtre active, si la fenêtre supprimée était la dernière alors la fenêtre active devient la précédente, si la fenêtre n'était pas la dernière fenêtre alors la fenêtre active devient la suivante sinon il n'y a plus de fenêtre active.
Pour la structure de la fonction, on teste s'il y a une fenêtre de créée, si cette condition est remplie alors on supprime la fenêtre. Par contre s'il y a plusieurs fenêtres de créees alors la suppresion de la fenêtre active va entrainer un redimensionnement des autres fenêtres selon le type de séparation de celles-ci.

- Touche "u" :
Une fois cette touche pressée, l'utilisateur doit rentrer une des 4 couleurs proposées dans le terminal et ceci changera la couleur de la fenêtre active.
La fonction teste d'abord s'l existe une fenêtre, si c'est le cas, on modifie la couleur de la fenêtre active.

- Touche 1-9 :
Chaque touche correspond à une couleur, en appuyant sur la touche la fenêtre active prend la couleur correspondante.
La fonction marche de la même manière que la fonction précédente.

