# Projet de compression de données en Haskell

Le projet consiste à implémenter différentes méthodes de compression de données en Haskell. Les méthodes étudiées sont les suivantes :

## RLE (Run-Length Encoding)
Méthodes statistiques :
- Huffman
- Shannon-Fano

Méthodes à dictionnaire :
- LZ78
- LZW

## Description succincte de l'implémentation mise en place

Pour chaque méthode, une fonction a été implémentée en Haskell. Les fonctions prennent en entrée un tableau d'octets et retournent un tableau d'octets compressés.

## Étude des méthodes de compression

### Méthode RLE

#### Quelles configurations donnent les meilleures performances ?
La performance de la compression RLE dépend de la taille des répétitions dans les données. La meilleure configuration est donc d'utiliser un codeur RLE qui utilise un dictionnaire pour stocker les répétitions les plus fréquentes.

### Méthodes statistiques

#### Arbre de codage
Avant d'utiliser les méthodes statistiques, il est nécessaire de construire un arbre de codage à partir des fréquences des symboles dans les données. Il consiste en un arbre binaire où chaque feuille représente un symbole et chaque branche représente un bit. Les symboles les plus fréquents sont placés près de la racine de l'arbre, tandis que les symboles les moins fréquents sont plus éloignés. L'arbre de codage est utilisé pour encoder les symboles en bits en parcourant l'arbre de la racine à la feuille correspondante. Les méthodes statistiques utilisent l'arbre de codage pour compresser les données en remplaçant les symboles par leur code binaire correspondant. Les méthodes statistiques utilisent également l'arbre de codage pour décompresser les données en remplaçant les codes binaires par les symboles correspondants. L'arbre de codage est généralement stocké dans un tableau de symboles et de fréquences.

Comparaison Huffman / Shannon-Fano
Les deux méthodes sont basées sur la construction d'un arbre de codage. La méthode Huffman est généralement plus performante que la méthode Shannon-Fano, mais elle est également plus complexe à implémenter.

#### Huffman
Les symboles de la table [(symbole, occ)] ont était transformé en tableau de [(Leaf, Occ)]. Ainsi je peux construire l’arbre en même temps que les fusions des cases. À chaque fusion l’arbre se créer petit à petit en partant des Feuilles pour arriver à la racine.
Pour cela j’ai créé un objet qui m’a permis de me simplifier la tâche. Une fonction mergenodes pour fusionné les noeuds et buildpriorityqueu pour construire mon tableau de feuille. 

BuildHuffan est la fonction récursive. Elle continue tant qu’il y a plus de 2 elements grace au patternmatching. 

#### ShannonFano
Les tables ont été divisées en deux afin de séparer le nombre de fréquences le plus équitablement possible des deux côtés. On répète cette opération jusqu'à obtenir un seul tableau. On construit l'arbre en même temps que les séparations en partant de la racine cette fois-ci. Chaque branche représentera un côté du tableau avec la fréquence totale de chaque côté.

#### Quelles configurations offrent les meilleures performances ?
La performance des méthodes statistiques dépend de la distribution des fréquences des symboles dans les données. La meilleure configuration consiste donc à utiliser une méthode qui adapte l'arbre de codage à la distribution des données.

### Méthodes à dictionnaire

#### Comparaison LZ78/LZW
Les deux méthodes sont basées sur la construction d'un dictionnaire de phrases. La méthode LZW est généralement plus performante que la méthode LZ78, mais elle est également plus complexe à implémenter. L'identification des motifs répétés est plus efficace dans la méthode LZW que dans la méthode LZ78 car elle utilise un dictionnaire plus grand dès le début. 
La méthode LZW est également plus efficace pour compresser des données qui contiennent des motifs répétés de longueur variable.
Selon la méthode LZW, le dictionnaire est initialisé avec les symboles de l'alphabet et les phrases de longueur 1. Les phrases sont ensuite ajoutées au dictionnaire à mesure qu'elles sont rencontrées dans les données. Les phrases sont codées en utilisant les indices des phrases dans le dictionnaire. La méthode LZW utilise un dictionnaire de taille fixe, tandis que la méthode LZ78 utilise un dictionnaire de taille variable.
Selon la méthode LZ78, le dictionnaire est initialisé avec les symboles de la phrase que l'on souhaite compresser uniquement. Le dictionnaire est ensuite mis à jour à chaque fois qu'une nouvelle phrase est rencontrée dans les données. Les phrases sont codées en utilisant les indices des phrases dans le dictionnaire, ainsi on peut concaténer les indices pour obtenir la phrase originale.

#### Quelles configurations donnent les meilleures performances ?
La performance des méthodes à dictionnaire dépend de la taille du dictionnaire et de la taille des phrases dans les données. La meilleure configuration est donc d'utiliser un dictionnaire de taille optimale et une méthode qui adapte la taille des phrases à la distribution des phrases dans les données.

## Conclusion

Le choix de la méthode de compression dépend des besoins spécifiques de l'application. Les méthodes RLE sont simples à implémenter et peuvent être très efficaces pour compresser des données qui contiennent de longues répétitions. Les méthodes statistiques sont plus complexes à implémenter, mais elles peuvent être plus performantes que les méthodes RLE pour compresser des données qui ont une distribution de fréquences de symboles non uniforme. Les méthodes à dictionnaire sont les plus performantes, mais elles sont également les plus complexes à implémenter.

## Remerciements

Ce travail a été réalisé dans le cadre du cours "Programmation fonctionnelle" de l'école CY TECH. Je remercie les enseignants et les étudiants de ce cours pour leur aide et leurs conseils.