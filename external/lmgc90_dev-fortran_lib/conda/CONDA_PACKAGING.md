# Explication résumé de la création d'un package conda

Avec des interrogations sur quelques concepts qui me sont restés obscurs.

## Quelques principes de base (cf [conda build doc](http://conda-forge.org/docs/maintainer/adding_pkgs.html))
1. Le fichier principal est meta.yaml, qui peut appeler build.sh (linux et mac) et/ou bld.bat (windows)
1. Ce fichier contient plusieurs sections indispensables :
    * __package__ pour le nom du paquet et sa version.
    * __source__ qui fait référence à une archive accessible sur le réseau (un tar.gz ou un zip).
    * __requirements__ qui list les paquet nécessaire à l'installation (section *build*), à l'utilisation (section *run*) et à ??l'environnement?? (section *host*).
    D'après ce que j'en comprend le *build* contient les outils de compilation, le *host* les outils indispensables à la compilation (dans notre cas python et numpy) et le *run* les outils indispensables à l'exécution.
    * __test__ avec les sous-sections *imports* (le paquet qu'on vient d'installer ?), *require* (en général nose), *source_files* (fichiers à copier) et *commands* (liste des commandes à exécuter)
1. J'utilise ensuite la commande `conda-build -c conda-forge .`  pour faire les essais de compilation en local.
Concrètement cette commande créé un environnement de build (avec les compilateurs si besoin), et un environnement final, construit le paquet depuis le build et met les executables dans le final, et fait les tests.
1. Lorsque le build est passé, on peut tester l'installation : `conda install -c conda-forge --use-local lmgc90`.
1. Lorsque les essais sont concluant, on fork [l'exemple de recette](https://github.com/conda-forge/staged-recipes/tree/master/recipes), on créer une nouvelle branch, commit, push et PR.
1. Le processus d'intégration continue se lance, et si tous les tests passent le package devrait etre validé sur conda-forge.

**Attention** : le conda build (via le meta.yaml) crée un environnement de compilation (build) et un environnement d'installation (host et/ou run), puis récupère un zip et fait l'installation à partir de ça.
Du coup il faut soit mettre un chemin local vers une archive (qu'il faut alors crée), soit avoir une archive accessible world wide et renseigner l'url de téléchargement dans le meta.yaml.
## En résumé, pour tester le conda-build et l'installation
Vérifier l'accès à l'archive et sa version. Pour ça il faut voir la partie **source** du meta.yaml.
- Soit l'URL est http, auquel cas il faut être sûr qu'elle pointe vers une version à jour de lmgc90.  
- soit l'URL est un chemin local, auquel cas il faut créer le zip et mettre le bon chemin vers l'archive.

Se mettre à la racine de lmgc90_dev puis : 
```bash
conda create --yes -c defaults -n test_build conda-build
conda activate test_build
conda build -c conda-forge .
```
Si tout s'est bien passé dans l'étape précédente, on peut essayer de l'installer :  
Sur Windows : 
```bash
conda install --use-local lmgc90 -c conda-forge
```
Sur Linux :
```bash
conda install -c ${CONDA_PREFIX}/conda-bld lmgc90 -c conda-forge
```
## TODO list
1. Est-ce que les tests doivent normalement passer ?
1. Faire les essais sur windows et mac
1. ~~Déboguer le problème de compilation Fortran... et peut être les bugs suivant.~~  __binutils!!__
1. ~~Créer une nouvelle archive accessible worldwide, avec les quelques modifications concernant la compilation~~
1. Mettre en place les tests python avec nosetest ?
1. Utiliser MUMPS de conda au lieu de recompiler.

## Notes en vrac
L'intérêt de tester le conda build dans un docker est de garantir que l'installation ne dépend d'aucun paquet du système hôte.
Les dockers ainsi crées, sous Linux (Debian) ou Windows, contiennent le minimum de programme système, ainsi l'ensemble des programmes viennent de conda.

Utilisation du docker sous Linux :
```bash
cd conda
./test_conda_build_linux.sh
```

Utilisation du docker sous Windows :
```bash
cd conda
./test_conda_build_windows.bat
```

Les étapes de ces scripts sont :
- Créer un archive (zip) avec le projet en cours
- Créer un dossier conda contenant le meta.yaml et le script de build (build.sh ou bld.bat)
- Créer un container Debian ou Windows server
- Copier l'archive et le dossier conda
- Tester conda build suivi de conda install
