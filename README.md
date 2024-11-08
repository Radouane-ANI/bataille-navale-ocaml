# Bataille Navale

**Bataille Navale** est un jeu de bataille navale développé en **OCaml**, conçu pour être joué dans le terminal. Le projet utilise le gestionnaire de build **Dune** et inclut des dépendances pour les tests aléatoires avec **QCheck** et **OUnit2**.

## Table des Matières

- [Installation](#installation)
- [Utilisation](#utilisation)
- [Règles du Jeu](#règles-du-jeu)
- [Structure du Code](#structure-du-code)

## Installation

### Prérequis

Assurez-vous d'avoir **OCaml** et **Dune** installés sur votre système. Vous pouvez les installer avec les commandes suivantes :

```opam install ocaml dune ```

### Cloner le dépôt

Clonez le dépôt depuis GitLab :

```git clone https://moule.informatique.univ-paris-diderot.fr/aouini/bataille-navalle.git ```

### Installation des dépendances

Le projet utilise des dépendances supplémentaires comme **QCheck** pour les tests. Installez-les avec :

```opam install . --deps-only ```

### Compilation du projet

Pour compiler le projet, exécutez :

```dune build ```

### Exécution du jeu

Lancez le jeu avec la commande suivante :

```dune exec ./bin/main.exe ```

## Utilisation

Le jeu se joue entièrement dans le terminal. Au début, chaque joueur doit placer ses navires sur une grille. Les joueurs alternent ensuite pour tirer sur les positions de la grille de l'adversaire en indiquant les coordonnées de leur tir.

Chaque tour, le programme indiquera si le tir est un **touché**, **manqué**, ou **coulé**, et actualisera la grille en conséquence.

### Commandes du jeu

```place <x>, puis <y>, puis <orientation(o/n)> :``` Pour placer un navire à une position spécifique.

```tir <x>, puis <y> :``` Pour effectuer un tir sur une case spécifique de la grille de l'adversaire.

## Règles du Jeu

Les règles de ce jeu suivent les règles classiques de la bataille navale :

Chaque joueur place ses navires sur une grille de **10x10**.
Les joueurs alternent pour tirer sur les positions de la grille de l'adversaire.
Un joueur gagne lorsqu'il a coulé tous les navires de l'adversaire.

## Structure du Code

**lib/** : Contient le code source principal du jeu.

**game.ml** : Définit la logique principale du jeu.

**plateau.ml** : Gère la grille de jeu et les positions des navires.

**test/** : Contient des tests écrits avec **QCheck** et **OUnit2** pour vérifier la validité des fonctions principales.
