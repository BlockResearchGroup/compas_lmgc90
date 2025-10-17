/* Declaration des variables*/

Longueur = 100.0;
L_mesh = 1.0;

/* Declaration de la geometrie */

Point (1) = {0, 0, 0, L_mesh};
Point (2) = {Longueur, 0, 0, L_mesh};
Line(1) = {1, 2};
Physical Line(2) = {1};
