/* Declaration des variables*/

Longueur = 1.0;
L_mesh = 2.0;

/* Declaration de la geometrie */

Point (1) = {0, 0, 0, L_mesh};
Point (2) = {0, 0, Longueur, L_mesh};
Point (3) = {Longueur, 0, 0, L_mesh};
Point (4) = {Longueur, 0, Longueur, L_mesh};
Point (5) = {2*Longueur, 0, 0, L_mesh};
Point (6) = {2*Longueur, 0, Longueur, L_mesh};
Point (7) = {3*Longueur, 0, 0, L_mesh};
Point (8) = {3*Longueur, 0, Longueur, L_mesh};
//
Line(1) = {1, 2};
Line(2) = {1, 3};
Line(3) = {2, 4};
Line(4) = {2, 3};
Line(5) = {1, 4};
//
Line(6) = {3, 4};
Line(7) = {3, 5};
Line(8) = {3, 6};
Line(9) = {4, 6};
Line(10) = {4, 5};
//
Line(11) = {5, 6};
Line(12) = {5, 7};
Line(13) = {5, 8};
Line(14) = {6, 7};
Line(15) = {6, 8};
//
Line(16) = {7, 8};
//
Physical Line(2) = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
