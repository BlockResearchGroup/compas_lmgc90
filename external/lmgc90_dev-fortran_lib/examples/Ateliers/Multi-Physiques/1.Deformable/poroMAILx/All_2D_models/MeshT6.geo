/* Declaration des variables*/

Largeur_X = 3.175;
Largeur_Y = 2.5;

nbr_elem_X = 3;
nbr_elem_Y = 10;
Progression = 1.0;

L_mesh = 0.25;

/* Declaration de la geometrie */

Point (1) = {0, 0, 0, L_mesh};
Point (2) = {Largeur_X, 0, 0, L_mesh/4.0};
Point (3) = {Largeur_X,Largeur_Y, 0, L_mesh/4.0};
Point (4) = {0, Largeur_Y, 0, L_mesh};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line Loop(5) = {1, 2, 3, 4};
Plane Surface(6) = {5};
Physical Line(7) = {1};
Physical Line(8) = {2};
Physical Line(9) = {3};
Physical Line(10) = {4};
Physical Surface(11) = {6};
Transfinite Line {-1} = nbr_elem_X+1 Using Progression Progression;
Transfinite Line {-2} = nbr_elem_Y+1 Using Progression Progression;
Transfinite Line {3} = nbr_elem_X+1 Using Progression Progression;
Transfinite Line {4} = nbr_elem_Y+1 Using Progression Progression;
Transfinite Surface {6} = {1, 2, 3, 4};
