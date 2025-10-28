/* Declaration des variables*/

Largeur_X = 20.0;
Largeur_Y = 100.0;
Largeur_X1 = 40.0;
nbr_elem_X = 10;
nbr_elem_Y = 20;
Progression = 1.2;

L_mesh = 0.25;

/* Declaration de la geometrie */

Point (1) = {0, 0, 0, L_mesh};
Point (2) = {Largeur_X, 0, 0, L_mesh/4.0};
Point (3) = {Largeur_X,Largeur_Y, 0, L_mesh/4.0};
Point (4) = {0, Largeur_Y, 0, L_mesh};

Point (5) = {Largeur_X1,Largeur_Y, 0, L_mesh/4.0};
Point (6) = {Largeur_X1,2.0*Largeur_Y, 0, L_mesh/4.0};
Point (7) = {0,2.0*Largeur_Y, 0, L_mesh/4.0};
Point (8) = {Largeur_X,2.0*Largeur_Y, 0, L_mesh/4.0};

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
Recombine Surface {6};

Line(12) = {3, 5};
Line(13) = {5, 6};
Line(14) = {6, 8};
Line(15) = {8, 7};
Line(16) = {7, 4};
Line(24) = {3, 8};

Line Loop(25) = {12, 13, 14, -24};
Plane Surface(26) = {25};
Line Loop(27) = {3, -16, -15, -24};
Plane Surface(28) = {-27};

Physical Line(29) = {12};
Physical Line(30) = {13};
Physical Line(31) = {16};
Physical Line(32) = {15, 14};
Physical Surface(33) = {26};
Physical Surface(34) = {28};

Transfinite Line {12} = nbr_elem_X+1 Using Progression Progression;
Transfinite Line {13} = nbr_elem_Y+1 Using Progression Progression;
Transfinite Line {-14} = nbr_elem_X+1 Using Progression Progression;
Transfinite Line {24} = nbr_elem_Y+1 Using Progression Progression;
Transfinite Line {15} = nbr_elem_X+1 Using Progression Progression;
Transfinite Line {-16} = nbr_elem_Y+1 Using Progression Progression;

Transfinite Surface {26} = {3, 5, 6, 8};
Transfinite Surface {28} = {4, 3, 8, 7};
Recombine Surface {26};
Recombine Surface {28};

