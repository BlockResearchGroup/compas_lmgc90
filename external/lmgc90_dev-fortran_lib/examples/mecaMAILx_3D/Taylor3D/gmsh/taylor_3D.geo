/********************************************************************* 
 *                                                                   *
 *  Définition de la géométrie pour le cas de la barre Taylor        *
 *                                                                   *
 *                                                                   *
 *********************************************************************/

// We start by defining a more complex geometry, using the same
// commands as in the previous examples:

// on définit le milimètre
mm = 1e-3 ;

// on définit le rayon de la barre
r = 3.2*mm ;

// on définit lalongueur de la barre
L = 32.4*mm ;

// on définit le nombre d'éléments suivant un rayon du quart de disque
nb_ele_r = 4 ;

// on définit le nombre d'éléments suivant la longueur de la barre
nb_ele_L = 12;

// on définit l'angle définissant le plan entre les deux prismes à coté
// en forme d'arc de cercle
phi = Pi/4 ;

// on définit une longueur caractérisatique pour les points, inutile, puisqu'on
// veut un maillage régulier, mais malgré tout indispensable
lc = 0.001 ;

/* définition de la géométrie */

/* on définit les points */
Point(1) = {0, 0, 0, lc} ;
Point(2) = {0.5*r, 0, 0, lc} ;
Point(3) = {r, 0, 0, lc} ;
Point(4) = {0, 0.5*r, 0, lc} ;
Point(5) = {0.5*r, 0.5*r, 0, lc} ;
Point(6) = {0, r, 0, lc} ;
Point(7) = {r*Cos(phi), r*Sin(phi), 0, lc} ;
Point(8) = {0, 0, L, lc} ;
Point(9) = {0.5*r, 0, L, lc} ;
Point(10) = {r, 0, L, lc} ;
Point(11) = {0, 0.5*r, L, lc} ;
Point(12) = {0.5*r, 0.5*r,  L, lc} ;
Point(13) = {0, r, L, lc} ;
Point(14) = {r*Cos(phi), r*Sin(phi), L, lc} ;

/* on définit les lignes */
Line(1) = {1,8}; 
Line(2) = {2,9};  
Line(3) = {3, 10};
Line(4) = {4,11};
Line(5) = {5,12};  
Line(6) = {6,13}; 
Line(7) = {7,14};  
Line(8) = {8,11};
Line(9) = {9,12};
Circle(10) = {10,8,14};
Line(11) = {11,13};
Line(12)= {1,4};
Line(13)= {2,5};
Circle(14) = {3,1,7}; 
Line(15) = {4,6};
Line(16) = {1,2};
Line(17) = {2,3};
Line(18) = {4,5};
Line(19) = {5,7};
Circle(20) = {6,1,7};
Line(21) = {8,9}; 
Line(22) = {9,10};
Line(23) = {11,12};
Line(24) = {12,14};  
Circle(25) = {13,8,14};

/* on en déduit les surfaces */
Line Loop(26) = {2,-21,-1,16};   Ruled Surface(1) = {26};
Line Loop(27) = {3,-22,-2,17};   Ruled Surface(2) = {27};
Line Loop(28) = {-23,-4,18,5};   Ruled Surface(3) = {28};
Line Loop(29) = {-25,-6,20,7};   Ruled Surface(4) = {29};
Line Loop(30) = {9,-23,-8,21};   Ruled Surface(5) = {30};
Line Loop(31) = {10,-24,-9,22};  Ruled Surface(6) = {31};
Line Loop(32) = {24,-25,-11,23}; Ruled Surface(7) = {32};
Line Loop(33) = {-4,-12,1,8};    Ruled Surface(8) = {33};
Line Loop(34) = {-5,-13,2,9};    Ruled Surface(9) = {34};
Line Loop(35) = {-7,-14,3,10};   Ruled Surface(10) = {35};
Line Loop(36) = {-6,-15,4,11};   Ruled Surface(11) = {36};
Line Loop(37) = {-7,-19,5,24};   Ruled Surface(12) = {37};
Line Loop(38) = {18,-13,-16,12}; Ruled Surface(13) = {38};
Line Loop(39) = {19,-14,-17,13}; Ruled Surface(14) = {39};
Line Loop(40) = {20,-19,-18,15}; Ruled Surface(15) = {40};

/* on en déduit le les volumes */

// pour le parallépipède définit par les plans: x=0, y=0, x=0.5*r, y=0.5*r
// z=0 et z=L
Surface Loop(16) = {5,-9,-3,8,13,1};
Volume(1) = {16};

Surface Loop(17) = {6,-10,-12,9,14,2};
Volume(2) = {17};

Surface Loop(18) = {7,-12,-4,11,15,3};
Volume(3) = {18};

/* définition du maillage */

// on place (nb_ele_L + 1) points équidistants sur les lignes sur la
// longueur de la barre
Transfinite Line{1,2,3,4,5,6,7} = nb_ele_L + 1;

// on place (nb_ele_r/2 + 1) points équidistants sur les lignes inscrite 
// dans les deux quarts de cercle :
Transfinite Line{8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25} = nb_ele_r/2 + 1;

// on spécifie les quatre points de l'interploation "transfinie", pour définir
// les surfaces des surfaces transfinies :
Transfinite Surface{1} = {1,2,9,8};
Transfinite Surface{2} = {2,3,10,9};
Transfinite Surface{3} = {4,5,12,11};
Transfinite Surface{4} = {6,7,14,13};
Transfinite Surface{5} = {11,8,9,12};
Transfinite Surface{6} = {12,9,10,14};
Transfinite Surface{7} = {13,11,12,14};
Transfinite Surface{8} = {11,4,1,8};
Transfinite Surface{9} = {5,2,9,12};
Transfinite Surface{10} = {7,3,10,14};
Transfinite Surface{11} = {13,6,4,11};
Transfinite Surface{12} = {7,5,12,14};
Transfinite Surface{13} = {4,5,2,1};
Transfinite Surface{14} = {5,7,3,2};
Transfinite Surface{15} = {6,7,5,4};

// on spécifie les huits points de l'interploation "transfinie", pour définir
// les surfaces des volumes transfinis :
Transfinite Volume{1} = {4,1,2,5,11,8,9,12};
Transfinite Volume{2} = {5,2,3,7,12,9,10,14};
Transfinite Volume{3} = {6,4,5,7,13,11,12,14};

// on réorganise les triangles en quadrangles :
Recombine Surface {1};
Recombine Surface {2};
Recombine Surface {3};
Recombine Surface {4};
Recombine Surface {5};
Recombine Surface {6};
Recombine Surface {7};
Recombine Surface {8};
Recombine Surface {9};
Recombine Surface {10};
Recombine Surface {11};
Recombine Surface {12};
Recombine Surface {13};
Recombine Surface {14};
Recombine Surface {15};

/* définition des entités physiques */

// on définit les surfaces où on impose le déplacement:

// conditions de symétrie:
Physical Surface(100) = {1, 2}; // y=0, vitesse imposée: v_y=0 
Physical Surface(101) = {8, 11}; // x=0, vitesse imposée: v_x=0

// contact uni-latéral avec le fondation rigide 
Physical Surface(102) = {13, 14, 15}; // z=0, vitesse imposée: v_z=0

// on définit le volume où donner une vitesse initiale:
Physical Volume(1000) = {1, 2, 3};
