/*************************************************************/
/* Définition de la géométrie pour le cas de la barre Taylor */
/*************************************************************/

/* définition des variables utilisées */

// on définit le milimètre
mm = 1e-3 ;

// on définit le nombre d'élément dans la direction (0x)
nb_ele_x = 12 ;

// on définit le nombre d'élément dans la direction (0y)
nb_ele_y = 4 ;

// on définit une longueur caractérisatique pour les points, inutile, puisqu'on
// veut un maillage régulier, mais malgré tout indispensable
lc = 0.001 ;

/* définition de la géométrie */

// on définit les quatre coins du rectangle :
Point(1) = {0, 0, 0, lc} ;
Point(2) = {3.*mm, 0, 0, lc} ;
Point(3) = {3.*mm, 1.*mm, 0, lc} ;
Point(4) = {0, 1.*mm, 0, lc} ;

// on définit les lignes constituant sa frontière :
Line(1) = {1, 2} ;
Line(2) = {2, 3} ;
Line(3) = {3, 4} ;
Line(4) = {4, 1} ;

// on peut alors définir la frontière, correctement orientée, du rectangle :
Line Loop(5) = {1, 2, 3, 4} ;

// et finalement, la surface constituée par le rectangle :
Plane Surface(6) = {5} ;

/* définition du maillage */

// on place (nb_ele_x + 1) points équidistants sur les lignes 1 et 3 :
Transfinite Line{1, 3} = nb_ele_x + 1 ;

// on place (nb_ele_y + 1) points équidistants sur les lignes 2 et 4 :
Transfinite Line{2, 4} = nb_ele_y + 1 ;

// on spécifie les quatre points de l'interploation "transfinie", pour définir
// le rectangle comme une surface transfinie :
Transfinite Surface{6} = {1, 2, 3, 4};

// on réorganise les traingles en quadrangles :
Recombine Surface{6};

// definition des groupes pour appliquer les condtions limites :
//    * point sur lequel on tire
Physical Point(8) = {2};
//    * point oppose a celui lequel on tire
Physical Point(9) = {3};
//    * ligne encastree
Physical Line(7) = {4};
//    * affecation d'un numero bidon, pour garder les elements de surface
Physical Surface(20) = {6};

