/* définition des variables utilisées */

mm = 1e-3 ;

nb_ele_x = 12 ;

nb_ele_y = 4 ;

lc = 1e-4 ;

lx=3.e-3;
ly=1.e-3;
ep=1.e-5;
p=lx*0.2; 

/* définition de la géométrie */

// on définit les quatre coins du rectangle :
Point(1) = { 0,  0, 0, lc} ;
Point(2) = {lx,  0, 0, lc} ;
Point(3) = {lx, ly, 0, lc} ;
Point(4) = { 0, ly, 0, lc} ;
Point(5) = { 0, (ly*0.5) + ep, 0, lc} ;
Point(6) = { p, ly*0.5, 0, lc} ;
Point(7) = { 0, (ly*0.5) - ep, 0, lc} ;

// on définit les lignes constituant sa frontière :
Line(1) = {1, 2} ;
Line(2) = {2, 3} ;
Line(3) = {3, 4} ;
Line(4) = {4, 5} ;
Line(5) = {5, 6} ;
Line(6) = {6, 7} ;
Line(7) = {7, 1} ;

// on peut alors définir la frontière, correctement orientée, du rectangle :
Line Loop(5) = {1, 2, 3, 4, 5, 6, 7} ;

// et finalement, la surface constituée par le rectangle :
Plane Surface(6) = {5} ;

// definition des groupes pour appliquer les condtions limites :
//    * point sur lequel on tire
Physical Point(8) = {4};
//    * ligne encastree
Physical Line(7) = {1};
//    * ligne contacteur bas
Physical Line(101) = {6};
//    * ligne contacteur haut
Physical Line(102) = {5};
//    * affecation d'un numero bidon, pour garder les elements de surface
Physical Surface(20) = {6};

