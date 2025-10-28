/********************************************************************/
/*                                                                  */
/*                         Inclusion matrice     		    */
/*                                                                  */
/********************************************************************/
        
/* définition des variables servant à définir le domaine */

// 	- l'unité de longueur: 
mm = 1e-03;

//	- la largeur:
l = 100.*mm;

//	- la hauteur:
h = 100.*mm;

// Inclusion :
r = 20*mm;

// définition de la longueur caractéristique:

lc = 5.*mm;
lcSphere = 5.*mm;

/* définition des points */

// on place les deux coins inférieurs du quadrilatère
Point(1) = {0, 0, 0, lc};
Point(2) = {l, 0, 0, lc};
Point(3) = {l, h, 0, lc};
Point(4) = {0, h, 0, lc};
Point(5) = {l/2, h/2, 0, lcSphere};
Point(6) = {l/2+r, h/2, 0, lcSphere};
Point(7) = {l/2, h/2+r, 0, lcSphere};
Point(8) = {l/2-r, h/2, 0, lcSphere};
Point(9) = {l/2, h/2-r, 0, lcSphere};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};

Circle(5) = {6, 5, 7};
Circle(6) = {7, 5, 8};
Circle(7) = {8, 5, 9};
Circle(8) = {9, 5, 6};

Line Loop(9) = {5, 6, 7, 8};
Plane Surface(10) = {9};
Line Loop(11) = {3, 4, 1, 2};
Plane Surface(12) = {11,-9};

Physical Surface('Granu') = {10};
Physical Surface('Matri') = {12};
Physical Line('cassG') = {8,7};
Physical Line('sainG') = {5,6};
Physical Line('left') = {4};
Physical Line('right') = {2};
Physical Line('up') = {3};
Physical Line('down') = {1};

Mesh.MshFileVersion = 2 ;



