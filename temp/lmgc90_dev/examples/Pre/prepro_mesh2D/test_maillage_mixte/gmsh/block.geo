
lx=0.15;
ly=0.05;

Point(1) = {lx, 0., 0., 0.01};
Point(2) = {lx, ly, 0., 0.01};
Point(3) = {0., ly, 0., 0.01};
Point(4) = {0., 0., 0., 0.01};
Line (1) = {1, 2};
Line (2) = {2, 3};
Line (3) = {3, 4};
Line (4) = {4, 1};

Line Loop(10)         = {1,2,3,4};
Plane Surface(20)     = {10};

/*Transfinite Line{1,3} = 6;
Transfinite Line{2,4} = 16;

Transfinite Surface{20} = {1,2,3,4};*/
Recombine Surface {20};

// groupes :
//   * la ligne du dessous
Physical Line(1) = {4};
 //   * la ligne du dessus
Physical Line(2) = {2};
//   * la surface du bloc
Physical Surface(3)={20};
