
// on définit une longueur caractérisatique pour les points, inutile, puisqu'on
// veut un maillage régulier, mais malgré tout indispensable
lc = 0.001 ;

Point(1) = {0, 0, 0, lc} ;
Point(2) = {1, 0, 0, lc} ;
Point(4) = {0, 1, 0, lc} ;
Point(5) = {1, 1, 0, lc} ;
Point(8) = {0, 0, 1, lc} ;
Point(9) = {1, 0, 1, lc} ;
Point(11) = {0, 1, 1, lc} ;
Point(12) = {1, 1,  1, lc} ;

Line(1) = {1,8}; 
Line(2) = {2,9};  
Line(4) = {4,11};
Line(5) = {5,12}; 
Line(8) = {8,11};
Line(9) = {9,12}; 
Line(12)= {1,4};
Line(13)= {2,5};
Line(16) = {1,2};
Line(18) = {4,5};
Line(21) = {8,9}; 
Line(23) = {11,12};

Line Loop(26) = {2,-21,-1,16};   Ruled Surface(1) = {26};
Line Loop(28) = {-23,-4,18,5};   Ruled Surface(3) = {28};
Line Loop(30) = {9,-23,-8,21};   Ruled Surface(5) = {30};
Line Loop(33) = {-4,-12,1,8};    Ruled Surface(8) = {33};
Line Loop(34) = {-5,-13,2,9};    Ruled Surface(9) = {34};
Line Loop(38) = {18,-13,-16,12}; Ruled Surface(13) = {38};

Surface Loop(16) = {5,-9,-3,8,13,1};
Volume(1) = {16};
 
Transfinite Line{1,2,4,5,8,9,12,13,16,18,21,23}=3;

Transfinite Surface{1} = {1,2,9,8};
Transfinite Surface{3} = {4,5,12,11};
Transfinite Surface{5} = {11,8,9,12};
Transfinite Surface{8} = {11,4,1,8};
Transfinite Surface{9} = {5,2,9,12};
Transfinite Surface{13} = {4,5,2,1};

Transfinite Volume{1} = {4,1,2,5,11,8,9,12};

Recombine Surface {1};
Recombine Surface {3};
Recombine Surface {5};
Recombine Surface {8};
Recombine Surface {9};
Recombine Surface {13};

Physical Surface(100) = {1};
Physical Surface(101) = {3};
Physical Surface(102) = {5};
Physical Surface(103) = {8};
Physical Surface(104) = {9};
Physical Surface(105) = {13};

Physical Volume(1000) = {1};

