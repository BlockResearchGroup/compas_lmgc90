
r_lopin = 50.;
l_lopin = 150.;

Point(1) = {0, 0, 0, 1};
Point(2) = {0.,r_lopin, 0, 1};
Point(3) = {-l_lopin,r_lopin, 0, 1};
Point(4) = {-l_lopin, 0, 0, 1};
Line (1) = {1, 2};
Line (2) = {2, 3};
Line (3) = {3, 4};
Line (4) = {4, 1};

Line Loop(10)         = {1,2,3,4};
Ruled Surface(20)     = {10};

Transfinite Line{1,3} = 10;
Transfinite Line{2,4} = 20;

Transfinite Surface{20} = {1,2,3,4};
Recombine Surface {20};

Physical Surface(20)={20};
Physical Line(21) = {1,2}; 
Physical Line(22) = {3};
Physical Line(23) = {4};  