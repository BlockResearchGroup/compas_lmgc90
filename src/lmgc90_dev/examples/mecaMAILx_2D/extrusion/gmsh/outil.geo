l_outil = 100.;
l_arr   = 50.;
alpha   = 15.*3.14/180.;
r_outil = 200.;
r_lopin = 50.;
finesse = 10;
grossier = 25.;
Point(1) = {-l_arr,r_lopin+l_arr*Sin(alpha), 0, finesse};
Point(2) = {-l_arr,r_outil, 0,grossier};
Point(3) = {l_outil-l_arr,r_outil, 0, grossier};
Point(4) = {l_outil-l_arr,r_lopin-(l_outil-l_arr)*Sin(alpha), 0,finesse};

Line (1) = {1, 4};
Line (2) = {4, 3};
Line (3) = {3, 2};
Line (4) = { 2,1};

Line Loop (5)       = {1,2,3,4};

Plane Surface(7)    = {5};
Physical Surface(8) = {7};
Physical Line(11)   = {1};
Physical Line(12)   = {2};
Physical Line(13)   = {3};
Physical Line(14)   = {4};