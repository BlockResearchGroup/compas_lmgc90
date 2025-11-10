Mesh.SaveAll = 1;
lc = 0.1;
//+
Point(1) = {-1., 0., 0., lc};
//+
Point(2) = { 0.,-1., 0., lc};
//+
Point(3) = { 1., 0., 0., lc};
//+
Point(4) = { 0., 1., 0., lc};
//+
Point(5) = { 0., 0., 1., lc};

//+
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line(5) = {1, 5};
Line(6) = {2, 5};
Line(7) = {3, 5};
Line(8) = {4, 5};
//+
Curve Loop(1) = {1, 6, -5};
Curve Loop(2) = {2, 7, -6};
Curve Loop(3) = {3, 8, -7};
Curve Loop(4) = {4, 5, -8};
//+
Plane Surface(1) = {1};
Plane Surface(2) = {2};
Plane Surface(3) = {3};
Plane Surface(4) = {4};
