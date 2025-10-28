
lc = 1.0 ;


// deposit brick
Point( 1) = {0, 0, 0, lc};
Point( 2) = {1, 0, 0, lc};
Point( 3) = {2, 0, 0, lc};
Point( 4) = {2, 2, 0, lc};
Point( 5) = {1, 2, 0, lc};
Point( 6) = {0, 2, 0, lc};
Point( 7) = {0, 0, 1, lc};
Point( 8) = {1, 0, 1, lc};
Point( 9) = {2, 0, 1, lc};
Point(10) = {2, 2, 1, lc};
Point(11) = {1, 2, 1, lc};
Point(12) = {0, 2, 1, lc};

// bottom
Line( 1) = {1,2};
Line( 2) = {2,3};
Line( 3) = {3,4};
Line( 4) = {4,5};
Line( 5) = {5,6};
Line( 6) = {6,1};

Line( 7) = {2,5};

// up
Line( 8) = { 7, 8};
Line( 9) = { 8, 9};
Line(10) = { 9,10};
Line(11) = {10,11};
Line(12) = {11,12};
Line(13) = {12, 7};

Line(14) = { 8,11};

// left
Line(15) = { 1, 7};
Line(16) = { 3, 9};

// rigth
Line(17) = { 4,10};
Line(18) = { 6,12};


// bottom
Line Loop(101) = {1, 7, 5, 6};
Line Loop(102) = {2, 3, 4,-7};

// up 
Line Loop(103) = {8,14,12, 13};
Line Loop(104) = {9,10,11,-14};

// left
Line Loop(105) = {1, 2, 16, - 9, - 8, -15};

// right
Line Loop(106) = {4, 5, 18, -12, -11, -17};

// front
Line Loop(107) = {3, 17, -10, -16};

// rear
Line Loop(108) = {6, 15, -13, -18};


Plane Surface(1) = {101};
Plane Surface(2) = {102};
Plane Surface(3) = {103};
Plane Surface(4) = {104};
Plane Surface(5) = {105};
Plane Surface(6) = {106};
Plane Surface(7) = {107};
Plane Surface(8) = {108};


Physical Surface("bottom rear" , 1) = {1};
Physical Surface("bottom front", 2) = {2};
Physical Surface("upper rear"  , 3) = {3};
Physical Surface("upper front" , 4) = {4};
Physical Surface("left"        , 5) = {5};
Physical Surface("right"       , 6) = {6};
Physical Surface("front"       , 7) = {7};
Physical Surface("rear"        , 8) = {8};


Surface Loop(9) = {-1, -2, 3, 4, -5, 6, 7, -8};
Volume(1) = {9};
Physical Volume(1000) = {1};
 
