
lc = 0.25 ;


// deposit brick
Point(1) = {0, 0, 0, lc};
Point(2) = {1, 0, 0, lc};
Point(3) = {2, 0, 0, lc};
Point(4) = {2, 1, 0, lc};
Point(5) = {1, 1, 0, lc};
Point(6) = {0, 1, 0, lc};

Line(1) = {1,2}; 
Line(2) = {2,3}; 
Line(3) = {3,4}; 
Line(4) = {4,5}; 
Line(5) = {5,6}; 
Line(6) = {6,1}; 

Line Loop(101) = {1, 2, 3, 4, 5, 6};

Plane Surface(1) = {101};
Physical Surface("brick", 1) = {1};

Physical Line("bottom left" , 2) = {1};
Physical Line("bottom right", 3) = {2};
Physical Line("upper left"  , 4) = {5};
Physical Line("upper right" , 5) = {4};
