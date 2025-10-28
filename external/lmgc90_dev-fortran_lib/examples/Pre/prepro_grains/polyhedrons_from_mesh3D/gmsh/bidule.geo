//+
Point(1)  = {0. , 0.  , 0. , 1.0};
//+
Point(2)  = {1. , 0.  , 0. , 1.0};
//+
Point(3)  = {1. , 1.  , 0. , 1.0};
//+
Point(4)  = {0. , 1.  , 0. , 1.0};
//+
Point(5)  = {0. , 0.  , 1. , 1.0};
//+
Point(6)  = {1. , 0.  , 1. , 1.0};
//+
Point(7)  = {1. , 1.  , 1. , 1.0};
//+
Point(8)  = {0. , 1.  , 1. , 1.0};
//+
Point(9)  = {0. , 1.5 , 0.5, 1.0};
//+
Point(10) = {1. , 1.5 , 0.5, 1.0};
//+
Point(11) = {1.5, 1.25, 0.5, 1.0};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 4};
//+
Line(4) = {4, 1};
//+
Line(5) = {5, 6};
//+
Line(6) = {6, 7};
//+
Line(7) = {7, 8};
//+
Line(8) = {8, 5};
//+
Line(9) = {1, 5};
//+
Line(10) = {2, 6};
//+
Line(11) = {3, 7};
//+
Line(12) = {4, 8};
//+
Line(13) = {3, 10};
//+
Line(14) = {10, 7};
//+
Line(15) = {4, 9};
//+
Line(16) = {9, 8};
//+
Line(17) = {9, 10};
//+
Line(18) = {3, 11};
//+
Line(19) = {10, 11};
//+
Line(20) = {7, 11};
//+
Line Loop(1) = {1, 2, 3, 4};
//+
Plane Surface(1) = {1};
//+
Line Loop(2) = {1, 10, -5, -9};
//+
Plane Surface(2) = {2};
//+
Line Loop(3) = {2, 11, -6, -10};
//+
Plane Surface(3) = {3};
//+
Line Loop(4) = {4, 9, -8, -12};
//+
Plane Surface(4) = {4};
//+
Line Loop(5) = {5, 6, 7, 8};
//+
Plane Surface(5) = {5};
//+
Line Loop(6) = {3, 15, 17, -13};
//+
Plane Surface(6) = {6};
//+
Line Loop(7) = {7, -16, 17, 14};
//+
Plane Surface(7) = {7};
//+
Line Loop(8) = {12, -16, -15};
//+
Plane Surface(8) = {8};
//+
Line Loop(9) = {11, 20, -18};
//+
Plane Surface(9) = {9};
//+
Line Loop(10) = {13, 19, -18};
//+
Plane Surface(10) = {10};
//+
Line Loop(11) = {19, -20, -14};
//+
Plane Surface(11) = {11};
//+
Line Loop(12) = {3, 12, -7, -11};
//+
Plane Surface(12) = {12};
//+
Line Loop(13) = {14, -11, 13};
//+
Plane Surface(13) = {13};
//+
Surface Loop(1) = {3, 1, 2, 5, 4, 12};
//+
Volume(1) = {1};
//+
Surface Loop(2) = {12, 6, 8, 7, 13};
//+
Volume(2) = {2};
//+
Surface Loop(3) = {10, 11, 9, 13};
//+
Volume(3) = {3};
//+
Physical Volume(1) = {1, 2, 3};
