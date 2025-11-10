/* Description geometrie */
R_annulus = 3.175;
R_nucleus = 2.065;
H_annulus = 2.5;

/* Description geometrie pour maillage */

L_mesh    = 1.0;

/* Description du maillage */
Nombre_element_base0 = 3;
Nombre_element_base1 = 2;
Nombre_element_haut = 10;

/* Frontiere annulus */
Point(1) = {0, 0, 0};
Point(2) = {R_annulus, 0, 0};
Point(3) = {-R_annulus, 0, 0};

Point(4) = {0, 0, H_annulus};
Point(5) = {R_annulus, 0, H_annulus};
Point(6) = {-R_annulus, 0, H_annulus};

Point(7) = {0,R_annulus, 0};
Point(8) = {0,-R_annulus, 0};

Point(9) = {0,R_annulus, H_annulus};
Point(10) = {0,-R_annulus, H_annulus};

/* Frontiere nucleus */
Point(11) = {0, 0, 0};
Point(12) = {R_nucleus, 0, 0};
Point(13) = {-R_nucleus, 0, 0};

Point(14) = {0, 0, H_annulus};
Point(15) = {R_nucleus, 0, H_annulus};
Point(16) = {-R_nucleus, 0, H_annulus};

Point(17) = {0,R_nucleus, 0};
Point(18) = {0,-R_nucleus, 0};

Point(19) = {0, R_nucleus, H_annulus};
Point(20) = {0, -R_nucleus, H_annulus};

/* Description pour maillage Quad4 */

Point(21) = {L_mesh,0,0};
Point(22) = {-L_mesh,0,0};
Point(23) = {0,L_mesh,0};
Point(24) = {0,-L_mesh,0};

Point(25) = {L_mesh,0,H_annulus};
Point(26) = {-L_mesh,0,H_annulus};
Point(27) = {0,L_mesh,H_annulus};
Point(28) = {0,-L_mesh,H_annulus};

/* Surface de la base */
Line(1) = {22, 24};
Line(2) = {24, 21};
Line(3) = {21, 23};
Line(4) = {23, 22};
Line(5) = {22, 13};
Line(6) = {13, 3};
Line(7) = {23, 17};
Line(8) = {17, 7};
Line(9) = {21, 12};
Line(10) = {12, 2};
Line(11) = {24, 18};
Line(12) = {18, 8};
Circle(13) = {3, 1, 8};
Circle(14) = {13, 1, 18};
Circle(15) = {8, 1, 2};
Circle(16) = {18, 1, 12};
Circle(17) = {2, 1, 7};
Circle(18) = {12, 1, 17};
Circle(19) = {7, 1, 3};
Circle(23) = {17, 1, 13};

Line Loop(21) = {4, 1, 2, 3};
Plane Surface(22) = {-21};

Line Loop(24) = {11, 16, -9, -2};
Plane Surface(25) = {-24};
Line Loop(26) = {9, 18, -7, -3};
Plane Surface(27) = {-26};
Line Loop(28) = {7, 23, -5, -4};
Plane Surface(29) = {-28};
Line Loop(30) = {5, 14, -11, -1};
Plane Surface(31) = {-30};
Line Loop(32) = {6, 13, -12, -14};
Plane Surface(33) = {-32};
Line Loop(34) = {12, 15, -10, -16};
Plane Surface(35) = {-34};
Line Loop(36) = {10, 17, -8, -18};
Plane Surface(37) = {-36};
Line Loop(38) = {23, 6, -19, -8};
Plane Surface(39) = {38};

/* Definition du maillage */

Transfinite Line {4, 1, 2, 3} = Nombre_element_base0 + 1 Using Progression 1;
Transfinite Line {18, 17, 15, 16, 13, 14, 23, 19} = Nombre_element_base0 + 1 Using Progression 1;
Transfinite Line {7, 8, 9, 10, 11, 12, 5, 6} = Nombre_element_base1 + 1 Using Progression 1;
Transfinite Surface {22} = {21, 23, 22, 24};
Transfinite Surface {27} = {21, 12, 17, 23};
Transfinite Surface {25} = {24, 18, 12, 21};
Transfinite Surface {31} = {22, 13, 18, 24};
Transfinite Surface {29} = {23, 17, 13, 22};
Transfinite Surface {33} = {13, 3, 8, 18};
Transfinite Surface {35} = {18, 8, 2, 12};
Transfinite Surface {37} = {2, 7, 17, 12};
Transfinite Surface {39} = {17, 7, 3, 13};
Recombine Surface {22, 25, 27, 29, 31, 33, 35, 37, 39};

/* Surface superieure du disque*/

Translate {0, 0, H_annulus} {
  Duplicata { Surface{33, 31, 29, 39, 27, 37, 25, 22, 35}; }
}
/*
Symmetry {5, 0, 0, 1} {
  Surface{75};
}
*/



Transfinite Line {46, 71, 61, 51, 53, 58, 68, 63, 73, 83, 43, 41} = Nombre_element_base0 + 1 Using Progression 1;
Transfinite Line {49, 44, 47, 42, 64, 54, 59, 69} = Nombre_element_base1 + 1 Using Progression 1;
Transfinite Surface {75} = {26, 28, 25, 27};
Transfinite Surface {50} = {16, 26, 27, 19};
Transfinite Surface {55} = {16, 19, 9, 6};
Transfinite Surface {40} = {6, 16, 10, 20};
Transfinite Surface {45} = {16, 26, 28, 20};
Transfinite Surface {60} = {19, 27, 25, 15};
Transfinite Surface {65} = {9, 19, 15, 5};
Transfinite Surface {70} = {25, 28, 20, 15};
Transfinite Surface {80} = {15, 20, 10, 5};
Recombine Surface {55, 50, 75, 65, 60, 80, 70, 45, 40};

/* Liaison entre surface superieure et inferieure */
Line(84) = {2, 5};
Line(85) = {12, 15};
Line(86) = {21, 25};
Line(87) = {23, 27};
Line(88) = {17, 19};
Line(89) = {7, 9};
Line(90) = {3, 6};
Line(91) = {13, 16};
Line(92) = {22, 26};
Line(93) = {24, 28};
Line(94) = {18, 20};
Line(95) = {8, 10};

Line Loop(96) = {46, -93, -1, 92};
Plane Surface(97) = {96};
Line Loop(98) = {51, -92, -4, 87};
Plane Surface(99) = {98};
Line Loop(100) = {3, 87, -61, -86};
Plane Surface(101) = {-100};
Line Loop(102) = {71, -86, -2, 93};
Plane Surface(103) = {102};
Line Loop(104) = {64, -86, 9, 85};
Plane Surface(105) = {104};
Line Loop(106) = {69, -85, 10, 84};
Plane Surface(107) = {106};
Line Loop(108) = {11, 94, -47, -93};
Plane Surface(109) = {108};
Line Loop(110) = {94, 42, -95, -12};
Plane Surface(111) = {-110};
Line Loop(112) = {92, -49, -91, -5};
Plane Surface(113) = {-112};
Line Loop(114) = {6, 90, 44, -91};
Plane Surface(115) = {114};
Line Loop(116) = {87, -54, -88, -7};
Plane Surface(117) = {-116};
Line Loop(118) = {59, -88, 8, 89};
Plane Surface(119) = {118};
Line Loop(120) = {85, 73, -94, 16};
Line Loop(122) = {63, -85, 18, 88};
Line Loop(124) = {88, -53, -91, -23};
Line Loop(126) = {91, 41, -94, -14};
Line Loop(128) = {83, -95, 15, 84};

Ruled Surface(130) = {128};
Line Loop(131) = {84, -68, -89, -17};
Ruled Surface(132) = {-131};
Line Loop(133) = {58, -89, 19, 90};
Ruled Surface(134) = {133};
Line Loop(135) = {90, -43, -95, -13};
Ruled Surface(136) = {-135};
Ruled Surface(137) = {120};
Ruled Surface(138) = {122};
Ruled Surface(139) = {-124};
Ruled Surface(140) = {-126};

/* Maillage dans la hauteur */

Transfinite Line {87, 88, 89, 92, 91, 90, 93, 94, 95, 86, 85, 84} = Nombre_element_haut + 1 Using Progression 1;
Transfinite Surface {99} = {27, 23, 22, 26};
Transfinite Surface {97} = {22, 26, 28, 24};
Transfinite Surface {103} = {25, 21, 24, 28};
Transfinite Surface {101} = {25, 21, 27, 23};
Transfinite Surface {117} = {19, 17, 23, 27};
Transfinite Surface {119} = {17, 19, 9, 7};
Transfinite Surface {113} = {22, 26, 16, 13};
Transfinite Surface {115} = {13, 16, 6, 3};
Transfinite Surface {109} = {24, 28, 20, 18};
Transfinite Surface {111} = {18, 20, 10, 8};
Transfinite Surface {105} = {21, 25, 15, 12};
Transfinite Surface {107} = {12, 15, 5, 2};
Transfinite Surface {117} = {23, 27, 19, 17};
Transfinite Surface {119} = {17, 19, 9, 7};
Transfinite Surface {139} = {19, 17, 13, 16};
Transfinite Surface {140} = {13, 16, 20, 18};
Transfinite Surface {137} = {20, 18, 12, 15};
Transfinite Surface {138} = {15, 12, 17, 19};

Recombine Surface {113, 97, 99, 101, 103, 105, 107, 117, 119, 115, 111, 109};

Recombine Surface {140, 139, 138, 137};

Transfinite Surface {130} = {8, 10, 5, 2};
Transfinite Surface {132} = {2, 5, 7, 9};
Transfinite Surface {134} = {7, 9, 6, 3};
Transfinite Surface {136} = {3, 6, 10, 8};

Recombine Surface {136, 130, 132, 134};

/* Description des volumes */

Surface Loop(141) = {22, 75, 103, 101, 99, 97};
Volume(142) = {141};
Surface Loop(143) = {70, 25, 103, 109, 105, 137};
Volume(144) = {143};
Surface Loop(145) = {60, 27, 138, 105, 101, 117};
Volume(146) = {145};
Surface Loop(147) = {50, 29, 99, 117, 113, 139};
Volume(148) = {147};
Surface Loop(149) = {97, 109, 140, 113, 45, 31};
Volume(150) = {149};
Surface Loop(151) = {80, 130, 35, 111, 137, 107};
Volume(152) = {151};
Surface Loop(153) = {111, 33, 136, 40, 140, 115};
Volume(154) = {153};
Surface Loop(155) = {134, 55, 39, 119, 139, 115};
Volume(156) = {155};
Surface Loop(157) = {132, 65, 37, 138, 119, 107};
Volume(158) = {157};

Transfinite Volume{142} = {26, 27, 25, 28, 22, 23, 21, 24};
Transfinite Volume{146} = {27, 19, 15, 25, 23, 17, 12, 21};
Transfinite Volume{158} = {19, 9, 5, 15, 17, 7, 2, 12};
Transfinite Volume{144} = {28, 25, 15, 20, 24, 21, 12, 18};
Transfinite Volume{152} = {20, 15, 5, 10, 18, 12, 2, 8};
Transfinite Volume{150} = {28, 20, 16, 26, 24, 18, 13, 22};
Transfinite Volume{154} = {20, 10, 6, 16, 18, 8, 3, 13};
Transfinite Volume{148} = {26, 16, 19, 27, 22, 13, 17, 23};
Transfinite Volume{156} = {16, 6, 9, 19, 13, 3, 7, 17};


/* Description des groupes physique */

Physical Surface(160) = {134, 136, 130, 132};
Physical Surface(161) = {-40, -45, -55, -50, -75, -80, -70, -65, -60};
Physical Surface(162) = {33, 31, 29, 39, 22, 25, 35, 37, 27};


Physical Volume(170) = {142, 144, 150, 148, 146};
Physical Volume(171) = {152, 154, 156, 158};


Physical Point(172) = {3};
Physical Point(173) = {2};
