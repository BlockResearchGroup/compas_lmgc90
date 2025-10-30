// import de la geometrie dessinee sous Salome
Merge "2_briques.brep";

// ajout d'un groupe pour placer des contacteurs sur la brique maillee
Physical Surface(10) = {17};

// on repere chaque entite volumique de la structure en lui affectant un nom 
Physical Volume("dalle") = {1};
Physical Volume("brique rigide") = {2};
Physical Volume("brique deformable") = {3};
Physical Point("coin") = {17};
