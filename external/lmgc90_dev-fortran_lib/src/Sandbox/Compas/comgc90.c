#include "stdbool.h"
#include "stdlib.h"
#include "stdio.h"

#include "wrap_lmgc90_compas.h"

int main(void) {

  // to define a pyramid
  //
  // define the faces (as triangles) of the polyedron
  int faces[12] = {1, 2, 3, 1, 2, 4, 2, 3, 4, 3, 1, 4};

  // define the vertices of the polyedron
  double vert[12] = {0.0, 0.0, 0.0 ,1.0, 0.0, 0.0 ,0.0, 1.0, 0.0 ,0.3, 0.3, 1.0};

  // define the coordinates of the center of inertia
  double coor[3]  = {0.0, 0.0, 1.0};

  int i;
  int i_step, nb_steps=100;

  double dt = 1e-3;
  double theta = 0.5;

  int nb_inters;
  struct lmgc90_inter_meca_3D * inters_array;
  int nb_bodies;
  struct lmgc90_rigid_body_3D * bodies_array;

  // first part of initialize
  lmgc90_initialize(dt, theta);

  // instead of reading from DATBOX
  // everything is hard-coded right now
  // ... the api must change to fit Compas format
  lmgc90_set_materials(1);
  lmgc90_set_tact_behavs(1);
  lmgc90_set_see_tables();
  lmgc90_set_nb_bodies(2);

  lmgc90_set_one_polyr(coor, faces, 4, vert, 4, false);

  // define a second polyedron... topped down, lower and fixed
  vert[11] = -1.0;
  coor[2] -=  1.e-2;
  lmgc90_set_one_polyr(coor, faces, 4, vert, 4, true);
  
  // finish the initialization part...
  lmgc90_close_before_computing();


  // now computing the time loop
  nb_bodies = lmgc90_get_nb_bodies();
  bodies_array = malloc( nb_bodies * sizeof(struct lmgc90_rigid_body_3D) );

  for( i_step=0; i_step<nb_steps; i_step++) {

      lmgc90_compute_one_step();

      nb_inters = lmgc90_get_nb_inters();

      inters_array = malloc( nb_inters * sizeof(struct lmgc90_inter_meca_3D) );
      lmgc90_get_all_inters(inters_array, nb_inters);

      // check interaction accessor
      printf( "interaction found : %d\n", nb_inters );
      for( i=0; i<nb_inters; i++ ) {
        printf( "%d -> %s %d/%d (%s)\n", i, inters_array[i].cdan, inters_array[i].icdbdy, inters_array[i].ianbdy, inters_array[i].status );
      }

      lmgc90_get_all_bodies(bodies_array, nb_bodies);

      printf( "bodies to get: %d\n", nb_bodies);
      for( i=0; i<nb_bodies; i++ ) {
        printf( "%d -> (%f, %f, %f)\n", i, bodies_array[i].coor[0], bodies_array[i].coor[1], bodies_array[i].coor[2] );
      }

      free(inters_array);

  } // end loop i_step

  lmgc90_finalize();

  free(bodies_array);

};
