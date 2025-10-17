#include "stdlib.h"
#include "stdio.h"

#include "wrap_lmgc90_compas.h"

int main(void) {

  int i;
  int i_step, nb_steps=100;
  int nb_inters;
  struct lmgc90_inter_meca_3D * inters_array;
  int nb_bodies;
  struct lmgc90_rigid_body_3D * bodies_array;

  lmgc90_initialize();

  nb_bodies = lmgc90_get_nb_bodies();
  bodies_array = malloc( nb_bodies * sizeof(struct lmgc90_rigid_body_3D) );

  for( i_step=0; i_step<nb_steps; i_step++) {

      lmgc90_compute_one_step();

      nb_inters = lmgc90_get_nb_inters();

      inters_array = malloc( nb_inters * sizeof(struct lmgc90_inter_meca_3D) );
      lmgc90_get_all_inters(inters_array, nb_inters);

      // check interaction accessor
      // printf( "interaction found : %d\n", nb_inters );
      // for( i=0; i<nb_inters; i++ ) {
      //   printf( "%d -> %s %d/%d (%s)\n", i, inters_array[i].cdan, inters_array[i].icdbdy, inters_array[i].ianbdy, inters_array[i].status );
      // }

      lmgc90_get_all_bodies(bodies_array, nb_bodies);

      // printf( "bodies to get: %d\n", nb_bodies);
      // for( i=0; i<nb_bodies; i++ ) {
      //   printf( "%d -> (%f, %f, %f)\n", i, bodies_array[i].coor[0], bodies_array[i].coor[1], bodies_array[i].coor[2] );
      // }
      free(inters_array);

  } // end loop i_step

  lmgc90_finalize();

  free(bodies_array);

};
