#include "stdlib.h"
#include "stdio.h"

#include "wrap_lmgc90_compas.h"

int main(void) {

  // Vertices
  // Faces
  

  int nb_inters, i_inter;
  struct lmgc90_inter_meca_3D * inter_array;

  lmgc90_initialize();
  lmgc90_compute();

  nb_inters = lmgc90_get_nb_inters();

  inter_array = malloc( nb_inters * sizeof(struct lmgc90_inter_meca_3D) );
  lmgc90_get_all_inters(inter_array, nb_inters);

  printf( "interaction found : %d\n", nb_inters );
  for( i_inter=0; i_inter<nb_inters; i_inter++ ) {
    printf( "%d -> %s %d/%d (%s)\n", i_inter, inter_array[i_inter].cdan, inter_array[i_inter].icdbdy, inter_array[i_inter].ianbdy, inter_array[i_inter].status );
  }

  lmgc90_finalize();

  free(inter_array);

};
