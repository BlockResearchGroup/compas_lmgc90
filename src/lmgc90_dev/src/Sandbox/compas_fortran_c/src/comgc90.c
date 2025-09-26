#include "stdlib.h"
#include "stdio.h"

#include "wrap_lmgc90_compas.h"

int main(void) {

  // Vertices
  // Faces
  

  int nb_inters, i_inter;
  struct lmgc90_inter_meca_3D * inter_array;

  // Set number of time steps (default is 100)
  lmgc90_set_nb_steps(50);  // Change this to desired number of iterations
  
  lmgc90_initialize();
  lmgc90_compute();

  nb_inters = lmgc90_get_nb_inters();

  inter_array = malloc( nb_inters * sizeof(struct lmgc90_inter_meca_3D) );
  lmgc90_get_all_inters(inter_array, nb_inters); // Retrieve all active contact interactions




  // These are all the contacts that exist at the final time step
  printf( "____________________________  interaction found : %d\n", nb_inters );
  for( i_inter=0; i_inter<nb_inters; i_inter++ ) {
    // inter_array holds nb_inters mechanical contact interactions (3D) filled by lmgc90_get_all_inters().
   inter_array[i_inter].
    printf( "comgc90.c %d -> %s %d/%d (%s)\n", 
      i_inter, 
      inter_array[i_inter].cdan,  // - cdan: interaction family/name (5-char string, e.g. 'PRPRx')
      inter_array[i_inter].icdbdy, // neighbor 1 - icdbdy / ianbdy: candidate / antagonist body IDs
      inter_array[i_inter].ianbdy, // neighbor 2
      "temp"// inter_array[i_inter].status // - status: contact status (5-char code)
      
    );
  }




  lmgc90_finalize();

  free(inter_array);

};
