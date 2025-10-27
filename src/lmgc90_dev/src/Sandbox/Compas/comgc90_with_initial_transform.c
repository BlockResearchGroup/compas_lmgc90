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
  struct lmgc90_rigid_body_3D * initial_bodies_array;  // Store initial state

  // first part of initialize
  lmgc90_initialize(dt, theta);

  // instead of reading from DATBOX
  // everything is hard-coded right now
  // ... the api must change to fit Compas format
  lmgc90_set_materials(1);
  lmgc90_set_tact_behavs(1);
  lmgc90_set_see_tables();
  lmgc90_set_nb_bodies(2);

  printf("=== ORIGINAL INPUT DATA ===\n");
  printf("Body 0 - Original center: [%f, %f, %f]\n", coor[0], coor[1], coor[2]);
  printf("Body 0 - Original vertices:\n");
  for(int v = 0; v < 4; v++) {
    printf("  Vertex %d: [%f, %f, %f]\n", v, vert[v*3], vert[v*3+1], vert[v*3+2]);
  }

  lmgc90_set_one_polyr(coor, faces, 4, vert, 4, false);

  // define a second polyedron... topped down, lower and fixed
  vert[11] = -1.0;
  coor[2] -=  1.e-2;
  printf("Body 1 - Original center: [%f, %f, %f]\n", coor[0], coor[1], coor[2]);
  lmgc90_set_one_polyr(coor, faces, 4, vert, 4, true);
  
  // finish the initialization part...
  lmgc90_close_before_computing();

  // *** CAPTURE INITIAL STATE AFTER LMGC90 SETUP ***
  nb_bodies = lmgc90_get_nb_bodies();
  bodies_array = malloc( nb_bodies * sizeof(struct lmgc90_rigid_body_3D) );
  initial_bodies_array = malloc( nb_bodies * sizeof(struct lmgc90_rigid_body_3D) );

  // Get initial transformations right after setup
  lmgc90_get_all_bodies(initial_bodies_array, nb_bodies);
  
  printf("\n=== INITIAL TRANSFORMATIONS (after LMGC90 setup) ===\n");
  for(i = 0; i < nb_bodies; i++) {
    printf("Body %d:\n", i);
    printf("  Inertia Center: [%f, %f, %f]\n", 
           initial_bodies_array[i].coor[0], initial_bodies_array[i].coor[1], initial_bodies_array[i].coor[2]);
    printf("  Inertia Frame (3x3 rotation matrix):\n");
    printf("    [%f, %f, %f]\n", initial_bodies_array[i].frame[0], initial_bodies_array[i].frame[1], initial_bodies_array[i].frame[2]);
    printf("    [%f, %f, %f]\n", initial_bodies_array[i].frame[3], initial_bodies_array[i].frame[4], initial_bodies_array[i].frame[5]);
    printf("    [%f, %f, %f]\n", initial_bodies_array[i].frame[6], initial_bodies_array[i].frame[7], initial_bodies_array[i].frame[8]);
    
    // 4x4 Transformation Matrix for COMPAS
    printf("  4x4 Transformation Matrix (for COMPAS):\n");
    printf("    [%f, %f, %f, %f]\n", initial_bodies_array[i].frame[0], initial_bodies_array[i].frame[1], initial_bodies_array[i].frame[2], initial_bodies_array[i].coor[0]);
    printf("    [%f, %f, %f, %f]\n", initial_bodies_array[i].frame[3], initial_bodies_array[i].frame[4], initial_bodies_array[i].frame[5], initial_bodies_array[i].coor[1]);
    printf("    [%f, %f, %f, %f]\n", initial_bodies_array[i].frame[6], initial_bodies_array[i].frame[7], initial_bodies_array[i].frame[8], initial_bodies_array[i].coor[2]);
    printf("    [%f, %f, %f, %f]\n", 0.0, 0.0, 0.0, 1.0);
    printf("\n");
  }

  // now computing the time loop
  for( i_step=0; i_step<nb_steps; i_step++) {

      lmgc90_compute_one_step();

      nb_inters = lmgc90_get_nb_inters();

      inters_array = malloc( nb_inters * sizeof(struct lmgc90_inter_meca_3D) );
      lmgc90_get_all_inters(inters_array, nb_inters);

      // check interaction accessor
      if(i_step % 20 == 0) {  // Print every 20 steps to reduce output
        printf( "Step %d - interactions found : %d\n", i_step, nb_inters );
      }

      lmgc90_get_all_bodies(bodies_array, nb_bodies);

      // Print transformation at specific steps
      if(i_step == 0 || i_step == 50 || i_step == 99) {
        printf("\n=== STEP %d TRANSFORMATIONS ===\n", i_step);
        for( i=0; i<nb_bodies; i++ ) {
          printf( "Body %d -> Center: (%f, %f, %f)\n", i, bodies_array[i].coor[0], bodies_array[i].coor[1], bodies_array[i].coor[2] );
          printf( "   Frame: [%f, %f, %f]\n", bodies_array[i].frame[0], bodies_array[i].frame[1], bodies_array[i].frame[2] );
          printf( "          [%f, %f, %f]\n", bodies_array[i].frame[3], bodies_array[i].frame[4], bodies_array[i].frame[5] );
          printf( "          [%f, %f, %f]\n", bodies_array[i].frame[6], bodies_array[i].frame[7], bodies_array[i].frame[8] );
          
          // Compute relative transformation from initial state
          printf( "   4x4 Transformation Matrix:\n");
          printf( "     [%f, %f, %f, %f]\n", bodies_array[i].frame[0], bodies_array[i].frame[1], bodies_array[i].frame[2], bodies_array[i].coor[0]);
          printf( "     [%f, %f, %f, %f]\n", bodies_array[i].frame[3], bodies_array[i].frame[4], bodies_array[i].frame[5], bodies_array[i].coor[1]);
          printf( "     [%f, %f, %f, %f]\n", bodies_array[i].frame[6], bodies_array[i].frame[7], bodies_array[i].frame[8], bodies_array[i].coor[2]);
          printf( "     [%f, %f, %f, %f]\n", 0.0, 0.0, 0.0, 1.0);
        }
      }

      free(inters_array);

  } // end loop i_step

  lmgc90_finalize();

  free(bodies_array);
  free(initial_bodies_array);

  return 0;
}
