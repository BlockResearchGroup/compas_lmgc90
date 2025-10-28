#include "stdbool.h"
#include "stdlib.h"
#include "stdio.h"
#include "wrap_lmgc90_compas.h"

// Helper function to extract initial transformation after LMGC90 setup
void get_initial_transformations(void) {
    int nb_bodies = lmgc90_get_nb_bodies();
    struct lmgc90_rigid_body_3D * bodies_array = malloc(nb_bodies * sizeof(struct lmgc90_rigid_body_3D));
    
    // Get initial state right after initialization
    lmgc90_get_all_bodies(bodies_array, nb_bodies);
    
    printf("=== INITIAL TRANSFORMATIONS (after LMGC90 setup) ===\n");
    for(int i = 0; i < nb_bodies; i++) {
        printf("Body %d:\n", i);
        printf("  Center: [%f, %f, %f]\n", 
               bodies_array[i].coor[0], bodies_array[i].coor[1], bodies_array[i].coor[2]);
        printf("  Frame (3x3 rotation matrix):\n");
        printf("    [%f, %f, %f]\n", bodies_array[i].frame[0], bodies_array[i].frame[1], bodies_array[i].frame[2]);
        printf("    [%f, %f, %f]\n", bodies_array[i].frame[3], bodies_array[i].frame[4], bodies_array[i].frame[5]);
        printf("    [%f, %f, %f]\n", bodies_array[i].frame[6], bodies_array[i].frame[7], bodies_array[i].frame[8]);
        
        // Compute 4x4 transformation matrix
        printf("  4x4 Transformation Matrix:\n");
        printf("    [%f, %f, %f, %f]\n", bodies_array[i].frame[0], bodies_array[i].frame[1], bodies_array[i].frame[2], bodies_array[i].coor[0]);
        printf("    [%f, %f, %f, %f]\n", bodies_array[i].frame[3], bodies_array[i].frame[4], bodies_array[i].frame[5], bodies_array[i].coor[1]);
        printf("    [%f, %f, %f, %f]\n", bodies_array[i].frame[6], bodies_array[i].frame[7], bodies_array[i].frame[8], bodies_array[i].coor[2]);
        printf("    [%f, %f, %f, %f]\n", 0.0, 0.0, 0.0, 1.0);
        printf("\n");
    }
    
    free(bodies_array);
}

// Helper function to compute transformation between two states
void compute_transformation_delta(struct lmgc90_rigid_body_3D* initial_state, 
                                 struct lmgc90_rigid_body_3D* current_state,
                                 int body_id) {
    printf("=== TRANSFORMATION DELTA for Body %d ===\n", body_id);
    
    // Translation delta
    double delta_t[3] = {
        current_state->coor[0] - initial_state->coor[0],
        current_state->coor[1] - initial_state->coor[1], 
        current_state->coor[2] - initial_state->coor[2]
    };
    
    printf("Translation delta: [%f, %f, %f]\n", delta_t[0], delta_t[1], delta_t[2]);
    
    // For rotation delta, you'd need to compute R_current * R_initial^T
    // This gives the relative rotation from initial to current state
    
    printf("Current frame:\n");
    printf("  [%f, %f, %f]\n", current_state->frame[0], current_state->frame[1], current_state->frame[2]);
    printf("  [%f, %f, %f]\n", current_state->frame[3], current_state->frame[4], current_state->frame[5]);
    printf("  [%f, %f, %f]\n", current_state->frame[6], current_state->frame[7], current_state->frame[8]);
}
