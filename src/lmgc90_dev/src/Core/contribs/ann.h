
#include "ANN/ANN.h"

/// number of kd trees stored
int nb_kd = 0;

/// list of kd trees stored
ANNkd_tree ** all_kd = NULL;

/// number of bd trees stored
int nb_bd = 0;

/// list of bd trees stored
ANNbd_tree ** all_bd = NULL;

/**
 * @brief Set the number of KD trees to store and allocate corresponding array
 * param[in] nb (int) : number of kd trees
 */
extern "C" void annSetNbKdTrees(int nb);

/**
 * @brief Add a new KD tree
 * param[in] i_tree (int) : index in list of KD tree to add
 * param[in] nodes (double **) : an array holding pointer on coordinates of the nodes to put in the tree
 * param[in] nb_nodes (int) : number of nodes to add in the tree
 * param[in] space_dim (int) : number of coordinates of each node
 */
extern "C" void annAddKdTree(int i_tree, double ** nodes, int nb_nodes, int space_dim);

/**
 * @brief Search the nearest nodes of a tree of a point
 * param[in] i_tree (int) : index in list of KD tree to test against
 * param[in] test (double *) : the point to test agains the nodes of the tree
 * param[in] nb_near (int) : number of nearest points looked for
 * param[in,out] nearests (int *) : list of index of nearest points (must be allocated beforehand to the size nb_near)
 * param[in,out] dists (double *) : square distance between test point and nearest points (must be allocated beforehand to the size nb_near)
 */
extern "C" void annSearchNearestKd(int i_tree, double * test, int nb_near, int * nearests, double * dists);

/**
 * @brief Search the nodes inside a boundary sphere/circle
 * param[in] i_tree (int) : index in list of KD tree to test against
 * param[in] tests (double *) : the center of each boundary spheres/circles
 * param[in] nb_tests (int) : number of spheres/circles to test
 * param[in] radius (double) : the radius of the boundary sphere/circle
 * param[out] in (int**) : list of index of nodes inside the boundary (allocation within the function)
 * param[out] dists (double**) : distance between center of boundaries and each point inside it
 * param[out] nb_in (int*) : max number of nodes inside the boundaries
 */
extern "C" void annRadiiSearchKd(int i_tree, double * tests, int nb_tests, double radius, int ** in, double ** dists, int * nb_in);

/**
 * @brief Set the number of BD trees to store and allocate corresponding array
 * param[in] nb (int) : number of kd trees
 */
extern "C" void annSetNbBdTrees(int nb);

/**
 * @brief Add a new BD tree
 * param[in] i_tree (int) : index in list of BD tree to add
 * param[in] nodes (double **) : an array holding pointer on coordinates of the nodes to put in the tree
 * param[in] nb_nodes (int) : number of nodes to add in the tree
 * param[in] space_dim (int) : number of coordinates of each node
 */
extern "C" void annAddBdTree(int i_tree, double ** nodes, int nb_nodes, int space_dim);

/**
 * @brief Search the nearest nodes of a tree of a point
 * param[in] i_tree (int) : index in list of BD tree to test against
 * param[in] test (double *) : the point to test agains the nodes of the tree
 * param[in] nb_near (int) : number of nearest points looked for
 * param[in,out] nearests (int *) : list of index of nearest points (must be allocated beforehand to the size nb_near)
 * param[in,out] dists (double *) : square distance between test point and nearest points (must be allocated beforehand to the size nb_near)
 */
extern "C" void annSearchNearestBd(int i_tree, double * test, int nb_near, int * nearests, double * dists);

/**
 * @brief Search the nodes inside a list of boundary spheres/circles
 * param[in] i_tree (int) : index in list of BD tree to test against
 * param[in] tests (double *) : the center of the boundary spheres/circles
 * param[in] nb_tests (int) : number of boundary spheres/circles to test
 * param[in] radius (double) : the radius of the boundary sphere/circle
 * param[out] in (int**) : list of index of nodes inside the boundary (allocation within the function)
 * param[out] dists (double**) : distance between center of boundaries and each point inside it
 * param[out] nb_in (int*) : max number of nodes inside the boundaries
 */
extern "C" void annRadiiSearchBd(int i_tree, double * tests, int nb_tests, double radius, int ** in, double ** dists, int * nb_in);

/**
 * @brief Free all allocated memory and reset global data
 */
extern "C" void annFinalize(void);
