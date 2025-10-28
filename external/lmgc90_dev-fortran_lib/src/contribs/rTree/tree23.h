
#ifndef TREE23_H
#define TREE23_H

#include "RTree.h"


typedef RTree<int, double, 2> RT2;
typedef RTree<int, double, 3> RT3;

bool fill_list(int* id, void* ilist);

/** \brief Reset the tree2 research tree */
extern "C" void reset_tree2(void);
/** \brief Reset the tree3 research tree */
extern "C" void reset_tree3(void);

/** \brief add a rectangle to tree2
 *@param[in] bmin  (double[2]) : bottom left coordinates
 *@param[in] bmax  (double[2]) : top right coordinates
 *@param[in] tacid (int)       : id of the rectangle to add
 */
extern "C" void add_to_tree2(double * bmin, double * bmax, int tacid);

/** \brief add a box to tree3
 *@param[in] bmin (double[3]) : bottom rear left coordinates
 *@param[in] bmax (double[3]) : top front right coordinates
 *@param[in] tacid (int)      : id of the box to add
 */
extern "C" void add_to_tree3(double * bmin, double * bmax, int tacid);

/** \brief get the list of rectangles in tree2 intersecting with input rectangle
 *@param[in]  bmin (double[2]) : bottom left coordinates
 *@param[in]  bmax (double[2]) : top right coordinates
 *@param[out] list (int*)      : list of intesecting rectangles
 * list array must be allocated of the size the number of rectangles in
 * tree2 + 1, first term is the number of intersecting rectangles and then
 * the list stored in a contingous manner.
 */
extern "C" void search_in_tree2(double * bmin, double * bmax, int* list);

/** \brief get the list of boxes in tree3 intersecting with input boxe 
 *@param[in]  bmin (double[2]) : bottom rear left coordinates
 *@param[in]  bmax (double[2]) : top front right coordinates
 *@param[out] list (int*)      : list of intesecting boxes
 * list array must be allocated of the size the number of boxes in
 * tree3 + 1, first term is the number of intersecting boxes and then
 * the list stored in a contingous manner.
 */
extern "C" void search_in_tree3(double * bmin, double * bmax, int* list);

#endif //TREE23_H
