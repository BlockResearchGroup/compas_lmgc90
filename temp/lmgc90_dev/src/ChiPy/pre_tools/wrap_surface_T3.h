/* fichier d'entête C++ associe au fichier wrap_surface_T3.f90:
   acces en C++ aux fonction du modules wrap_surface_T3 */

#ifndef wrap_surface_T3
#define wrap_surface_T3

   /* procedure qui calcule le volume, les coordonnees du centre d'inertie et la matrice d'inertie d'un objet decrit par une 
      triangulation de son enveloppe */
   /**
    * @fn void surface_T3_compute_volume_inertia(double * rvector_in, int rlength_in, int *ivvector_inout, int ilength_inout, double * rvector_out, int rlength_out, double *vector_out2, int length_out2, double *res);
    * @brief Computes the volume of an object described by a triangulated surface
    * @warning 
    *    1) we assume size_coor is three times the number of nodes and size_connec is three times the number of elements
    *    python call: x_G, I, vol=surface_T3_compute_volume_inertia(coor, connec, 3, 9)
    * @param[in] coor_size (int): size of coor
    * @param[in] coor (double *): node coordinates
    * @param[in] connec_size (int): size of connec
    * @param[out] vol (double *): computed volume
    * @param[out] x_G (double *): mass center coordinates
    * @param[in] x_G_size (int): size of x_G
    * @param[out] I (double *): inertia matrix, stored a a vector
    * @param[in] I_size (int): size of I
    */
   extern "C" void surface_T3_compute_volume_inertia(double * rvector_in, int rlength_in, int *ivector_inout, int ilength_inout, double * rvector_out, int rlength_out, double * rvector_out2, int rlength_out2, double *res);

   /* procedure qui attribue un numero d'entite aux triangles, a partir d'une recherche de composantes connexes */
   /**
    * @fn void surface_T3_identify_entities(int nbnode, int max_adj_ele_2_node, int * ivector_in, int ilength_in, int *ivector_out, int ilength_out)
    * @brief Attributes an entity number to triangles, by computing connected components
    * @warning 
    *    1) we assume size_connec is three times the number of elements and size_ele2entity is the number of elements
    *    python call: ele2entity=surface_T3_identify_entities(nbnode, max_adj_ele_2_node, connec, nbele)
    * @param[in] nbnode (int): the number of nodes
    * @param[in] max_adj_ele_2_node (int): the maximal number of adjacent elements per node
    * @param[in] connec (int *): connecivity of elements
    * @param[in] connec_size (int): size of connec
    * @param[out] ele2entity (double *): entity number for each element
    * @param[in] ele2entity_size (int): size of ele2entity
    */
   extern "C" void surface_T3_identify_entities(int nbnode, int max_adj_ele_2_node, int * ivector_in, int ilength_in, int * ivector_out, int ilength_out);

#endif
