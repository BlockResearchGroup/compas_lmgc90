/* fichier d'entête C++ associe au fichier wrap_mesh2D.f90:
   acces en C++ aux fonction du modules wrap_mesh2D */

#ifndef wrap_mesh2D
#define wrap_mesh2D

   /* procedure qui calcule les indices correspondant a un noeud, pour un
      maillage en Q4 */
   /**
    * @fn void mesh2D_GetIndicesMeshQ4(int n, int *i, int *j) 
    * @brief this function gives the couple (i, j) of indices coresponding to a given node n 
    * @warning 
    * python call: [i, j]=mesh2D_GetIndicesMeshQ4(n)
    * @param[in] n (int): the given node 
    * @param[out] i (int *): index in the u direction
    * @param[out] j (int *): index in the v direction
    */
   extern "C" void mesh2D_GetIndicesMeshQ4(int n, int *ires, int *ires2);

   /* procedure qui calcule les tailles des vecteurs pour stocker un nouveau
      maillage en Q4 */
   /**
    * @fn void mesh2D_SizeMeshQ4(int nb_elem_x, int nb_elem_y, int *size_coor, 
    *   int *size_nb_node_per_ele, int *size_conn) 
    * @brief this function computes the sizes of vectors used to store a mesh 
    *  made of Q4 in the following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2   4   6\n
    *    *---*---*\n
    *    | 1 | 2 |\n
    *    *---*---*\n
    *    1   3   5\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6]
    *    - nb_node_per_ele = [4, 4]
    *    - conn = [1, 3, 4, 2, 3, 5, 6, 4] 
    * @warning 
    * python call: [size_coor, size_nb_node_per_ele, size_conn]=mesh2D_SizeMeshQ4(nb_elem_x, nb_elem_y)
    * @param[in] nb_elem_x (int): number of elements in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements in the vertical direction
    * @param[out] size_coor (int *): size of coor 
    * @param[out] size_nb_node_per_ele (int *): size of nb_node_per_ele 
    * @param[out] size_conn (int *): size of conn
    */
   extern "C" void mesh2D_SizeMeshQ4(int nb_elem_x, int nb_elem_y, 
      int *ires, int *ires2, int *ires3);

   /* procedure qui calcule les tailles des vecteurs pour stocker un nouveau
      maillage en Q4, splitte en 2 T3 */
   /**
    * @fn void mesh2D_SizeMesh2T3(int nb_elem_x, int nb_elem_y, int *size_coor, 
    *   int *size_nb_node_per_ele, int *size_conn) 
    * @brief this function computes the sizes of vectors used to store a mesh 
    *  made of T3 --- obtained by splitting a Q4 in two T3 --- in the following
    *  generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2    4\n
    *    *----*\n
    *    | 1 /|\n
    *    |  / |\n
    *    | /  |\n
    *    |/ 2 |\n
    *    *----*\n
    *    1    3\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4]
    *    - nb_node_per_ele = [3, 3]
    *    - conn = [1, 3, 4, 2, 1, 4] 
    * @warning 
    * python call: [size_coor, size_nb_node_per_ele, size_conn]=mesh2D_SizeMesh2T3(nb_elem_x, nb_elem_y)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[out] size_coor (int *): size of coor 
    * @param[out] size_nb_node_per_ele (int *): size of nb_node_per_ele 
    * @param[out] size_conn (int *): size of conn
    */
   extern "C" void mesh2D_SizeMesh2T3(int nb_elem_x, int nb_elem_y, 
      int *ires, int *ires2, int *ires3);

   /* procedure qui calcule les tailles des vecteurs pour stocker un nouveau
      maillage en Q4, splitte en 4 T3 */
   /**
    * @fn void mesh2D_SizeMesh4T3(int nb_elem_x, int nb_elem_y, int *size_coor, 
    *   int *size_nb_node_per_ele, int *size_conn) 
    * @brief this function computes the sizes of vectors used to store a mesh 
    *  made of T3 --- obtained by splitting a Q4 in four T3 --- in the 
    * following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2     4\n
    *    *-----*\n
    *    |\ 4 /|\n
    *    | \ / |\n
    *    |1 5 3|\n
    *    | / \ |\n
    *    |/ 2 \|\n
    *    *-----*\n
    *    1     3\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5]
    *    - nb_node_per_ele = [3, 3, 3, 3]
    *    - conn = [1, 5, 2, 1, 3, 5, 3, 4, 5, 2, 5, 4] 
    * @warning 
    * python call: [size_coor, size_nb_node_per_ele, size_conn]=mesh2D_SizeMesh4T3(nb_elem_x, nb_elem_y)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[out] size_coor (int *): size of coor 
    * @param[out] size_nb_node_per_ele (int *): size of nb_node_per_ele 
    * @param[out] size_conn (int *): size of conn
    */
   extern "C" void mesh2D_SizeMesh4T3(int nb_elem_x, int nb_elem_y, 
      int *ires, int *ires2, int *ires3);

   /* procedure qui calcule les tailles des vecteurs pour stocker un nouveau
      maillage en Q8 */
   /**
    * @fn void mesh2D_SizeMeshQ8(int nb_elem_x, int nb_elem_y, int *size_coor, 
    *   int *size_nb_node_per_ele, int *size_conn) 
    * @brief this function computes the sizes of vectors used to store a mesh 
    *  made of Q8
    * following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    3   7   3\n
    *    *---*---*\n
    *    |       |\n
    *    |       |\n
    *  8 *   1   * 6\n
    *    |       |\n
    *    |       |\n
    *    *---*---*\n
    *    1   5   2\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8]
    *    - nb_node_per_ele = [8]
    *    - conn = [1, 2, 3, 4, 5, 6, 7, 8] 
    * @warning 
    * python call: [size_coor, size_nb_node_per_ele, size_conn]=mesh2D_SizeMesh4T3(nb_elem_x, nb_elem_y)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[out] size_coor (int *): size of coor 
    * @param[out] size_nb_node_per_ele (int *): size of nb_node_per_ele 
    * @param[out] size_conn (int *): size of conn
    */
   extern "C" void mesh2D_SizeMeshQ8(int nb_elem_x, int nb_elem_y, 
      int *ires, int *ires2, int *ires3);

   /* procedure qui calcule un nouveau maillage en Q4 */
   /**
    * @fn void mesh2D_MeshQ4(double x0, double y0, double lx, double ly,
    *        int nb_elem_x, int nb_elem_y, double *coor, int size_coor,
    *        int *nb_node_per_ele, int size_nb_node_per_ele, int *conn,
    *        int size_conn)
    * @brief this function computes and returns a mesh made of Q4 in the 
    *   following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2   4   6\n
    *    *---*---*\n
    *    | 1 | 2 |\n
    *    *---*---*\n
    *    1   3   5\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6]
    *    - nb_node_per_ele = [4, 4]
    *    - conn = [1, 3, 4, 2, 3, 5, 6, 4] 
    * @warning 
    * python call: [coor, nb_node_per_ele, conn]=mesh2D_MeshQ4(x0, y0, lx, ly, 
    *    nb_elem_x, nb_elem_y, size_coor, size_nb_node_per_ele, size_conn)
    * @param[in] x0 (double): abscissa of the lower left corner of the rectangle
    * @param[in] y0 (double): ordinate of the lower left corner of the rectangle
    * @param[in] lx (double): length of the mesh, following the axis (Ox)
    * @param[in] ly (double): length of the mesh, following the axis (Oy)
    * @param[in] nb_elem_x (int): number of elements in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements in the vertical direction
    * @param[in] size_coor (int): size of coor 
    * @param[in] size_nb_node_per_ele (int): size of nb_node_per_ele 
    * @param[in] size_conn (int): size of conn
    * @param[out] coor (double *): vector of coordinates of the nodes 
    *    [x1, y1, x2, y2, ...]
    * @param[out] nb_node_per_ele (int *): nb_node_per_ele(i) contains the 
    *    number of nodes for element i, i in [1, number of elements]
    * @param[out] conn (int *): vector storing the connectivity of the elements
    *    [n11, n12n n13, n21, n22, n23, n24, ...]
    */
   extern "C" void mesh2D_MeshQ4(double x0, double y0, double lx, double ly, 
      int nb_elem_x, int nb_elem_y, double * rvector_out, int rlength_out, 
      int * ivector_out, int ilength_out, int * ivector_out2, int ilength_out2);

   /* procedure qui calcule un nouveau maillage en T3, obtenus en coupant en 
    * deux des Q4 */
   /**
    * @fn void mesh2D_Mesh2T3(double x0, double y0, double lx, double ly,
    *        int nb_elem_x, int nb_elem_y, double *coor, int size_coor,
    *        int *nb_node_per_ele, int size_nb_node_per_ele, int *conn,
    *        int size_conn)
    * @brief this function computes an returns a mesh made of T3 --- obtained 
    *   by splitting a Q4 in two T3 --- in the following
    *   generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2    4\n
    *    *----*\n
    *    | 1 /|\n
    *    |  / |\n
    *    | /  |\n
    *    |/ 2 |\n
    *    *----*\n
    *    1    3\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4]
    *    - nb_node_per_ele = [3, 3]
    *    - conn = [1, 3, 4, 2, 1, 4] 
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * @warning 
    * python call: [coor, nb_node_per_ele, conn]=mesh2D_Mesh2T3(x0, y0, lx, ly, 
    *    nb_elem_x, nb_elem_y, size_coor, size_nb_node_per_ele, size_conn)
    * @param[in] x0 (double): abscissa of the lower left corner of the rectangle
    * @param[in] y0 (double): ordinate of the lower left corner of the rectangle
    * @param[in] lx (double): length of the mesh, following the axis (Ox)
    * @param[in] ly (double): length of the mesh, following the axis (Oy)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[in] size_coor (int): size of coor 
    * @param[in] size_nb_node_per_ele (int): size of nb_node_per_ele 
    * @param[in] size_conn (int): size of conn
    * @param[out] coor (double *): vector of coordinates of the nodes 
    *    [x1, y1, x2, y2, ...]
    * @param[out] nb_node_per_ele (int *): nb_node_per_ele(i) contains the 
    *    number of nodes for element i, i in [1, number of elements]
    * @param[out] conn (int *): vector storing the connectivity of the elements
    *    [n11, n12n n13, n21, n22, n23, n24, ...]
    */
   extern "C" void mesh2D_Mesh2T3(double x0, double y0, double lx, double ly, 
      int nb_elem_x, int nb_elem_y, double * rvector_out, int rlength_out, 
      int * ivector_out, int ilength_out, int *ivector_out2, int ilength_out2);

   /* procedure qui calcule un nouveau maillage en T3, obtenus en coupant en 
    * quatre des Q4 */
   /**
    * @fn void mesh2D_Mesh4T3(double x0, double y0, double lx, double ly,
    *        int nb_elem_x, int nb_elem_y, double *coor, int size_coor,
    *        int *nb_node_per_ele, int size_nb_node_per_ele, int *conn,
    *        int size_conn)
    * @brief this function computes and return a mesh  made of T3 --- obtained 
    *   by splitting a Q4 in four T3 --- in the following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    2     4\n
    *    *-----*\n
    *    |\ 4 /|\n
    *    | \ / |\n
    *    |1 5 3|\n
    *    | / \ |\n
    *    |/ 2 \|\n
    *    *-----*\n
    *    1     3\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5]
    *    - nb_node_per_ele = [3, 3, 3, 3]
    *    - conn = [1, 5, 2, 1, 3, 5, 3, 4, 5, 2, 5, 4] 
    * @warning 
    * python call: [coor, nb_node_per_ele, conn]=mesh2D_Mesh4T3(x0, y0, lx, ly, 
    *    nb_elem_x, nb_elem_y, size_coor, size_nb_node_per_ele, size_conn)
    * @param[in] x0 (double): abscissa of the lower left corner of the rectangle
    * @param[in] y0 (double): ordinate of the lower left corner of the rectangle
    * @param[in] lx (double): length of the mesh, following the axis (Ox)
    * @param[in] ly (double): length of the mesh, following the axis (Oy)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[in] size_coor (int): size of coor 
    * @param[in] size_nb_node_per_ele (int): size of nb_node_per_ele 
    * @param[in] size_conn (int): size of conn
    * @param[out] coor (double *): vector of coordinates of the nodes 
    *    [x1, y1, x2, y2, ...]
    * @param[out] nb_node_per_ele (int *): nb_node_per_ele(i) contains the 
    *    number of nodes for element i, i in [1, number of elements]
    * @param[out] conn (int *): vector storing the connectivity of the elements
    *    [n11, n12n n13, n21, n22, n23, n24, ...]
    */
   extern "C" void mesh2D_Mesh4T3(double x0, double y0, double lx, double ly, 
      int nb_elem_x, int nb_elem_y, double * rvector_out, int rlength_out, 
      int * ivector_out, int ilength_out, int * ivector_out2, int ilength_out2);

   /* procedure qui calcule un nouveau maillage en Q8 */
   /**
    * @fn void mesh2D_MeshQ8(double x0, double y0, double lx, double ly,
    *        int nb_elem_x, int nb_elem_y, double *coor, int size_coor,
    *        int *nb_node_per_ele, int size_nb_node_per_ele, int *conn,
    *        int size_conn)
    * @brief this function computes and returns a mesh made of Q8 in the 
    *   following generic format:\n
    *    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
    *    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
    *         element i, i in [1, number of elements]
    *    - conn: vector storing the connectivity of the elements
    *         [n11, n12n n13, n21, n22, n23, n24, ...]
    * consider the following little mesh:\n
    *    3   7   3\n
    *    *---*---*\n
    *    |       |\n
    *    |       |\n
    *  8 *   1   * 6\n
    *    |       |\n
    *    |       |\n
    *    *---*---*\n
    *    1   5   2\n
    * the vectors for this mesh read:
    *    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8]
    *    - nb_node_per_ele = [8]
    *    - conn = [1, 2, 3, 4, 5, 6, 7, 8] 
    * @warning 
    * python call: [coor, nb_node_per_ele, conn]=mesh2D_MeshQ8(x0, y0, lx, ly, 
    *    nb_elem_x, nb_elem_y, size_coor, size_nb_node_per_ele, size_conn)
    * @param[in] x0 (double): abscissa of the lower left corner of the rectangle
    * @param[in] y0 (double): ordinate of the lower left corner of the rectangle
    * @param[in] lx (double): length of the mesh, following the axis (Ox)
    * @param[in] ly (double): length of the mesh, following the axis (Oy)
    * @param[in] nb_elem_x (int): number of elements Q4 in the horizontal 
    *    direction
    * @param[in] nb_elem_y (int): number of elements Q4 in the vertical 
    *    direction
    * @param[in] size_coor (int): size of coor 
    * @param[in] size_nb_node_per_ele (int): size of nb_node_per_ele 
    * @param[in] size_conn (int): size of conn
    * @param[out] coor (double *): vector of coordinates of the nodes 
    *    [x1, y1, x2, y2, ...]
    * @param[out] nb_node_per_ele (int *): nb_node_per_ele(i) contains the 
    *    number of nodes for element i, i in [1, number of elements]
    * @param[out] conn (int *): vector storing the connectivity of the elements
    *    [n11, n12n n13, n21, n22, n23, n24, ...]
    */
   extern "C" void mesh2D_MeshQ8(double x0, double y0, double lx, double ly, 
      int nb_elem_x, int nb_elem_y, double * rvector_out, int rlength_out, 
      int * ivector_out, int ilength_out, int * ivector_out2, int ilength_out2);

#endif
