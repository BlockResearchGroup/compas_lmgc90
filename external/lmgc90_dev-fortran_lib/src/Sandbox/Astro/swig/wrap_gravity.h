
#ifndef wrap_gravity_h
#define wrap_gravity_h

 /**
  * \fn void self_gravity_ComputeFext2D(double* pos, int dim, int nb_part, double* masses, int mass_size,
                                        double theta, double dt, double* vector_out, int length);
  * \brief compute the gravitation force term to add to Fext in lmgc90
  * must explain about the semi-explicit thing
  * \param[in]  pos(double[dim,nb_part]) : position in space of particles
  * \param[in]  dim : dimension of a position vector (i.e. 3)
  * \param[in]  nb_part : number of particles in pos array
  * \param[in]  masse(double[mass_size]) : mass of the particles
  * \param[in]  mass_size : size of masses array (equals number of particles)
  * \param[in]  theta(double) : value of theta in scheme
  * \param[in]  dt(double)    : time step
  * \param[out] vector_out(double[length])  : integrated gravity term to add to lmgc90
  * \param[in]  length: size of vector_out (equals dim*nb_part) (fortran memory storing)
  */
  extern "C" void self_gravity_ComputeFext2D(double* pos, int dim, int nb_part,
                                             double* masses, int mass_size,
                                             double theta, double dt,
                                             double* vector_out, int length);

 /**
  * \fn void self_gravity_ComputeFext3D(double* pos, int nb_part, int dim, double* masses, int mass_size,
                                        double* fext, int nb_part, int dim, double theta, double dt);
  * \brief compute the gravitation force term to add to Fext in lmgc90
  * must explain about the semi-explicit thing
  * \param[in]  pos(double[dim,nb_part]) : position in space of particles
  * \param[in]  dim : dimension of a position vector (i.e. 3)
  * \param[in]  nb_part : number of particles in pos array
  * \param[in]  masses(double[nb_part]) : masse of the particles
  * \param[in]  mass_size : size of masses array (equals number of particles)
  * \param[in]  theta(double) : value of theta in scheme
  * \param[in]  dt(double)    : time step
  * \param[out] vector_out(double[length])  : integrated gravity term to add to lmgc90
  * \param[in]  length: size of vector_out (equals dim*nb_part) (fortran memory storing)
  */
  extern "C" void self_gravity_ComputeFext3D(double* pos, int dim, int nb_part,
                                             double* masses, int mass_size,
                                             double theta, double dt,
                                             double* vector_out, int length);


#endif /* wrap_gravity_h */
