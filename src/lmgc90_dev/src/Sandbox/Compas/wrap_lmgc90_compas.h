/*==========================================================================
 *
 * Copyright 2000-2025 CNRS-UM.
 *
 * This file is part of a software (LMGC90) which is a computer program
 * which purpose is to modelize interaction problems (contact, multi-Physics,etc).
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 * To report bugs, suggest enhancements, etc. to the Authors, contact
 * Frederic Dubois.
 *
 * frederic.dubois@umontpellier.fr
 *
 *=========================================================================*/

#ifndef wrap_lmgc90_compas_h
#define wrap_lmgc90_compas_h

// take from lmgc90 core... should find a way to automatize
#define MAX_NB_INT 19

  struct lmgc90_inter_meca_3D {
    char   cdan[5];
    int   icdan;
    char   cdbdy[5];
    int   icdbdy;
    char   anbdy[5];
    int   ianbdy;
    char   cdtac[5];
    int   icdtac;
    char   antac[5];
    int   iantac;
    int   icdsci;
    int   iansci;
    char   behav[5];
    char   status[5];
    double coor[3];
    double uc[9];
    double rloc[3];
    double vloc[3];
    double gap;
    int    nb_int;
    double internals[MAX_NB_INT];
  } ;

  struct lmgc90_rigid_body_3D {
    double coor[3];
    double frame[9];
  } ;

  extern void lmgc90_initialize(double dt, double theta);

  extern void lmgc90_set_materials(int nb);
  extern void lmgc90_set_tact_behavs(int nb);
  extern void lmgc90_set_see_tables(void);
  extern void lmgc90_set_nb_bodies(int nb);
  extern void lmgc90_set_one_polyr(double coor[3], int * faces, int nb_faces, double * vertices, int nb_v, bool fixed);
  extern void lmgc90_close_before_computing(void);

  extern void lmgc90_compute_one_step(void);
  extern void lmgc90_finalize(void);


  // accessors

  extern  int lmgc90_get_nb_inters();
  extern void lmgc90_get_all_inters(struct lmgc90_inter_meca_3D * all_inters, int size);

  extern  int lmgc90_get_nb_bodies();
  extern void lmgc90_get_all_bodies(struct lmgc90_rigid_body_3D * all_bodies, int size);

#endif /* wrap_lmgc90_compas_h */
