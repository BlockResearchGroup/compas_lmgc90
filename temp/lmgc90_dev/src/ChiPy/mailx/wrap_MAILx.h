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
 * Frederic Dubois or Remy Mozul.
 *
 * frederic.dubois@umontpellier.fr
 * remy.mozul@umontpellier.fr
 *
 *=========================================================================*/

#ifndef wrap_MAILx_h
#define wrap_MAILx_h

 /**
  * @fn void MAILx_ReadBodies(char const * version = "")
  * @brief read MAILx from DATBOX/BODIES.DAT
  *
  * Input string is of form vX.Y where X is major version number and Y is minor one.  
  * If not specified, last available version is used.
  *
  * @cond PYDOC
  * python usage : MAILx_ReadBodies(version)
  *
  * param[in] version (string) : file format version to use
  * @endcond
  *
  * @cond CDOC
  * param[in] version (char *) : file format version to use
  * @endcond
  */
  extern "C" void MAILx_ReadBodies(char const * version = "");

 /**
  * @fn void MAILx_WriteBodies(char const * version = "")
  * @brief write MAILx to OUTBOX/BODIES.OUT
  *
  * Input string is of form vX.Y where X is major version number and Y is minor one.  
  * If not specified, last available version is used.
  *
  * @cond PYDOC
  * python usage : MAILx_WriteBodies(version)
  *
  * param[in] version (string) : file format version to use
  * @endcond
  *
  * @cond CDOC
  * param[in] version (char *) : file format version to use
  * @endcond
  */
  extern "C" void MAILx_WriteBodies(char const * version = "");

 /**
  * @fn void MAILx_WriteLastGPV(void)
  * @brief write OUTBOX/GPV.LAST
  *
  * @cond PYDOC
  * python usage : MAILx_WriteLastGPV()
  * @endcond
  */
  extern "C" void MAILx_WriteLastGPV(void);

 /**
  * @fn void MAILx_WriteOutGPV(void)
  * @brief write OUTBOX/GPV.OUT.x
  *
  * @cond PYDOC
  * python usage : MAILx_WriteOutGPV()
  * @endcond
  */
  extern "C" void MAILx_WriteOutGPV(void);

 /**
  * @fn void MAILx_DisplayOutGPV(void)
  * @brief display GPV values
  *
  * @cond PYDOC
  * python usage : MAILx_DisplayOutGPV()
  * @endcond
  */
  extern "C" void MAILx_DisplayOutGPV(void);

 /**
  * @fn void MAILx_AddDof2InBodies(void)
  * @brief set cooref = cooref + X
  *
  * @cond PYDOC
  * python usage : MAILx_AddDof2InBodies()
  * @endcond
  */
  extern "C" void MAILx_AddDof2InBodies(void);
  
   /**
  * @fn int MAILx_GetNbMAILx(void)
  * @brief Get the number of MAILx
  *
  * @cond PYDOC
  * python usage : nb_MAILx = GetNbMAILx()
  *
  * @return nb_MAILx (integer) : number of MAILx
  * @endcond
  *
  * @cond CDOC
  * @return nb_MAILx (int) : number of MAILx
  * @endcond
  */
  extern "C" int MAILx_GetNbMAILx(void);
  
  /**
  * @fn int MAILx_GetNbCell(int IdBody)
  * @brief Get the number of Cells of a given MAILx
  *
  * @cond PYDOC
  * python usage : nb_MAILx = GetNbCell(IdBody)
  *
  * @param[in] IdBody (integer) : id of the concern body
  *
  * @return nb_cell (integer) : number of cell
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @return nb_cell (int)                 : number of cell in MAILx
  * @endcond
  */
  extern "C" int MAILx_GetNbCell(int IdBody);

  /**
  * @fn void MAILx_SetCoorRef(int IdBody, double * rvector_in, int rlength_in)
  * @brief set reference coordinates on a given body
  *
  * @cond PYDOC
  * python usage : MAILx_SetCoorRef(IdBody, f, length)
  *
  * @param[in] IdBody (integer) : id of the concern body
  *
  * @param[in] f (double array) : value of the vitesse
  *
  * @param[in] length (integer) : length of vector
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void MAILx_SetCoorRef(int IdBody, double * rvector_in, int rlength_in);

 /**
  * @fn double MAILx_GetCoordNodty(int,int,int)
  * @brief Get one coordinate of a node of a body
  *
  * @cond PYDOC
  * python usage : x = MAILx_GetCoordNodty(int ibdty,int inodty,int icomp)
  *
  * @param[in] ibdyty (integer)            : rank of considered body
  *
  * @param[in] inodty (integer)            : the node
  *
  * @param[in] icomp (integer)             : the component
  *
  * @return x (double)                 : coordinate of node 
  * @endcond
  *
  * @cond CDOC
  * @param ibdyty (integer)            : rank of considered body
  * @param inodty (integer)            : the node
  * @param icomp  (integer)            : the component
  * @return x (double) 
  * @endcond
  */

extern "C" double MAILx_GetCoordNodty(int ivalue1,int ivalue2 ,int);

 /**
  * @fn double MAILx_GetCoordsNodty(int,int,double*,int)
  * @brief Get the coordinates of a node of a body
  *
  * @cond PYDOC
  * python usage : x = MAILx_GetCoordsNodty(int ibdty, int inodty, int length)
  *
  * @param[in] ibdyty (integer)            : rank of considered body
  *
  * @param[in] inodty (integer)            : the node
  *
  * @param[in] length  (integer)           : the number of component
  *
  * @return x   (double array)         : the desired vector
  * @endcond
  *
  * @cond CDOC
  * @param ibdyty (integer)            : rank of considered body
  * @param inodty (integer)            : the node
  * @param length (integer)            : size of the vector
  * @param[in,out] vector_out (double *) : the vector to get, must be of the right size
  * @endcond
  */

extern "C" void MAILx_GetCoordsNodty(int ivalue1, int ivalue2, double * rvector_out, int rlength_out);

/**
  * @fn int MAILx_GetNbNodes(int)
  * @brief Get the number of nodes of a given MAILx 
  *
  * @cond PYDOC
  * python usage : nb_nodes = MAILx_GetNbNodes(ibdyty)
  *
  * @param[in] ibdyty (integer)    : body id
  *
  * @return nb_nodes (integer) : number of nodes of the body 
  * @endcond
  *
  * @cond CDOC
  * @param  (int) body id
  * @return (int) number of nodes
  * @endcond
  */

extern "C" int MAILx_GetNbNodes(int);

/**
  * @fn void MAILx_InitNodalFields(int ibdyty,int nb_fields)
  * @brief Set the number of nodal_fields for a given body  
  *
  * @cond PYDOC
  * python usage : MAILx_InitNodalFields(ibdyty,nb_nodal_fields)
  *
  * @param ibdyty (integer)    : body id
  * @param nb_nodal_fields (integer) : number of nodal fields required 
  * @endcond
  *
  * @cond CDOC
  * @param ibdyty (int) number of nodes
  * @param nb_nodal_fields (int) number of nodal fields
  * @endcond
  */

extern "C" void MAILx_InitNodalFields(int ibdyty,int nb_fields);

/**
  * @fn void MAILx_InitNodalField(int ibdyty, char* name,int rank, int sz)
  * @brief Set name and size of a nodal_field of a given body  
  *
  * @cond PYDOC
  * python usage : MAILx_InitNodalField(ibdyty,name,rank,sz)
  *
  * @param ibdyty (integer) : body id
  * @param name (string)    : field name
  * @param rank (integer)   : field rank
  * @param sz (integer)     : size of the field
  * @endcond
  *
  * @cond CDOC
  * @param ibdyty (int)     : body id
  * @param name (char[30])) : field name
  * @param rank (int)       : field rank
  * @param sz (int)         : size of the field
  * @endcond
  */

extern "C" void MAILx_InitNodalField(int ibdyty, char* name,int rank, int sz);

/**
  * @fn void MAILx_SetNodalField(int ibdyty,int rank, double * rvector_in, int rlength_in)
  * @brief Set a nodal_field of a given body  
  *
  * @cond PYDOC
  * python usage : MAILx_SetNodalField(ibdyty,rank,field)
  *
  * @param ibdyty (integer)     : body id
  * @param rank (integer)       : field rank
  * @param field (double vector) : field
  * @endcond
  *
  * @cond CDOC
  * @param ibdyty (int)               : body id
  * @param rank (int)                 : field rank
  * @param field (double[length])     : field
  * @param length (int)               : size of the field
  * @endcond
  */

extern "C" void MAILx_SetNodalField(int ibdyty, int rank,  double * rvector_in, int rlength_in);

/**
  * @fn void MAILx_CleanMemory(void)
  * @brief Free all memory allocated within MAILx module
  *
  * @cond PYDOC
  * python usage : MAILx_CleanMemory()
  * @endcond
  */
extern "C" void MAILx_CleanMemory(void);

#endif /* wrap_MAILx_h */
