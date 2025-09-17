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
 *==========================================================================*/

#ifndef wrap_tact_behav_h
#define wrap_tact_behav_h


/**
 * @fn void tact_behav_OpenBehavContainer(void)
 * @brief open the container (access as a linked list) in order to add/remove objects
 *
 * @cond PYDOC
 * python usage : tact_behav_OpenBehavContainer()
 * @endcond
 */
 extern "C" void tact_behav_OpenBehavContainer(void);

/**
 * @fn void tact_behav_CloseBehavContainer(void)
 * @brief close the container (access as an array)  
 *
 * @cond PYDOC
 * python usage : tact_behav_TactBehavContainer()
 * @endcond
 */
 extern "C" void tact_behav_CloseBehavContainer(void);

/**
 * @fn void tact_behav_OpenSeeContainer(void)
 * @brief open the container (access as a linked list) in order to add/remove objects
 *
 * @cond PYDOC
 * python usage : tact_behav_OpenSeeContainer()
 * @endcond
 */
 extern "C" void tact_behav_OpenSeeContainer(void);

/**
 * @fn void tact_behav_CloseSeeContainer(void)
 * @brief close the container (access as an array)   
 *
 * @cond PYDOC
 * python usage : tact_behav_CloseSeeContainer()
 * @endcond
 */
 extern "C" void tact_behav_CloseSeeContainer(void);

/**
 * @fn void tact_behav_FillContainersFromFile(void)
 * @brief read DATBOX/TACT_BEHAV.DAT and fill the containers (see and tact)
 *
 * @cond PYDOC
 * python usage : tact_behav_FillContainersFromFile()
 * @endcond
 */
 extern "C" void tact_behav_FillContainersFromFile(void);

/**
 * @fn void tact_behav_AddToSeeContainer(char * c_cdbdy,char * c_cdtac,char * c_cdcol,
                                         char * c_behav,
                                         char * c_anbdy,char * c_antac,char * c_ancol,
                                         double c_alert,double c_global_alert);
 * @brief add a see table to the container
 *
 * @cond PYDOC
 * python usage : tact_behav_AddToSeeContainer(cdbdy,cdtac,cdcol,behav,anbdy,antac,ancol,alert,global_alert)
 * @endcond
 */
 extern "C" void tact_behav_AddToSeeContainer(char * c_cdbdy,char * c_cdtac,char * c_cdcol,
                                              char * c_behav,
                                              char * c_anbdy,char * c_antac,char * c_ancol,
                                              double c_alert,double c_global_alert);

/**
 * @fn void tact_behav_ReadBehaviours(void)
 * @brief open + fill + close
 *
 * @cond PYDOC
 * python usage : tact_behav_ReadBehaviours()
 * @endcond
 */
 extern "C" void tact_behav_ReadBehaviours(void);

/**
 * @fn void tact_behav_CollectOutTactBehav(void)
 * @brief old fashion read from OUTBOX/TACT_BEHAV.OUT
 *
 * @cond PYDOC
 * python usage : tact_behav_CollectOutTactBehav()
 * @endcond
 */
 extern "C" void tact_behav_CollectOutTactBehav(void);

/**
 * @fn void tact_behav_WriteBehaviours(void)
 * @brief write (replace) tact and see to OUTBOX/TACT_BEHAV.OUT
 *
 * @cond PYDOC
 * python usage : tact_behav_WriteBehaviours()
 * @endcond
 */
 extern "C" void tact_behav_WriteBehaviours(void);

/**
 * @fn void tact_behav_AppendOutTactBehav(void)
 * @brief  write (append) tact and see to OUTBOX/TACT_BEHAV.OUT
 *
 * @cond PYDOC
 * python usage : tact_behav_AppendOutTactBehav()
 * @endcond
 */
 extern "C" void tact_behav_AppendOutTactBehav(void);

/**
 * @fn void tact_behav_RebuildInTactBehav(void)
 * @brief write (replace) tact and see to DATBOX/TACT_BEHAV.DAT
 *
 * @cond PYDOC
 * python usage : tact_behav_RebuildInTactBehav()
 * @endcond
 */
 extern "C" void tact_behav_RebuildInTactBehav(void);

/**
 * @fn void tact_behav_CleanOutTactBehav(void)
 * @brief erase OUTBOX/TACT_BEHAV.OUT
 *
 * @cond PYDOC
 * python usage : tact_behav_CleanOutTactBehav()
 * @endcond
 */
 extern "C" void tact_behav_CleanOutTactBehav(void);

/**
 * @fn int tact_behav_GetNbTactBehav(void);
 * @brief get the number of tact laws
 *
 * @cond PYDOC
 * python usage : nb_tact_behav = tact_behav_GetNbTactBehav()
 * @param[out] nb_tact_behav (integer) : number of contact behaviour in lmgc90
 * @endcond
 *
 * @cond CDOC
 * @return (int) : number of contact behaviour in lmgc90
 * @endcond
 */
 extern "C" int tact_behav_GetNbTactBehav(void);

/**
 * @fn void tact_behav_GetTactBehav(int i_tb, char** string_out, int* string_size, int* real_size, char** c5, double** r8_vector, int* r8_size);
 * @brief get information related to a given tact law
 *
 * @cond PYDOC
 * python usage : [lawty, behav, param] = tact_behav_GetTactBehav(i_tb)
 * @param[in]  i_tb (integer) : rank (in the contact laws list) of the desired tact_behav
 * @param[out] lawty (string) : type of the contact law
 * @param[out] behav (string) : name of the contact law
 * @param[out] param (real vector) : parameters of the law
 * @endcond
 *
 * @cond CDOC
 * @param[in]  i_tb (int)           : rank of the desired contact behaviour
 * @param[out] string_out (char **) : index of the contact law
 * @param[out] string_size (int * ) : size of string_out
 * @param[out] real_size   (int * ) : max size of string pointed by string_out on Fortran side
 * @param[out] c5 (char**) : name of the contact law
 * @param[out] r8_vector (double **) : parameters of the law
 * @param[out] r8_size (int*) : size of array pointer by r8_vector
 * @endcond
 */
 extern "C" void tact_behav_GetTactBehav(int i_tb, char** string_out, int* string_size, int* real_size, char** c5, double** r8_vector, int* r8_size);

 /**
  * @fn void tact_behav_GetInternalComment(int ivalue, char** string_out, int* string_size, int* real_size)
  * @brief Get internal variables comment of a given interaction law
  *
  * @cond PYDOC
  * python usage : comment = tact_behav_GetInternalComment(ilaw)
  * @param[in] ilaw (integer)   : rank of the interaction law
  * @return comment (char[100]) : the string to get
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue (int) : rank of the interaction law
  * @param[out] string_out (char **) : internal comment of the interaction law
  * @param[out] string_size (int * ) : size of string_out
  * @param[out] real_size   (int * ) : max size of string pointed by string_out on Fortran side
  * @endcond
  */
  extern "C" void tact_behav_GetInternalComment(int ivalue, char** string_out, int* string_size, int* real_size);

/**
 * @fn void void tact_behav_SetCZMwithInitialFriction(double pow)
 * @brief define the way friction evolve with damage: =0. constant value, (1. - beta)**pow otherwize
 *
 * @cond PYDOC
 * python usage : tact_behav_SetCZMwithInitialFriction(pow)
 * @param[in] pow (real)  : parameter of power law evlution for friction mu(beta)=mu_s*(1-beta)**pow
 * @endcond
 *
 */
 extern "C" void tact_behav_SetCZMwithInitialFriction(double pow = 0.);

/**
 * @fn void void tact_behav_initFrictionEvolution()
 * @brief [experimental] read a friction time evolution map
 *
 * @cond PYDOC
 * python usage : tact_behav_initFrictionEvolution()
 * @endcond
 *
 */
 extern "C" void tact_behav_initFrictionEvolution();

/**
 * @fn void tact_behav_setRandomFriction( double r8 )
 * @brief Active variation of local friction
 *
 * @cond PYDOC
 * python usage : tact_behav_setRandomFriction(r8)
 * @endcond
 *
 */
extern "C" void tact_behav_setRandomFriction(double r8);

/**
 * @fn int tact_behav_GetTactBehavRankFromName(char * cvalue1)
 * @brief get the rank (in the list of tact laws) of a tact behav law
 *
 * @cond PYDOC
 * python usage : rank = tact_behav_GetTactBehavRankFromName(c5)
 * @endcond
 *
 */
extern "C" int tact_behav_GetTactBehavRankFromName(char * cvalue1);

/**
 * @fn int tact_behav_GetParamRankFromName(int i_tact, char* cvalue1)
 * @brief get the rank of a param for a given tact behav law
 *
 * @cond PYDOC
 * python usage : rank = tact_behav_GetParamRankFromName(i_tact,c5)
 * @endcond
 *
 */
extern "C" int tact_behav_GetParamRankFromName(int i_tact, char* cvalue1);

/**
 * @fn double tact_behav_GetParam(int i_tact, int i_param)
 * @brief get the value of a parameter
 *
 * @cond PYDOC
 * python usage : param = tact_behav_GetParam(i_tact,i_param)
 * @param[in] i_tact (integer)  : rank of the interaction law
 * @param[in] i_param (integer) : rank of the parameter
 * @param[out] param (real )    : value of the parameter
 * @endcond
 *
 */
extern "C" double tact_behav_GetParam(int i_tact,  int i_param);

/**
 * @fn void tact_behav_SetParam(int i_tact, int i_param, double r8 )
 * @brief set the value ...
 *
 * @cond PYDOC
 * python usage : tact_behav_SetParam(i_tact, i_param, param)
 * @param[in] i_tact (integer)  : rank of the interaction law
 * @param[in] i_param (integer) : rank of the parameter
 * @param[in] param (real )     : value of the parameter

 * @endcond
 *
 */
extern "C" void tact_behav_SetParam(int i_tact,  int i_param, double r8);

 /**
  * @fn void tact_behav_GetLawInternalComment(char name, char** string_out, int* string_size, int* real_size)
  * @brief Get internal comment of an interaction law 
  *
  * @cond PYDOC
  * python usage : comment = tact_behav_GetLawInternalComment(law_type_name)
  * @param[in] name (char[30])  : type of the interaction law
  * @return comment (char[100]) : the string to get
  * @endcond
  *
  * @cond CDOC
  * @param[in]  name (char *) : type of the interaction law (string of size 30)
  * @param[out] string_out (char **) : internal comment of the interaction law
  * @param[out] string_size (int * ) : size of string_out
  * @param[out] real_size   (int * ) : max size of string pointed by string_out on Fortran side
  * @endcond
  */
  extern "C" void tact_behav_GetLawInternalComment(char * name, char** string_out, int* string_size, int* real_size);

/* fd obsolete law are now managing mixity */
/*    2019-02-07 keep intentionally this here for the moment */
/* /\** */
/*  * @fn void tact_behav_SetG1overG2(double r8 ) */
/*  * @brief set the value GI/GII */
/*  * */
/*  * @cond PYDOC */
/*  * python usage : tact_behav_SetG1overG2(param) */
/*  * @endcond */
/*  * */
/*  *\/ */
/* extern "C" void tact_behav_SetG1overG2(double r8); */

/* /\** */
/*  * @fn void tact_behav_SetS1overS2(double r8 ) */
/*  * @brief set the value SI/SII */
/*  * */
/*  * @cond PYDOC */
/*  * python usage : tact_behav_SetS1overS2(param) */
/*  * @endcond */
/*  * */
/*  *\/ */
/* extern "C" void tact_behav_SetS1overS2(double r8); */

/* /\** */
/*  * @fn void tact_behav_SetD1overD2(double r8 ) */
/*  * @brief set the value DI/DII for Tvergaard - Hutchinson trapezoidal law */
/*  * */
/*  * @cond PYDOC */
/*  * python usage : tact_behav_SetD1overD2(param) */
/*  * @endcond */
/*  * */
/*  *\/ */
/* extern "C" void tact_behav_SetD1overD2(double r8); */

/**
 * @fn void tact_behav_SetRNcap(double r8 )
 * @brief set a maximal compression value
 *
 * @cond PYDOC
 * python usage : tact_behav_SetRNcap(param)
 * @endcond
 *
 */
extern "C" void tact_behav_SetRNcap(double r8);

/**
 * @fn void tact_behav_SetDilatancyParameters(double fric, double height )
 * @brief set dilatancy parameters
 *
 * @cond PYDOC
 * python usage : tact_behav_SetDilatancyParameters(fric,height)
 * @endcond
 *
 */
extern "C" void tact_behav_SetDilatancyParameters(double fric, double height);

/**
 * @fn void tact_behav_SetPressureParameters(int ibehav, int flag,  double* rvector_in, int rlength_in)
 * @brief set pressure parameters
 *
 * @cond PYDOC
 * @param[in] ibehav (integer)      : rank of the tact behav
 * @param[in] flag (integer)        : kind of build-in pressure law (0 no pressure, 1: time dependent, 2: linearly progressive since crack starts, 3: exponentially progressive since crack starts, 4: external) 
 * @param[in] params (double array) : the new value of the params [p0,dp,tau,alpha]
 *
 * python usage : tact_behav_SetPressureParameters(ibehav,flag,params)
 * @endcond
 *
 */
extern "C" void tact_behav_SetPressureParameters(int ibehav, int flag, double* rvector_in, int rlength_in);

/**
 * @fn void tact_behav_CleanMemory(void)
 * @brief Free all memory allocated within tact_behav module
 *
 * @cond PYDOC
 * python usage : tact_behav_CleanMemory()
 * @endcond
 */
extern "C" void tact_behav_CleanMemory(void);

#endif /* wrap_tact_behav_h */
