/*===========================================================================
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
 *===========================================================================*/

#ifndef wrap_overall_h
#define wrap_overall_h
  
/*
Time evolution policy
*/

/**
 * @fn void overall_Initialize(void)
 * @brief Initialize LMGC90
 *
 * @cond PYDOC
 * python usage : overall_Initialize()
 * @endcond
 */
 extern "C" void overall_Initialize(void);

/**
 * @fn void overall_Finalize(void)
 * @brief Finalize LMGC90
 *
 * @cond PYDOC
 * python usage : overall_Finalize()
 * @endcond
 */
 extern "C" void overall_Finalize(void);

/**
 * @fn void overall_InitEntityList(void)
 * @brief Initialize entity list : must be done after LoadTactors
 *
 * @cond PYDOC
 * python usage : overall_InitEntityList()
 * @endcond
 */
 extern "C" void overall_InitEntityList(void);

/**
 * @fn void TimeEvolution_SetTimeStep(double value)
 * @brief Set value of the time step
 *
 * @cond PYDOC
 * python usage : TimeEvolution_SetTimeStep(dt)
 * @param[in] dt (double) : value of time step
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (double) : value of time step
 * @endcond
 */
 extern "C" void TimeEvolution_SetTimeStep(double value);

/**
 * @fn void TimeEvolution_IncrementStep(void)
 * @brief Increment curent time, time step and eventually initialize NR loop counter
 *
 * @cond PYDOC
 * python usage : TimeEvolution_IncrementStep()
 * @endcond
 */
 extern "C" void TimeEvolution_IncrementStep(void);

/**
 * @fn void TimeEvolution_UpdateStep(void)
 * @brief update the initial time to the current time
 *
 * @cond PYDOC
 * python usage : TimeEvolution_UpdateStep()
 * @endcond
 */
 extern "C" void TimeEvolution_UpdateStep(void);

/**
 * @fn void TimeEvolution_DisplayStep(void)
 * @brief Display time evolution step informations 
 *
 * @cond PYDOC
 * python usage : TimeEvolution_DisplayStep()
 * @endcond
 */
 extern "C" void TimeEvolution_DisplayStep(void);

/**
 * @fn void TimeEvolution_SetInitialStep(int ivalue)
 * @brief Set the rank of the first time step
 *
 * @cond PYDOC
 * python usage : TimeEvolution_SetInitialStep(first_step)
 * @param[in] first_step (integer) : rank of the first time step
 * @endcond
 *
 * @cond CDOC
 * @param[in] ivalue (int) :  rank of the first time step
 * @endcond
 */
 extern "C" void TimeEvolution_SetInitialStep(int ivalue);

/**
 * @fn void TimeEvolution_SetInitialTime(double value)
 * @brief Set initial time
 *
 * @cond PYDOC
 * python usage : TimeEvolution_SetInitialTime(t_init)
 * @param[in] t_init (double) : initial time
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (double) : initial time
 * @endcond
 */
 extern "C" void TimeEvolution_SetInitialTime(double value);

/**
 * @fn double TimeEvolution_GetTime(void)
 * @brief get current time
 *
 * @cond PYDOC
 * python usage : time = TimeEvolution_GetTime()
 * @return time (double) : current time
 * @endcond
 *
 * @cond CDOC
 * @return time (double) : current time
 * @endcond
 */
 extern "C" double TimeEvolution_GetTime(void);

/**
 * @fn double TimeEvolution_GetTimeStep(void)
 * @brief get current time step
 *
 * @cond PYDOC
 * python usage : dt = TimeEvolution_GetTimeStep()
 * @return dt (double) : time step
 * @endcond
 *
 * @cond CDOC
 * @return dt (double) : time step
 * @endcond
 */
 extern "C" double TimeEvolution_GetTimeStep(void);

/**
 * @fn int TimeEvolution_GetStep(void)
 * @brief get current step number
 *
 * @cond PYDOC
 * python usage : it = TimeEvolution_GetStep()
 * @return it (int) : current step number
 * @endcond
 *
 * @cond CDOC
 * @return it (int) : current step number
 * @endcond
 */
 extern "C" int TimeEvolution_GetStep(void);

/**
 * @fn void TimeEvolution_WriteLastDof(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteLastDof()
 * @endcond
 */
 extern "C" void TimeEvolution_WriteLastDof(void);

/**
 * @fn void TimeEvolution_WriteOutDof(int Nstep_writeDof=1)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteOutDof(Nstep_writeDof)
 * @param[in] Nstep_writeDof (integer) : periodicity of DOF write
 * @endcond
 *
 * @cond CDOC
 * @param[in] Nstep_writeDof (int) : periodicity of DOF write
 * @endcond
 */
 extern "C" void TimeEvolution_WriteOutDof(int Nstep_writeDof=1);

/**
 * @fn void TimeEvolution_DisplayOutDof(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_DisplayOutDof()
 * @endcond
 */
 extern "C" void TimeEvolution_DisplayOutDof(void);

/**
 * @fn void TimeEvolution_WriteLastRnod(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteLastRnod()
 * @endcond
 */
 extern "C" void TimeEvolution_WriteLastRnod(void);


/**
 * @fn void TimeEvolution_WriteOutRnod(int entier)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteOutRnod(nstep)
 * @param[in] nstep (integer) : a freq of writing
 * @endcond
 *
 * @cond CDOC
 * @param[in] nstep (int) : a freq of writing
 * @endcond
 */
 extern "C" void TimeEvolution_WriteOutRnod(int entier);

/**
 * @fn void TimeEvolution_DisplayOutRnod(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_DisplayOutRnod()
 * @endcond
 */
 extern "C" void TimeEvolution_DisplayOutRnod(void);

/**
 * @fn void TimeEvolution_WriteLastVlocRloc(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteLastVlocRloc()
 * @endcond
 */
 extern "C" void TimeEvolution_WriteLastVlocRloc(void);

/**
 * @fn void TimeEvolution_WriteOutVlocRloc(int Nstep_writeVlocRloc=1)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteOutVlocRloc(nstep)
 * @param[in] nstep (integer) : a freq of writing
 * @endcond
 *
 * @cond CDOC
 * @param[in] nstep (int) : a freq of writing
 * @endcond
 */
 extern "C" void TimeEvolution_WriteOutVlocRloc(int Nstep_writeVlocRloc=1);

/**
 * @fn void TimeEvolution_DisplayOutVlocRloc(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_DisplayOutVlocRloc()
 * @endcond
 */
 extern "C" void TimeEvolution_DisplayOutVlocRloc(void);

/**
 * @fn void TimeEvolution_WriteLastGPV(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteLastGPV()
 * @endcond
 */
 extern "C" void TimeEvolution_WriteLastGPV(void);

/**
 * @fn void TimeEvolution_WriteOutGPV(int entier)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteOutGPV(nstep)
 * @param[in] nstep (integer) : a freq of writing
 * @endcond
 *
 * @cond CDOC
 * @param[in] nstep (int) : a freq of writing
 * @endcond
 */
 extern "C" void TimeEvolution_WriteOutGPV(int entier);

/**
 * @fn void TimeEvolution_ReadIniDof(int num=0)
 * @brief Read header of a DOF file
 *
 * @cond PYDOC
 * python usage : TimeEvolution_ReadIniDof(num=0)
 * @param[in] num (integer) : num of file to read
 * @endcond
 *
 * @cond CDOC
 * @param[in] num (int) : num of a file to read
 * @endcond
 *
 */
 extern "C" void TimeEvolution_ReadIniDof(int num=0);

/**
 * @fn void TimeEvolution_ReadIniVlocRloc(int num=0)
 * @brief Read header of a VlocRloc file
 *
 * @cond PYDOC
 * python usage : TimeEvolution_ReadIniVlocRloc(num=0)
 * @param[in] num (integer) : num of file to read
 * @endcond
 *
 * @cond CDOC
 * @param[in] num (int) : num of a file to read
 * @endcond
 *
 */
 extern "C" void TimeEvolution_ReadIniVlocRloc(int num=0);

/**
 * @fn void TimeEvolution_ReadIniGPV(int num=0)
 * @brief Read header of a GPV file
 *
 * @cond PYDOC
 * python usage : TimeEvolution_ReadIniGPV(num=0)
 * @param[in] num (integer) : num of file to read
 * @endcond
 *
 * @cond CDOC
 * @param[in] num (int) : num of a file to read
 * @endcond
 *
 */
 extern "C" void TimeEvolution_ReadIniGPV(int num=0);


/*
Newton Raphson policy
*/


/**
 * @fn void NewtonRaphson_Initialize(double tol)
 * @brief initialize Newton Raphson Loop
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_Initialize(tol)
 * @param[in] tol (double) : tolerance
 * @endcond
 *
 * @cond CDOC
 * @param[in] tol (double) : tolerance
 * @endcond
 */
 extern "C" void NewtonRaphson_Initialize(double value);

/**
 * @fn int NewtonRaphson_CheckConvergence(double norm)
 * @brief check if Newton Raphson loop converges
 *
 * @cond PYDOC
 * python usage : iconv = NewtonRaphson_CheckConvergence(norm)
 * @param[in] norm (double) : value to check
 * @return iconv (integer) : convergence status
 * @endcond
 *
 *   - iconv = 0 : converges  
 *   - iconv = 1 : unknown  
 *   - iconv = 2 : diverges  
 *
 * @cond CDOC
 * @return iconv (int) : convergence status
 * @endcond
 */
 extern "C" int NewtonRaphson_CheckConvergence(double value);

/**
 * @fn int NewtonRaphson_ComputeTimeStep(void)
 * @brief manages time step evolution depending on newton raphson convergence
 *
 * @cond PYDOC
 * python usage : itodo = NewtonRaphson_ComputeTimeStep()
 * 
 * @return itodo (integer) : what to do now
 * @endcond
 *
 * - itodo = 0 : just keep going (time step may have been modified)
 * - itodo = 1 : redo time step (time step has been decreased)
 * - itodo = 2 : it's hopeless just stop where you are
 *
 * @cond CDOC
 * @return itodo (int) : what to do now
 * @endcond
 */
 extern "C" int NewtonRaphson_ComputeTimeStep(void);

/**
 * @fn void NewtonRaphson_SetMinTimeStep(double value)
 * @brief Set value of the mininum possible time step
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetMinTimeStep(dt)
 * @param[in] dt (double) : minimum value of time step
 * @endcond
 *
 * \n Needed only if adaptive time step feature is used\n 
 *
 * @cond CDOC
 * @param[in] value (double) : minimum value of time step
 * @endcond
 */
 extern "C" void NewtonRaphson_SetMinTimeStep(double value);

/**
 * @fn void NewtonRaphson_SetMaxTimeStep(double value)
 * @brief Set value of the maximum possible time step
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetMaxTimeStep(dt)
 * @param[in] dt (double) : maximum value of time step
 * @endcond
 *
 * \n Needed only if adaptive time step feature is used\n 
 *
 * @cond CDOC
 * @param[in] value (double) : maximum value of time step
 * @endcond
 */
 extern "C" void NewtonRaphson_SetMaxTimeStep(double value);

/**
 * @fn void NewtonRaphson_SetFinalTime(double value)
 * @brief Set final time
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetFinalTime(t_final)
 * @param[in] t_final (double) : final time
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (double) : final time
 * @endcond
 */
 extern "C" void NewtonRaphson_SetFinalTime(double value);

/**
 * @fn void NewtonRaphson_SetMaxIter(int ivalue)
 * @brief Max number of iterations - default is 50
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetMaxIter(max_iter)
 * @param[in] max_iter (integer) : 
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (int) : 
 * @endcond
 */
 extern "C" void NewtonRaphson_SetMaxIter(int ivalue);

/**
 * @fn void NewtonRaphson_SetGoodIter(int value)
 * @brief Set the max number of iterations for good convergence - default is 10 
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetGoodIter(good_iter)
 * @param[in] good_iter (integer) : 
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (int) : 
 * @endcond
 */
 extern "C" void NewtonRaphson_SetGoodIter(int ivalue);

/**
 * @fn void NewtonRaphson_SetBadIter(int value)
 * @brief Set the max number of iterations for bad convergence - default is 30 
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetBadIter(bad_iter)
 * @param[in] bad_iter (integer) : 
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (int) : 
 * @endcond
 */
 extern "C" void NewtonRaphson_SetBadIter(int ivalue);

/**
 * @fn void NewtonRaphson_SetIncPatience(int value)
 * @brief Set the number of increments to adapt the time step when successive good convergence  (increase time step) or bad convergence (decrease time step) - default is 3 
 *
 * @cond PYDOC
 * python usage : NewtonRaphson_SetIncPatience(patience)
 * @param[in] patience (integer) : 
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (int) : 
 * @endcond
 */
 extern "C" void NewtonRaphson_SetIncPatience(int ivalue);

/*
Contact detection
*/

/**
 * @fn void overall_SelectProxTactors(int Nstep_rough_seek=1)
 * @brief Prepare contact detection
 *
 * @cond PYDOC
 * python usage : overall_SelectProxTactors(Nstep_rough_seek)
 * @param[in] Nstep_rough_seek (integer) : periodicity of rough detection
 * @endcond
 *
 * @cond CDOC
 * @param[in] Nstep_rough_seek (int) : periodicity of rough detection
 * @endcond
 */
 extern "C" void overall_SelectProxTactors(int Nstep_rough_seek=1);

/**
 * @fn void overall_DisplayProxTactors(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_DisplayProxTactors()
 * @endcond
 */
 extern "C" void overall_DisplayProxTactors(void);



/**
 * @fn void overall_DIME(int idim, int imod)
 * @brief set space dimension and in 2D the modelling assumption
 *
 * @cond PYDOC
 * python usage : overall_DIME(idim, imod)
 * @param[in] idim (integer) : dimension (2 or 3)
 * @param[in] imod (integer) : kind of model (2D only)
 * @endcond
 *
 * - imod = 1 => plane strain 
 * - imod = 2 => plane stress 
 * - imod = 3 => axisymmetric 
 *
 * @cond CDOC
 * @param[in] idim (int) : dimension (2 or 3)
 * @param[in] imod (int) : mod of analysis (2D only)
 * @endcond
 */
 extern "C" void overall_DIME(int idim, int imod);

/*
Integrator policy
*/

/**
 * @fn void Integrator_InitTheta(double value)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitTheta(theta)
 * @param[in] theta (double) : value of theta in integrator
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (double) : value of theta in integrator
 * @endcond
 */
 extern "C" void Integrator_InitTheta(double value);

/**
 * @fn void Integrator_InitQS(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitQS()
 * @endcond
 *
 * @cond CDOC
 * @endcond
 */
 extern "C" void Integrator_InitQS(void);

/**
 * @fn void Integrator_InitCrankNickolson(double value)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitCrankNickolson(theta)
 * @param[in] theta (double) : value of theta in integrator
 * @endcond
 *
 * @cond CDOC
 * @param[in] value (double) : value of theta in integrator
 * @endcond
 */
 extern "C" void Integrator_InitCrankNickolson(double value);

/**
 * @fn void Integrator_InitGear(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitGear()
 * @endcond
 */
 extern "C" void Integrator_InitGear(void);

/**
 * @fn void Integrator_InitVerlet(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitVerlet()
 * @endcond
 */
 extern "C" void Integrator_InitVerlet(void);
 
 /**
 * @fn void Integrator_InitBeta2(double value)
 * @brief
 *
 * @cond PYDOC
 * python usage : Integrator_InitBeta2(value)
 * @param[in] value (double)  : numeric diffusion ([0.5,1] and 0.5 is conservative)
 * @endcond
 *
 * @cond CDOC
 * @param[in] alpha_b (double) : numeric diffusion ([0.5,1] and 0.5 is conservative) 
 * @endcond

 */
 extern "C" void Integrator_InitBeta2(double value);

/**
 * @fn void Integrator_SetContactDetectionConfiguration(double alpha_b, double alpha_e)
 * @brief set the parameters necessary to define the contact detection configuration (default: 1-theta, 0.)
 *
 * @cond PYDOC
 * python usage : Integrator_SetContactDetectionConfiguration(alpha_b,alpha_e)
 * @param[in] alpha_b (double) : value of the V_begin weight
 * @param[in] alpha_e (double) : value of the V weight
 * @endcond
 *
 * @cond CDOC
 * @param[in] alpha_b (double) : value of the V_begin weight
 * @param[in] alpha_e (double) : value of the V weight
 * @endcond
 */
extern "C" void Integrator_SetContactDetectionConfiguration(double alpha_b, double alpha_e);



/**
 * @fn void overall_RequireXxlComputation(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_RequireXxlComputation()
 * @endcond
 */
 extern "C" void overall_RequireXxlComputation(void);

/**
 * @fn void overall_UpdatePostData(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_UpdatePostData()
 * @endcond
 */
 extern "C" void overall_UpdatePostData(void);
   
/**
 * @fn void overall_InitPostData(int ifirst, int ilast)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_InitPostData(ifirst, ilast)
 * @param[in] ifirst (integer) : 
 * @param[in] ilast (integer)  : 
 * @endcond
 *
 * @cond CDOC
 * @param[in] ifirst (int) : 
 * @param[in] ilast (int)  : 
 * @endcond
 */
 extern "C" void overall_InitPostData(int ifirst, int ilast);

/**
 * @fn void overall_SetWorkingDirectory(char * cvalue1)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_SetWorkingDirectory(path)
 * @param[in] path (string) : set path to DATBOX directory
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue1 (char *) : set path to DATBOX directory
 * @endcond
 */
 extern "C" void overall_SetWorkingDirectory(char * cvalue1);

/**
 * @fn void overall_GetWorkingDirectory(char**string_out, int* string_size, int* real_size);
 * @brief
 *
 * @cond PYDOC
 * python usage :  path = overall_GetWorkingDirectory()
 *
 * @return path (string) : working directory
 * @endcond
 *
 * @cond CDOC
 * @param[out] string_out (char**) : pointer on string of working directory
 * @param[out] string_size (int*)  : size of string pointed by string_out on C side
 * @param[out] real_size   (int*)  : max size of string pointed by string_out on Fortran side
 * @endcond
 */
 extern "C" void overall_GetWorkingDirectory(char** string_out, int* string_size, int* real_size);

/*
Writes
*/

/**
 * @fn void overall_WriteDrivenDof(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_WriteDrivenDof()
 * @endcond
 */
 extern "C" void overall_WriteDrivenDof(void);

/**
 * @fn void overall_WriteOutDisplayFile(int freq_display=0)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_WriteOutDisplayFile(freq_display)
 * @param[in] freq_display (integer) : periodicity of display write
 * @endcond
 *
 * @cond CDOC
 * @param[in] Nstep_write (int) : periodicity of display write
 * @endcond
 */
 extern "C" void overall_WriteOutDisplayFile(int freq_display=0);

/**
 * @fn void TimeEvolution_ReadIniMpValues(int num=0)
 * @brief Read header of a MP_VALUES file
 *
 * @cond PYDOC
 * python usage : TimeEvolution_ReadIniMpValues(num=0)
 * @param[in] num (integer) : num of file to read
 * @endcond
 *
 * @cond CDOC
 * @param[in] num (int) : num of a file to read
 * @endcond
 *
 */
 extern "C" void TimeEvolution_ReadIniMpValues(int num=0);

/**
 * @fn void TimeEvolution_WriteOutMpValues(int nstep=1)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteOutMpValues(nstep)
 * @param[in] nstep (integer) : a freq of writing
 * @endcond
 *
 * @cond CDOC
 * @param[in] nstep (int) : a freq of writing
 * @endcond
 */
 extern "C" void TimeEvolution_WriteOutMpValues(int entier=1);

/**
 * @fn void TimeEvolution_WriteLastMpValues(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : TimeEvolution_WriteLastMpValues()
 * @endcond
 */
 extern "C" void TimeEvolution_WriteLastMpValues(void);

/**
 * @fn void overall_WriteBodies(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_WriteBodies()
 * @endcond
 */
 extern "C" void overall_WriteBodies(void);

/**
 * @fn void overall_CleanOutBodies(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_CleanOutBodies()
 * @endcond
 */
 extern "C" void overall_CleanOutBodies(void);

/**
 * @fn void overall_RebuildInBodies(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_RebuildInBodies()
 * @endcond
 */
 extern "C" void overall_RebuildInBodies(void);

/**
 * @fn void overall_CleanWriteOutFlags(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : overall_CleanWriteOutFlags()
 * @endcond
 */
 extern "C" void overall_CleanWriteOutFlags(void);

/*
reads
*/


/*
*/

/**
 * @fn void overall_UseExperimentalDev(void)
 * @brief Activate some unstable devs
 *
 * @cond PYDOC
 * python usage : overall_UseExperimentalDev()
 * @endcond
 */
 extern "C" void overall_UseExperimentalDev(void);

/**
 * @fn void overall_UseExternalFem(void)
 * @brief Allow to use the externalFem library instead of lmgc90 Fem lib
 *
 * @cond PYDOC
 * python usage : overall_UseExternalFem()
 * @endcond
 */
 extern "C" void overall_UseExternalFem(void);

/**
 * @fn in overall_GetMaxInternalTact(void)
 * @brief get max of internal for tact
 *
 * @cond PYDOC
 * python usage : nb = overall_GetMaxInternalTact()
 * @return nb (integer) : maximum number of internal for interactions
 * @endcond
 *
 * @cond CDOC
 * @return nb (int) : maximum number of internal for interactions
 * @endcond
 */
 extern "C" int overall_GetMaxInternalTact(void);


#endif /* wrap_overall_h */
