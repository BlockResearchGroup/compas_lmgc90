.. py:currentmodule:: pylmgc90.chipy.lmgc90

Kernel options
==============

* 2D

  ** Options**
  
* 3D

  Two ways are possible to call the contact solver : flat or macro.
  
  Be careful to not mixing functions dedicated to a given solver. 
  
  * flat solver

     :py:func:`nlgs_3D_ScrambleContactOrder`

     :py:func:`nlgs_3D_QuickScrambleContactOrder`

     :py:func:`nlgs_3D_ReverseContactOrder`

     :py:func:`nlgs_SetCheckType` (check_type, tolerance, relaxation)

     :py:func:`nlgs_ExPrep` (storage) 

       **NLGS**
       
       :py:func:`nlgs_3D_ExIter` (nb_iter)

       convergence = :py:func:`nlgs_3D_AfterIterCheck`

       :py:func:`nlgs_3D_ExPost`

       **Jacobi**
  
       :py:func:`nlgs_3D_ExIterJacobi` (nb_iter)

       convergence = :py:func:`nlgs_3D_AfterIterCheckJacobi`

       :py:func:`nlgs_3D_ExPostJacobi`
     

     :py:func:`nlgs_3D_DisplayAfterIterCheck`

     :py:func:`nlgs_3D_ScaleRloc`

  * macro solver
     
     :py:func:`nlgs_3D_SetWithQuickScramble`

     :py:func:`nlgs_3D_UseJacobiSolver` (True|False)

     :py:func:`nlgs_3D_ExSolver` (storage, checktype, tol, relax, nb_iter_check, nb_block_iter) 
    
     
  * Common functions

     :py:func:`nlgs_3D_ComputeRnod`

     :py:func:`nlgs_3D_DisplayTacInfo` (itac)


   **Options**

   :py:func:`nlgs_3D_DiagonalResolution`
     
   :py:func:`nlgs_3D_IsInitialized`
   
   **Output**   

   :py:func:`nlgs_3D_WriteNormCheck`
