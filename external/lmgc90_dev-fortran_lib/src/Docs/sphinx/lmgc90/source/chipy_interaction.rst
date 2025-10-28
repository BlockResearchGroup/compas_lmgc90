.. automodule:: pylmgc90.chipy
.. py:currentmodule:: pylmgc90.chipy

Interactions options
====================

 - Common

   - :py:func:`~macro.SelectProxTactors` can take `freq_detect` argument
     to specify the frequency at which the neighbourhood detection must be made
   - :py:func:`~lmgc90.overall_SelectProxTactors` can take `freq_detect` argument
     to specify the frequency at which the neighbourhood detection must be made
   - :py:func:`~macro.StockRloc` 
   - :py:func:`~macro.RecupRloc` 

 - 2D

 - 3D    

   - PRPR

     :py:func:`~lmgc90.PRPRx_SetTolRecupRloc` (tol)
     
     **Detection**

     :py:func:`~lmgc90.PRPRx_SelectProxTactors` (reset=0)

     :py:func:`~lmgc90.PRPRx_CleanMemory`

     :py:func:`~lmgc90.PRPRx_ShrinkPolyrFaces` (shrink)

     :py:func:`~lmgc90.PRPRx_LowSizeArrayPolyr` (sfactor)

     **Cundall Common plane**

       :py:func:`~lmgc90.PRPRx_UseCpCundallDetection` (nb_iter)

       :py:func:`~lmgc90.PRPRx_SetCundallNeighbor` (neighbor)

     **Face to face common plane**
       
       :py:func:`~lmgc90.PRPRx_UseCpF2fExplicitDetection` (tol)

       :py:func:`~lmgc90.PRPRx_UseCpF2fDetection` (tol,iter)

       :py:func:`~lmgc90.PRPRx_SetF2fMinimalSurfaceSize` (tol)

     **Non convex detection**
       
       :py:func:`~lmgc90.PRPRx_UseNcDetection` (gdist)

       :py:func:`~lmgc90.PRPRx_UseNcF2fDetection` (gdist,tol)

       :py:func:`~lmgc90.PRPRx_UseNcF2fExplicitDetection` (gdist,tol)
       
       :py:func:`~lmgc90.PRPRx_WithNodalContact`

     **Stono detection**

       :py:func:`~lmgc90.PRPRx_UseStoDetection` (is_explicit, decomp, tol, with_kappa)

       :py:func:`~lmgc90.PRPRx_ForceF2fDetection` ()

       :py:func:`~lmgc90.PRPRx_ForceNcDetection` ()

     **External detection**

       :py:func:`~lmgc90.PRPRx_UseExternalDetection` ()

     **Accessors**

     nb_PRPRx = :py:func:`~lmgc90.inter_handler_3D_getNb` ( PRPRx_ID )

     vector = :py:func:`~lmgc90.PRPRx_GetInteractionVector` (datatype, icdan)

     :py:func:`~lmgc90.PRPRx_SetInteractionInternal` (i, icdan, value)

     value = :py:func:`~lmgc90.PRPRx_GetInteractionInternal` (i, icdan)

     comment= :py:func:`~lmgc90.PRPRx_GetInteractionInternalComment` (icdan)

     array = :py:func:`~lmgc90.inter_handler_3D_getAll` ( PRPRx_ID )

     **IO**

     :py:func:`~lmgc90.PRPRx_ReadIniVlocRloc`
       
     :py:func:`~lmgc90.PRPRx_WriteLastVlocRloc`

     :py:func:`~lmgc90.PRPRx_WriteOutVlocRloc`

     :py:func:`~lmgc90.PRPRx_BinaryReadIniVlocRloc`

     :py:func:`~lmgc90.PRPRx_BinaryWriteLastVlocRloc`

     :py:func:`~lmgc90.PRPRx_BinaryWriteOutVlocRloc`

     **Log**

     :py:func:`~lmgc90.PRPRx_DisplayProxTactors`

     :py:func:`~lmgc90.PRPRx_DisplayOutVlocRloc`
     
     **Debugging**
     
     :py:func:`~lmgc90.PRPRx_VerboseF2F` (cd,an)

     **Display**

     :py:func:`~lmgc90.PRPRx_VisavisVTKDrawAll`

     :py:func:`~lmgc90.PRPRx_CpVTKDrawAll` (rd)

     :py:func:`~lmgc90.PRPRx_SetReactionTrackingLength` (length)
     

