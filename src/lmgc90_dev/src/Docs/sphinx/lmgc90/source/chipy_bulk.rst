.. py:currentmodule:: pylmgc90.chipy.lmgc90

Bulk models options
===================

  - RBDY2

  - RBDY3

    **Time Loop**

    :py:func:`RBDY3_ReadBodies`

    :py:func:`RBDY3_LoadBehaviours`

    :py:func:`RBDY3_ReadIniDof`

    :py:func:`RBDY3_ReadDrivenDof`

    :py:func:`RBDY3_ComputeMass`
    
    :py:func:`RBDY3_IncrementStep`

    :py:func:`RBDY3_ComputeFext`
	
    :py:func:`RBDY3_ComputeBulk`
	
    :py:func:`RBDY3_ComputeFreeVelocity`
	
    :py:func:`RBDY3_ComputeContactDetectionConfiguration`
	
    :py:func:`RBDY3_ComputeDof`
	
    :py:func:`RBDY3_UpdateDof`
	
    :py:func:`RBDY3_FatalDamping`   *arguments: freq*

    :py:func:`RBDY3_PartialDamping` *arguments: nb_steps, Vmax*

    :py:func:`RBDY3_CleanMemory`
	
    **Setting Options**
    
    :py:func:`RBDY3_KeepIniDofOrder`
	
    :py:func:`RBDY3_NewRotationScheme`
	
    :py:func:`RBDY3_AvoidBodyRotation`
	
    :py:func:`RBDY3_SetInvisibleSmallObjects` *arguments: radius*
	
    :py:func:`RBDY3_SetVisible`  *arguments: ibdyty*
	
    :py:func:`RBDY3_SetInvisible` *arguments: ibdyty*
	
    :py:func:`RBDY3_SetVisibleVlocyDrivenDof` *arguments: ibdyty,iccdof*
	
    :py:func:`RBDY3_SetInvisibleVlocyDrivenDof` *arguments: ibdyty,iccdof*
	
    :py:func:`RBDY3_SetXminBoundary` *arguments: Zmin*
	
    :py:func:`RBDY3_SetXmaxBoundary` *arguments: Xmax*
	
    :py:func:`RBDY3_SetYminBoundary` *arguments: Zmin*
	
    :py:func:`RBDY3_SetYmaxBoundary` *arguments: Ymax*
	
    :py:func:`RBDY3_SetZminBoundary` *arguments: Zmin*
	
    :py:func:`RBDY3_SetZmaxBoundary` *arguments: Zmax*
	
    :py:func:`RBDY3_SetXPeriodicCondition` *arguments: xperiod*
	
    :py:func:`RBDY3_SetYPeriodicCondition` *arguments: yperiod*

    **Accessors**

    nb_RBDY3 = :py:func:`RBDY3_GetNbRBDY3`
	
    :py:func:`RBDY3_SetVlocyDrivenDof`   *arguments: ibdyty, idrvdof, value*

    visible = :py:func:`RBDY3_IsVisible`   *arguments: ibdyty*
	
    density = :py:func:`RBDY3_GetBodyDensity`  *arguments: ibdyty*
	
    inertia = :py:func:`RBDY3_GetBodyInertia`  *arguments: ibdyty*
	
    inertia = :py:func:`RBDY3_GetAllInertia`
	
    :py:func:`RBDY3_PutBodyVector`   *arguments: datatype, ibdyty, vector*
	
    :py:func:`RBDY3_PutAllBodyVector`   *arguments: datatype, matrix*
	
    vector = :py:func:`RBDY3_GetBodyVector` *arguments: datatype, ibdyty*
	
    matrix = :py:func:`RBDY3_GetAllBodyVector` *arguments: datatype*
	
    vector_ptr = :py:func:`RBDY3_GetPtrBodyVector` *arguments: datatype, ibdyty*
	
    mass = :py:func:`RBDY3_GetMass` *arguments: ibdyty*
	
    masses = :py:func:`RBDY3_GetAllMass`
	
    name = :py:func:`RBDY3_GetBehavior` *arguments: ibdyty*
	
    ibehav = :py:func:`RBDY3_GetBulkBehavNumber` *arguments: ibdyty*
	
    nb = :py:func:`RBDY3_GetNbContactor` *arguments: ibdyty*
	
    type = :py:func:`RBDY3_GetContactorType` *arguments: ibdyty,itacty*
	
    color = :py:func:`RBDY3_GetContactorColor` *arguments: ibdyty,itacty*

    **IO**
    
    :py:func:`RBDY3_WriteBodies`

    :py:func:`RBDY3_WriteDrivenDof`	
	
    :py:func:`RBDY3_WriteLastDof`
	
    :py:func:`RBDY3_WriteOutDof`  *arguments: ifrom=0, ito=0*
	
    :py:func:`RBDY3_WriteLastRnod`
	
    :py:func:`RBDY3_WriteOutRnod`
	
    :py:func:`RBDY3_DisplayOutDof`
	
    :py:func:`RBDY3_DisplayOutRnod`

    :py:func:`RBDY3_SkipInvisible`

    **Multiphysics**

    :py:func:`RBDY3_IncrementWSvsT`

    **Obsolete**

    :py:func:`RBDY3_ReadCompressedBodies`

  - mecaMAILx
 
  - therMAILx


    
