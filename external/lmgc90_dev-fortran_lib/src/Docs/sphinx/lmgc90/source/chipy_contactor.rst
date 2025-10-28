.. py:currentmodule:: pylmgc90.chipy.lmgc90

Contactors options
==================

 - 2D

   - ALpxx

      :py:func:`ALpxx_LoadTactors` ()

      :py:func:`ALpxx_PushPreconNodes` ()
      
      :py:func:`ALpxx_CleanMemory` ()
      
   - CLxxx
     
      :py:func:`CLxxx_LoadTactors` ()

      :py:func:`CLxxx_SetNbNodesByCLxxx` (nb_nodes)

      :py:func:`CLxxx_PushPreconNodes` ()

      :py:func:`CLxxx_CleanMemory` ()

   - DISKL

      :py:func:`DISKL_LoadTactors` ()

      :py:func:`DISKL_PushPreconNodes` ()

      :py:func:`DISKL_CleanMemory` ()
      
   - PT2DL
     
      :py:func:`PT2DL_LoadTactors` ()

      :py:func:`PT2DL_PushPreconNodes` ()

      :py:func:`PT2DL_GetNbPT2DL` ()

      :py:func:`PT2DL_GetNbPT2TL` ()

      :py:func:`PT2DL_ComputeConvectiveFlux` ()

      :py:func:`PT2DL_AssembThermKT` ()

      :py:func:`PT2DL_AssembThermRHS` ()

      :py:func:`PT2DL_CleanMemory` ()

   - DISKx
   - JONCx
   - POLYG

   - PT2Dx
   - xKSID

 - 3D

   - POLYR

     **General**

     :py:func:`POLYR_LoadTactors` ()

     :py:func:`POLYR_CleanMemory` ()

     **Properties**

     :py:func:`POLYR_ModifyRadius` (ratio)

     :py:func:`POLYR_SetThresholdBigPolyr` (ratio)

     :py:func:`POLYR_SkipAutomaticReorientation` ()

     :py:func:`POLYR_SkipHEBuild` ()

     :py:func:`POLYR_TopologyAngle` (angle)

     :py:func:`POLYR_FlatnessAngle` (angle)

     **Accesors**

     nb_POLYR = :py:func:`POLYR_GetNbPOLYR` ()

     polyr2rbdy3 = :py:func:`POLYR_GetPOLYR2BDYTY` ()

     polyr2rbdy3 = :py:func:`POLYR_GetPtrPOLYR2BDYTY` ()

     color = :py:func:`POLYR_GetContactorColor` (itacty)

     vertex = :py:func:`POLYR_GetVertex` (itacty)

     vertex = :py:func:`POLYR_GetPtrVertexRef` (itacty)

     vertex = :py:func:`POLYR_GetPtrVertexTT` (itacty)

     normal = :py:func:`POLYR_GetPtrNormalTT` (itacty)

     connec = :py:func:`POLYR_GetPtrConnectivity` (itacty)

     connec = :py:func:`POLYR_GetPtrAllConnectivities` () 

     **Display**
      
     :py:func:`POLYR_MoveToConfigurationTT` ()

     :py:func:`POLYR_UpdatePostdata` () 

     nb_scalarfields = :py:func:`POLYR_GetNbScalarFields` ()

     nb_pointOutlines = :py:func:`POLYR_GetNbPointOutlines` ()

     outlines = :py:func:`POLYR_InitOutlines` ()

     coor,connectivity = :py:func:`POLYR_GetWireframe` (itacty)  : only for visualisation
     
     :py:func:`POLYR_SaveVertex` ()


      
