.. py:currentmodule:: pylmgc90.chipy.lmgc90

Interaction models options
==========================

  **General**

  :py:func:`tact_behav_ReadBehaviours` ()
 
  :py:func:`tact_behav_CleanMemory` ()

  **IO**

  :py:func:`tact_behav_WriteBehaviours` ()

  **Options**
  
  :py:func:`tact_behav_initFrictionEvolution` ()

  :py:func:`tact_behav_setRandomFriction` (r8)

  **Accessors**

  nb_tact_behav = :py:func:`tact_behav_GetNbTactBehav` ()

  rank = :py:func:`tact_behav_GetTactBehavRankFromName` (c5)

  rank = :py:func:`tact_behav_GetParamRankFromName` (i_tact,c5)

  param = :py:func:`tact_behav_GetParam` (i_tact,i_param)

  :py:func:`tact_behav_SetParam` (i_tact, i_param, param)
    
  [lawty, behav, param] = :py:func:`tact_behav_GetTactBehav` (i_tb)
  
  **Managing CZM**
  
  :py:func:`tact_behav_SetCZMwithInitialFriction` (int pow)

  **Unstable**

  :py:func:`tact_behav_SetRNcap` (param)

  :py:func:`tact_behav_SetDilatancyParameters` (fric,height)
