.. py:currentmodule:: pylmgc90.pre

Visibility table definition
===========================

A *visibility_table* object created by  :py:class:`see_table` is necessary to perform interaction detection. 
It allows to relate two sets, Candidate Body/Contactor/Color and Antagoniste Body/Contactor/Color,
to an interaction behavior model/material. 

*behav* may be a *tact_behav* object or a *string*.

Only contact with gap lower than alert distance are kept.

Halo is given to reduce neighbor detection when using meshed surface. 


**Example:** ::

 vt = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='BLUEx', 
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BLUEx', 
                    behav=lcsas,  alert=0.01, halo=0.05)

