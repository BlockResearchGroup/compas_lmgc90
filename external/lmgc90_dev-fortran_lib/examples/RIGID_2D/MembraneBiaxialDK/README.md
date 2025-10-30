# Biaxial loading #

## Isostatic state ##

`cd IsoState`
Run `python command.py`.  
It will generate a state under isostatic stress load. 


## Axial deformation ##

`cd ../Defo`  
Copy the solution of the previous computation to initialize computation.  
`cp ../IsoState/OUTBOX/DOF.OUT.50 DATBOX/DOF.INI`  
`cp ../IsoState/OUTBOX/Vloc_Rloc.OUT.50 DATBOX/Vloc_Rloc.INI`  
Run `python command.py`.  