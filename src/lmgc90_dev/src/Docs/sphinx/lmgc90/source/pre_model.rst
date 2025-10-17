
.. py:currentmodule:: pylmgc90.pre

Model definition
================

Mainly with the :py:class:`model`

Rigid:
------

Available elements:
  
  * Point : Rxx2D
  * Point : Rxx3D 

Available options: **None**

Example ::

  mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=2)



Mechanical (MECAx) :
--------------------

Available elements (I: isoparametric, B: bar, D: discrete, S: SHB) : 

  * S2xxx : SPRNG (D), BARxx (B)
  * T3xxx : T3xxx (I), DKTxx
  * T6xxx : T6xxx (I)
  * Q4xxx : Q4xxx (I), Q4P0x (I)
  * Q8xxx : Q8xxx (I), Q8Rxx (I)
  * TE4xx : TE4xx (I), TE4lx (I)
  * TE10x : TE10x (I)
  * H8xxx : H8xxx (I)
  * H20xx : H20xx (I), H20Rx (I)
  * PRI6x : PRI6x (I), SHB6x (S)
  * PRI15 : PRI15 (I)   

Available options:

 * kinematic : `small`, `large`
 * formulation : `UpdtL`, `TotaL`
 * mass_storage : `\lump_`, `coher`
 * material :  `\elas_` , `elasd`, `\neoh_`, `hyper`, `hyp_d`, `J2iso`, `J2mix`, `kvisc`
 * anisotropy : `\iso__`, `ortho`
 * external_model : `\MatL_`, `\Demfi`, `\Umat_`, `\no___`
 * discrete : `\yes__`, `\no___`
 * external_fields : list of string
 * external_vfields : list of string

Example ::

  mod = pre.model(name='Q4MLx', physics='MECAx', element='Q4xxx',
                  dimension=2, external_model='yes__', kinematic='small',
                  material='elas_', anisotropy='iso__', mass_storage='lump_')

Thermal (THERx) :
-----------------

Available elements (I: isoparametric, B: bar): 

  * S2xxx : S2xth (B)
  * T3xxx : T3xxx (I)
  * T6xxx : T6xxx (I)
  * Q4xxx : Q4xxx (I)
  * Q8xxx : Q8xxx (I), Q8Rxx (I)
  * TE4xx : TE4xx (I)
  * TE10x : TE10x (I)
  * H8xxx : H8xxx (I)
  * H20xx : H20xx (I), H20Rx (I)

 
Available options:

 * capacity_storage : `\lump_`, `coher`
 * formulation : `class`, `stdvf`, `linvf`
 * external_model : `\yes__`, `\no___`
 * convection_type : `\supg_`, `cente`
 * external_fields : list of string
 * external_vfields : list of string

Example ::

 Model_Thermique = pre.model(name='DIFFU', physics='THERx', element='T3xxx', 
                             dimension = dimension, external_model='no___', capacity_storage='lump_',
                             formulation = 'class', external_fields = ['COCO','SPHV'])

Porous (POROx) :
----------------

Available elements: 

  * T3xxx : T33xx
  * T6xxx : T63xx
  * Q4xxx : Q44xx
  * Q8xxx : Q84xx
  * TE4xx : TE44x
  * TE10x : TE104
  * H8xxx :H88xx
  * H20xx :H208x
 
Available options:

 * kinematic : `small`, `large`
 * formulation : `UpdtL`, `TotaL`
 * mass_storage : `\lump_`, `coher`
 * material : `\elas_`, `elasd`, `\neoh_`, `hyper`, `hyp_d`, `J2iso`, `J2mix`, `kvisc`
 * anisotropy : `\iso__`, `ortho`
 * external_model : `\yes__`, `\no___`
 * capacity_storage : `\lump_`, `coher`
 * convection_type  : `\supg_`, `\char_`, `cente`
 * physical_type: `fluid`, `solid`
 * external_fields : list of string
 * external_vfields : list of string

Example ::

 Porous_model = pre.model(name='toto_', physics='POROx', element='Q84xx', dimension = 2, 
                          external_model='no___', kinematic='small', material='elas_',anisotropy='iso__',
                          mass_storage='coher',  physical_type = 'solid', capacity_storage='lump_',
                          convection_type = 'supg_')
