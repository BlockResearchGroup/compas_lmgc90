
.. py:currentmodule:: pylmgc90.pre

Material definition
===================

A material is defined using :py:class:`material`. It consists in a
name ( 5 characters), a type and a list of parameters which depends on
the type.


RIGID type:
-----------

only density is necessary ::

 tdur = pre.material(name='TDURx', materialType='RIGID', density=1000.)


ELAS type:
----------

elastic material :: 

 ma1 = pre.material(name='steel', materialType='ELAS', density=0.25e+4, 
                    elas='standard', young=0.1e+15, nu=0.2, anisotropy='isotropic')


ELAS_DILA type:
---------------

elastic material :: 

 ma1 = pre.material(name='steel', materialType='ELAS_DILA', density=0.25e+4, 
                    elas='standard', young=0.1e+15, nu=0.2, anisotropy='isotropic',
                    dilatation=1e-5, T_ref_meca=20. )


VISCO_ELAS type:
----------------

visco elastic material ::

 steel = pre.material(name='steel', materialType='VISCO_ELAS', density=8.93e3, 
                      elas='standard', anisotropy='isotropic', young=1.17e11, nu=0.35, 
                      viscous_model='KelvinVoigt', viscous_young=1.17e9, viscous_nu=0.35)  


ELAS_PLAS type:
---------------

visco elasto plastic material ::

 steel = pre.material(name='steel', materialType='ELAS_PLAS', density=8.93e3, 
                      elas='standard', anisotropy='isotropic', young=1.17e11, nu=0.35, 
                      critere='Von-Mises', isoh='linear', iso_hard=4.e8, isoh_coeff=1e8, cinh='none', visc='none')  


THERMO_ELAS type:
-----------------

thermo elastic material ::

 mat = pre.material(name='steel', materialType='THERMO_ELAS', density=1.0,
                    elas='standard', young=0.0, nu=0.0, anisotropy='isotropic', dilatation = 0.0,
                    T_ref_meca = 0.0, conductivity='field', specific_capacity='field')


PORO_ELAS type:
---------------

poro elastic material ::

 mat = pre.material(name='steel', materialType='PORO_ELAS', density=1.0,
                    elas='standard', young=0.0, nu=0.0, anisotropy='isotropic', 
                    hydro_cpl = 0.0, conductivity='field', specific_capacity='field')

DISCRETE type:
--------------

give explicitely the matrices of a 1D element  :: 

 mat = pre.material(name='steel', materialType='DISCRETE', masses=[0., 0., 0.],
                    stiffnesses=[kx, ky, kz], viscosities=[0., 0., 0.])

USER_MAT type:
--------------

It is possible to give a MatLib material file ::

 acier = pre.material(name='steel', materialType='USER_MAT', density=0., file_mat='elas.mat')

