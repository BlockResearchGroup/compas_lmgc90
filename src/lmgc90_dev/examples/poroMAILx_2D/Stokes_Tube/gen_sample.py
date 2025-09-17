#! /usr/bin python
# -*- coding:latin-1 -*-

###################################################################
# Importation de module complementaire
import math,os,sys
import numpy as np
###################################################################
# Importation de module complementaire perso
from pylmgc90 import pre

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de modeles
mods  = pre.models()
#   * de materiaux
mats  = pre.materials()
#   * pour les tables de visibilite
svs   = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 2D
dim = 2
# definition d'un modele diffusif couple en terme source
Porous = pre.model(name='Poro_', physics='POROx', element='Q84xx',external_model='no___',\
                   kinematic='small', material='elas_',anisotropy='iso__',\
                   mass_storage='lump_', dimension=dim, capacity_storage='lump_',\
                   physical_type = 'fluid', convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous)

# on definit le materiau constitutif du probleme
Biot = pre.material(name='biot_', materialType='PORO_ELAS', density=0.0, specific_capacity=0.0, conductivity=0.0, \
                    elas = 'standard', young = 2.0e-6, nu = 0.0, anisotropy = 'isotropic', hydro_cpl = 1.0)

# on l'ajoute dans le contenaur
mats.addMaterial(Biot)

# definition du cube, maille en hexaederes :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail1 = pre.readMesh('MESH/Mesh.msh', dim)

Fluid = pre.buildMeshedAvatar(mesh=mail1,model=Porous,material=Biot)

# ajout du cube dans le conteneur de corps
bodies += Fluid

# application des conditions aux limites :
# Axe axisymetrie
Fluid.imposeDrivenDof(group='31', component=1 , dofty='vlocy')
Fluid.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
# Entree du fluide
Fluid.imposeDrivenDof(group='7', component=2 , dofty='vlocy', description = 'evolution', evolutionFile = 'Vimp_t.txt')
Fluid.imposeDrivenDof(group='32', component=3 , dofty='vlocy')
# Surface de la conduite
Fluid.imposeDrivenDof(group='8' , component=[1,2] , dofty='vlocy')
Fluid.imposeDrivenDof(group='29', component=[1,2] , dofty='vlocy')
Fluid.imposeDrivenDof(group='30', component=[1,2] , dofty='vlocy')

# application des conditions initiales :
Fluid.imposeInitValue(group='all',component=[1,2,3] ,value=[0.,0.,0.])

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
