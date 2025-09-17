# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 17:57:56 2021

@author: mandrein
"""
import pathlib
from matplotlib import pyplot as plt

import post


_postpro=pathlib.Path('./POSTPRO')    

dep = post.get_3D_dep(_postpro,1)
fint = post.get_3D_fint(_postpro,1)

dz = dep['depz'].to_numpy()
fz = fint['fintz'].to_numpy()

Fig=plt.figure(1)

ax = plt.gca()

plt.plot(dz,fz,label='fz(uz)')

plt.xlabel('deplacement [mm]')
plt.ylabel('Force [N]')
plt.legend()
Fig.savefig('Results.png',format='png', dpi=400)
plt.show()



