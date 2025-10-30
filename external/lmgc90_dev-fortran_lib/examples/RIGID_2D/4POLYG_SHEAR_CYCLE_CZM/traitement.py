import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mlp


file1 = './POSTPRO/BODY_0000002.DAT'
file2 = './POSTPRO/TORQUE_0000004.DAT'

M1 = []
M2 = []
with open( file1,'r') as F1, open( file2,'r')  as F2:
  for lineF1 in F1.readlines():
      M1.append( [float(x.replace('D','E')) for x in lineF1.split()] )
  for lineF2 in F2.readlines():
      M2.append( [float(y.replace('D','E')) for y in lineF2.split()] )


# convertir la liste de liste M en matrice (ndarray)
mat1 = np.array(M1)*(1000)
mat2 = np.array(M2)*(-0.5*1.e-6)

# trace de la courbe

plt.plot(mat1[:,5],mat2[:,2])
plt.xticks(family='serif')
plt.yticks(family='serif')
plt.xlabel('Deplacements (m)', fontsize='x-large', family='serif')
plt.ylabel('Force (N)', fontsize='x-large', family='serif')

plt.savefig('courbe_force_deplacement.pdf')
#plt.show()
F1.close()
F2.close()


