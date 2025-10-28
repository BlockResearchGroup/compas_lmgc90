from math import *
import numpy as np
import matplotlib.pyplot as plt
import sys


if __name__ == '__main__':    

    # traction

    # dep_p,s,dep_tot
    dl=np.genfromtxt("./POSTPRO/Dep_0000001.DAT")
    dr=np.genfromtxt("./POSTPRO/Dep_0000002.DAT")    
    fl=np.genfromtxt("./POSTPRO/Fint_0000001.DAT")
    fr=np.genfromtxt("./POSTPRO/Fint_0000002.DAT")    

    plt.title('horizontal displacement - time')

    plt.xlabel(r'$t (s)$')
    plt.ylabel(r'$u_x (mm)$')

    plt.plot(dr[:,0],1e3*dr[:,1],'ro-')
    plt.plot(dl[:,0],1e3*dl[:,1],'bo-')

    plt.show()

    
    plt.subplot(1,2,1)
    plt.title('vertical total force - horizontal displacement')
    plt.xlabel(r'$u_x (mm)$')
    plt.ylabel(r'$f_z (kN)$')

    plt.plot(1e3*dr[:,1],1e-3*(fl[:,6]+fr[:,6]),'mo-')

    plt.subplot(1,2,2)
    plt.title('right horizontal force - horizontal displacement')
    plt.xlabel(r'$u_x (mm)$')
    plt.ylabel(r'$f_x (kN)$')

    plt.plot(1e3*dl[:,1],1e-3*fr[:,4],'mo-')

    plt.show()


